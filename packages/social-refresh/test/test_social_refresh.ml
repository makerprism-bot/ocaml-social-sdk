let rfc3339_in_seconds seconds =
  let now = Ptime_clock.now () in
  let target =
    match Ptime.add_span now (Ptime.Span.of_int_s seconds) with
    | Some t -> t
    | None -> now
  in
  Ptime.to_rfc3339 target

let make_credentials ?refresh_token ?expires_at access_token =
  {
    Social_core.access_token;
    refresh_token;
    expires_at;
    token_type = "Bearer";
  }

let test_refresh_time_boundaries () =
  let fresh = Social_refresh.Time.needs_refresh ~refresh_window_seconds:1800 (rfc3339_in_seconds 2000) in
  let near = Social_refresh.Time.needs_refresh ~refresh_window_seconds:1800 (rfc3339_in_seconds 1700) in
  let equal = Social_refresh.Time.needs_refresh ~refresh_window_seconds:1800 (rfc3339_in_seconds 1800) in
  assert (fresh = Ok false);
  assert (near = Ok true);
  assert (equal = Ok true);
  print_endline "✓ refresh_time boundaries"

let test_refresh_time_malformed_timestamp () =
  match Social_refresh.Time.needs_refresh ~refresh_window_seconds:1800 "not-a-timestamp" with
  | Ok _ -> failwith "Malformed timestamp should fail"
  | Error _ -> print_endline "✓ refresh_time malformed timestamp"

let test_refresh_decision_engine () =
  let policy = Social_refresh.default_policy in
  let no_expiry = make_credentials "a" in
  let fresh = make_credentials ~expires_at:(rfc3339_in_seconds 4000) "b" in
  let stale = make_credentials ~expires_at:(rfc3339_in_seconds 10) "c" in
  assert (Social_refresh.Decision.decide ~policy no_expiry = Social_refresh.Skip);
  assert (Social_refresh.Decision.decide ~policy fresh = Social_refresh.Skip);
  assert (Social_refresh.Decision.decide ~policy stale = Social_refresh.Refresh_required);
  print_endline "✓ refresh_decision skip/refresh_required"

let test_orchestrator_missing_refresh_token () =
  let statuses = ref [] in
  let result = ref None in
  let load_credentials ~account_id:_ on_success _on_error =
    on_success (make_credentials ~expires_at:(rfc3339_in_seconds 5) "expired")
  in
  let perform_refresh ~credentials:_ _on_success on_error =
    on_error (Error_types.Auth_error Error_types.Missing_credentials)
  in
  let persist_credentials ~account_id:_ ~credentials:_ on_success _on_error = on_success () in
  let update_health ~account_id:_ ~status ~error_message:_ on_success _on_error =
    statuses := status :: !statuses;
    on_success ()
  in
  Social_refresh.Orchestrator.ensure_valid_access_token
    ~account_id:"acct"
    ~load_credentials
    ~perform_refresh
    ~persist_credentials
    ~update_health
    (fun _ -> result := Some (Ok ()))
    (fun err -> result := Some (Error err));
  (match !result with
   | Some (Error (Error_types.Auth_error Error_types.Missing_credentials)) -> ()
   | _ -> failwith "Expected Missing_credentials");
  assert (List.mem "token_expired" !statuses);
  print_endline "✓ orchestrator missing refresh token"

let test_orchestrator_refresh_failure_mapping () =
  let statuses = ref [] in
  let result = ref None in
  let load_credentials ~account_id:_ on_success _on_error =
    on_success (make_credentials ~expires_at:(rfc3339_in_seconds 5) ~refresh_token:"bad" "expired")
  in
  let perform_refresh ~credentials:_ _on_success on_error =
    on_error (Error_types.Auth_error (Error_types.Refresh_failed "invalid refresh token"))
  in
  let persist_credentials ~account_id:_ ~credentials:_ on_success _on_error = on_success () in
  let update_health ~account_id:_ ~status ~error_message:_ on_success _on_error =
    statuses := status :: !statuses;
    on_success ()
  in
  Social_refresh.Orchestrator.ensure_valid_access_token
    ~account_id:"acct"
    ~load_credentials
    ~perform_refresh
    ~persist_credentials
    ~update_health
    (fun _ -> result := Some (Ok ()))
    (fun err -> result := Some (Error err));
  (match !result with
   | Some (Error (Error_types.Auth_error (Error_types.Refresh_failed _))) -> ()
   | _ -> failwith "Expected Refresh_failed");
  assert (List.mem "refresh_failed" !statuses);
  print_endline "✓ orchestrator refresh failure mapping"

let test_orchestrator_preserves_refresh_token_when_missing_in_response () =
  let persisted = ref None in
  let result = ref None in
  let load_credentials ~account_id:_ on_success _on_error =
    on_success (make_credentials ~expires_at:(rfc3339_in_seconds 5) ~refresh_token:"old_refresh" "expired")
  in
  let perform_refresh ~credentials:_ on_success _on_error =
    on_success (make_credentials ~expires_at:(rfc3339_in_seconds 3600) "new_access")
  in
  let persist_credentials ~account_id:_ ~credentials on_success _on_error =
    persisted := Some credentials;
    on_success ()
  in
  let update_health ~account_id:_ ~status:_ ~error_message:_ on_success _on_error = on_success () in
  Social_refresh.Orchestrator.ensure_valid_access_token
    ~account_id:"acct"
    ~load_credentials
    ~perform_refresh
    ~persist_credentials
    ~update_health
    (fun credentials -> result := Some (Ok credentials))
    (fun err -> result := Some (Error err));
  (match !result with
   | Some (Ok credentials) ->
       assert (credentials.Social_core.access_token = "new_access");
       assert (credentials.Social_core.refresh_token = Some "old_refresh")
   | _ -> failwith "Expected refreshed credentials");
  (match !persisted with
   | Some credentials -> assert (credentials.Social_core.refresh_token = Some "old_refresh")
   | None -> failwith "Expected persisted credentials");
  print_endline "✓ orchestrator refresh token preservation"

let test_orchestrator_health_transitions () =
  let statuses = ref [] in
  let result = ref None in
  let load_credentials ~account_id:_ on_success _on_error =
    on_success (make_credentials ~expires_at:(rfc3339_in_seconds 5000) "valid_access")
  in
  let perform_refresh ~credentials:_ _on_success on_error =
    on_error (Error_types.Auth_error (Error_types.Refresh_failed "should not be called"))
  in
  let persist_credentials ~account_id:_ ~credentials:_ on_success _on_error = on_success () in
  let update_health ~account_id:_ ~status ~error_message:_ on_success _on_error =
    statuses := status :: !statuses;
    on_success ()
  in
  Social_refresh.Orchestrator.ensure_valid_access_token
    ~account_id:"acct"
    ~load_credentials
    ~perform_refresh
    ~persist_credentials
    ~update_health
    (fun _ -> result := Some (Ok ()))
    (fun err -> result := Some (Error err));
  (match !result with
   | Some (Ok ()) -> ()
   | _ -> failwith "Expected healthy pass-through");
  assert (List.mem "healthy" !statuses);
  print_endline "✓ orchestrator healthy transition"

let () =
  test_refresh_time_boundaries ();
  test_refresh_time_malformed_timestamp ();
  test_refresh_decision_engine ();
  test_orchestrator_missing_refresh_token ();
  test_orchestrator_refresh_failure_mapping ();
  test_orchestrator_preserves_refresh_token_when_missing_in_response ();
  test_orchestrator_health_transitions ();
  print_endline "social-refresh tests passed"
