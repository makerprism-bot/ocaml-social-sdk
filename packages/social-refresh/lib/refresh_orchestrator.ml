let merge_refreshed_credentials ~(current : Social_core.credentials) ~(refreshed : Social_core.credentials) =
  let refresh_token =
    match refreshed.Social_core.refresh_token with
    | Some token when String.trim token <> "" -> Some token
    | _ -> current.Social_core.refresh_token
  in
  {
    refreshed with
    Social_core.refresh_token;
  }

let ensure_valid_access_token
    ?(policy = Refresh_types.default_policy)
    ?(map_load_error = fun err -> Error_types.Network_error (Error_types.Connection_failed err))
    ?(map_persist_error = fun err -> Error_types.Network_error (Error_types.Connection_failed err))
    ?(map_health_error = fun err -> Error_types.Network_error (Error_types.Connection_failed err))
    ~account_id
    ~load_credentials
    ~perform_refresh
    ~persist_credentials
    ~update_health
    on_success
    on_error =
  let fail_health status error_message mapped_error =
    update_health ~account_id ~status ~error_message:(Some error_message)
      (fun () -> on_error mapped_error)
      (fun err -> on_error (map_health_error err))
  in
  load_credentials ~account_id
    (fun credentials ->
       match Refresh_decision.decide ~policy credentials with
       | Refresh_types.Skip ->
           update_health ~account_id ~status:"healthy" ~error_message:None
             (fun () -> on_success credentials)
             (fun err -> on_error (map_health_error err))
       | Refresh_types.Refresh_not_possible err ->
           fail_health "token_expired" (Error_types.error_to_string err) err
       | Refresh_types.Refresh_required ->
           perform_refresh ~credentials
             (fun refreshed_credentials ->
                let merged = merge_refreshed_credentials ~current:credentials ~refreshed:refreshed_credentials in
                persist_credentials ~account_id ~credentials:merged
                  (fun () ->
                     update_health ~account_id ~status:"healthy" ~error_message:None
                       (fun () -> on_success merged)
                       (fun err -> on_error (map_health_error err)))
                  (fun err ->
                     on_error (map_persist_error err)))
             (fun err ->
                match err with
                | Error_types.Auth_error Error_types.Missing_credentials ->
                    fail_health "token_expired" "No refresh token available" err
                | _ ->
                    fail_health "refresh_failed" (Error_types.error_to_string err) err))
    (fun err ->
       on_error (map_load_error err))
