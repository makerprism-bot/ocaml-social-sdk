let parse_rfc3339 rfc3339 =
  match Ptime.of_rfc3339 rfc3339 with
  | Ok (time, _, _) -> Ok time
  | Error _ -> Error ("Invalid RFC3339 timestamp: " ^ rfc3339)

let expires_within_window ~refresh_window_seconds ~now expires_at =
  let window = Ptime.Span.of_int_s refresh_window_seconds in
  match Ptime.add_span now window with
  | Some threshold -> not (Ptime.is_later expires_at ~than:threshold)
  | None -> false

let needs_refresh ~refresh_window_seconds expires_at =
  match parse_rfc3339 expires_at with
  | Error err -> Error err
  | Ok expires_at_ptime ->
      let now = Ptime_clock.now () in
      Ok (expires_within_window ~refresh_window_seconds ~now expires_at_ptime)
