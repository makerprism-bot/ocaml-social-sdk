let decide ~policy (credentials : Social_core.credentials) =
  match credentials.expires_at with
  | None -> Refresh_types.Skip
  | Some expires_at ->
      (match Refresh_time.needs_refresh ~refresh_window_seconds:policy.Refresh_types.refresh_window_seconds expires_at with
       | Ok false -> Refresh_types.Skip
       | Ok true -> Refresh_types.Refresh_required
       | Error _ -> Refresh_types.Refresh_required)
