type refresh_policy = {
  refresh_window_seconds: int;
}

type refresh_decision =
  | Skip
  | Refresh_required

type refresh_error =
  | Credential_load_failed of string
  | Missing_refresh_token
  | Expiry_parse_failed of string
  | Refresh_request_failed of Error_types.error
  | Credential_persist_failed of string
  | Health_update_failed of string

type refresh_outcome =
  | Used_existing_token of Social_core.credentials
  | Refreshed of Social_core.credentials
  | Refresh_failed of refresh_error

let default_policy = {
  refresh_window_seconds = 1800;
}

let refresh_error_to_string = function
  | Credential_load_failed msg -> "credential load failed: " ^ msg
  | Missing_refresh_token -> "missing refresh token"
  | Expiry_parse_failed msg -> "expiry parse failed: " ^ msg
  | Refresh_request_failed err -> Error_types.error_to_string err
  | Credential_persist_failed msg -> "credential persist failed: " ^ msg
  | Health_update_failed msg -> "health update failed: " ^ msg
