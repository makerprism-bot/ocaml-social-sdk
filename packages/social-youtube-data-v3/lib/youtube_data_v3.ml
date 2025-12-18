(** YouTube Data API v3 Provider
    
    This implementation supports YouTube Shorts uploads.
    
    - Google OAuth 2.0 with PKCE
    - Access tokens expire after 1 hour
    - Refresh tokens don't expire (unless revoked)
    - Resumable upload for videos
*)

open Social_core

(** OAuth 2.0 module for YouTube Data API v3
    
    YouTube uses Google's OAuth 2.0 infrastructure with full PKCE support.
    
    Key characteristics:
    - PKCE is supported and recommended (S256 challenge method)
    - Access tokens expire after 1 hour
    - Refresh tokens never expire (unless revoked by user)
    - Uses URL-based scopes (e.g., https://www.googleapis.com/auth/youtube.upload)
    - Requires "access_type=offline" to receive refresh token
    - Requires "prompt=consent" to force consent and receive refresh token
    
    Required environment variables (or pass directly to functions):
    - YOUTUBE_CLIENT_ID: OAuth 2.0 Client ID from Google Cloud Console
    - YOUTUBE_CLIENT_SECRET: OAuth 2.0 Client Secret
    - YOUTUBE_REDIRECT_URI: Registered callback URL
*)
module OAuth = struct
  (** Scope definitions for YouTube Data API
      Note: YouTube uses full URL-based scopes, not short names
  *)
  module Scopes = struct
    (** Scopes for reading channel/video information *)
    let read = [
      "https://www.googleapis.com/auth/youtube.readonly";
    ]
    
    (** Scopes required for video upload *)
    let write = [
      "https://www.googleapis.com/auth/youtube.upload";
      "https://www.googleapis.com/auth/youtube";
    ]
    
    (** All commonly used scopes for YouTube management *)
    let all = [
      "https://www.googleapis.com/auth/youtube";
      "https://www.googleapis.com/auth/youtube.upload";
      "https://www.googleapis.com/auth/youtube.readonly";
      "https://www.googleapis.com/auth/youtube.force-ssl";
    ]
    
    (** Operations that can be performed with YouTube API *)
    type operation = 
      | Upload_video
      | Read_channel
      | Read_videos
      | Manage_videos
    
    (** Get scopes required for specific operations *)
    let for_operations ops =
      let needs_upload = List.exists (fun o -> o = Upload_video) ops in
      let needs_read = List.exists (fun o -> o = Read_channel || o = Read_videos) ops in
      let needs_manage = List.exists (fun o -> o = Manage_videos) ops in
      (if needs_manage then ["https://www.googleapis.com/auth/youtube"] else []) @
      (if needs_upload then ["https://www.googleapis.com/auth/youtube.upload"] else []) @
      (if needs_read && not needs_manage then ["https://www.googleapis.com/auth/youtube.readonly"] else [])
  end
  
  (** Platform metadata for YouTube OAuth *)
  module Metadata = struct
    (** YouTube/Google supports PKCE with S256 method *)
    let supports_pkce = true
    
    (** YouTube supports token refresh *)
    let supports_refresh = true
    
    (** Access tokens expire after 1 hour *)
    let access_token_seconds = Some 3600
    
    (** Refresh tokens never expire (unless revoked) *)
    let refresh_token_seconds = None
    
    (** Recommended buffer before expiry (10 minutes) *)
    let refresh_buffer_seconds = 600
    
    (** Maximum retry attempts for token operations *)
    let max_refresh_attempts = 5
    
    (** Google OAuth 2.0 authorization endpoint *)
    let authorization_endpoint = "https://accounts.google.com/o/oauth2/v2/auth"
    
    (** Google OAuth 2.0 token endpoint *)
    let token_endpoint = "https://oauth2.googleapis.com/token"
    
    (** Token revocation endpoint *)
    let revocation_endpoint = "https://oauth2.googleapis.com/revoke"
    
    (** YouTube API base URL *)
    let api_base = "https://www.googleapis.com/youtube/v3"
    
    (** YouTube Upload API base URL *)
    let upload_base = "https://www.googleapis.com/upload/youtube/v3"
  end
  
  (** PKCE helper module for YouTube/Google OAuth *)
  module Pkce = struct
    (** Generate a code challenge from a code verifier using SHA256
        
        @param code_verifier The randomly generated code verifier (43-128 chars)
        @return Base64-URL-encoded SHA256 hash of the code verifier
    *)
    let generate_challenge code_verifier =
      let digest = Digestif.SHA256.digest_string code_verifier in
      let raw = Digestif.SHA256.to_raw_string digest in
      Base64.encode_string ~pad:false raw
      |> String.map (function '+' -> '-' | '/' -> '_' | c -> c)
    
    (** Code challenge method for Google OAuth *)
    let challenge_method = "S256"
  end
  
  (** Generate authorization URL for YouTube OAuth 2.0 flow with PKCE
      
      Note: Uses "access_type=offline" and "prompt=consent" to ensure
      a refresh token is returned.
      
      @param client_id Google OAuth 2.0 Client ID
      @param redirect_uri Registered callback URL
      @param state CSRF protection state parameter
      @param code_verifier PKCE code verifier (will be hashed for challenge)
      @param scopes OAuth scopes to request (defaults to Scopes.write)
      @return Full authorization URL to redirect user to
  *)
  let get_authorization_url ~client_id ~redirect_uri ~state ~code_verifier ?(scopes=Scopes.write) () =
    let code_challenge = Pkce.generate_challenge code_verifier in
    let scope_str = String.concat " " scopes in
    let params = [
      ("client_id", client_id);
      ("redirect_uri", redirect_uri);
      ("response_type", "code");
      ("scope", scope_str);
      ("state", state);
      ("access_type", "offline");  (* Required to get refresh token *)
      ("prompt", "consent");       (* Force consent to get refresh token *)
      ("code_challenge", code_challenge);
      ("code_challenge_method", Pkce.challenge_method);
    ] in
    let query = List.map (fun (k, v) ->
      Printf.sprintf "%s=%s" k (Uri.pct_encode v)
    ) params |> String.concat "&" in
    Printf.sprintf "%s?%s" Metadata.authorization_endpoint query
  
  (** Make functor for OAuth operations that need HTTP client *)
  module Make (Http : HTTP_CLIENT) = struct
    (** Exchange authorization code for access token with PKCE
        
        @param client_id Google OAuth 2.0 Client ID
        @param client_secret Google OAuth 2.0 Client Secret
        @param redirect_uri Registered callback URL
        @param code Authorization code from callback
        @param code_verifier PKCE code verifier (same as used for challenge)
        @param on_success Continuation receiving credentials
        @param on_error Continuation receiving error message
    *)
    let exchange_code ~client_id ~client_secret ~redirect_uri ~code ~code_verifier on_success on_error =
      let body = Printf.sprintf
        "grant_type=authorization_code&code=%s&redirect_uri=%s&client_id=%s&client_secret=%s&code_verifier=%s"
        (Uri.pct_encode code)
        (Uri.pct_encode redirect_uri)
        (Uri.pct_encode client_id)
        (Uri.pct_encode client_secret)
        (Uri.pct_encode code_verifier)
      in
      let headers = [
        ("Content-Type", "application/x-www-form-urlencoded");
      ] in
      
      Http.post ~headers ~body Metadata.token_endpoint
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let access_token = json |> member "access_token" |> to_string in
              let refresh_token = 
                try Some (json |> member "refresh_token" |> to_string)
                with _ -> None in
              let expires_in = 
                try json |> member "expires_in" |> to_int
                with _ -> 3600 (* Default to 1 hour *)
              in
              let expires_at = 
                let now = Ptime_clock.now () in
                match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
                | Some exp -> Some (Ptime.to_rfc3339 exp)
                | None -> None in
              let token_type = 
                try json |> member "token_type" |> to_string
                with _ -> "Bearer" in
              let creds : credentials = {
                access_token;
                refresh_token;
                expires_at;
                token_type;
              } in
              on_success creds
            with e ->
              on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Token exchange failed (%d): %s" response.status response.body))
        on_error
    
    (** Refresh access token
        
        Google access tokens expire after 1 hour. Use this to get a new
        access token using the refresh token (which never expires).
        
        Note: Google doesn't always return a new refresh token. If not
        returned, continue using the original refresh token.
        
        @param client_id Google OAuth 2.0 Client ID
        @param client_secret Google OAuth 2.0 Client Secret
        @param refresh_token The refresh token from initial auth
        @param on_success Continuation receiving refreshed credentials
        @param on_error Continuation receiving error message
    *)
    let refresh_token ~client_id ~client_secret ~refresh_token on_success on_error =
      let body = Printf.sprintf
        "grant_type=refresh_token&refresh_token=%s&client_id=%s&client_secret=%s"
        (Uri.pct_encode refresh_token)
        (Uri.pct_encode client_id)
        (Uri.pct_encode client_secret)
      in
      let headers = [
        ("Content-Type", "application/x-www-form-urlencoded");
      ] in
      
      Http.post ~headers ~body Metadata.token_endpoint
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let access_token = json |> member "access_token" |> to_string in
              (* Google doesn't always return new refresh token *)
              let new_refresh_token = 
                try Some (json |> member "refresh_token" |> to_string)
                with _ -> Some refresh_token in
              let expires_in = 
                try json |> member "expires_in" |> to_int
                with _ -> 3600
              in
              let expires_at = 
                let now = Ptime_clock.now () in
                match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
                | Some exp -> Some (Ptime.to_rfc3339 exp)
                | None -> None in
              let token_type = 
                try json |> member "token_type" |> to_string
                with _ -> "Bearer" in
              let creds : credentials = {
                access_token;
                refresh_token = new_refresh_token;
                expires_at;
                token_type;
              } in
              on_success creds
            with e ->
              on_error (Printf.sprintf "Failed to parse refresh response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Token refresh failed (%d): %s" response.status response.body))
        on_error
    
    (** Revoke access or refresh token
        
        @param token The token to revoke (access or refresh)
        @param on_success Continuation called on success
        @param on_error Continuation receiving error message
    *)
    let revoke_token ~token on_success on_error =
      let url = Printf.sprintf "%s?token=%s" 
        Metadata.revocation_endpoint (Uri.pct_encode token) in
      
      Http.post ~headers:[] ~body:"" url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            on_success ()
          else
            on_error (Printf.sprintf "Token revocation failed (%d): %s" response.status response.body))
        on_error
    
    (** Get user's channel info using access token
        
        @param access_token Valid access token
        @param on_success Continuation receiving channel info as JSON
        @param on_error Continuation receiving error message
    *)
    let get_channel_info ~access_token on_success on_error =
      let url = Printf.sprintf "%s/channels?part=snippet,statistics&mine=true" 
        Metadata.api_base in
      let headers = [
        ("Authorization", "Bearer " ^ access_token);
      ] in
      
      Http.get ~headers url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              on_success json
            with e ->
              on_error (Printf.sprintf "Failed to parse channel info: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "Get channel info failed (%d): %s" response.status response.body))
        on_error
  end
end

(** Configuration module type for YouTube provider *)
module type CONFIG = sig
  module Http : HTTP_CLIENT
  
  val get_env : string -> string option
  val get_credentials : account_id:string -> (credentials -> unit) -> (string -> unit) -> unit
  val update_credentials : account_id:string -> credentials:credentials -> (unit -> unit) -> (string -> unit) -> unit
  val encrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val decrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val update_health_status : account_id:string -> status:string -> error_message:string option -> (unit -> unit) -> (string -> unit) -> unit
end

(** Make functor to create YouTube provider with given configuration *)
module Make (Config : CONFIG) = struct
  let youtube_api_base = "https://www.googleapis.com/youtube/v3"
  let youtube_upload_base = "https://www.googleapis.com/upload/youtube/v3"
  let google_oauth_base = "https://oauth2.googleapis.com"
  
  (** {1 Platform Constants} *)
  
  let max_title_length = 100
  let max_description_length = 5000
  
  (** {1 Validation Functions} *)
  
  (** Validate a single post's content *)
  let validate_post ~text ?(media_count=0) () =
    let errors = ref [] in
    
    (* Check description length *)
    let text_len = String.length text in
    if text_len > max_description_length then
      errors := Error_types.Text_too_long { length = text_len; max = max_description_length } :: !errors;
    
    (* YouTube Shorts requires a video *)
    if media_count < 1 then
      errors := Error_types.Media_required :: !errors;
    
    if !errors = [] then Ok ()
    else Error (List.rev !errors)
  
  (** Validate thread content - YouTube only posts first item *)
  let validate_thread ~texts ~media_counts () =
    if List.length texts = 0 then
      Error [Error_types.Thread_empty]
    else
      (* Only validate first post since YouTube doesn't support threads *)
      let text = List.hd texts in
      let media_count = try List.hd media_counts with _ -> 0 in
      match validate_post ~text ~media_count () with
      | Error errs -> Error [Error_types.Thread_post_invalid { index = 0; errors = errs }]
      | Ok () -> Ok ()
  
  (** Parse API error response and return structured Error_types.error *)
  let parse_api_error ~status_code ~response_body =
    try
      let json = Yojson.Basic.from_string response_body in
      let open Yojson.Basic.Util in
      let error_msg = 
        try 
          let error = json |> member "error" in
          try error |> member "message" |> to_string
          with _ -> response_body
        with _ -> response_body
      in
      
      (* Map common YouTube/Google errors *)
      if status_code = 401 then
        Error_types.Auth_error Error_types.Token_invalid
      else if status_code = 403 then
        Error_types.Auth_error (Error_types.Insufficient_permissions ["youtube.upload"])
      else if status_code = 429 then
        Error_types.Rate_limited { 
          retry_after_seconds = Some 60;
          limit = None;
          remaining = Some 0;
          reset_at = None;
        }
      else
        Error_types.Api_error {
          status_code;
          message = error_msg;
          platform = Platform_types.YouTubeShorts;
          raw_response = Some response_body;
          request_id = None;
        }
    with _ ->
      Error_types.Api_error {
        status_code;
        message = response_body;
        platform = Platform_types.YouTubeShorts;
        raw_response = Some response_body;
        request_id = None;
      }
  
  (** Check if token is expired or expiring soon *)
  let is_token_expired_buffer ~buffer_seconds expires_at_opt =
    match expires_at_opt with
    | None -> false
    | Some expires_at_str ->
        try
          match Ptime.of_rfc3339 expires_at_str with
          | Ok (expires_at, _, _) ->
              let now = Ptime_clock.now () in
              let buffer = Ptime.Span.of_int_s buffer_seconds in
              (match Ptime.add_span now buffer with
               | Some future -> not (Ptime.is_later expires_at ~than:future)
               | None -> false)
          | Error _ -> true
        with _ -> true
  
  (** Refresh OAuth 2.0 access token *)
  let refresh_access_token ~client_id ~client_secret ~refresh_token on_success on_error =
    let url = google_oauth_base ^ "/token" in
    
    let body = Printf.sprintf
      "grant_type=refresh_token&refresh_token=%s&client_id=%s&client_secret=%s"
      (Uri.pct_encode refresh_token)
      (Uri.pct_encode client_id)
      (Uri.pct_encode client_secret)
    in
    
    let headers = [
      ("Content-Type", "application/x-www-form-urlencoded");
    ] in
    
    Config.Http.post ~headers ~body url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let open Yojson.Basic.Util in
            let json = Yojson.Basic.from_string response.body in
            let new_access = json |> member "access_token" |> to_string in
            (* Google doesn't always return new refresh token *)
            let new_refresh = 
              try json |> member "refresh_token" |> to_string
              with _ -> refresh_token
            in
            let expires_in = json |> member "expires_in" |> to_int in
            let expires_at = 
              let now = Ptime_clock.now () in
              match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
              | Some exp -> Ptime.to_rfc3339 exp
              | None -> Ptime.to_rfc3339 now
            in
            on_success (new_access, new_refresh, expires_at)
          with e ->
            on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
        else
          on_error (Printf.sprintf "Token refresh failed (%d): %s" response.status response.body))
      on_error
  
  (** Ensure valid OAuth 2.0 access token, refreshing if needed *)
  let ensure_valid_token ~account_id on_success on_error =
    Config.get_credentials ~account_id
      (fun creds ->
        (* Check if token needs refresh (10 minute buffer due to short lifetime) *)
        if is_token_expired_buffer ~buffer_seconds:600 creds.expires_at then
          (* Token expiring soon, refresh it *)
          match creds.refresh_token with
          | None ->
              Config.update_health_status ~account_id ~status:"token_expired" 
                ~error_message:(Some "No refresh token available")
                (fun () -> on_error "No refresh token available - please reconnect")
                on_error
          | Some refresh_token ->
              let client_id = Config.get_env "YOUTUBE_CLIENT_ID" |> Option.value ~default:"" in
              let client_secret = Config.get_env "YOUTUBE_CLIENT_SECRET" |> Option.value ~default:"" in
              
              refresh_access_token ~client_id ~client_secret ~refresh_token
                (fun (new_access, new_refresh, expires_at) ->
                  (* Update stored credentials *)
                  let updated_creds = {
                    access_token = new_access;
                    refresh_token = Some new_refresh;
                    expires_at = Some expires_at;
                    token_type = "Bearer";
                  } in
                  Config.update_credentials ~account_id ~credentials:updated_creds
                    (fun () ->
                      Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
                        (fun () -> on_success new_access)
                        on_error)
                    on_error)
                (fun err ->
                  Config.update_health_status ~account_id ~status:"refresh_failed" 
                    ~error_message:(Some err)
                    (fun () -> on_error err)
                    on_error)
        else
          (* Token still valid *)
          Config.update_health_status ~account_id ~status:"healthy" ~error_message:None
            (fun () -> on_success creds.access_token)
            on_error)
      on_error
  
  (** Upload video to YouTube Shorts *)
  let post_single ~account_id ~text ~media_urls ?(alt_texts=[]) on_result =
    let _ = alt_texts in (* YouTube doesn't support alt text for videos *)
    let media_count = List.length media_urls in
    match validate_post ~text ~media_count () with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        (* Validation ensures media_urls is non-empty *)
        let video_url = List.hd media_urls in
        ensure_valid_token ~account_id
          (fun access_token ->
            (* Download video *)
            Config.Http.get ~headers:[] video_url
              (fun video_response ->
                if video_response.status >= 200 && video_response.status < 300 then
                  let video_data = video_response.body in
                  let content_type = 
                    match List.assoc_opt "content-type" video_response.headers with
                    | Some ct -> ct
                    | None -> "video/mp4"
                  in
                  
                  (* Step 1: Initialize resumable upload with metadata *)
                  let video_metadata = `Assoc [
                    ("snippet", `Assoc [
                      ("title", `String (String.sub text 0 (min (String.length text) max_title_length)));
                      ("description", `String (text ^ " #Shorts"));
                      ("tags", `List [`String "shorts"; `String "short"]);
                      ("categoryId", `String "22"); (* People & Blogs *)
                    ]);
                    ("status", `Assoc [
                      ("privacyStatus", `String "public");
                      ("selfDeclaredMadeForKids", `Bool false);
                    ]);
                  ] in
                  
                  let init_url = youtube_upload_base ^ "/videos?uploadType=resumable&part=snippet,status" in
                  let init_headers = [
                    ("Authorization", "Bearer " ^ access_token);
                    ("Content-Type", "application/json");
                    ("X-Upload-Content-Length", string_of_int (String.length video_data));
                    ("X-Upload-Content-Type", content_type);
                  ] in
                  
                  let metadata_str = Yojson.Basic.to_string video_metadata in
                  
                  Config.Http.post ~headers:init_headers ~body:metadata_str init_url
                    (fun init_response ->
                      if init_response.status >= 200 && init_response.status < 300 then
                        (* Get upload URL from Location header *)
                        match List.assoc_opt "location" init_response.headers with
                        | Some upload_url ->
                            (* Step 2: Upload video data to resumable URL *)
                            let upload_headers = [
                              ("Content-Type", content_type);
                              ("Content-Length", string_of_int (String.length video_data));
                            ] in
                            
                            Config.Http.put ~headers:upload_headers ~body:video_data upload_url
                              (fun upload_response ->
                                if upload_response.status >= 200 && upload_response.status < 300 then
                                  try
                                    let open Yojson.Basic.Util in
                                    let json = Yojson.Basic.from_string upload_response.body in
                                    let video_id = json |> member "id" |> to_string in
                                    on_result (Error_types.Success video_id)
                                  with _e ->
                                    on_result (Error_types.Failure (Error_types.Internal_error (Printf.sprintf "Failed to parse response: %s" upload_response.body)))
                                else
                                  on_result (Error_types.Failure (parse_api_error ~status_code:upload_response.status ~response_body:upload_response.body)))
                              (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err)))
                        | None ->
                            on_result (Error_types.Failure (Error_types.Internal_error (Printf.sprintf "No upload URL in response: %s" init_response.body)))
                      else
                        on_result (Error_types.Failure (parse_api_error ~status_code:init_response.status ~response_body:init_response.body)))
                    (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err)))
                else
                  on_result (Error_types.Failure (Error_types.Internal_error (Printf.sprintf "Failed to download video (%d)" video_response.status))))
              (fun err -> on_result (Error_types.Failure (Error_types.Internal_error err))))
          (fun err -> on_result (Error_types.Failure (Error_types.Auth_error (Error_types.Refresh_failed err))))
  
  (** Post thread (YouTube doesn't support threads, posts only first item) *)
  let post_thread ~account_id ~texts ~media_urls_per_post ?(alt_texts_per_post=[]) on_result =
    let _ = alt_texts_per_post in
    let media_counts = List.map List.length media_urls_per_post in
    match validate_thread ~texts ~media_counts () with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        (* YouTube only supports single videos, so we post just the first *)
        let first_text = List.hd texts in
        let first_media = List.hd media_urls_per_post in
        let total_requested = List.length texts in
        post_single ~account_id ~text:first_text ~media_urls:first_media
          (fun outcome ->
            match outcome with
            | Error_types.Success video_id ->
                let thread_result = {
                  Error_types.posted_ids = [video_id];
                  failed_at_index = None;
                  total_requested;
                } in
                if total_requested > 1 then
                  on_result (Error_types.Partial_success {
                    result = thread_result;
                    warnings = [Error_types.Generic_warning { 
                      code = "youtube_no_threads"; 
                      message = Printf.sprintf "YouTube does not support threads. Only first of %d items posted." total_requested;
                      recoverable = false 
                    }]
                  })
                else
                  on_result (Error_types.Success thread_result)
            | Error_types.Partial_success { result = video_id; warnings } ->
                let thread_result = {
                  Error_types.posted_ids = [video_id];
                  failed_at_index = None;
                  total_requested;
                } in
                on_result (Error_types.Partial_success { result = thread_result; warnings })
            | Error_types.Failure err ->
                on_result (Error_types.Failure err))
  
  (** OAuth authorization URL with PKCE *)
  let get_oauth_url ~redirect_uri ~state ~code_verifier on_success on_error =
    let client_id = Config.get_env "YOUTUBE_CLIENT_ID" |> Option.value ~default:"" in
    
    if client_id = "" then
      on_error "YouTube client ID not configured"
    else (
      (* Generate code_challenge from code_verifier using SHA256 *)
      let code_challenge = 
        let digest = Digestif.SHA256.digest_string code_verifier in
        let raw = Digestif.SHA256.to_raw_string digest in
        Base64.encode_string ~pad:false raw
        |> String.map (function '+' -> '-' | '/' -> '_' | c -> c)
      in
      
      let scopes = "https://www.googleapis.com/auth/youtube.upload https://www.googleapis.com/auth/youtube" in
      
      let params = [
        ("client_id", client_id);
        ("redirect_uri", redirect_uri);
        ("response_type", "code");
        ("scope", scopes);
        ("state", state);
        ("access_type", "offline"); (* Get refresh token *)
        ("prompt", "consent"); (* Force consent to get refresh token *)
        ("code_challenge", code_challenge);
        ("code_challenge_method", "S256");
      ] in
      
      let query = List.map (fun (k, v) ->
        Printf.sprintf "%s=%s" k (Uri.pct_encode v)
      ) params |> String.concat "&" in
      
      let url = google_oauth_base ^ "/auth?" ^ query in
      on_success url
    )
  
  (** Exchange OAuth code for access token with PKCE *)
  let exchange_code ~code ~redirect_uri ~code_verifier on_success on_error =
    let client_id = Config.get_env "YOUTUBE_CLIENT_ID" |> Option.value ~default:"" in
    let client_secret = Config.get_env "YOUTUBE_CLIENT_SECRET" |> Option.value ~default:"" in
    
    if client_id = "" || client_secret = "" then
      on_error "YouTube OAuth credentials not configured"
    else (
      let url = google_oauth_base ^ "/token" in
      let body = Printf.sprintf
        "grant_type=authorization_code&code=%s&redirect_uri=%s&client_id=%s&client_secret=%s&code_verifier=%s"
        (Uri.pct_encode code)
        (Uri.pct_encode redirect_uri)
        (Uri.pct_encode client_id)
        (Uri.pct_encode client_secret)
        (Uri.pct_encode code_verifier)
      in
      
      let headers = [
        ("Content-Type", "application/x-www-form-urlencoded");
      ] in
      
      Config.Http.post ~headers ~body url
        (fun response ->
          if response.status >= 200 && response.status < 300 then
            try
              let json = Yojson.Basic.from_string response.body in
              let open Yojson.Basic.Util in
              let access_token = json |> member "access_token" |> to_string in
              let refresh_token = 
                try Some (json |> member "refresh_token" |> to_string)
                with _ -> None
              in
              let expires_in = json |> member "expires_in" |> to_int in
              let expires_at = 
                let now = Ptime_clock.now () in
                match Ptime.add_span now (Ptime.Span.of_int_s expires_in) with
                | Some exp -> Ptime.to_rfc3339 exp
                | None -> Ptime.to_rfc3339 now
              in
              let credentials = {
                access_token;
                refresh_token;
                expires_at = Some expires_at;
                token_type = "Bearer";
              } in
              on_success credentials
            with e ->
              on_error (Printf.sprintf "Failed to parse token response: %s" (Printexc.to_string e))
          else
            on_error (Printf.sprintf "OAuth exchange failed (%d): %s" response.status response.body))
        on_error
    )
  
  (** Validate content length *)
  let validate_content ~text =
    let len = String.length text in
    if len = 0 then
      Error "Text cannot be empty"
    else if len > 5000 then
      Error (Printf.sprintf "YouTube description should be under 5000 characters (current: %d)" len)
    else
      Ok ()
end
