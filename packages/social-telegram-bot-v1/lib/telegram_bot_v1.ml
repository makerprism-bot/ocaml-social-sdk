open Social_core

let telegram_api_base = "https://api.telegram.org"
let max_text_length = 4096
let max_caption_length = 1024
let max_media_items = 1

let bot_token_pattern = Str.regexp "[0-9][0-9]+:[A-Za-z0-9_-]+"
let bearer_pattern = Str.regexp {|Bearer[ \t]+[A-Za-z0-9._~+/=-]+|}

let string_contains haystack needle =
  try
    ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
    true
  with Not_found -> false

let redact_sensitive_text ?token text =
  let redacted_token =
    match token with
    | Some value when String.trim value <> "" ->
        Str.global_replace (Str.regexp_string value) "[REDACTED_TOKEN]" text
    | _ -> text
  in
  redacted_token
  |> Str.global_replace bot_token_pattern "[REDACTED_TOKEN]"
  |> Str.global_replace bearer_pattern "Bearer [REDACTED]"

let find_header headers key =
  let target = String.lowercase_ascii key in
  headers
  |> List.find_map (fun (k, v) ->
         if String.lowercase_ascii k = target then Some v else None)

let parse_retry_after headers =
  match find_header headers "retry-after" with
  | Some raw ->
      (match int_of_string_opt (String.trim raw) with
       | Some value when value >= 0 -> Some value
       | _ -> None)
  | None -> None

let is_http_url value =
  let lower = String.lowercase_ascii (String.trim value) in
  String.starts_with ~prefix:"https://" lower
  || String.starts_with ~prefix:"http://" lower

let parse_message_id json =
  let open Yojson.Basic.Util in
  let result = json |> member "result" in
  match result |> member "message_id" with
  | `Int i -> Some (string_of_int i)
  | `String s when String.trim s <> "" -> Some (String.trim s)
  | _ -> None

let parse_error_code json =
  let open Yojson.Basic.Util in
  try json |> member "error_code" |> to_int with _ -> 0

let parse_error_description json =
  let open Yojson.Basic.Util in
  try json |> member "description" |> to_string with _ -> "Telegram API error"

let parse_retry_after_from_payload json =
  let open Yojson.Basic.Util in
  try Some (json |> member "parameters" |> member "retry_after" |> to_int) with _ -> None

let is_negative_numeric value =
  match int_of_string_opt (String.trim value) with
  | Some n when n < 0 -> true
  | _ -> false

let is_positive_numeric value =
  match int_of_string_opt (String.trim value) with
  | Some n when n > 0 -> true
  | _ -> false

let is_probable_dm_target target =
  let trimmed = String.trim target in
  trimmed <> ""
  && not (String.starts_with ~prefix:"@" trimmed)
  && is_positive_numeric trimmed

let thread_warning_from_error err =
  Error_types.Generic_warning {
    code = "thread_incomplete";
    message = Error_types.error_to_string err;
    recoverable = false;
  }

type media_route =
  | Send_photo
  | Send_video

let media_route_of_url media_url =
  let lowered = String.lowercase_ascii (String.trim media_url) in
  let path =
    try Uri.path (Uri.of_string lowered) with _ -> lowered
  in
  if Filename.check_suffix path ".jpg"
     || Filename.check_suffix path ".jpeg"
     || Filename.check_suffix path ".png"
     || Filename.check_suffix path ".webp"
     || Filename.check_suffix path ".gif"
  then Some Send_photo
  else if Filename.check_suffix path ".mp4"
          || Filename.check_suffix path ".mov"
          || Filename.check_suffix path ".webm"
          || Filename.check_suffix path ".mkv"
  then Some Send_video
  else None

module type CONFIG = sig
  module Http : HTTP_CLIENT

  val get_env : string -> string option
  val get_credentials : account_id:string -> (credentials -> unit) -> (string -> unit) -> unit
  val update_credentials : account_id:string -> credentials:credentials -> (unit -> unit) -> (string -> unit) -> unit
  val encrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val decrypt : string -> (string -> unit) -> (string -> unit) -> unit
  val update_health_status : account_id:string -> status:string -> error_message:string option -> (unit -> unit) -> (string -> unit) -> unit

  val get_chat_id : account_id:string -> target:string -> (string -> unit) -> (string -> unit) -> unit
end

module Make (Config : CONFIG) = struct
  let platform = Platform_types.Telegram

  let set_health ~account_id ~status ~error_message =
    Config.update_health_status ~account_id ~status ~error_message
      (fun () -> ())
      (fun _ -> ())

  let update_health_for_error ~account_id = function
    | Error_types.Auth_error Error_types.Token_invalid ->
        set_health ~account_id ~status:"token_invalid" ~error_message:(Some "Telegram token is invalid")
    | Error_types.Auth_error (Error_types.Insufficient_permissions _) ->
        set_health ~account_id ~status:"permission_denied" ~error_message:(Some "Telegram bot lacks channel/group posting permissions")
    | _ -> ()

  let parse_api_error ~token ~status_code ~headers ~response_body =
    let sanitized_body = redact_sensitive_text ~token response_body in
    let parse_generic_api_error message =
      Error_types.Api_error {
        status_code;
        message = redact_sensitive_text ~token message;
        platform;
        raw_response = Some sanitized_body;
        request_id = None;
      }
    in
    if status_code = 401 then
      Error_types.Auth_error Error_types.Token_invalid
    else if status_code = 403 then
      Error_types.Auth_error (Error_types.Insufficient_permissions ["channel_or_group_posting"])
    else if status_code = 429 then
      let retry_after_from_payload =
        try
          let json = Yojson.Basic.from_string response_body in
          parse_retry_after_from_payload json
        with _ -> None
      in
      Error_types.Rate_limited {
        retry_after_seconds =
          (match retry_after_from_payload with
           | Some value -> Some value
           | None -> parse_retry_after headers);
        limit = None;
        remaining = Some 0;
        reset_at = None;
      }
    else
      try
        let json = Yojson.Basic.from_string response_body in
        let payload_code = parse_error_code json in
        let description = parse_error_description json |> redact_sensitive_text ~token in
        let retry_after =
          match parse_retry_after_from_payload json with
          | Some value -> Some value
          | None -> parse_retry_after headers
        in
        match payload_code with
        | 401 -> Error_types.Auth_error Error_types.Token_invalid
        | 403 -> Error_types.Auth_error (Error_types.Insufficient_permissions ["channel_or_group_posting"])
        | 429 ->
            Error_types.Rate_limited {
              retry_after_seconds = retry_after;
              limit = None;
              remaining = Some 0;
              reset_at = None;
            }
        | code when code >= 500 || status_code >= 500 -> parse_generic_api_error description
        | _ -> parse_generic_api_error description
      with _ ->
        parse_generic_api_error sanitized_body

  let with_access_token ~account_id on_success on_error =
    Config.get_credentials ~account_id
      (fun creds ->
        let token = String.trim creds.access_token in
        if token = "" then
          let err = Error_types.Auth_error Error_types.Missing_credentials in
          update_health_for_error ~account_id err;
          on_error err
        else if not (string_contains token ":") then
          let err = Error_types.Auth_error Error_types.Token_invalid in
          update_health_for_error ~account_id err;
          on_error err
        else
          on_success token)
      (fun _ ->
        let err = Error_types.Auth_error Error_types.Missing_credentials in
        update_health_for_error ~account_id err;
        on_error err)

  let resolve_chat_id ~account_id ~target on_result =
    let normalized_target = String.trim target in
    let target_to_resolve = if normalized_target = "" then "default" else normalized_target in
    if is_probable_dm_target target_to_resolve then
      on_result
        (Error
           (Error_types.Validation_error
              [ Error_types.Invalid_url "Telegram connector supports broadcast targets only (channels/groups), not direct-message chat IDs" ]))
    else if is_negative_numeric target_to_resolve then
      on_result (Ok target_to_resolve)
    else
      Config.get_chat_id ~account_id ~target:target_to_resolve
        (fun resolved ->
          if is_probable_dm_target resolved then
            on_result
              (Error
                 (Error_types.Validation_error
                    [ Error_types.Invalid_url "Resolved chat ID appears to be a direct-message target; only channels/groups are allowed" ]))
          else
            on_result (Ok (String.trim resolved)))
        (fun err ->
          on_result (Error (Error_types.Resource_not_found (redact_sensitive_text err))))

  let validate_post ~text ~media_count =
    let errors = ref [] in
    let text_length = String.length text in
    if String.trim text = "" && media_count = 0 then
      errors := Error_types.Text_empty :: !errors;
    if media_count = 0 && text_length > max_text_length then
      errors := Error_types.Text_too_long { length = text_length; max = max_text_length } :: !errors;
    if media_count > 0 && text_length > max_caption_length then
      errors := Error_types.Text_too_long { length = text_length; max = max_caption_length } :: !errors;
    if media_count > max_media_items then
      errors := Error_types.Too_many_media { count = media_count; max = max_media_items } :: !errors;
    if !errors = [] then Ok () else Error (List.rev !errors)

  let validate_thread ~texts ~media_urls_per_post =
    if texts = [] then Error [ Error_types.Thread_empty ]
    else if List.length texts <> List.length media_urls_per_post then
      let text_count = List.length texts in
      let media_count = List.length media_urls_per_post in
      Error
        [ Error_types.Thread_post_invalid {
            index = min text_count media_count;
            errors =
              [ Error_types.Invalid_url
                  (Printf.sprintf
                     "post_thread input mismatch: texts has %d items but media_urls_per_post has %d items"
                     text_count
                     media_count) ];
          } ]
    else
      let errors =
        List.mapi
          (fun i text ->
            let media_count = List.nth media_urls_per_post i |> List.length in
            match validate_post ~text ~media_count with
            | Ok () -> None
            | Error post_errors ->
                Some (Error_types.Thread_post_invalid { index = i; errors = post_errors }))
          texts
        |> List.filter_map (fun x -> x)
      in
      if errors = [] then Ok () else Error errors

  let post_telegram_request ~account_id ~token ~method_name ~payload on_result =
    let url = Printf.sprintf "%s/bot%s/%s" telegram_api_base token method_name in
    let headers = [ ("Content-Type", "application/json") ] in
    let body = Yojson.Basic.to_string (`Assoc payload) in
    Config.Http.post ~headers ~body url
      (fun response ->
        if response.status >= 200 && response.status < 300 then
          try
            let json = Yojson.Basic.from_string response.body in
            let open Yojson.Basic.Util in
            let ok = try json |> member "ok" |> to_bool with _ -> false in
            if ok then
              (match parse_message_id json with
               | Some message_id ->
                   set_health ~account_id ~status:"healthy" ~error_message:None;
                   on_result (Error_types.Success message_id)
               | None ->
                   on_result
                     (Error_types.Failure
                        (Error_types.Internal_error
                           "Malformed Telegram success payload: missing result.message_id")))
            else
              let err =
                parse_api_error
                  ~token
                  ~status_code:response.status
                  ~headers:response.headers
                  ~response_body:response.body
              in
              update_health_for_error ~account_id err;
              on_result (Error_types.Failure err)
          with _ ->
            let err = Error_types.Internal_error "Malformed Telegram response payload" in
            on_result (Error_types.Failure err)
        else
          let err =
            parse_api_error
              ~token
              ~status_code:response.status
              ~headers:response.headers
              ~response_body:response.body
          in
          update_health_for_error ~account_id err;
          on_result (Error_types.Failure err))
      (fun network_err ->
        let err =
          Error_types.Network_error
            (Error_types.Connection_failed (redact_sensitive_text ~token network_err))
        in
        on_result (Error_types.Failure err))

  let post_single
      ~account_id
      ~text
      ~media_urls
      ?(alt_texts = [])
      ?(target = "default")
      on_result =
    let _ = alt_texts in
    let media_count = List.length media_urls in
    match validate_post ~text ~media_count with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        let media_validation_error =
          match media_urls with
          | [] -> None
          | [ media_url ] ->
              if not (is_http_url media_url) then
                Some (Error_types.Invalid_url media_url)
              else
                (match media_route_of_url media_url with
                 | Some _ -> None
                 | None -> Some (Error_types.Media_unsupported_format media_url))
          | _ :: _ -> Some (Error_types.Too_many_media { count = List.length media_urls; max = 1 })
        in
        (match media_validation_error with
         | Some err -> on_result (Error_types.Failure (Error_types.Validation_error [ err ]))
         | None ->
             resolve_chat_id ~account_id ~target
               (function
                 | Error err -> on_result (Error_types.Failure err)
                 | Ok chat_id ->
                     with_access_token ~account_id
                       (fun token ->
                         let payload =
                           match media_urls with
                           | [] ->
                               [ ("chat_id", `String chat_id);
                                 ("text", `String text) ]
                           | [ media_url ] ->
                               (match media_route_of_url media_url with
                                | Some Send_photo ->
                                    [ ("chat_id", `String chat_id);
                                      ("photo", `String media_url) ]
                                    @
                                    (if String.trim text = "" then []
                                     else [ ("caption", `String text) ])
                                | Some Send_video ->
                                    [ ("chat_id", `String chat_id);
                                      ("video", `String media_url) ]
                                    @
                                    (if String.trim text = "" then []
                                     else [ ("caption", `String text) ])
                                | None -> [])
                           | _ -> []
                         in
                         let method_name =
                           match media_urls with
                           | [] -> "sendMessage"
                           | [ media_url ] ->
                               (match media_route_of_url media_url with
                                | Some Send_photo -> "sendPhoto"
                                | Some Send_video -> "sendVideo"
                                | None -> "sendMessage")
                           | _ -> "sendMessage"
                         in
                         post_telegram_request ~account_id ~token ~method_name ~payload on_result)
                       (fun err -> on_result (Error_types.Failure err))))

  let post_thread
      ~account_id
      ~texts
      ~media_urls_per_post
      ?(alt_texts_per_post = [])
      ?(target = "default")
      on_result =
    let _ = alt_texts_per_post in
    match validate_thread ~texts ~media_urls_per_post with
    | Error errs -> on_result (Error_types.Failure (Error_types.Validation_error errs))
    | Ok () ->
        let total_requested = List.length texts in
        let rec loop index acc posted_texts posted_media =
          match posted_texts, posted_media with
          | [], _ | _, [] ->
              on_result
                (Error_types.Success {
                   Error_types.posted_ids = List.rev acc;
                   failed_at_index = None;
                   total_requested;
                 })
          | text :: rest_texts, media_urls :: rest_media ->
              post_single ~account_id ~text ~media_urls ~target
                (function
                  | Error_types.Success post_id ->
                      loop (index + 1) (post_id :: acc) rest_texts rest_media
                  | Error_types.Partial_success { result = post_id; warnings } ->
                      let thread_result = {
                        Error_types.posted_ids = List.rev (post_id :: acc);
                        failed_at_index = None;
                        total_requested;
                      } in
                      on_result (Error_types.Partial_success { result = thread_result; warnings })
                  | Error_types.Failure err ->
                      let thread_result = {
                        Error_types.posted_ids = List.rev acc;
                        failed_at_index = Some index;
                        total_requested;
                      } in
                      if acc = [] then on_result (Error_types.Failure err)
                      else
                        on_result
                          (Error_types.Partial_success {
                             result = thread_result;
                             warnings = [ thread_warning_from_error err ];
                           }))
        in
        loop 0 [] texts media_urls_per_post

  let validate_access ?(target = "default") ~account_id on_result =
    resolve_chat_id ~account_id ~target
      (function
        | Error err -> on_result (Error_types.Failure err)
        | Ok _chat_id ->
            with_access_token ~account_id
              (fun token ->
                let url = Printf.sprintf "%s/bot%s/getMe" telegram_api_base token in
                Config.Http.get url
                  (fun response ->
                    if response.status >= 200 && response.status < 300 then
                      try
                        let json = Yojson.Basic.from_string response.body in
                        let open Yojson.Basic.Util in
                        let ok = try json |> member "ok" |> to_bool with _ -> false in
                        if ok then (
                          set_health ~account_id ~status:"healthy" ~error_message:None;
                          on_result (Error_types.Success ()))
                        else
                          let err =
                            parse_api_error
                              ~token
                              ~status_code:response.status
                              ~headers:response.headers
                              ~response_body:response.body
                          in
                          update_health_for_error ~account_id err;
                          on_result (Error_types.Failure err)
                      with _ ->
                        on_result
                          (Error_types.Failure
                             (Error_types.Internal_error "Malformed Telegram getMe payload"))
                    else
                      let err =
                        parse_api_error
                          ~token
                          ~status_code:response.status
                          ~headers:response.headers
                          ~response_body:response.body
                      in
                      update_health_for_error ~account_id err;
                      on_result (Error_types.Failure err))
                  (fun network_err ->
                    on_result
                      (Error_types.Failure
                         (Error_types.Network_error
                            (Error_types.Connection_failed (redact_sensitive_text ~token network_err)))))
              )
              (fun err -> on_result (Error_types.Failure err)))
end
