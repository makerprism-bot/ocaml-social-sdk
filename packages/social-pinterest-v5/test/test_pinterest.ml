(** Tests for Pinterest API v5 Provider *)

open Social_core
open Social_pinterest_v5

(** Helper to check if string contains substring *)
let string_contains s substr =
  try
    ignore (Str.search_forward (Str.regexp_string substr) s 0);
    true
  with Not_found -> false

(** Helper to handle outcome type for tests *)
let _handle_outcome on_success on_error outcome =
  match outcome with
  | Error_types.Success result -> on_success result
  | Error_types.Partial_success { result; _ } -> on_success result
  | Error_types.Failure err -> on_error (Error_types.error_to_string err)

(** Helper to handle api_result type for tests - converts to legacy on_success/on_error *)
let _handle_api_result on_success on_error result =
  match result with
  | Ok value -> on_success value
  | Error err -> on_error (Error_types.error_to_string err)

(** Mock HTTP client *)
module Mock_http = struct
  let requests = ref []
  let response_queue = ref []
  
  let reset () =
    requests := [];
    response_queue := []
  
  let set_responses responses =
    response_queue := responses
  
  let get_next_response () =
    match !response_queue with
    | [] -> None
    | r :: rest ->
        response_queue := rest;
        Some r
  
  include (struct
  let get ?(headers=[]) url on_success on_error =
    requests := ("GET", url, headers, "") :: !requests;
    match get_next_response () with
    | Some response -> on_success response
    | None -> on_error "No mock response set"
  
  let post ?(headers=[]) ?(body="") url on_success on_error =
    requests := ("POST", url, headers, body) :: !requests;
    match get_next_response () with
    | Some response -> on_success response
    | None -> on_error "No mock response set"
  
  let put ?(headers=[]) ?(body="") url on_success on_error =
    requests := ("PUT", url, headers, body) :: !requests;
    match get_next_response () with
    | Some response -> on_success response
    | None -> on_error "No mock response set"
  
  let delete ?(headers=[]) url on_success on_error =
    requests := ("DELETE", url, headers, "") :: !requests;
    match get_next_response () with
    | Some response -> on_success response
    | None -> on_error "No mock response set"
  
  let post_multipart ?(headers=[]) ~parts url on_success on_error =
    let _body_str = Printf.sprintf "multipart with %d parts" (List.length parts) in
    requests := ("POST_MULTIPART", url, headers, "") :: !requests;
    match get_next_response () with
    | Some response -> on_success response
    | None -> on_error "No mock response set"
  end : HTTP_CLIENT)
end

(** Mock config *)
module Mock_config = struct
  module Http = Mock_http
  
  let env_vars = ref []
  let credentials_store = ref []
  let health_statuses = ref []
  let logs = ref []
  let cache = ref []
  
  let reset () =
    env_vars := [];
    credentials_store := [];
    health_statuses := [];
    logs := [];
    cache := [];
    Mock_http.reset ()
  
  let set_env key value =
    env_vars := (key, value) :: !env_vars
  
  let get_env key =
    List.assoc_opt key !env_vars
  
  let _set_credentials ~account_id ~credentials =
    credentials_store := (account_id, credentials) :: !credentials_store
  
  let get_credentials ~account_id on_success on_error =
    match List.assoc_opt account_id !credentials_store with
    | Some creds -> on_success creds
    | None -> on_error "Credentials not found"
  
  let update_credentials ~account_id ~credentials on_success _on_error =
    credentials_store := (account_id, credentials) :: 
      (List.remove_assoc account_id !credentials_store);
    on_success ()
  
  let encrypt data on_success _on_error =
    on_success ("encrypted:" ^ data)
  
  let decrypt data on_success on_error =
    if String.starts_with ~prefix:"encrypted:" data then
      on_success (String.sub data 10 (String.length data - 10))
    else
      on_error "Invalid encrypted data"
  
  let update_health_status ~account_id ~status ~error_message on_success _on_error =
    health_statuses := (account_id, status, error_message) :: !health_statuses;
    on_success ()
  
  (* New enhanced config functions *)
  let _log level message =
    logs := (level, message) :: !logs
  
  let _get_cache key =
    List.assoc_opt key !cache
  
  let _set_cache key value _ttl =
    cache := (key, value) :: List.remove_assoc key !cache
  
  let _current_time () =
    Unix.time ()
end

module Pinterest = Make(Mock_config)

let setup_valid_credentials ?(account_id="test_account") () =
  Mock_config.reset ();
  Mock_config._set_credentials
    ~account_id
    ~credentials:{
      access_token = "test_access_token";
      refresh_token = Some "test_refresh_token";
      expires_at = None;
      token_type = "Bearer";
    }

(** Test: OAuth URL generation *)
let test_oauth_url () =
  Mock_config.reset ();
  Mock_config.set_env "PINTEREST_CLIENT_ID" "test_client_id";
  
  let state = "test_state_123" in
  let redirect_uri = "https://example.com/callback" in
  
  Pinterest.get_oauth_url ~redirect_uri ~state
    (fun url ->
      assert (string_contains url "client_id=test_client_id");
      assert (string_contains url "state=test_state_123");
      assert (string_contains url "pins:write");
      print_endline "✓ OAuth URL generation")
    (fun err -> failwith ("OAuth URL failed: " ^ err))

(** Test: Token exchange *)
let test_token_exchange () =
  Mock_config.reset ();
  Mock_config.set_env "PINTEREST_CLIENT_ID" "test_client";
  Mock_config.set_env "PINTEREST_CLIENT_SECRET" "test_secret";
  
  let response_body = {|{
    "access_token": "new_access_token_123",
    "refresh_token": "refresh_token_456",
    "token_type": "bearer",
    "expires_in": 2592000
  }|} in
  
  Mock_http.set_responses [{ status = 200; body = response_body; headers = [] }];
  
  Pinterest.exchange_code 
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun creds ->
      assert (creds.access_token = "new_access_token_123");
      assert (creds.refresh_token = Some "refresh_token_456");
      assert (creds.token_type = "Bearer");
      (* Pinterest tokens are long-lived with no defined expiration *)
      assert (creds.expires_at = None);
      print_endline "✓ Token exchange")
    (fun err -> failwith ("Token exchange failed: " ^ err))

(** Test: Get all boards *)
(* TODO: Function get_all_boards not implemented yet
let test_get_all_boards () =
  Mock_config.reset ();
  
  let response_body = {|{
    "items": [
      {"id": "board_123", "name": "My Board", "privacy": "PUBLIC"},
      {"id": "board_456", "name": "Another Board", "privacy": "PRIVATE"}
    ]
  }|} in
  
  Mock_http.set_responses [{ status = 200; body = response_body; headers = [] }];
  
  Pinterest.get_all_boards ~access_token:"test_token"
    (fun boards ->
      assert (List.length boards = 2);
      assert ((List.nth boards 0).id = "board_123");
      assert ((List.nth boards 0).name = "My Board");
      print_endline "✓ Get all boards")
    (fun err -> failwith ("Get boards failed: " ^ err))
*)

(** Test: Content validation *)
let test_content_validation () =
  (* Valid content *)
  (match Pinterest.validate_content ~text:"Check out this pin!" with
   | Ok () -> print_endline "✓ Valid content passes"
   | Error e -> failwith ("Valid content failed: " ^ e));
  
  (* Empty content *)
  (match Pinterest.validate_content ~text:"" with
   | Error _ -> print_endline "✓ Empty content rejected"
   | Ok () -> failwith "Empty content should fail");
  
  (* Too long *)
  let long_text = String.make 501 'x' in
  (match Pinterest.validate_content ~text:long_text with
   | Error msg when string_contains msg "500" -> 
       print_endline "✓ Long content rejected"
   | _ -> failwith "Long content should fail")

(** Test: Post requires image *)
let test_post_requires_image () =
  Mock_config.reset ();
  
  Pinterest.post_single
    ~account_id:"test_account"
    ~text:"Test"
    ~media_urls:[]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error _) ->
          print_endline "✓ Post requires image (validation error)"
      | Error_types.Success _ ->
          failwith "Should fail without image"
      | _ ->
          failwith "Expected validation error")

let test_validate_media_file_rejects_large_image () =
  let oversized = (20 * 1024 * 1024) + 1 in
  let media : Platform_types.post_media = {
    media_type = Platform_types.Image;
    mime_type = "image/jpeg";
    file_size_bytes = oversized;
    width = None;
    height = None;
    duration_seconds = None;
    alt_text = None;
  } in
  match Pinterest.validate_media_file ~media with
  | Error [Error_types.Media_too_large _] ->
      print_endline "✓ Large image rejected by media validation"
  | Error errs ->
      failwith ("Unexpected validation error: " ^ String.concat ", " (List.map Error_types.validation_error_to_string errs))
  | Ok () ->
      failwith "Oversized image should fail media validation"

let test_post_single_full_image_flow_with_alt_text () =
  setup_valid_credentials ();

  let boards_response =
    { status = 200; body = {|{"items":[{"id":"board_123"}]}|}; headers = [] }
  in
  let image_download_response =
    { status = 200; body = "fake_image_binary"; headers = [ ("content-type", "image/jpeg; charset=binary") ] }
  in
  let media_upload_response =
    { status = 201; body = {|{"media_id":"media_456"}|}; headers = [] }
  in
  let pin_create_response =
    { status = 201; body = {|{"id":"pin_789"}|}; headers = [] }
  in

  Mock_http.set_responses [
    boards_response;
    image_download_response;
    media_upload_response;
    pin_create_response;
  ];

  Pinterest.post_single
    ~account_id:"test_account"
    ~text:"Pin title and description"
    ~media_urls:["https://example.com/image.jpg"]
    ~alt_texts:[Some "Accessible alt text"]
    (fun outcome ->
      match outcome with
      | Error_types.Success pin_id ->
          assert (pin_id = "pin_789");
          let chronological = List.rev !Mock_http.requests in
          assert (List.length chronological = 4);

          let (m1, u1, h1, _) = List.nth chronological 0 in
          assert (m1 = "GET");
          assert (string_contains u1 "/boards");
          assert (List.assoc_opt "Authorization" h1 = Some "Bearer test_access_token");

          let (m2, u2, _, _) = List.nth chronological 1 in
          assert (m2 = "GET");
          assert (u2 = "https://example.com/image.jpg");

          let (m3, u3, h3, _) = List.nth chronological 2 in
          assert (m3 = "POST_MULTIPART");
          assert (string_contains u3 "/media");
          assert (List.assoc_opt "Authorization" h3 = Some "Bearer test_access_token");

          let (m4, u4, h4, body4) = List.nth chronological 3 in
          assert (m4 = "POST");
          assert (string_contains u4 "/pins");
          assert (List.assoc_opt "Authorization" h4 = Some "Bearer test_access_token");

          let json = Yojson.Basic.from_string body4 in
          let open Yojson.Basic.Util in
          assert ((json |> member "board_id" |> to_string) = "board_123");
          assert ((json |> member "alt_text" |> to_string) = "Accessible alt text");
          assert ((json |> member "media_source" |> member "source_type" |> to_string) = "image_base64");
          assert ((json |> member "media_source" |> member "media_id" |> to_string) = "media_456");
          print_endline "✓ post_single full image flow with alt text"
      | Error_types.Failure err ->
          failwith ("post_single full flow failed: " ^ Error_types.error_to_string err)
      | Error_types.Partial_success _ ->
          failwith "Expected full success for single pin flow")

let test_post_single_truncates_title_to_100_chars () =
  setup_valid_credentials ();

  let boards_response =
    { status = 200; body = {|{"items":[{"id":"board_123"}]}|}; headers = [] }
  in
  let image_download_response =
    { status = 200; body = "fake_image_binary"; headers = [ ("content-type", "image/jpeg") ] }
  in
  let media_upload_response =
    { status = 201; body = {|{"media_id":"media_456"}|}; headers = [] }
  in
  let pin_create_response =
    { status = 201; body = {|{"id":"pin_789"}|}; headers = [] }
  in

  Mock_http.set_responses [
    boards_response;
    image_download_response;
    media_upload_response;
    pin_create_response;
  ];

  let long_text = String.make 120 'x' in

  Pinterest.post_single
    ~account_id:"test_account"
    ~text:long_text
    ~media_urls:["https://example.com/image.jpg"]
    (fun outcome ->
      match outcome with
      | Error_types.Success _ ->
          let chronological = List.rev !Mock_http.requests in
          let (_, _, _, body) = List.nth chronological 3 in
          let json = Yojson.Basic.from_string body in
          let open Yojson.Basic.Util in
          let title = json |> member "title" |> to_string in
          let description = json |> member "description" |> to_string in
          assert (String.length title = 100);
          assert (description = long_text);
          print_endline "✓ post_single truncates title to 100 chars"
      | Error_types.Failure err ->
          failwith ("post_single title truncation test failed: " ^ Error_types.error_to_string err)
      | Error_types.Partial_success _ ->
          failwith "Expected full success for single pin flow")

let test_post_single_validation_before_upload_blocks_large_image () =
  setup_valid_credentials ();

  let boards_response =
    { status = 200; body = {|{"items":[{"id":"board_123"}]}|}; headers = [] }
  in
  let oversized_image = String.make ((20 * 1024 * 1024) + 1) 'x' in
  let image_download_response =
    { status = 200; body = oversized_image; headers = [ ("content-type", "image/jpeg") ] }
  in

  Mock_http.set_responses [
    boards_response;
    image_download_response;
  ];

  Pinterest.post_single
    ~account_id:"test_account"
    ~text:"Large image pin"
    ~media_urls:["https://example.com/oversized.jpg"]
    ~validate_media_before_upload:true
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Internal_error msg) ->
          assert (string_contains msg "Validation failed");
          let chronological = List.rev !Mock_http.requests in
          assert (List.length chronological = 2);
          let (m1, u1, _, _) = List.nth chronological 0 in
          let (m2, u2, _, _) = List.nth chronological 1 in
          assert (m1 = "GET");
          assert (string_contains u1 "/boards");
          assert (m2 = "GET");
          assert (u2 = "https://example.com/oversized.jpg");
          print_endline "✓ Validation blocks oversized image before media upload"
      | Error_types.Failure err ->
          failwith ("Wrong error type for oversized validation: " ^ Error_types.error_to_string err)
      | Error_types.Success _ ->
          failwith "Oversized image should fail before upload"
      | Error_types.Partial_success _ ->
          failwith "Oversized image should not produce partial success")

let test_post_thread_partial_success_posts_only_first () =
  setup_valid_credentials ();

  let boards_response =
    { status = 200; body = {|{"items":[{"id":"board_123"}]}|}; headers = [] }
  in
  let image_download_response =
    { status = 200; body = "fake_image_binary"; headers = [ ("content-type", "image/jpeg") ] }
  in
  let media_upload_response =
    { status = 201; body = {|{"media_id":"media_456"}|}; headers = [] }
  in
  let pin_create_response =
    { status = 201; body = {|{"id":"pin_789"}|}; headers = [] }
  in

  Mock_http.set_responses [
    boards_response;
    image_download_response;
    media_upload_response;
    pin_create_response;
  ];

  Pinterest.post_thread
    ~account_id:"test_account"
    ~texts:["first"; "second"]
    ~media_urls_per_post:[
      ["https://example.com/first.jpg"];
      ["https://example.com/second.jpg"];
    ]
    (fun outcome ->
      match outcome with
      | Error_types.Partial_success { result; warnings } ->
          assert (result.Error_types.posted_ids = ["pin_789"]);
          assert (result.Error_types.total_requested = 2);
          assert (result.Error_types.failed_at_index = None);
          assert (List.length warnings = 1);
          (match List.hd warnings with
           | Error_types.Generic_warning { code; _ } -> assert (code = "pinterest_no_threads")
           | _ -> failwith "Expected Generic_warning for thread fallback");
          let chronological = List.rev !Mock_http.requests in
          assert (List.length chronological = 4);
          print_endline "✓ post_thread returns partial success and posts only first item"
      | Error_types.Success _ ->
          failwith "Expected partial success when posting multi-item thread"
      | Error_types.Failure err ->
          failwith ("post_thread partial success test failed: " ^ Error_types.error_to_string err))

let test_validate_media_file_accepts_video () =
  let media : Platform_types.post_media = {
    media_type = Platform_types.Video;
    mime_type = "video/mp4";
    file_size_bytes = 5 * 1024 * 1024;
    width = Some 1080;
    height = Some 1920;
    duration_seconds = Some 15.0;
    alt_text = None;
  } in
  match Pinterest.validate_media_file ~media with
  | Ok () ->
      print_endline "✓ Video media validation accepts supported video"
  | Error errs ->
      failwith ("Unexpected validation result: " ^ String.concat ", " (List.map Error_types.validation_error_to_string errs))

let test_post_single_video_url_uses_video_media_flow () =
  setup_valid_credentials ();

  let boards_response =
    { status = 200; body = {|{"items":[{"id":"board_123"}]}|}; headers = [] }
  in
  let video_download_response =
    { status = 200; body = "fake_video_binary"; headers = [ ("content-type", "video/mp4") ] }
  in
  let media_register_response =
    { status = 201; body =
      {|{"media_id":"media_456","upload_url":"https://uploads.pinterest.com","upload_parameters":{"key":"media/key.mp4","policy":"abc"}}|};
      headers = [] }
  in
  let media_upload_response =
    { status = 204; body = ""; headers = [] }
  in
  let media_status_registered =
    { status = 200; body = {|{"media_id":"media_456","status":"registered"}|}; headers = [] }
  in
  let media_status_processing =
    { status = 200; body = {|{"media_id":"media_456","status":"processing"}|}; headers = [] }
  in
  let media_status_response =
    { status = 200; body = {|{"media_id":"media_456","status":"succeeded"}|}; headers = [] }
  in
  let thumbnail_probe_response =
    { status = 200; body = "fake_poster_binary"; headers = [ ("content-type", "image/jpeg") ] }
  in
  let pin_create_response =
    { status = 201; body = {|{"id":"pin_789"}|}; headers = [] }
  in

  Mock_http.set_responses [
    boards_response;
    video_download_response;
    media_register_response;
    media_upload_response;
    media_status_registered;
    media_status_processing;
    media_status_response;
    thumbnail_probe_response;
    pin_create_response;
  ];

  Pinterest.post_single
    ~account_id:"test_account"
    ~text:"Video URL should be posted"
    ~media_urls:["https://example.com/video.mp4"; "https://example.com/poster.jpg"]
    (fun outcome ->
      match outcome with
      | Error_types.Success pin_id ->
          assert (pin_id = "pin_789");
          let chronological = List.rev !Mock_http.requests in
          assert (List.length chronological = 9);

          let (m3, u3, _, b3) = List.nth chronological 2 in
          assert (m3 = "POST");
          assert (string_contains u3 "/media");
          assert (string_contains b3 "media_type");
          assert (string_contains b3 "video");

          let (m4, u4, _, _) = List.nth chronological 3 in
          assert (m4 = "POST_MULTIPART");
          assert (u4 = "https://uploads.pinterest.com");

          let (m5, u5, _, _) = List.nth chronological 4 in
          assert (m5 = "GET");
          assert (string_contains u5 "/media/media_456");

          let (m6, u6, _, _) = List.nth chronological 5 in
          assert (m6 = "GET");
          assert (string_contains u6 "/media/media_456");

          let (m7, u7, _, _) = List.nth chronological 6 in
          assert (m7 = "GET");
          assert (string_contains u7 "/media/media_456");

          let (m8, u8, _, _) = List.nth chronological 7 in
          assert (m8 = "GET");
          assert (u8 = "https://example.com/poster.jpg");

          let (m9, u9, _, b9) = List.nth chronological 8 in
          assert (m9 = "POST");
          assert (string_contains u9 "/pins");
          let pin_json = Yojson.Basic.from_string b9 in
          let open Yojson.Basic.Util in
          assert ((pin_json |> member "media_source" |> member "source_type" |> to_string) = "video_id");
          assert ((pin_json |> member "media_source" |> member "media_id" |> to_string) = "media_456");
          assert ((pin_json |> member "media_source" |> member "cover_image_url" |> to_string) = "https://example.com/poster.jpg");
          print_endline "✓ Video URL uses Pinterest video media flow"
      | Error_types.Failure err ->
          failwith ("Video URL should succeed via video media flow: " ^ Error_types.error_to_string err)
      | Error_types.Partial_success _ ->
          failwith "Video URL should complete with full success")

let test_post_single_unknown_extension_image_falls_back_to_image_flow () =
  setup_valid_credentials ();

  let boards_response =
    { status = 200; body = {|{"items":[{"id":"board_123"}]}|}; headers = [] }
  in
  let first_download_image_response =
    { status = 200; body = "fake_image_binary"; headers = [ ("content-type", "image/jpeg") ] }
  in
  let image_media_upload_response =
    { status = 201; body = {|{"media_id":"media_img_001"}|}; headers = [] }
  in
  let pin_create_response =
    { status = 201; body = {|{"id":"pin_img_001"}|}; headers = [] }
  in

  Mock_http.set_responses [
    boards_response;
    first_download_image_response;
    first_download_image_response;
    image_media_upload_response;
    pin_create_response;
  ];

  Pinterest.post_single
    ~account_id:"test_account"
    ~text:"Unknown extension should fallback to image flow"
    ~media_urls:["https://example.com/asset?id=42"]
    (fun outcome ->
      match outcome with
      | Error_types.Success pin_id ->
          assert (pin_id = "pin_img_001");
          let chronological = List.rev !Mock_http.requests in
          assert (List.length chronological = 5);

          let (m2, _, _, _) = List.nth chronological 1 in
          assert (m2 = "GET");

          let (m3, _, _, _) = List.nth chronological 2 in
          assert (m3 = "GET");

          let (m4, u4, _, _) = List.nth chronological 3 in
          assert (m4 = "POST_MULTIPART");
          assert (string_contains u4 "/media");

          let (_, _, _, pin_body) = List.nth chronological 4 in
          let pin_json = Yojson.Basic.from_string pin_body in
          let open Yojson.Basic.Util in
          assert ((pin_json |> member "media_source" |> member "source_type" |> to_string) = "image_base64");
          print_endline "✓ Unknown extension media falls back to image flow on image content-type"
      | Error_types.Failure err ->
          failwith ("Unknown-extension image fallback failed: " ^ Error_types.error_to_string err)
      | Error_types.Partial_success _ ->
          failwith "Unknown-extension image fallback should complete with full success")

let test_post_single_unknown_extension_video_uses_video_flow () =
  setup_valid_credentials ();

  let boards_response =
    { status = 200; body = {|{"items":[{"id":"board_123"}]}|}; headers = [] }
  in
  let first_download_video_response =
    { status = 200; body = "fake_video_binary"; headers = [ ("content-type", "video/mp4") ] }
  in
  let media_register_response =
    { status = 201; body =
      {|{"media_id":"media_vid_002","upload_url":"https://uploads.pinterest.com","upload_parameters":{"key":"media/key2.mp4","policy":"abc"}}|};
      headers = [] }
  in
  let media_upload_response =
    { status = 204; body = ""; headers = [] }
  in
  let media_status_response =
    { status = 200; body = {|{"media_id":"media_vid_002","status":"succeeded"}|}; headers = [] }
  in
  let thumbnail_probe_response =
    { status = 200; body = "fake_poster_binary"; headers = [ ("content-type", "image/jpeg") ] }
  in
  let pin_create_response =
    { status = 201; body = {|{"id":"pin_vid_002"}|}; headers = [] }
  in

  Mock_http.set_responses [
    boards_response;
    first_download_video_response;
    media_register_response;
    media_upload_response;
    media_status_response;
    thumbnail_probe_response;
    pin_create_response;
  ];

  Pinterest.post_single
    ~account_id:"test_account"
    ~text:"Unknown extension should use video flow"
    ~media_urls:["https://example.com/asset?id=video"; "https://example.com/poster.jpg"]
    (fun outcome ->
      match outcome with
      | Error_types.Success pin_id ->
          assert (pin_id = "pin_vid_002");
          let chronological = List.rev !Mock_http.requests in
          assert (List.length chronological = 7);
          let (m3, u3, _, b3) = List.nth chronological 2 in
          assert (m3 = "POST");
          assert (string_contains u3 "/media");
          assert (string_contains b3 "video");
          let (m6, u6, _, _) = List.nth chronological 5 in
          assert (m6 = "GET");
          assert (u6 = "https://example.com/poster.jpg");
          let (_, _, _, pin_body) = List.nth chronological 6 in
          let pin_json = Yojson.Basic.from_string pin_body in
          let open Yojson.Basic.Util in
          assert ((pin_json |> member "media_source" |> member "source_type" |> to_string) = "video_id");
          assert ((pin_json |> member "media_source" |> member "cover_image_url" |> to_string) = "https://example.com/poster.jpg");
          print_endline "✓ Unknown extension media uses video flow when content-type is video"
      | Error_types.Failure err ->
          failwith ("Unknown-extension video route failed: " ^ Error_types.error_to_string err)
      | Error_types.Partial_success _ ->
          failwith "Unknown-extension video route should complete with full success")

let test_post_single_unknown_extension_non_media_returns_clean_error () =
  setup_valid_credentials ();

  let boards_response =
    { status = 200; body = {|{"items":[{"id":"board_123"}]}|}; headers = [] }
  in
  let first_download_pdf_response =
    { status = 200; body = "fake_pdf_binary"; headers = [ ("content-type", "application/pdf") ] }
  in

  Mock_http.set_responses [
    boards_response;
    first_download_pdf_response;
  ];

  Pinterest.post_single
    ~account_id:"test_account"
    ~text:"Unknown extension non-media should fail"
    ~media_urls:["https://example.com/asset?id=pdf"]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Internal_error msg) ->
          assert (string_contains msg "Unsupported media content-type: application/pdf");
          assert (not (string_contains msg "non_video_content_type:"));
          print_endline "✓ Unknown extension non-media fails with clean unsupported content-type error"
      | Error_types.Failure err ->
          failwith ("Expected internal unsupported content-type error: " ^ Error_types.error_to_string err)
      | Error_types.Success _ ->
          failwith "Non-media URL should not succeed"
      | Error_types.Partial_success _ ->
          failwith "Non-media URL should not partially succeed")

let test_post_single_video_content_type_with_charset_is_supported () =
  setup_valid_credentials ();

  let boards_response =
    { status = 200; body = {|{"items":[{"id":"board_123"}]}|}; headers = [] }
  in
  let video_download_response =
    { status = 200; body = "fake_video_binary"; headers = [ ("content-type", "video/mp4; charset=binary") ] }
  in
  let media_register_response =
    { status = 201; body =
      {|{"media_id":"media_789","upload_url":"https://uploads.pinterest.com","upload_parameters":{"key":"media/key789.mp4","policy":"abc"}}|};
      headers = [] }
  in
  let media_upload_response =
    { status = 204; body = ""; headers = [] }
  in
  let media_status_response =
    { status = 200; body = {|{"media_id":"media_789","status":"succeeded"}|}; headers = [] }
  in
  let pin_create_response =
    { status = 201; body = {|{"id":"pin_789_charset"}|}; headers = [] }
  in

  Mock_http.set_responses [
    boards_response;
    video_download_response;
    media_register_response;
    media_upload_response;
    media_status_response;
    pin_create_response;
  ];

  Pinterest.post_single
    ~account_id:"test_account"
    ~text:"Video content-type params"
    ~media_urls:["https://example.com/video-param"]
    (fun outcome ->
      match outcome with
      | Error_types.Success pin_id ->
          assert (pin_id = "pin_789_charset");
          let chronological = List.rev !Mock_http.requests in
          let (_, _, _, body3) = List.nth chronological 2 in
          assert (string_contains body3 "\"media_type\":\"video\"");
          print_endline "✓ Video content-type with parameters is normalized for Pinterest"
      | Error_types.Failure err ->
          failwith ("Video content-type normalization should succeed: " ^ Error_types.error_to_string err)
      | Error_types.Partial_success _ ->
          failwith "Expected full success for video content-type normalization")

let test_post_single_video_ignores_non_image_thumbnail_url () =
  setup_valid_credentials ();

  let boards_response =
    { status = 200; body = {|{"items":[{"id":"board_123"}]}|}; headers = [] }
  in
  let video_download_response =
    { status = 200; body = "fake_video_binary"; headers = [ ("content-type", "video/mp4") ] }
  in
  let media_register_response =
    { status = 201; body =
      {|{"media_id":"media_790","upload_url":"https://uploads.pinterest.com","upload_parameters":{"key":"media/key790.mp4","policy":"abc"}}|};
      headers = [] }
  in
  let media_upload_response =
    { status = 204; body = ""; headers = [] }
  in
  let media_status_response =
    { status = 200; body = {|{"media_id":"media_790","status":"succeeded"}|}; headers = [] }
  in
  let pin_create_response =
    { status = 201; body = {|{"id":"pin_790"}|}; headers = [] }
  in

  Mock_http.set_responses [
    boards_response;
    video_download_response;
    media_register_response;
    media_upload_response;
    media_status_response;
    pin_create_response;
  ];

  Pinterest.post_single
    ~account_id:"test_account"
    ~text:"Video should ignore non-image thumbnail URL"
    ~media_urls:["https://example.com/video.mp4"; "https://example.com/thumb"]
    (fun outcome ->
      match outcome with
      | Error_types.Success pin_id ->
          assert (pin_id = "pin_790");
          let chronological = List.rev !Mock_http.requests in
          let (_, _, _, pin_body) = List.nth chronological 5 in
          let pin_json = Yojson.Basic.from_string pin_body in
          let open Yojson.Basic.Util in
          assert ((pin_json |> member "media_source" |> member "source_type" |> to_string) = "video_id");
          assert ((pin_json |> member "media_source" |> member "cover_image_url" |> to_string_option) = None);
          print_endline "✓ Video flow ignores non-image thumbnail URL candidate"
      | Error_types.Failure err ->
          failwith ("Video flow should ignore non-image thumbnail and still succeed: " ^ Error_types.error_to_string err)
      | Error_types.Partial_success _ ->
          failwith "Expected full success when non-image thumbnail is ignored")

let test_validate_media_file_rejects_oversized_video () =
  let media : Platform_types.post_media = {
    media_type = Platform_types.Video;
    mime_type = "video/mp4";
    file_size_bytes = (2 * 1024 * 1024 * 1024) + 1;
    width = Some 1080;
    height = Some 1920;
    duration_seconds = Some 15.0;
    alt_text = None;
  } in
  match Pinterest.validate_media_file ~media with
  | Error [Error_types.Media_too_large _] ->
      print_endline "✓ Oversized video rejected by media validation"
  | Error errs ->
      failwith ("Unexpected oversized-video validation result: " ^ String.concat ", " (List.map Error_types.validation_error_to_string errs))
  | Ok () ->
      failwith "Oversized video should fail media validation"

(** Run all tests *)
let () =
  print_endline "\n=== Pinterest Provider Tests ===\n";
  test_oauth_url ();
  test_token_exchange ();
  (* test_get_all_boards (); *) (* TODO: Function not implemented *)
  test_content_validation ();
  test_post_requires_image ();
  test_validate_media_file_rejects_large_image ();
  test_post_single_full_image_flow_with_alt_text ();
  test_post_single_truncates_title_to_100_chars ();
  test_post_single_validation_before_upload_blocks_large_image ();
  test_post_thread_partial_success_posts_only_first ();
  test_validate_media_file_accepts_video ();
  test_validate_media_file_rejects_oversized_video ();
  test_post_single_video_url_uses_video_media_flow ();
  test_post_single_unknown_extension_image_falls_back_to_image_flow ();
  test_post_single_unknown_extension_video_uses_video_flow ();
  test_post_single_unknown_extension_non_media_returns_clean_error ();
  test_post_single_video_content_type_with_charset_is_supported ();
  test_post_single_video_ignores_non_image_thumbnail_url ();
  print_endline "\n=== All tests passed! ===\n"
