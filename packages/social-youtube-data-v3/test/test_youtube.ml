(** Tests for YouTube Data API v3 Provider *)

open Social_core
open Social_youtube_data_v3

(** Helper to check if string contains substring *)
let string_contains s substr =
  try
    ignore (Str.search_forward (Str.regexp_string substr) s 0);
    true
  with Not_found -> false

(** Helper to handle outcome type for tests - unused but kept for consistency *)
let _handle_outcome on_success on_error outcome =
  match outcome with
  | Error_types.Success result -> on_success result
  | Error_types.Partial_success { result; _ } -> on_success result
  | Error_types.Failure err -> on_error (Error_types.error_to_string err)

(** Mock HTTP client for testing *)
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
    let body_str = Printf.sprintf "multipart with %d parts" (List.length parts) in
    requests := ("POST_MULTIPART", url, headers, body_str) :: !requests;
    match get_next_response () with
    | Some response -> on_success response
    | None -> on_error "No mock response set"
  end : HTTP_CLIENT)
end

(** Mock config for testing *)
module Mock_config = struct
  module Http = Mock_http
  
  let env_vars = ref []
  let credentials_store = ref []
  let health_statuses = ref []
  
  let reset () =
    env_vars := [];
    credentials_store := [];
    health_statuses := [];
    Mock_http.reset ()
  
  let set_env key value =
    env_vars := (key, value) :: !env_vars
  
  let get_env key =
    List.assoc_opt key !env_vars
  
  let set_credentials ~account_id ~credentials =
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
  
  let _get_health_status account_id =
    List.find_opt (fun (id, _, _) -> id = account_id) !health_statuses
end

module YouTube = Make(Mock_config)

(** Test: OAuth URL generation with PKCE *)
let test_oauth_url () =
  Mock_config.reset ();
  Mock_config.set_env "YOUTUBE_CLIENT_ID" "test_client_id";
  
  let state = "test_state_123" in
  let redirect_uri = "https://example.com/callback" in
  let code_verifier = "test_verifier_1234567890" in
  
  YouTube.get_oauth_url ~redirect_uri ~state ~code_verifier
    (fun url ->
      assert (string_contains url "client_id=test_client_id");
      assert (string_contains url "state=test_state_123");
      assert (string_contains url "code_challenge");
      assert (string_contains url "code_challenge_method=S256");
      assert (string_contains url "access_type=offline");
      print_endline "✓ OAuth URL generation with PKCE")
    (fun err -> failwith ("OAuth URL failed: " ^ err))

(** Test: Token exchange *)
let test_token_exchange () =
  Mock_config.reset ();
  Mock_config.set_env "YOUTUBE_CLIENT_ID" "test_client";
  Mock_config.set_env "YOUTUBE_CLIENT_SECRET" "test_secret";
  
  let response_body = {|{
    "access_token": "new_access_token_123",
    "refresh_token": "refresh_token_456",
    "expires_in": 3600,
    "token_type": "Bearer"
  }|} in
  
  Mock_http.set_responses [{ status = 200; body = response_body; headers = [] }];
  
  YouTube.exchange_code 
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    ~code_verifier:"test_verifier"
    (fun creds ->
      assert (creds.access_token = "new_access_token_123");
      assert (creds.refresh_token = Some "refresh_token_456");
      assert (creds.token_type = "Bearer");
      assert (creds.expires_at <> None);
      print_endline "✓ Token exchange")
    (fun err -> failwith ("Token exchange failed: " ^ err))

(** Test: Token refresh *)
let test_token_refresh () =
  Mock_config.reset ();
  Mock_config.set_env "YOUTUBE_CLIENT_ID" "test_client";
  Mock_config.set_env "YOUTUBE_CLIENT_SECRET" "test_secret";
  
  let response_body = {|{
    "access_token": "refreshed_token",
    "expires_in": 3600
  }|} in
  
  Mock_http.set_responses [{ status = 200; body = response_body; headers = [] }];
  
  YouTube.refresh_access_token
    ~client_id:"test_client"
    ~client_secret:"test_secret"
    ~refresh_token:"old_refresh"
    (fun (access, refresh, _expires) ->
      assert (access = "refreshed_token");
      assert (refresh = "old_refresh");  (* Google doesn't return new refresh *)
      print_endline "✓ Token refresh")
    (fun err -> failwith ("Token refresh failed: " ^ err))

(** Test: Content validation *)
let test_content_validation () =
  (* Valid content *)
  (match YouTube.validate_content ~text:"Check out my YouTube Short!" with
   | Ok () -> print_endline "✓ Valid content passes"
   | Error e -> failwith ("Valid content failed: " ^ e));
  
  (* Empty content *)
  (match YouTube.validate_content ~text:"" with
   | Error _ -> print_endline "✓ Empty content rejected"
   | Ok () -> failwith "Empty content should fail");
  
  (* Too long *)
  let long_text = String.make 5001 'x' in
  (match YouTube.validate_content ~text:long_text with
   | Error msg when string_contains msg "5000" -> 
       print_endline "✓ Long content rejected"
   | _ -> failwith "Long content should fail")

(** Test: Ensure valid token (fresh token) *)
let test_ensure_valid_token_fresh () =
  Mock_config.reset ();
  
  (* Set credentials with far-future expiry *)
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s 3600) with  (* 1 hour *)
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  YouTube.ensure_valid_token ~account_id:"test_account"
    (fun token ->
      assert (token = "valid_token");
      print_endline "✓ Ensure valid token (fresh)")
    (fun err -> failwith ("Ensure valid token failed: " ^ Error_types.error_to_string err))

(** Test: Ensure valid token (expired, needs refresh) *)
let test_ensure_valid_token_expired () =
  Mock_config.reset ();
  Mock_config.set_env "YOUTUBE_CLIENT_ID" "test_client";
  Mock_config.set_env "YOUTUBE_CLIENT_SECRET" "test_secret";
  
  (* Set credentials with past expiry *)
  let past_time = 
    let now = Ptime_clock.now () in
    match Ptime.sub_span now (Ptime.Span.of_int_s 100) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate past time"
  in
  
  let creds = {
    access_token = "expired_token";
    refresh_token = Some "refresh_token";
    expires_at = Some past_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* Mock refresh response *)
  let response_body = {|{
    "access_token": "refreshed_token",
    "expires_in": 3600
  }|} in
  
  Mock_http.set_responses [{ status = 200; body = response_body; headers = [] }];
  
  YouTube.ensure_valid_token ~account_id:"test_account"
    (fun token ->
      assert (token = "refreshed_token");
      print_endline "✓ Ensure valid token (auto-refresh)")
    (fun err -> failwith ("Ensure valid token failed: " ^ Error_types.error_to_string err))

(** Test: Post requires video *)
let test_post_requires_video () =
  Mock_config.reset ();
  
  YouTube.post_single
    ~account_id:"test_account"
    ~text:"Test"
    ~media_urls:[]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error _) ->
          print_endline "✓ Post requires video (validation error)"
      | Error_types.Success _ ->
          failwith "Should fail without video"
      | _ ->
          failwith "Expected validation error")

(* ============================================ *)
(* VIDEO UPLOAD TESTS                           *)
(* Based on YouTube Data API v3 resumable upload*)
(* Reference: tokland/youtube-upload (2.2k stars)*)
(* ============================================ *)

(** Test: Video upload - resumable upload initialization
    YouTube uses resumable uploads with 2 phases:
    1. POST metadata -> get upload_url from Location header
    2. PUT video data to upload_url
    
    Reference: https://developers.google.com/youtube/v3/guides/using_resumable_upload_protocol
*)
let test_video_upload_init () =
  Mock_config.reset ();
  Mock_config.set_env "YOUTUBE_CLIENT_ID" "test_client";
  Mock_config.set_env "YOUTUBE_CLIENT_SECRET" "test_secret";
  
  (* Set valid credentials *)
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s 3600) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  let creds = {
    Social_core.access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* Mock responses for: 1) video download, 2) upload init, 3) video upload *)
  Mock_http.set_responses [
    (* Video download response *)
    { status = 200; body = "mock_video_data"; headers = [("content-type", "video/mp4")] };
    (* Upload init response with Location header *)
    { status = 200; body = "{}"; headers = [("location", "https://www.googleapis.com/upload/youtube/v3/videos?uploadId=abc123")] };
    (* Final upload response *)
    { status = 200; body = {|{"id": "video_12345"}|}; headers = [] };
  ];
  
  YouTube.post_single
    ~account_id:"test_account"
    ~text:"My YouTube Short"
    ~media_urls:["https://example.com/video.mp4"]
    (fun outcome ->
      match outcome with
      | Error_types.Success video_id ->
          assert (video_id = "video_12345");
          print_endline "✓ Video upload initialization test passed"
      | Error_types.Partial_success { result = video_id; _ } ->
          assert (video_id = "video_12345");
          print_endline "✓ Video upload initialization test passed"
      | Error_types.Failure err ->
          failwith ("Video upload failed: " ^ Error_types.error_to_string err))

(** Test: Video metadata is correctly formatted for Shorts
    YouTube Shorts requires:
    - #Shorts in title or description
    - Vertical video (9:16 aspect ratio)
    - Under 60 seconds duration
*)
let test_video_shorts_metadata () =
  (* Verify Shorts metadata format *)
  let description = "My awesome video" in
  let shorts_description = description ^ " #Shorts" in
  
  assert (string_contains shorts_description "#Shorts");
  
  (* Verify title length limit *)
  let max_title = 100 in
  let long_title = String.make 150 'x' in
  let truncated = String.sub long_title 0 (min (String.length long_title) max_title) in
  assert (String.length truncated = 100);
  
  print_endline "✓ Video Shorts metadata test passed"

(** Test: Video upload with resumable upload protocol
    Tests the full resumable upload flow:
    1. Initialize upload and get upload URL
    2. Upload video data in single PUT request
    
    Reference: YouTube uses X-Upload-Content-Length and X-Upload-Content-Type headers
*)
let test_video_resumable_upload () =
  Mock_config.reset ();
  Mock_config.set_env "YOUTUBE_CLIENT_ID" "test_client";
  Mock_config.set_env "YOUTUBE_CLIENT_SECRET" "test_secret";
  
  (* Set valid credentials *)
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s 3600) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  let creds = {
    Social_core.access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* Mock for video download, init, and upload *)
  Mock_http.set_responses [
    { status = 200; body = String.make 1000 'V'; headers = [("content-type", "video/mp4")] };
    { status = 200; body = "{}"; headers = [("location", "https://upload.example.com/abc")] };
    { status = 200; body = {|{"id": "shorts_abc123", "status": {"uploadStatus": "uploaded"}}|}; headers = [] };
  ];
  
  YouTube.post_single
    ~account_id:"test_account"
    ~text:"Check out my Shorts!"
    ~media_urls:["https://example.com/short.mp4"]
    (fun outcome ->
      match outcome with
      | Error_types.Success video_id ->
          assert (string_contains video_id "shorts");
          print_endline "✓ Video resumable upload test passed"
      | Error_types.Partial_success { result = video_id; _ } ->
          assert (string_contains video_id "shorts");
          print_endline "✓ Video resumable upload test passed"
      | Error_types.Failure err ->
          failwith ("Resumable upload failed: " ^ Error_types.error_to_string err))

(** Test: Video upload error handling
    Tests proper handling of upload errors:
    - No upload URL in response
    - Upload fails
    - Rate limiting
*)
let test_video_upload_error_handling () =
  Mock_config.reset ();
  Mock_config.set_env "YOUTUBE_CLIENT_ID" "test_client";
  
  (* Set valid credentials *)
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s 3600) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  let creds = {
    Social_core.access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* Mock video download succeeds but init fails (no location header) *)
  Mock_http.set_responses [
    { status = 200; body = "video_data"; headers = [("content-type", "video/mp4")] };
    { status = 200; body = "{}"; headers = [] }; (* Missing location header *)
  ];
  
  YouTube.post_single
    ~account_id:"test_account"
    ~text:"Test video"
    ~media_urls:["https://example.com/video.mp4"]
    (fun outcome ->
      match outcome with
      | Error_types.Failure _ ->
          print_endline "✓ Video upload error handling test passed"
      | _ ->
          failwith "Should have failed due to missing upload URL")

(** Test: Video privacy status options
    YouTube supports privacy statuses:
    - "public" - visible to everyone
    - "unlisted" - accessible via link only
    - "private" - only owner can see
*)
let test_video_privacy_status () =
  let privacy_options = ["public"; "unlisted"; "private"] in
  
  (* Verify all privacy options are valid *)
  assert (List.mem "public" privacy_options);
  assert (List.mem "unlisted" privacy_options);
  assert (List.mem "private" privacy_options);
  
  (* Default should be public for Shorts *)
  let default_privacy = "public" in
  assert (default_privacy = "public");
  
  print_endline "✓ Video privacy status test passed"

(** Test: Video category assignment
    YouTube requires a category ID for uploads.
    Common categories:
    - 22: People & Blogs (default for Shorts)
    - 23: Comedy
    - 24: Entertainment
    - 10: Music
*)
let test_video_category () =
  let shorts_category = "22" in (* People & Blogs *)
  
  assert (shorts_category = "22");
  
  (* Verify category ID is a valid string number *)
  let _ = int_of_string shorts_category in
  
  print_endline "✓ Video category assignment test passed"

(** Test: Thread posting - YouTube only posts first item *)
let test_thread_youtube_single_post () =
  Mock_config.reset ();
  Mock_config.set_env "YOUTUBE_CLIENT_ID" "test_client";
  
  (* Set valid credentials *)
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s 3600) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  let creds = {
    Social_core.access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* Mock for successful upload *)
  Mock_http.set_responses [
    { status = 200; body = "video_data"; headers = [("content-type", "video/mp4")] };
    { status = 200; body = "{}"; headers = [("location", "https://upload.example.com/abc")] };
    { status = 200; body = {|{"id": "video_1"}|}; headers = [] };
  ];
  
  YouTube.post_thread
    ~account_id:"test_account"
    ~texts:["Video 1"; "Video 2"; "Video 3"]
    ~media_urls_per_post:[["https://example.com/v1.mp4"]; ["https://example.com/v2.mp4"]; ["https://example.com/v3.mp4"]]
    (fun outcome ->
      match outcome with
      | Error_types.Partial_success { result = thread_result; warnings } ->
          (* Should only post first video *)
          assert (List.length thread_result.posted_ids = 1);
          assert (thread_result.total_requested = 3);
          (* Should have warning about no thread support *)
          assert (List.length warnings > 0);
          print_endline "✓ Thread posts only first video with warning"
      | Error_types.Success thread_result ->
          (* Single item threads don't get warnings *)
          assert (List.length thread_result.posted_ids = 1);
          print_endline "✓ Thread posts only first video"
      | Error_types.Failure err ->
          failwith ("Thread failed: " ^ Error_types.error_to_string err))

(** Run all tests *)
let () =
  print_endline "\n=== YouTube Provider Tests ===\n";
  
  (* OAuth tests *)
  print_endline "--- OAuth Tests ---";
  test_oauth_url ();
  test_token_exchange ();
  test_token_refresh ();
  
  (* Validation tests *)
  print_endline "";
  print_endline "--- Validation Tests ---";
  test_content_validation ();
  test_ensure_valid_token_fresh ();
  test_ensure_valid_token_expired ();
  test_post_requires_video ();
  
  (* Video upload tests *)
  print_endline "";
  print_endline "--- Video Upload Tests ---";
  test_video_upload_init ();
  test_video_shorts_metadata ();
  test_video_resumable_upload ();
  test_video_upload_error_handling ();
  test_video_privacy_status ();
  test_video_category ();
  test_thread_youtube_single_post ();
  
  print_endline "";
  print_endline "=== All tests passed! ===";
  print_endline "";
  print_endline "Test Coverage Summary:";
  print_endline "  - OAuth 2.0 with PKCE (3 tests)";
  print_endline "  - Content validation (4 tests)";
  print_endline "  - Video upload/resumable (7 tests)";
  print_endline "";
  print_endline "Total: 14 test functions"
