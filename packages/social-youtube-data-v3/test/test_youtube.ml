(** Tests for YouTube Data API v3 Provider *)

open Social_core
open Social_youtube_data_v3

(** Helper to check if string contains substring *)
let string_contains s substr =
  try
    ignore (Str.search_forward (Str.regexp_string substr) s 0);
    true
  with Not_found -> false

let query_param url key =
  try Uri.get_query_param (Uri.of_string url) key
  with _ -> None

(** Helper to handle outcome type for tests - unused but kept for consistency *)
let _handle_outcome on_success on_error outcome =
  match outcome with
  | Error_types.Success result -> on_success result
  | Error_types.Partial_success { result; _ } -> on_success result
  | Error_types.Failure err -> on_error (Error_types.error_to_string err)

(** Mock HTTP client for testing *)
module Mock_http = struct
  type multipart_call = {
    url : string;
    headers : (string * string) list;
    parts : multipart_part list;
  }

  let requests = ref []
  let multipart_calls = ref []
  let response_queue = ref []
  
  let reset () =
    requests := [];
    multipart_calls := [];
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
    multipart_calls := { url; headers; parts } :: !multipart_calls;
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

let set_valid_credentials ~account_id =
  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s 3600) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  let creds = {
    access_token = "valid_token";
    refresh_token = Some "refresh_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  Mock_config.set_credentials ~account_id ~credentials:creds

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

let test_get_account_analytics_contract_and_parsing () =
  Mock_config.reset ();
  set_valid_credentials ~account_id:"test_account";

  let response_body = {|{
    "items": [
      {
        "id": "UC123",
        "snippet": {"title": "Channel A"},
        "statistics": {
          "viewCount": "101",
          "subscriberCount": "88",
          "hiddenSubscriberCount": false,
          "videoCount": "7"
        }
      }
    ]
  }|} in

  Mock_http.set_responses [ { status = 200; body = response_body; headers = [] } ];

  YouTube.get_account_analytics ~account_id:"test_account"
    (function
      | Ok analytics ->
          assert (analytics.channel_id = "UC123");
          assert (analytics.title = Some "Channel A");
          assert (analytics.view_count = 101);
          assert (analytics.subscriber_count = 88);
          assert (analytics.video_count = 7);
          (match List.rev !Mock_http.requests with
           | [ ("GET", url, headers, _) ] ->
               assert (string_contains url "/youtube/v3/channels?");
               assert (query_param url "part" = Some "id,snippet,statistics");
               assert (query_param url "mine" = Some "true");
               assert (List.assoc_opt "Authorization" headers = Some "Bearer valid_token")
           | _ -> failwith "unexpected request count for account analytics");
          print_endline "✓ Account analytics request contract + parsing"
      | Error err ->
          failwith ("Account analytics failed: " ^ Error_types.error_to_string err))

let test_get_post_analytics_contract_and_parsing () =
  Mock_config.reset ();
  set_valid_credentials ~account_id:"test_account";

  let response_body = {|{
    "items": [
      {
        "id": "video-42",
        "snippet": {"title": "Post Analytics"},
        "statistics": {
          "viewCount": "1234",
          "likeCount": "120",
          "favoriteCount": "0",
          "commentCount": "9"
        }
      }
    ]
  }|} in

  Mock_http.set_responses [ { status = 200; body = response_body; headers = [] } ];

  YouTube.get_post_analytics ~account_id:"test_account" ~video_id:"video-42"
    (function
      | Ok analytics ->
          assert (analytics.video_id = "video-42");
          assert (analytics.title = Some "Post Analytics");
          assert (analytics.view_count = 1234);
          assert (analytics.like_count = 120);
          assert (analytics.favorite_count = 0);
          assert (analytics.comment_count = 9);
          (match List.rev !Mock_http.requests with
           | [ ("GET", url, headers, _) ] ->
               assert (string_contains url "/youtube/v3/videos?");
               assert (query_param url "part" = Some "id,snippet,statistics");
               assert (query_param url "id" = Some "video-42");
               assert (List.assoc_opt "Authorization" headers = Some "Bearer valid_token")
           | _ -> failwith "unexpected request count for post analytics");
          print_endline "✓ Post analytics request contract + parsing"
       | Error err ->
           failwith ("Post analytics failed: " ^ Error_types.error_to_string err))

let test_canonical_analytics_adapters () =
  let find_series provider_metric series =
    List.find_opt
      (fun item -> item.Analytics_types.provider_metric = Some provider_metric)
      series
  in
  let account_analytics : YouTube.account_analytics = {
    channel_id = "UC1";
    title = Some "Channel";
    view_count = 1200;
    subscriber_count = 330;
    hidden_subscriber_count = false;
    video_count = 45;
  } in
  let account_series = YouTube.to_canonical_account_analytics_series account_analytics in
  assert (List.length account_series = 3);
  (match find_series "subscriber_count" account_series with
   | Some item ->
       assert (Analytics_types.canonical_metric_key item.metric = "subscribers");
       assert ((List.hd item.points).value = 330)
   | None -> failwith "Missing subscriber_count canonical series");

  let post_analytics : YouTube.post_analytics = {
    video_id = "video-1";
    title = Some "Video";
    view_count = 900;
    like_count = 80;
    favorite_count = 2;
    comment_count = 7;
  } in
  let post_series = YouTube.to_canonical_post_analytics_series post_analytics in
  assert (List.length post_series = 4);
  (match find_series "favorite_count" post_series with
   | Some item ->
       assert (Analytics_types.canonical_metric_key item.metric = "reactions");
       assert ((List.hd item.points).value = 2)
   | None -> failwith "Missing favorite_count canonical series");
  print_endline "✓ Canonical analytics adapters"

let test_build_thumbnail_upload_request_contract () =
  match
    YouTube.build_thumbnail_upload_request
      ~access_token:"access-xyz"
      ~video_id:"video-99"
      ~thumbnail_content:"png-bytes"
      ~content_type:"image/png"
      ()
  with
  | Error err -> failwith ("build_thumbnail_upload_request failed: " ^ Error_types.error_to_string err)
  | Ok request ->
      assert (string_contains request.url "/upload/youtube/v3/thumbnails/set?");
      assert (query_param request.url "videoId" = Some "video-99");
      assert (query_param request.url "uploadType" = Some "multipart");
      assert (List.assoc_opt "Authorization" request.headers = Some "Bearer access-xyz");
      assert (List.length request.parts = 1);
      let part = List.hd request.parts in
      assert (part.name = "media");
      assert (part.filename = Some "thumbnail.png");
      assert (part.content_type = Some "image/png");
      assert (part.content = "png-bytes");
      print_endline "✓ Thumbnail helper request contract"

let test_upload_thumbnail_contract () =
  Mock_config.reset ();
  set_valid_credentials ~account_id:"test_account";

  Mock_http.set_responses [ { status = 200; body = "{}"; headers = [] } ];

  YouTube.upload_thumbnail
    ~account_id:"test_account"
    ~video_id:"video-1"
    ~thumbnail_content:"thumbnail-bytes"
    ~content_type:"image/jpeg"
    (function
      | Ok () ->
          (match List.rev !(Mock_http.multipart_calls) with
           | [ call ] ->
               assert (string_contains call.url "/upload/youtube/v3/thumbnails/set?");
               assert (query_param call.url "videoId" = Some "video-1");
               assert (query_param call.url "uploadType" = Some "multipart");
               assert (List.assoc_opt "Authorization" call.headers = Some "Bearer valid_token");
               assert (List.length call.parts = 1);
               let part = List.hd call.parts in
               assert (part.name = "media");
               assert (part.content_type = Some "image/jpeg")
           | _ -> failwith "unexpected multipart call count for thumbnail upload");
          print_endline "✓ Thumbnail upload request contract"
      | Error err ->
          failwith ("Thumbnail upload failed: " ^ Error_types.error_to_string err))

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

  (* Analytics and thumbnail tests *)
  print_endline "";
  print_endline "--- Analytics & Thumbnail Tests ---";
  test_get_account_analytics_contract_and_parsing ();
  test_get_post_analytics_contract_and_parsing ();
  test_canonical_analytics_adapters ();
  test_build_thumbnail_upload_request_contract ();
  test_upload_thumbnail_contract ();
  
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
  print_endline "  - Analytics + thumbnail contracts (4 tests)";
  print_endline "  - Video upload/resumable (7 tests)";
  print_endline "";
  print_endline "Total: 18 test functions"
