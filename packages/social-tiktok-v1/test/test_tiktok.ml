(** Basic tests for TikTok API types and validation *)

open Social_core

(** Mock HTTP client for testing *)
module Mock_http : Social_core.HTTP_CLIENT = struct
  let get ?headers:_ _url on_success _on_error =
    (* Return mock video content *)
    on_success {
      Social_core.status = 200;
      headers = [("content-type", "video/mp4")];
      body = "mock_video_content";
    }
  
  let post ?headers:_ ?body:_ url on_success _on_error =
    if String.ends_with ~suffix:"video/init/" url then
      on_success {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data":{"publish_id":"pub_123","upload_url":"https://upload.tiktok.com/video"}}|};
      }
    else if String.ends_with ~suffix:"creator_info/query/" url then
      on_success {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data":{"creator_avatar_url":"https://example.com/avatar.jpg","creator_username":"testuser","creator_nickname":"Test User","privacy_level_options":["PUBLIC_TO_EVERYONE","SELF_ONLY"],"comment_disabled":false,"duet_disabled":false,"stitch_disabled":false,"max_video_post_duration_sec":600}}|};
      }
    else if String.ends_with ~suffix:"status/fetch/" url then
      on_success {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data":{"status":"PUBLISH_COMPLETE","publicaly_available_post_id":[{"id":"video_123"}]}}|};
      }
    else
      on_success {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"access_token":"test_token","refresh_token":"test_refresh","expires_in":86400}|};
      }
  
  let post_multipart ?headers:_ ~parts:_ _url on_success _on_error =
    on_success {
      Social_core.status = 200;
      headers = [];
      body = "{}";
    }
  
  let put ?headers:_ ?body:_ _url on_success _on_error =
    on_success {
      Social_core.status = 200;
      headers = [];
      body = "{}";
    }
  
  let delete ?headers:_ _url on_success _on_error =
    on_success {
      Social_core.status = 200;
      headers = [];
      body = "{}";
    }
end

(** Mock config for testing *)
module Mock_config = struct
  module Http = Mock_http
  
  let get_env key = 
    match key with
    | "TIKTOK_CLIENT_KEY" -> Some "test_client_key"
    | "TIKTOK_CLIENT_SECRET" -> Some "test_client_secret"
    | _ -> None
  
  let get_credentials ~account_id:_ on_success _on_error =
    on_success {
      Social_core.access_token = "test_access_token";
      refresh_token = Some "test_refresh_token";
      expires_at = Some "2099-12-31T23:59:59Z";
      token_type = "Bearer";
    }
  
  let update_credentials ~account_id:_ ~credentials:_ on_success _on_error =
    on_success ()
  
  let encrypt _data on_success _on_error =
    on_success "encrypted_data"
  
  let decrypt _data on_success _on_error =
    on_success "decrypted_data"
  
  let update_health_status ~account_id:_ ~status:_ ~error_message:_ on_success _on_error =
    on_success ()
end

module TikTok = Social_tiktok_v1.Make(Mock_config)

(** Helper to handle outcome type for tests *)
let handle_outcome on_success on_error outcome =
  match outcome with
  | Error_types.Success result -> on_success result
  | Error_types.Partial_success { result; _ } -> on_success result
  | Error_types.Failure err -> on_error (Error_types.error_to_string err)

(** Helper to handle thread_result outcome *)
let handle_thread_outcome on_success on_error outcome =
  match outcome with
  | Error_types.Success result -> on_success result.Error_types.posted_ids
  | Error_types.Partial_success { result; _ } -> on_success result.Error_types.posted_ids
  | Error_types.Failure err -> on_error (Error_types.error_to_string err)

let test_validate_video_ok () =
  print_string "Test: validate_video OK... ";
  match Social_tiktok_v1.validate_video ~duration_sec:30 ~file_size_bytes:10_000_000 ~width:1080 ~height:1920 with
  | Ok () -> print_endline "PASSED"
  | Error msg -> failwith ("Expected Ok, got Error: " ^ msg)

let test_validate_video_too_short () =
  print_string "Test: validate_video too short... ";
  match Social_tiktok_v1.validate_video ~duration_sec:1 ~file_size_bytes:10_000_000 ~width:1080 ~height:1920 with
  | Error _ -> print_endline "PASSED"
  | Ok () -> failwith "Expected Error for too short video"

let test_validate_video_too_large () =
  print_string "Test: validate_video too large... ";
  match Social_tiktok_v1.validate_video ~duration_sec:30 ~file_size_bytes:100_000_000 ~width:1080 ~height:1920 with
  | Error _ -> print_endline "PASSED"
  | Ok () -> failwith "Expected Error for too large video"

let test_privacy_level_roundtrip () =
  print_string "Test: privacy_level roundtrip... ";
  let levels = [Social_tiktok_v1.PublicToEveryone; MutualFollowFriends; SelfOnly] in
  List.iter (fun level ->
    let s = Social_tiktok_v1.string_of_privacy_level level in
    let level2 = Social_tiktok_v1.privacy_level_of_string s in
    assert (level = level2)
  ) levels;
  print_endline "PASSED"

let test_authorization_url () =
  print_string "Test: authorization_url... ";
  let url = Social_tiktok_v1.get_authorization_url 
    ~client_id:"test_client"
    ~redirect_uri:"https://example.com/callback"
    ~scope:"user.info.basic,video.publish"
    ~state:"random_state"
  in
  assert (String.length url > 0);
  assert (String.sub url 0 5 = "https");
  print_endline "PASSED"

let test_post_single_no_media () =
  print_string "Test: post_single returns validation error without media... ";
  let error_received = ref false in
  TikTok.post_single
    ~account_id:"test_account"
    ~text:"Test caption"
    ~media_urls:[]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error errs) ->
          (* Should contain Media_required since TikTok requires video *)
          let has_media_required = List.exists (function Error_types.Media_required -> true | _ -> false) errs in
          if has_media_required then begin
            error_received := true;
            print_endline "PASSED"
          end else
            failwith "Expected Media_required validation error"
      | _ ->
          failwith "Expected validation error");
  assert !error_received

let test_post_single_caption_too_long () =
  print_string "Test: post_single returns validation error for long caption... ";
  let error_received = ref false in
  let long_caption = String.make 2300 'a' in
  TikTok.post_single
    ~account_id:"test_account"
    ~text:long_caption
    ~media_urls:["https://example.com/video.mp4"]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error _) ->
          error_received := true;
          print_endline "PASSED"
      | _ ->
          failwith "Expected validation error");
  assert !error_received

let test_post_single_success () =
  print_string "Test: post_single success... ";
  let success_called = ref false in
  TikTok.post_single
    ~account_id:"test_account"
    ~text:"Test caption"
    ~media_urls:["https://example.com/video.mp4"]
    (handle_outcome
      (fun _publish_id ->
        success_called := true;
        print_endline "PASSED")
      (fun err ->
        failwith ("Unexpected error: " ^ err)));
  assert !success_called

let test_post_thread_success () =
  print_string "Test: post_thread success... ";
  let success_called = ref false in
  TikTok.post_thread
    ~account_id:"test_account"
    ~texts:["First video"; "Second video"]
    ~media_urls_per_post:[["https://example.com/video1.mp4"]; ["https://example.com/video2.mp4"]]
    (handle_thread_outcome
      (fun post_ids ->
        assert (List.length post_ids = 2);
        success_called := true;
        print_endline "PASSED")
      (fun err ->
        failwith ("Unexpected error: " ^ err)));
  assert !success_called

let test_post_thread_empty () =
  print_string "Test: post_thread returns validation error for empty thread... ";
  let error_received = ref false in
  TikTok.post_thread
    ~account_id:"test_account"
    ~texts:[]
    ~media_urls_per_post:[]
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error _) ->
          error_received := true;
          print_endline "PASSED"
      | _ ->
          failwith "Expected validation error");
  assert !error_received

(** Helper to handle api_result for tests *)
let handle_api_result on_success on_error result =
  match result with
  | Ok value -> on_success value
  | Error err -> on_error (Error_types.error_to_string err)

let test_get_creator_info () =
  print_string "Test: get_creator_info success... ";
  let success_called = ref false in
  TikTok.get_creator_info
    ~account_id:"test_account"
    (handle_api_result
      (fun info ->
        assert (info.Social_tiktok_v1.creator_username = "testuser");
        assert (info.creator_nickname = "Test User");
        assert (info.max_video_post_duration_sec = 600);
        success_called := true;
        print_endline "PASSED")
      (fun err ->
        failwith ("Unexpected error: " ^ err)));
  assert !success_called

let test_check_publish_status () =
  print_string "Test: check_publish_status success... ";
  let success_called = ref false in
  TikTok.check_publish_status
    ~account_id:"test_account"
    ~publish_id:"pub_123"
    (handle_api_result
      (fun status ->
        (match status with
        | Social_tiktok_v1.Published video_id ->
            assert (video_id = "video_123");
            success_called := true;
            print_endline "PASSED"
        | _ ->
            failwith "Expected Published status"))
      (fun err ->
        failwith ("Unexpected error: " ^ err)));
  assert !success_called

let test_exchange_code () =
  print_string "Test: exchange_code success... ";
  let success_called = ref false in
  TikTok.exchange_code
    ~code:"auth_code_123"
    ~redirect_uri:"https://example.com/callback"
    (handle_api_result
      (fun creds ->
        assert (creds.Social_core.access_token = "test_token");
        assert (creds.refresh_token = Some "test_refresh");
        success_called := true;
        print_endline "PASSED")
      (fun err ->
        failwith ("Unexpected error: " ^ err)));
  assert !success_called

let test_get_oauth_url () =
  print_string "Test: get_oauth_url success... ";
  let success_called = ref false in
  TikTok.get_oauth_url
    ~redirect_uri:"https://example.com/callback"
    ~state:"random_state"
    ~code_verifier:""
    (handle_api_result
      (fun url ->
        assert (String.length url > 0);
        assert (String.sub url 0 5 = "https");
        success_called := true;
        print_endline "PASSED")
      (fun err ->
        failwith ("Unexpected error: " ^ err)));
  assert !success_called

(** {1 Video Upload Tests} *)

(** Test: Video validation - resolution too low *)
let test_validate_video_resolution_too_low () =
  print_string "Test: validate_video resolution too low... ";
  match Social_tiktok_v1.validate_video ~duration_sec:30 ~file_size_bytes:10_000_000 ~width:240 ~height:240 with
  | Error msg when String.length msg > 0 -> print_endline "PASSED"
  | Error _ -> print_endline "PASSED"
  | Ok () -> failwith "Expected Error for low resolution video"

(** Test: Video validation - resolution too high *)
let test_validate_video_resolution_too_high () =
  print_string "Test: validate_video resolution too high... ";
  match Social_tiktok_v1.validate_video ~duration_sec:30 ~file_size_bytes:10_000_000 ~width:8000 ~height:8000 with
  | Error msg when String.length msg > 0 -> print_endline "PASSED"
  | Error _ -> print_endline "PASSED"
  | Ok () -> failwith "Expected Error for high resolution video"

(** Test: Video validation - duration too long *)
let test_validate_video_duration_too_long () =
  print_string "Test: validate_video duration too long... ";
  match Social_tiktok_v1.validate_video ~duration_sec:700 ~file_size_bytes:10_000_000 ~width:1080 ~height:1920 with
  | Error msg when String.length msg > 0 -> print_endline "PASSED"
  | Error _ -> print_endline "PASSED"
  | Ok () -> failwith "Expected Error for too long video"

(** Test: Video at exact limits *)
let test_validate_video_at_limits () =
  print_string "Test: validate_video at exact limits... ";
  (* Exactly at max duration (600s), max size (50MB), max resolution (4096) *)
  match Social_tiktok_v1.validate_video 
    ~duration_sec:600 
    ~file_size_bytes:(50 * 1024 * 1024)
    ~width:4096 
    ~height:2160 with
  | Ok () -> print_endline "PASSED"
  | Error msg -> failwith ("Expected Ok at limits, got Error: " ^ msg)

(** Test: Caption validation - valid caption *)
let test_validate_caption_valid () =
  print_string "Test: validate_caption valid... ";
  match Social_tiktok_v1.validate_caption "Check out my video! #tiktok #viral" with
  | Ok () -> print_endline "PASSED"
  | Error msg -> failwith ("Expected Ok, got Error: " ^ msg)

(** Test: Caption validation - too long *)
let test_validate_caption_too_long () =
  print_string "Test: validate_caption too long... ";
  let long_caption = String.make 2300 'a' in
  match Social_tiktok_v1.validate_caption long_caption with
  | Error msg when String.length msg > 0 -> print_endline "PASSED"
  | Error _ -> print_endline "PASSED"
  | Ok () -> failwith "Expected Error for too long caption"

(** Test: Post info creation with defaults *)
let test_make_post_info_defaults () =
  print_string "Test: make_post_info with defaults... ";
  let info = Social_tiktok_v1.make_post_info ~title:"Test video" () in
  assert (info.title = "Test video");
  assert (info.privacy_level = Social_tiktok_v1.SelfOnly);  (* Default is SelfOnly *)
  assert (info.disable_duet = false);
  assert (info.disable_comment = false);
  assert (info.disable_stitch = false);
  assert (info.video_cover_timestamp_ms = None);
  print_endline "PASSED"

(** Test: Post info creation with all options *)
let test_make_post_info_all_options () =
  print_string "Test: make_post_info with all options... ";
  let info = Social_tiktok_v1.make_post_info 
    ~title:"My viral video"
    ~privacy_level:Social_tiktok_v1.PublicToEveryone
    ~disable_duet:true
    ~disable_comment:true
    ~disable_stitch:true
    ~video_cover_timestamp_ms:5000
    () in
  assert (info.title = "My viral video");
  assert (info.privacy_level = Social_tiktok_v1.PublicToEveryone);
  assert (info.disable_duet = true);
  assert (info.disable_comment = true);
  assert (info.disable_stitch = true);
  assert (info.video_cover_timestamp_ms = Some 5000);
  print_endline "PASSED"

(** Test: Post info to JSON serialization *)
let test_post_info_to_json () =
  print_string "Test: post_info_to_json... ";
  let info = Social_tiktok_v1.make_post_info 
    ~title:"Test"
    ~privacy_level:Social_tiktok_v1.MutualFollowFriends
    () in
  let json = Social_tiktok_v1.post_info_to_json info in
  let json_str = Yojson.Basic.to_string json in
  assert (String.length json_str > 0);
  assert (try ignore (String.index json_str '{'); true with Not_found -> false);
  print_endline "PASSED"

(** Test: Privacy level string roundtrip *)
let test_all_privacy_levels () =
  print_string "Test: all privacy levels... ";
  let levels = [
    Social_tiktok_v1.PublicToEveryone;
    Social_tiktok_v1.MutualFollowFriends;
    Social_tiktok_v1.SelfOnly
  ] in
  List.iter (fun level ->
    let s = Social_tiktok_v1.string_of_privacy_level level in
    let level2 = Social_tiktok_v1.privacy_level_of_string s in
    assert (level = level2)
  ) levels;
  print_endline "PASSED"

(** Test: Init video upload request *)
let test_init_video_upload () =
  print_string "Test: init_video_upload... ";
  let success_called = ref false in
  let post_info = Social_tiktok_v1.make_post_info 
    ~title:"Test upload" 
    ~privacy_level:Social_tiktok_v1.SelfOnly
    () in
  TikTok.init_video_upload
    ~account_id:"test_account"
    ~post_info
    ~video_size:1000000
    (handle_api_result
      (fun (publish_id, upload_url) ->
        assert (publish_id = "pub_123");
        assert (String.length upload_url > 0);
        success_called := true;
        print_endline "PASSED")
      (fun err ->
        failwith ("Unexpected error: " ^ err)));
  assert !success_called

(** Test: post_info privacy level options *)
let test_post_info_privacy_options () =
  print_string "Test: post_info with all privacy options... ";
  
  (* Test each privacy level creates valid post_info *)
  let levels = [
    Social_tiktok_v1.PublicToEveryone;
    Social_tiktok_v1.MutualFollowFriends;
    Social_tiktok_v1.SelfOnly;
  ] in
  
  List.iter (fun privacy ->
    let info = Social_tiktok_v1.make_post_info ~title:"Test" ~privacy_level:privacy () in
    assert (info.privacy_level = privacy);
    (* Verify JSON serialization includes correct privacy level *)
    let json = Social_tiktok_v1.post_info_to_json info in
    let json_str = Yojson.Basic.to_string json in
    let expected_str = Social_tiktok_v1.string_of_privacy_level privacy in
    assert (try ignore (Str.search_forward (Str.regexp_string expected_str) json_str 0); true with Not_found -> false)
  ) levels;
  
  print_endline "PASSED"

(** Test: Thread validation - missing media *)
let test_thread_validation_missing_media () =
  print_string "Test: thread validation - missing media... ";
  let error_received = ref false in
  TikTok.post_thread
    ~account_id:"test_account"
    ~texts:["First post"; "Second post"]
    ~media_urls_per_post:[["https://example.com/video1.mp4"]; []]  (* Second post missing media *)
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error _) ->
          error_received := true;
          print_endline "PASSED"
      | Error_types.Partial_success _ ->
          (* Partial success could also happen if first post succeeded but second validation failed *)
          error_received := true;
          print_endline "PASSED"
      | _ ->
          failwith "Expected validation error for missing media");
  assert !error_received

(** Test: Content validation function *)
let test_validate_content () =
  print_string "Test: validate_content... ";
  (* Valid content *)
  (match TikTok.validate_content ~text:"Valid caption #tiktok" with
   | Ok () -> ()
   | Error msg -> failwith ("Valid content should pass: " ^ msg));
  
  (* Too long content *)
  let long_text = String.make 2500 'x' in
  (match TikTok.validate_content ~text:long_text with
   | Error _ -> ()  (* Expected *)
   | Ok () -> failwith "Long content should fail");
  
  print_endline "PASSED"

let () =
  print_endline "\n=== TikTok Provider Tests ===\n";
  
  print_endline "--- Video Validation ---";
  test_validate_video_ok ();
  test_validate_video_too_short ();
  test_validate_video_too_large ();
  test_validate_video_resolution_too_low ();
  test_validate_video_resolution_too_high ();
  test_validate_video_duration_too_long ();
  test_validate_video_at_limits ();
  
  print_endline "\n--- Caption Validation ---";
  test_validate_caption_valid ();
  test_validate_caption_too_long ();
  test_validate_content ();
  
  print_endline "\n--- Privacy & Post Info ---";
  test_privacy_level_roundtrip ();
  test_all_privacy_levels ();
  test_make_post_info_defaults ();
  test_make_post_info_all_options ();
  test_post_info_to_json ();
  test_authorization_url ();
  
  print_endline "\n--- Post Operations ---";
  test_post_single_no_media ();
  test_post_single_caption_too_long ();
  test_post_single_success ();
  test_post_info_privacy_options ();
  test_post_thread_success ();
  test_post_thread_empty ();
  test_thread_validation_missing_media ();
  
  print_endline "\n--- API Operations ---";
  test_get_creator_info ();
  test_check_publish_status ();
  test_init_video_upload ();
  test_exchange_code ();
  test_get_oauth_url ();
  
  print_endline "\n=== All 28 tests passed! ==="
