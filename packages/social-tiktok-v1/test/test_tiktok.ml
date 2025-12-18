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
    if String.length url > 0 && (
      String.sub url (String.length url - min 5 (String.length url)) (min 5 (String.length url)) = "init/" ||
      String.ends_with ~suffix:"video/init/" url
    ) then
      on_success {
        Social_core.status = 200;
        headers = [("content-type", "application/json")];
        body = {|{"data":{"publish_id":"pub_123","upload_url":"https://upload.tiktok.com/video"}}|};
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
          (* Should contain Text_empty since TikTok requires video *)
          let has_text_empty = List.exists (function Error_types.Text_empty -> true | _ -> false) errs in
          if has_text_empty then begin
            error_received := true;
            print_endline "PASSED"
          end else
            failwith "Expected Text_empty validation error"
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

let () =
  print_endline "\n=== TikTok Provider Tests ===\n";
  
  print_endline "--- Validation ---";
  test_validate_video_ok ();
  test_validate_video_too_short ();
  test_validate_video_too_large ();
  test_privacy_level_roundtrip ();
  test_authorization_url ();
  
  print_endline "\n--- Post Operations ---";
  test_post_single_no_media ();
  test_post_single_caption_too_long ();
  test_post_single_success ();
  test_post_thread_success ();
  test_post_thread_empty ();
  
  print_endline "\n=== All 10 tests passed! ==="
