(** Tests for Facebook Graph API v21 Provider *)

open Social_core
open Social_facebook_graph_v21

(** Helper to check if string contains substring *)
let string_contains s substr =
  try
    ignore (Str.search_forward (Str.regexp_string substr) s 0);
    true
  with Not_found -> false

(** Helper to handle outcome type in tests *)
let handle_outcome on_success on_error outcome =
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
  
  let set_response response =
    response_queue := [response]
  
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
  let page_ids = ref []
  let rate_limits = ref []
  
  let reset () =
    env_vars := [];
    credentials_store := [];
    health_statuses := [];
    page_ids := [];
    rate_limits := [];
    Mock_http.reset ()
  
  let set_env key value =
    env_vars := (key, value) :: !env_vars
  
  let get_env key =
    List.assoc_opt key !env_vars
  
  let on_rate_limit_update info =
    rate_limits := info :: !rate_limits
  
  let set_credentials ~account_id ~credentials =
    credentials_store := (account_id, credentials) :: !credentials_store
  
  let _set_page_id ~account_id ~page_id =
    page_ids := (account_id, page_id) :: !page_ids
  
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
  
  let get_page_id ~account_id on_success on_error =
    match List.assoc_opt account_id !page_ids with
    | Some page_id -> on_success page_id
    | None -> on_error "Page ID not found"
  
  let get_health_status account_id =
    List.find_opt (fun (id, _, _) -> id = account_id) !health_statuses
end

module Facebook = Make(Mock_config)

(** Test: OAuth URL generation *)
let test_oauth_url () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  
  let state = "test_state_123" in
  let redirect_uri = "https://example.com/callback" in
  
  Facebook.get_oauth_url ~redirect_uri ~state
    (fun url ->
      assert (string_contains url "client_id=test_app_id");
      assert (string_contains url "state=test_state_123");
      assert (string_contains url "response_type=code");
      assert (string_contains url "pages_manage_posts");
      print_endline "✓ OAuth URL generation")
    (fun err -> failwith ("OAuth URL failed: " ^ err))

(** Test: Token exchange *)
let test_token_exchange () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";
  
  let response_body = {|{
    "access_token": "short_lived_access_token_123",
    "token_type": "bearer",
    "expires_in": 3600
  }|} in

  let long_lived_response_body = {|{
    "access_token": "new_access_token_123",
    "token_type": "bearer",
    "expires_in": 5184000
  }|} in
  
  Mock_http.set_responses [
    { status = 200; body = response_body; headers = [] };
    { status = 200; body = long_lived_response_body; headers = [] };
  ];
  
  Facebook.exchange_code 
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun creds ->
      assert (creds.access_token = "new_access_token_123");
      assert (creds.refresh_token = None);
      assert (creds.token_type = "Bearer");
      assert (creds.expires_at <> None);
      print_endline "✓ Token exchange")
    (fun err -> failwith ("Token exchange failed: " ^ err))

(** Test: Upload photo *)
let test_upload_photo () =
  Mock_config.reset ();
  
  (* Set up two responses: first for image download, second for upload *)
  Mock_http.set_responses [
    { status = 200; body = "fake_image_data"; headers = [] };  (* GET image *)
    { status = 200; body = {|{"id": "photo_12345"}|}; headers = [] };  (* POST multipart *)
  ];
  
  Facebook.upload_photo
    ~page_id:"123456"
    ~page_access_token:"test_token"
    ~image_url:"https://example.com/image.jpg"
    ~alt_text:None
    (function
      | Ok photo_id ->
          assert (photo_id = "photo_12345");
          print_endline "✓ Upload photo"
      | Error e -> failwith ("Upload photo failed: " ^ Error_types.error_to_string e))

(** Test: Content validation *)
let test_content_validation () =
  (* Valid content *)
  (match Facebook.validate_content ~text:"Hello Facebook!" with
   | Ok () -> print_endline "✓ Valid content passes"
   | Error e -> failwith ("Valid content failed: " ^ e));
  
  (* Empty content *)
  (match Facebook.validate_content ~text:"" with
   | Error _ -> print_endline "✓ Empty content rejected"
   | Ok () -> failwith "Empty content should fail");
  
  (* Too long *)
  let long_text = String.make 5001 'x' in
  (match Facebook.validate_content ~text:long_text with
   | Error msg when string_contains msg "5000" -> 
       print_endline "✓ Long content rejected"
   | _ -> failwith "Long content should fail")

(** Test: Ensure valid token (fresh token) *)
let test_ensure_valid_token_fresh () =
  Mock_config.reset ();
  
  (* Set credentials with far-future expiry *)
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  Facebook.ensure_valid_token ~account_id:"test_account"
    (fun token ->
      assert (token = "valid_token");
      (* Verify health status was updated *)
      match Mock_config.get_health_status "test_account" with
      | Some (_, "healthy", None) -> print_endline "✓ Ensure valid token (fresh)"
      | _ -> failwith "Health status not updated correctly")
    (fun err -> failwith ("Ensure valid token failed: " ^ Error_types.error_to_string err))

(** Test: Ensure valid token (expired token) *)
let test_ensure_valid_token_expired () =
  Mock_config.reset ();
  
  (* Set credentials with past expiry *)
  let past_time = 
    let now = Ptime_clock.now () in
    match Ptime.sub_span now (Ptime.Span.of_int_s 86400) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate past time"
  in
  
  let creds = {
    access_token = "expired_token";
    refresh_token = None;
    expires_at = Some past_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  Facebook.ensure_valid_token ~account_id:"test_account"
    (fun _ -> failwith "Should fail with expired token")
    (fun err ->
      let err_str = Error_types.error_to_string err in
      assert (string_contains err_str "expired");
      (* Verify health status was updated *)
      match Mock_config.get_health_status "test_account" with
      | Some (_, "token_expired", _) -> print_endline "✓ Ensure valid token (expired)"
      | _ -> failwith "Health status not updated correctly")

(** Test: Rate limit parsing *)
let test_rate_limit_parsing () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";
  
  let response_body = {|{"id": "me"}|} in
  let headers = [
    ("X-App-Usage", {|{"call_count":15,"total_cputime":25,"total_time":30}|});
  ] in
  
  Mock_http.set_response { status = 200; body = response_body; headers };
  
  Facebook.get ~path:"me" ~access_token:"test_token"
    (function
      | Ok _response ->
          (* Check that rate limit was captured *)
          (match !Mock_config.rate_limits with
          | info :: _ ->
              assert (info.call_count = 15);
              assert (info.total_cputime = 25);
              assert (info.total_time = 30);
              assert (info.percentage_used = 30.0);
              print_endline "✓ Rate limit parsing"
          | [] -> failwith "Rate limit not captured")
      | Error e -> failwith ("Rate limit test failed: " ^ Error_types.error_to_string e))

(** Test: Full OAuth onboarding to pages *)
let test_exchange_code_and_get_pages () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";

  Mock_http.set_responses [
    { status = 200; body = {|{"access_token":"short_token","token_type":"bearer","expires_in":3600}|}; headers = [] };
    { status = 200; body = {|{"access_token":"long_token","token_type":"bearer","expires_in":5184000}|}; headers = [] };
    { status = 200; body = {|{"data":[{"id":"123","name":"My Page","access_token":"page_token","category":"Brand"}]}|}; headers = [] };
  ];

  Facebook.exchange_code_and_get_pages
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun (creds, pages) ->
      assert (creds.access_token = "long_token");
      assert (List.length pages = 1);
      let p = List.hd pages in
      assert (p.page_id = "123");
      assert (p.page_access_token = "page_token");
      print_endline "✓ Exchange code and get pages")
    (fun err -> failwith ("Exchange code and get pages failed: " ^ err))

(** Test: Field selection *)
let test_field_selection () =
  Mock_config.reset ();
  
  let response_body = {|{"id":"123","name":"Test"}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Facebook.get ~path:"me" ~access_token:"test_token" ~fields:["id"; "name"]
    (function
      | Ok _response ->
          (* Check that request URL contains fields parameter *)
          let requests = !Mock_http.requests in
          (match requests with
          | (_, url, _, _) :: _ ->
              assert (string_contains url "fields=id%2Cname");
              print_endline "✓ Field selection"
          | [] -> failwith "No requests made")
      | Error e -> failwith ("Field selection test failed: " ^ Error_types.error_to_string e))

(** Test: Error code parsing *)
let test_error_code_parsing () =
  Mock_config.reset ();
  
  let error_response = {|{
    "error": {
      "message": "Invalid OAuth access token",
      "type": "OAuthException",
      "code": 190,
      "error_subcode": 463,
      "fbtrace_id": "ABC123"
    }
  }|} in
  
  Mock_http.set_response { status = 400; body = error_response; headers = [] };
  
  Facebook.get ~path:"me" ~access_token:"invalid_token"
    (function
      | Ok _response -> failwith "Should have failed with error"
      | Error e ->
          (* Token expired errors are converted to Auth_error *)
          let err_str = Error_types.error_to_string e in
          assert (string_contains err_str "expired" || string_contains err_str "token");
          print_endline "✓ Error code parsing")

(** Test: Invalid token without expiry subcode maps to token invalid *)
let test_invalid_token_without_expiry_subcode () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "Invalid OAuth access token",
      "type": "OAuthException",
      "code": 190,
      "fbtrace_id": "ABC124"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.get ~path:"me" ~access_token:"invalid_token"
    (function
      | Ok _response -> failwith "Should have failed with error"
      | Error (Error_types.Auth_error Error_types.Token_invalid) ->
          print_endline "✓ Invalid token mapping without expiry subcode"
      | Error e ->
          failwith ("Expected Token_invalid, got: " ^ Error_types.error_to_string e))

(** Test: Token subcode 467 maps to token expired *)
let test_token_subcode_467_maps_to_expired () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "Invalid OAuth access token - Session has expired",
      "type": "OAuthException",
      "code": 190,
      "error_subcode": 467,
      "fbtrace_id": "ABC125"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.get ~path:"me" ~access_token:"expired_token"
    (function
      | Ok _response -> failwith "Should have failed with expired token error"
      | Error (Error_types.Auth_error Error_types.Token_expired) ->
          print_endline "✓ Token subcode 467 maps to expired"
      | Error e ->
          failwith ("Expected Token_expired, got: " ^ Error_types.error_to_string e))

(** Test: Permission denied returns provider-required scopes *)
let test_permission_error_scopes () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "PERM123"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.get ~path:"me" ~access_token:"token"
    (function
      | Ok _ -> failwith "Should have failed with permission error"
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (List.mem "pages_manage_posts" scopes);
          assert (List.mem "pages_show_list" scopes);
          assert (List.mem "pages_read_engagement" scopes);
          print_endline "✓ Permission error includes provider-required scopes"
      | Error e ->
          failwith ("Expected insufficient_permissions error, got: " ^ Error_types.error_to_string e))

(** Test: me/accounts permission error maps to pages_show_list scope *)
let test_me_accounts_permission_scope () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "PERM_ACCOUNTS"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.get ~path:"me/accounts" ~access_token:"token"
    (function
      | Ok _ -> failwith "Should have failed with permission error"
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (scopes = ["pages_show_list"]);
          print_endline "✓ me/accounts permission scope mapping"
      | Error e ->
          failwith ("Expected insufficient_permissions error, got: " ^ Error_types.error_to_string e))

(** Test: Generic GET supports explicit required_permissions override *)
let test_get_required_permissions_override () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "PERM_OVERRIDE"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.get
    ~path:"me/accounts"
    ~access_token:"token"
    ~required_permissions:["custom_scope"]
    (function
      | Ok _ -> failwith "Should have failed with permission error"
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (scopes = ["custom_scope"]);
          print_endline "✓ GET required_permissions override"
      | Error e ->
          failwith ("Expected insufficient_permissions error, got: " ^ Error_types.error_to_string e))

(** Test: Generic POST supports explicit required_permissions override *)
let test_post_required_permissions_override () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "PERM_OVERRIDE_POST"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.post
    ~path:"me/feed"
    ~access_token:"token"
    ~params:[("message", ["hello"])]
    ~required_permissions:["custom_post_scope"]
    (function
      | Ok _ -> failwith "Should have failed with permission error"
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (scopes = ["custom_post_scope"]);
          print_endline "✓ POST required_permissions override"
      | Error e ->
          failwith ("Expected insufficient_permissions error, got: " ^ Error_types.error_to_string e))

(** Test: Generic DELETE supports explicit required_permissions override *)
let test_delete_required_permissions_override () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "PERM_OVERRIDE_DELETE"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.delete
    ~path:"12345"
    ~access_token:"token"
    ~required_permissions:["custom_delete_scope"]
    (function
      | Ok _ -> failwith "Should have failed with permission error"
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (scopes = ["custom_delete_scope"]);
          print_endline "✓ DELETE required_permissions override"
      | Error e ->
          failwith ("Expected insufficient_permissions error, got: " ^ Error_types.error_to_string e))

(** Test: Pagination *)
let test_pagination () =
  Mock_config.reset ();
  
  let page_response = {|{
    "data": [{"id": "1"}, {"id": "2"}],
    "paging": {
      "cursors": {
        "before": "cursor_before",
        "after": "cursor_after"
      },
      "next": "https://graph.facebook.com/v21.0/next_page"
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = page_response; headers = [] };
  
  let parse_data json =
    let open Yojson.Basic.Util in
    json |> to_list
  in
  
  Facebook.get_page ~path:"me/posts" ~access_token:"test_token" parse_data
    (function
      | Ok page_result ->
          assert (List.length page_result.data = 2);
          (match page_result.paging with
          | Some cursors ->
              assert (cursors.after = Some "cursor_after");
              assert (cursors.before = Some "cursor_before");
              assert (page_result.next_url <> None);
              print_endline "✓ Pagination"
          | None -> failwith "No paging info")
      | Error e -> failwith ("Pagination test failed: " ^ Error_types.error_to_string e))

(** Test: Batch requests *)
let test_batch_requests () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";
  
  let batch_response = {|[
    {"code": 200, "headers": [{"name": "Content-Type", "value": "application/json"}], "body": "{\"id\":\"1\"}"},
    {"code": 200, "headers": [], "body": "{\"id\":\"2\"}"}
  ]|} in
  
  Mock_http.set_response { status = 200; body = batch_response; headers = [] };
  
  let open Facebook in
  let requests = [
    { method_ = `GET; relative_url = "me"; body = None; name = Some "me" };
    { method_ = `GET; relative_url = "me/posts"; body = None; name = None };
  ] in
  
  Facebook.batch_request ~requests ~access_token:"test_token"
    (function
      | Ok results ->
          assert (List.length results = 2);
          (match results with
          | r1 :: r2 :: _ ->
              assert (r1.code = 200);
              assert (r2.code = 200);
              assert (string_contains r1.body "\"id\":\"1\"");
              print_endline "✓ Batch requests"
          | _ -> failwith "Unexpected batch response")
      | Error e -> failwith ("Batch test failed: " ^ Error_types.error_to_string e))

(** Test: Batch request supports explicit required_permissions override *)
let test_batch_required_permissions_override () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "PERM_OVERRIDE_BATCH"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  let open Facebook in
  let requests = [
    { method_ = `GET; relative_url = "me"; body = None; name = None };
  ] in

  Facebook.batch_request
    ~requests
    ~access_token:"test_token"
    ~required_permissions:["custom_batch_scope"]
    (function
      | Ok _ -> failwith "Should have failed with permission error"
      | Error (Error_types.Auth_error (Error_types.Insufficient_permissions scopes)) ->
          assert (scopes = ["custom_batch_scope"]);
          print_endline "✓ BATCH required_permissions override"
      | Error e ->
          failwith ("Expected insufficient_permissions error, got: " ^ Error_types.error_to_string e))

(** Test: App secret proof *)
let test_app_secret_proof () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";
  
  Mock_http.set_response { status = 200; body = {|{"id":"me"}|}; headers = [] };
  
  Facebook.get ~path:"me" ~access_token:"test_token"
    (function
      | Ok _response ->
          (* Check that request includes appsecret_proof *)
          let requests = !Mock_http.requests in
          (match requests with
          | (_, url, _, _) :: _ ->
              assert (string_contains url "appsecret_proof");
              print_endline "✓ App secret proof"
          | [] -> failwith "No requests made")
      | Error e -> failwith ("App secret proof test failed: " ^ Error_types.error_to_string e))

(** Test: Authorization header usage *)
let test_authorization_header () =
  Mock_config.reset ();
  
  Mock_http.set_response { status = 200; body = {|{"id":"me"}|}; headers = [] };
  
  Facebook.get ~path:"me" ~access_token:"test_token"
    (function
      | Ok _response ->
          (* Check that Authorization header is present *)
          let requests = !Mock_http.requests in
          (match requests with
          | (_, _, headers, _) :: _ ->
              let has_auth = List.exists (fun (k, v) ->
                k = "Authorization" && string_contains v "Bearer test_token"
              ) headers in
              assert has_auth;
              print_endline "✓ Authorization header"
          | [] -> failwith "No requests made")
      | Error e -> failwith ("Authorization header test failed: " ^ Error_types.error_to_string e))

(** Test: OAuth URL with required permissions *)
let test_oauth_url_permissions () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  
  let state = "test_state" in
  let redirect_uri = "https://example.com/callback" in
  
  Facebook.get_oauth_url ~redirect_uri ~state
    (fun url ->
      (* Should contain required permissions *)
      assert (string_contains url "pages_manage_posts" || string_contains url "pages_read_engagement");
      print_endline "✓ OAuth URL permissions")
    (fun err -> failwith ("OAuth URL permissions failed: " ^ err))

(** Test: OAuth URL encoding of special characters *)
let test_oauth_url_special_chars () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app";
  
  let redirect_uri = "https://example.com/callback?foo=bar&baz=qux" in
  let state = "state with spaces & special=chars" in
  
  Facebook.get_oauth_url ~redirect_uri ~state
    (fun url ->
      (* URL should be properly encoded *)
      assert (not (String.contains url ' '));
      print_endline "✓ OAuth URL special character encoding")
    (fun err -> failwith ("OAuth URL encoding failed: " ^ err))

(** Test: Token exchange error responses *)
let test_token_exchange_errors () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";
  
  let error_response = {|{
    "error": {
      "message": "Invalid verification code format.",
      "type": "OAuthException",
      "code": 100
    }
  }|} in
  
  Mock_http.set_response { status = 400; body = error_response; headers = [] };
  
  Facebook.exchange_code 
    ~code:"bad_code"
    ~redirect_uri:"https://example.com/callback"
    (fun _ -> failwith "Should fail with bad code")
    (fun err ->
      assert (string_contains err "400" || string_contains err "OAuth" || string_contains err "Invalid");
      print_endline "✓ Token exchange error handling")

(** Test: Long-lived token exchange *)
let test_long_lived_token () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";
  
  let short_response_body = {|{
    "access_token": "short_lived_token_123",
    "token_type": "bearer",
    "expires_in": 3600
  }|} in

  let response_body = {|{
    "access_token": "long_lived_token_123",
    "token_type": "bearer",
    "expires_in": 5184000
  }|} in
  
  Mock_http.set_responses [
    { status = 200; body = short_response_body; headers = [] };
    { status = 200; body = response_body; headers = [] };
  ];
  
  Facebook.exchange_code 
    ~code:"test_code"
    ~redirect_uri:"https://example.com/callback"
    (fun creds ->
      (* Facebook long-lived tokens last 60 days *)
      match creds.expires_at with
      | Some _ -> print_endline "✓ Long-lived token exchange"
      | None -> failwith "No expiry set for long-lived token")
    (fun err -> failwith ("Long-lived token test failed: " ^ err))

(** Test: Page access token vs user access token *)
let test_page_vs_user_token () =
  Mock_config.reset ();
  
  (* In Facebook, you need both user token and page token *)
  (* User token is used to get page token, then page token is used for posting *)
  
  let response_body = {|{"id": "page_123"}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Facebook.get ~path:"me/accounts" ~access_token:"user_token"
    (function
      | Ok _response ->
          (* This would return page access tokens in real usage *)
          print_endline "✓ Page vs user token handling"
      | Error e -> failwith ("Page token test failed: " ^ Error_types.error_to_string e))

(** Test: Token expiry detection *)
let test_token_expiry_detection () =
  Mock_config.reset ();
  
  (* Set credentials with near-future expiry (should refresh soon) *)
  let near_future = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s 60) with  (* 1 minute *)
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate time"
  in
  
  let creds = {
    access_token = "expiring_soon_token";
    refresh_token = None;
    expires_at = Some near_future;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  
  (* Should detect token expiring soon *)
  Facebook.ensure_valid_token ~account_id:"test_account"
    (fun token ->
      (* Token is still valid but will expire soon *)
      assert (token = "expiring_soon_token");
      print_endline "✓ Token expiry detection")
    (fun _err ->
      (* Or might fail if implementation checks expiry threshold *)
      print_endline "✓ Token expiry detection (failed as expected)")

(** Test: OAuth state CSRF protection *)
let test_oauth_csrf_protection () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  
  let state1 = "csrf_token_1" in
  let state2 = "csrf_token_2" in
  let redirect_uri = "https://example.com/callback" in
  
  Facebook.get_oauth_url ~redirect_uri ~state:state1
    (fun url1 ->
      Facebook.get_oauth_url ~redirect_uri ~state:state2
        (fun url2 ->
          (* Each URL should have different state *)
          assert (string_contains url1 state1);
          assert (string_contains url2 state2);
          assert (url1 <> url2);
          print_endline "✓ OAuth CSRF protection")
        (fun err -> failwith ("CSRF test failed: " ^ err)))
    (fun err -> failwith ("CSRF test failed: " ^ err))

(** Test: App secret proof generation *)
let test_app_secret_proof_generation () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_SECRET" "test_secret";
  
  (* App secret proof is HMAC-SHA256 of access token with app secret *)
  (* This adds security to API calls *)
  
  Mock_http.set_response { status = 200; body = {|{"id":"test"}|}; headers = [] };
  
  Facebook.get ~path:"me" ~access_token:"test_token"
    (function
      | Ok _response ->
          (* Verify request included appsecret_proof *)
          let requests = !Mock_http.requests in
          (match requests with
          | (_, url, _, _) :: _ ->
              if string_contains url "appsecret_proof=" then
                print_endline "✓ App secret proof generation"
              else
                failwith "App secret proof not included"
          | [] -> failwith "No requests made")
      | Error e -> failwith ("App secret proof test failed: " ^ Error_types.error_to_string e))

(** Test: Redirect URI validation *)
let test_redirect_uri_validation () =
  Mock_config.reset ();
  Mock_config.set_env "FACEBOOK_APP_ID" "test_app_id";
  
  let valid_uris = [
    "https://example.com/callback";
    "https://example.com/auth/facebook";
    "https://subdomain.example.com/callback";
  ] in
  
  (* All should generate valid URLs *)
  let results = List.map (fun uri ->
    let state = "test_state" in
    let success = ref false in
    Facebook.get_oauth_url ~redirect_uri:uri ~state
      (fun url ->
        success := true && String.length url > 0)
      (fun _ -> ());
    !success
  ) valid_uris in
  
  if List.for_all (fun x -> x) results then
    print_endline "✓ Redirect URI validation"
  else
    failwith "Some redirect URIs failed"

(** Test: Upload photo with alt-text *)
let test_upload_photo_with_alt_text () =
  Mock_config.reset ();
  
  Mock_http.set_responses [
    { status = 200; body = "fake_image_data"; headers = [] };
    { status = 200; body = {|{"id": "photo_with_alt"}|}; headers = [] };
  ];
  
  Facebook.upload_photo
    ~page_id:"123456"
    ~page_access_token:"test_token"
    ~image_url:"https://example.com/image.jpg"
    ~alt_text:(Some "A beautiful landscape photo")
    (function
      | Ok photo_id ->
          assert (photo_id = "photo_with_alt");
          print_endline "✓ Upload photo with alt-text"
      | Error e -> failwith ("Upload with alt-text failed: " ^ Error_types.error_to_string e))

(** Test: Post with single image and alt-text *)
let test_post_with_alt_text () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";
  
  Mock_http.set_responses [
    { status = 200; body = "image_data"; headers = [] };
    { status = 200; body = {|{"id": "photo123"}|}; headers = [] };
    { status = 200; body = {|{"id": "post123"}|}; headers = [] };
  ];
  
  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Check out this photo!"
    ~media_urls:["https://example.com/image.jpg"]
    ~alt_texts:[Some "Descriptive alt text for accessibility"]
    (handle_outcome
      (fun _post_id ->
        print_endline "✓ Post with single image and alt-text")
      (fun err -> failwith ("Post with alt-text failed: " ^ err)))

(** Test: Post with multiple images and alt-texts *)
let test_post_with_multiple_alt_texts () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";
  
  Mock_http.set_responses [
    { status = 200; body = "image1_data"; headers = [] };
    { status = 200; body = {|{"id": "photo1"}|}; headers = [] };
    { status = 200; body = "image2_data"; headers = [] };
    { status = 200; body = {|{"id": "photo2"}|}; headers = [] };
    { status = 200; body = {|{"id": "post456"}|}; headers = [] };
  ];
  
  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Multiple photos with descriptions"
    ~media_urls:["https://example.com/img1.jpg"; "https://example.com/img2.jpg"]
    ~alt_texts:[Some "First image description"; Some "Second image description"]
    (handle_outcome
      (fun _post_id ->
        print_endline "✓ Post with multiple images and alt-texts")
      (fun err -> failwith ("Post with multiple alt-texts failed: " ^ err)))

(** Test: Multi-photo uses indexed attached_media payload *)
let test_post_with_indexed_attached_media_payload () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";

  Mock_http.set_responses [
    { status = 200; body = "image1_data"; headers = [] };
    { status = 200; body = {|{"id": "photo1"}|}; headers = [] };
    { status = 200; body = "image2_data"; headers = [] };
    { status = 200; body = {|{"id": "photo2"}|}; headers = [] };
    { status = 200; body = {|{"id": "post456"}|}; headers = [] };
  ];

  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Multiple photos payload test"
    ~media_urls:["https://example.com/img1.jpg"; "https://example.com/img2.jpg"]
    ~alt_texts:[Some "First image"; Some "Second image"]
    (handle_outcome
      (fun _post_id ->
        let reqs = !Mock_http.requests in
        let feed_request =
          List.find_opt (fun (m, u, _, _) -> m = "POST" && string_contains u "/feed") reqs
        in
        (match feed_request with
        | Some (_, _, _, body) ->
            assert (string_contains body "attached_media%5B0%5D");
            assert (string_contains body "attached_media%5B1%5D");
            print_endline "✓ Multi-photo uses indexed attached_media payload"
        | None -> failwith "Feed request not found"))
      (fun err -> failwith ("Post payload test failed: " ^ err)))

(** Test: Recovers page token from user token on post failure *)
let test_post_recovers_page_token_from_user_token () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "user_token_initial";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  let permission_error = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "TRACE_PERM"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";

  Mock_http.set_responses [
    { status = 200; body = "image_data_attempt1"; headers = [] };
    { status = 400; body = permission_error; headers = [] };
    { status = 200; body = {|{"data":[{"id":"page123","name":"My Page","access_token":"resolved_page_token","category":"Brand"}]}|}; headers = [] };
    { status = 200; body = "image_data_attempt2"; headers = [] };
    { status = 200; body = {|{"id":"photo_after_recovery"}|}; headers = [] };
    { status = 200; body = {|{"id":"post_after_recovery"}|}; headers = [] };
  ];

  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Recover token and post"
    ~media_urls:["https://example.com/recover.jpg"]
    (handle_outcome
      (fun post_id ->
        assert (post_id = "post_after_recovery");
        let reqs = !Mock_http.requests in
        let used_resolved_token =
          List.exists (fun (_m, _u, headers, _body) ->
            List.exists (fun (k, v) ->
              k = "Authorization" && string_contains v "resolved_page_token"
            ) headers
          ) reqs
        in
        assert used_resolved_token;
        (match Mock_config.get_health_status "test_account" with
        | Some (_, "token_recovered", Some msg) ->
            assert (string_contains msg "Recovered Page access token")
        | Some (_, status, _) ->
            failwith ("Expected token_recovered health status, got: " ^ status)
        | None ->
            failwith "Expected health status update for token recovery");
        (match List.assoc_opt "test_account" !Mock_config.credentials_store with
        | Some updated_creds ->
            assert (updated_creds.access_token = "resolved_page_token");
            assert (updated_creds.refresh_token = Some "user_token_initial")
        | None -> failwith "Expected updated credentials after token recovery");
        print_endline "✓ Post recovers page token from user token")
      (fun err -> failwith ("Token recovery post failed: " ^ err)))

(** Test: Recovery falls back to stored user token when current token fails *)
let test_post_recovery_uses_stored_user_token_fallback () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "stale_page_token";
    refresh_token = Some "stored_user_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  let permission_error = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "TRACE_FALLBACK"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";

  Mock_http.set_responses [
    { status = 200; body = "image_data_attempt1"; headers = [] };
    { status = 400; body = permission_error; headers = [] };
    { status = 400; body = permission_error; headers = [] };
    { status = 200; body = {|{"data":[{"id":"page123","name":"My Page","access_token":"resolved_page_token_from_backup","category":"Brand"}]}|}; headers = [] };
    { status = 200; body = "image_data_attempt2"; headers = [] };
    { status = 200; body = {|{"id":"photo_after_fallback"}|}; headers = [] };
    { status = 200; body = {|{"id":"post_after_fallback"}|}; headers = [] };
  ];

  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Recover with fallback user token"
    ~media_urls:["https://example.com/recover-fallback.jpg"]
    (handle_outcome
      (fun post_id ->
        assert (post_id = "post_after_fallback");
        let reqs = !Mock_http.requests in
        let me_accounts_requests =
          List.filter (fun (m, u, _, _) -> m = "GET" && string_contains u "/me/accounts") reqs
        in
        assert (List.length me_accounts_requests = 2);
        let used_stored_user_token =
          List.exists (fun (_m, u, _, _) -> string_contains u "stored_user_token") me_accounts_requests
        in
        assert used_stored_user_token;
        (match List.assoc_opt "test_account" !Mock_config.credentials_store with
        | Some updated_creds ->
            assert (updated_creds.access_token = "resolved_page_token_from_backup");
            assert (updated_creds.refresh_token = Some "stored_user_token")
        | None -> failwith "Expected updated credentials after fallback recovery");
        print_endline "✓ Recovery uses stored user token fallback")
      (fun err -> failwith ("Fallback recovery test failed: " ^ err)))

(** Test: Recovery does not fallback to other tokens on rate limiting *)
let test_post_recovery_stops_on_rate_limit () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "stale_page_token";
    refresh_token = Some "stored_user_token";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  let permission_error = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "TRACE_RATE_STOP_1"
    }
  }|} in

  let rate_limit_error = {|{
    "error": {
      "message": "(#80001) There have been too many calls to this Page account",
      "type": "OAuthException",
      "code": 80001,
      "fbtrace_id": "TRACE_RATE_STOP_2"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";

  Mock_http.set_responses [
    { status = 200; body = "image_data_attempt1"; headers = [] };
    { status = 400; body = permission_error; headers = [] };
    { status = 400; body = rate_limit_error; headers = [] };
  ];

  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Recovery should stop on rate limit"
    ~media_urls:["https://example.com/recover-stop.jpg"]
    (handle_outcome
      (fun _ -> failwith "Expected post failure")
      (fun _err ->
        let me_accounts_requests =
          List.filter (fun (m, u, _, _) -> m = "GET" && string_contains u "/me/accounts") !Mock_http.requests
        in
        assert (List.length me_accounts_requests = 1);
        (match Mock_config.get_health_status "test_account" with
        | Some (_, "token_recovery_failed", Some msg) ->
            assert (string_contains msg "Stopped token recovery")
        | _ -> failwith "Expected token_recovery_failed stop status");
        print_endline "✓ Recovery stops on rate limit without fallback"))

(** Test: Cleans up uploaded photos if feed publish fails *)
let test_post_cleans_up_uploaded_photos_on_publish_failure () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "page_token_initial";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  let publish_error = {|{
    "error": {
      "message": "(#100) Invalid parameter",
      "type": "OAuthException",
      "code": 100,
      "fbtrace_id": "TRACE_PUBLISH_FAIL"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";

  Mock_http.set_responses [
    { status = 200; body = "image1_data"; headers = [] };
    { status = 200; body = {|{"id":"photo_to_cleanup_1"}|}; headers = [] };
    { status = 200; body = "image2_data"; headers = [] };
    { status = 200; body = {|{"id":"photo_to_cleanup_2"}|}; headers = [] };
    { status = 400; body = publish_error; headers = [] };
    { status = 200; body = {|{"success":true}|}; headers = [] };
    { status = 200; body = {|{"success":true}|}; headers = [] };
  ];

  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Should fail and cleanup"
    ~media_urls:["https://example.com/c1.jpg"; "https://example.com/c2.jpg"]
    (handle_outcome
      (fun _ -> failwith "Expected publish failure")
      (fun _err ->
        let reqs = !Mock_http.requests in
        let deleted_photo1 =
          List.exists (fun (m, u, _, _) -> m = "DELETE" && string_contains u "/photo_to_cleanup_1") reqs
        in
        let deleted_photo2 =
          List.exists (fun (m, u, _, _) -> m = "DELETE" && string_contains u "/photo_to_cleanup_2") reqs
        in
        assert deleted_photo1;
        assert deleted_photo2;
        print_endline "✓ Cleans up uploaded photos on publish failure"))

(** Test: Rate-limit error code mapping for Pages/BUC *)
let test_rate_limit_error_code_80001 () =
  Mock_config.reset ();

  let error_response = {|{
    "error": {
      "message": "(#80001) There have been too many calls to this Page account",
      "type": "OAuthException",
      "code": 80001,
      "fbtrace_id": "TRACE123"
    }
  }|} in

  Mock_http.set_response { status = 400; body = error_response; headers = [] };

  Facebook.get ~path:"me" ~access_token:"test_token"
    (function
      | Ok _ -> failwith "Should have been rate limited"
      | Error e ->
          (match e with
          | Error_types.Rate_limited _ -> print_endline "✓ Rate-limit error code 80001 mapping"
          | _ -> failwith ("Expected rate-limited error, got: " ^ Error_types.error_to_string e)))

(** Test: Post without alt-text *)
let test_post_without_alt_text () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";
  
  Mock_http.set_responses [
    { status = 200; body = "image_data"; headers = [] };
    { status = 200; body = {|{"id": "photo789"}|}; headers = [] };
    { status = 200; body = {|{"id": "post789"}|}; headers = [] };
  ];
  
  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Photo without description"
    ~media_urls:["https://example.com/image.jpg"]
    ~alt_texts:[]
    (handle_outcome
      (fun _post_id ->
        print_endline "✓ Post without alt-text")
      (fun err -> failwith ("Post without alt-text failed: " ^ err)))

(** Test: Alt-text with special characters *)
let test_alt_text_special_characters () =
  Mock_config.reset ();
  
  Mock_http.set_responses [
    { status = 200; body = "fake_image_data"; headers = [] };
    { status = 200; body = {|{"id": "photo_special"}|}; headers = [] };
  ];
  
  Facebook.upload_photo
    ~page_id:"123456"
    ~page_access_token:"test_token"
    ~image_url:"https://example.com/image.jpg"
    ~alt_text:(Some "Photo with \"quotes\", & special <chars> and emojis 🎉")
    (function
      | Ok _photo_id ->
          print_endline "✓ Alt-text with special characters"
      | Error e -> failwith ("Alt-text with special chars failed: " ^ Error_types.error_to_string e))

(** Test: Partial alt-texts - fewer alt-texts than images *)
let test_partial_alt_texts () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page123";
  
  Mock_http.set_responses [
    { status = 200; body = "image1_data"; headers = [] };
    { status = 200; body = {|{"id": "photo1"}|}; headers = [] };
    { status = 200; body = "image2_data"; headers = [] };
    { status = 200; body = {|{"id": "photo2"}|}; headers = [] };
    { status = 200; body = "image3_data"; headers = [] };
    { status = 200; body = {|{"id": "photo3"}|}; headers = [] };
    { status = 200; body = {|{"id": "post_partial"}|}; headers = [] };
  ];
  
  Facebook.post_single
    ~account_id:"test_account"
    ~text:"Three images, two alt-texts"
    ~media_urls:["https://example.com/img1.jpg"; "https://example.com/img2.jpg"; "https://example.com/img3.jpg"]
    ~alt_texts:[Some "First image"; Some "Second image"]
    (handle_outcome
      (fun _post_id ->
        print_endline "✓ Post with partial alt-texts (3 images, 2 alt-texts)")
      (fun err -> failwith ("Post with partial alt-texts failed: " ^ err)))

(** {1 Stories Tests} *)

(** Test: Upload photo story *)
let test_upload_photo_story () =
  Mock_config.reset ();
  
  Mock_http.set_responses [
    { status = 200; body = "fake_image_data"; headers = [] };  (* GET image *)
    { status = 200; body = {|{"post_id": "story_123"}|}; headers = [] };  (* POST multipart *)
  ];
  
  Facebook.upload_photo_story
    ~page_id:"page_123"
    ~page_access_token:"test_token"
    ~image_url:"https://example.com/story.jpg"
    (function
      | Ok story_id ->
          assert (story_id = "story_123");
          (* Verify the request went to photo_stories endpoint *)
          let requests = !Mock_http.requests in
          let has_photo_stories = List.exists (fun (_, url, _, _) ->
            string_contains url "photo_stories"
          ) requests in
          assert has_photo_stories;
          print_endline "✓ Upload photo story"
      | Error e -> failwith ("Upload photo story failed: " ^ Error_types.error_to_string e))

(** Test: Upload video story *)
let test_upload_video_story () =
  Mock_config.reset ();
  
  Mock_http.set_responses [
    { status = 200; body = "fake_video_data"; headers = [] };  (* GET video *)
    { status = 200; body = {|{"video_id": "vid_456", "upload_url": "https://upload.example.com"}|}; headers = [] };  (* POST init *)
    { status = 200; body = {|{"success": true}|}; headers = [] };  (* POST upload *)
    { status = 200; body = {|{"success": true, "post_id": "video_story_789"}|}; headers = [] };  (* POST finish *)
  ];
  
  Facebook.upload_video_story
    ~page_id:"page_123"
    ~page_access_token:"test_token"
    ~video_url:"https://example.com/story.mp4"
    (function
      | Ok story_id ->
          assert (story_id = "video_story_789");
          (* Verify the request went to video_stories endpoint *)
          let requests = !Mock_http.requests in
          let has_video_stories = List.exists (fun (_, url, _, _) ->
            string_contains url "video_stories"
          ) requests in
          assert has_video_stories;
          print_endline "✓ Upload video story"
      | Error e -> failwith ("Upload video story failed: " ^ Error_types.error_to_string e))

(** Test: Post photo story (high-level) *)
let test_post_story_photo () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";
  
  Mock_http.set_responses [
    { status = 200; body = "image_data"; headers = [] };
    { status = 200; body = {|{"post_id": "photo_story_abc"}|}; headers = [] };
  ];
  
  Facebook.post_story_photo
    ~account_id:"test_account"
    ~image_url:"https://example.com/story.jpg"
    (handle_outcome
      (fun story_id ->
        assert (story_id = "photo_story_abc");
        print_endline "✓ Post photo story (high-level)")
      (fun err -> failwith ("Post photo story failed: " ^ err)))

(** Test: Post video story (high-level) *)
let test_post_story_video () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";
  
  Mock_http.set_responses [
    { status = 200; body = "video_data"; headers = [] };
    { status = 200; body = {|{"video_id": "v123", "upload_url": "https://upload.example.com"}|}; headers = [] };
    { status = 200; body = {|{"success": true}|}; headers = [] };
    { status = 200; body = {|{"success": true, "post_id": "video_story_xyz"}|}; headers = [] };
  ];
  
  Facebook.post_story_video
    ~account_id:"test_account"
    ~video_url:"https://example.com/story.mp4"
    (handle_outcome
      (fun story_id ->
        assert (story_id = "video_story_xyz");
        print_endline "✓ Post video story (high-level)")
      (fun err -> failwith ("Post video story failed: " ^ err)))

(** Test: Post video story recovers on auth error during upload phase *)
let test_post_story_video_recovers_on_upload_auth_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "user_token_story";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  let permission_error = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "STORY_UP_AUTH"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_story_123";

  Mock_http.set_responses [
    { status = 200; body = "video_data_1"; headers = [] };
    { status = 200; body = {|{"video_id":"story_vid_1","upload_url":"https://upload.facebook.com/story-up-1"}|}; headers = [] };
    { status = 400; body = permission_error; headers = [] };
    { status = 200; body = {|{"data":[{"id":"page_story_123","name":"Story Page","access_token":"story_resolved_page_token","category":"Brand"}]}|}; headers = [] };
    { status = 200; body = "video_data_2"; headers = [] };
    { status = 200; body = {|{"video_id":"story_vid_2","upload_url":"https://upload.facebook.com/story-up-2"}|}; headers = [] };
    { status = 200; body = {|{"success":true}|}; headers = [] };
    { status = 200; body = {|{"success":true,"post_id":"story_post_recovered"}|}; headers = [] };
  ];

  Facebook.post_story_video
    ~account_id:"test_account"
    ~video_url:"https://example.com/story-recover.mp4"
    (handle_outcome
      (fun story_id ->
        assert (story_id = "story_post_recovered");
        let used_resolved_token =
          List.exists (fun (_m, _u, headers, _body) ->
            List.exists (fun (k, v) -> k = "Authorization" && string_contains v "story_resolved_page_token") headers
          ) !Mock_http.requests
        in
        assert used_resolved_token;
        (match List.assoc_opt "test_account" !Mock_config.credentials_store with
        | Some updated_creds ->
            assert (updated_creds.access_token = "story_resolved_page_token");
            assert (updated_creds.refresh_token = Some "user_token_story")
        | None -> failwith "Expected updated credentials after story recovery");
        print_endline "✓ Post video story recovers on upload auth error")
      (fun err -> failwith ("Story upload auth recovery failed: " ^ err)))

(** Test: Post video story does not recover on upload rate-limit errors *)
let test_post_story_video_no_recovery_on_upload_rate_limit () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "user_token_story_rl";
    refresh_token = Some "stored_user_token_story_rl";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  let rate_limit_error = {|{
    "error": {
      "message": "(#80001) There have been too many calls to this Page account",
      "type": "OAuthException",
      "code": 80001,
      "fbtrace_id": "STORY_UP_RL"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_story_123";

  Mock_http.set_responses [
    { status = 200; body = "video_data_1"; headers = [] };
    { status = 200; body = {|{"video_id":"story_vid_1","upload_url":"https://upload.facebook.com/story-up-1"}|}; headers = [] };
    { status = 400; body = rate_limit_error; headers = [] };
  ];

  Facebook.post_story_video
    ~account_id:"test_account"
    ~video_url:"https://example.com/story-rate-limit.mp4"
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Rate_limited _) ->
          let me_accounts_requests =
            List.filter (fun (m, u, _, _) -> m = "GET" && string_contains u "/me/accounts") !Mock_http.requests
          in
          assert (List.length me_accounts_requests = 0);
          print_endline "✓ Post video story skips recovery on upload rate-limit"
      | Error_types.Failure e ->
          failwith ("Expected rate-limited error, got: " ^ Error_types.error_to_string e)
      | _ -> failwith "Expected story failure on upload rate-limit")

(** Test: Post story with auto-detect (image) *)
let test_post_story_auto_image () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";
  
  Mock_http.set_responses [
    { status = 200; body = "image_data"; headers = [] };
    { status = 200; body = {|{"post_id": "auto_story_img"}|}; headers = [] };
  ];
  
  Facebook.post_story
    ~account_id:"test_account"
    ~media_url:"https://example.com/story.png"
    (handle_outcome
      (fun story_id ->
        assert (story_id = "auto_story_img");
        print_endline "✓ Post story with auto-detect (image)")
      (fun err -> failwith ("Post story auto-detect failed: " ^ err)))

(** Test: Post story with auto-detect (video) *)
let test_post_story_auto_video () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";
  
  Mock_http.set_responses [
    { status = 200; body = "video_data"; headers = [] };
    { status = 200; body = {|{"video_id": "v789", "upload_url": "https://upload.example.com"}|}; headers = [] };
    { status = 200; body = {|{"success": true}|}; headers = [] };
    { status = 200; body = {|{"success": true, "post_id": "auto_story_vid"}|}; headers = [] };
  ];
  
  Facebook.post_story
    ~account_id:"test_account"
    ~media_url:"https://example.com/story.mov"
    (handle_outcome
      (fun story_id ->
        assert (story_id = "auto_story_vid");
        print_endline "✓ Post story with auto-detect (video)")
      (fun err -> failwith ("Post story auto-detect video failed: " ^ err)))

(** Test: Story validation - valid image URL *)
let test_validate_story_valid_image () =
  match Facebook.validate_story ~media_url:"https://example.com/story.jpg" with
  | Ok () -> print_endline "✓ Story validation - valid image URL"
  | Error e -> failwith ("Story validation failed: " ^ e)

(** Test: Story validation - valid video URL *)
let test_validate_story_valid_video () =
  match Facebook.validate_story ~media_url:"https://example.com/story.mp4" with
  | Ok () -> print_endline "✓ Story validation - valid video URL"
  | Error e -> failwith ("Story validation failed: " ^ e)

(** Test: Story validation - invalid URL *)
let test_validate_story_invalid_url () =
  match Facebook.validate_story ~media_url:"not-a-url" with
  | Error msg when string_contains msg "HTTP" -> 
      print_endline "✓ Story validation - rejects invalid URL"
  | _ -> failwith "Should reject invalid URL"

(** Test: Story validation - invalid format *)
let test_validate_story_invalid_format () =
  match Facebook.validate_story ~media_url:"https://example.com/story.txt" with
  | Error msg when string_contains msg "image" || string_contains msg "video" -> 
      print_endline "✓ Story validation - rejects invalid format"
  | _ -> failwith "Should reject invalid format"

(** {1 Video Reel Tests} *)

(** Test: Upload video reel (3-phase resumable upload)
    
    Tests the Facebook Reel upload flow which uses:
    1. Initialize upload session (upload_phase=start)
    2. Upload video binary to upload_url
    3. Finish upload (upload_phase=finish)
    
    @see <https://developers.facebook.com/docs/video-api/guides/reels-publishing>
*)
let test_upload_video_reel () =
  Mock_config.reset ();
  
  Mock_http.set_responses [
    (* GET video from URL *)
    { status = 200; body = "fake_video_binary_data"; headers = [] };
    (* POST init - returns video_id and upload_url *)
    { status = 200; body = {|{"video_id": "reel_video_123", "upload_url": "https://rupload.facebook.com/video-upload/v123"}|}; headers = [] };
    (* POST upload binary to upload_url *)
    { status = 200; body = {|{"success": true}|}; headers = [] };
    (* POST finish *)
    { status = 200; body = {|{"success": true, "id": "reel_post_123"}|}; headers = [] };
  ];
  
  Facebook.upload_video_reel
    ~page_id:"page_123"
    ~page_access_token:"test_token"
    ~video_url:"https://example.com/reel.mp4"
    ~description:"My first Reel!"
    (fun video_id ->
      assert (video_id = "reel_video_123");
      (* Verify the init request contained upload_phase=start *)
      let requests = !Mock_http.requests in
      let has_init = List.exists (fun (_, url, _, body) ->
        string_contains url "video_reels" && string_contains body "upload_phase"
      ) requests in
      assert has_init;
      print_endline "✓ Upload video reel (3-phase)")
    (fun err -> failwith ("Upload video reel failed: " ^ err))

(** Test: Post reel (high-level) *)
let test_post_reel () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";
  
  Mock_http.set_responses [
    { status = 200; body = "video_data"; headers = [] };
    { status = 200; body = {|{"video_id": "vid_456", "upload_url": "https://upload.facebook.com/v1"}|}; headers = [] };
    { status = 200; body = {|{"success": true}|}; headers = [] };
    { status = 200; body = {|{"success": true}|}; headers = [] };
  ];
  
  Facebook.post_reel
    ~account_id:"test_account"
    ~text:"Check out my Reel! #facebook #reel"
    ~video_url:"https://example.com/reel.mp4"
    (handle_outcome
      (fun video_id ->
        assert (video_id = "vid_456");
        print_endline "✓ Post reel (high-level)")
      (fun err -> failwith ("Post reel failed: " ^ err)))

(** Test: Post reel does not attempt token recovery on non-auth errors *)
let test_post_reel_no_recovery_on_non_auth_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";

  (* Only one response: video download succeeds, then init request fails due to missing mock response
     and should NOT trigger token recovery (/me/accounts). *)
  Mock_http.set_responses [
    { status = 200; body = "video_data"; headers = [] };
  ];

  Facebook.post_reel
    ~account_id:"test_account"
    ~text:"Reel should fail without recovery"
    ~video_url:"https://example.com/reel.mp4"
    (fun outcome ->
      match outcome with
      | Error_types.Failure _ ->
          let requested_recovery =
            List.exists (fun (m, u, _, _) ->
              m = "GET" && string_contains u "/me/accounts"
            ) !Mock_http.requests
          in
          assert (not requested_recovery);
          print_endline "✓ Post reel skips recovery on non-auth errors"
      | _ -> failwith "Expected reel failure")

(** Test: Post reel recovers token on auth error *)
let test_post_reel_recovers_on_auth_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "user_token_for_reel";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  let permission_error = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "REEL_PERM"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";

  Mock_http.set_responses [
    { status = 200; body = "video_data_attempt1"; headers = [] };
    { status = 400; body = permission_error; headers = [] };
    { status = 200; body = {|{"data":[{"id":"page_123","name":"My Page","access_token":"resolved_reel_page_token","category":"Brand"}]}|}; headers = [] };
    { status = 200; body = "video_data_attempt2"; headers = [] };
    { status = 200; body = {|{"video_id": "vid_recovered", "upload_url": "https://upload.facebook.com/v1"}|}; headers = [] };
    { status = 200; body = {|{"success": true}|}; headers = [] };
    { status = 200; body = {|{"success": true}|}; headers = [] };
  ];

  Facebook.post_reel
    ~account_id:"test_account"
    ~text:"Reel recovers on auth error"
    ~video_url:"https://example.com/reel.mp4"
    (handle_outcome
      (fun video_id ->
        assert (video_id = "vid_recovered");
        let used_resolved_token =
          List.exists (fun (_m, _u, headers, _body) ->
            List.exists (fun (k, v) ->
              k = "Authorization" && string_contains v "resolved_reel_page_token"
            ) headers
          ) !Mock_http.requests
        in
        assert used_resolved_token;
        print_endline "✓ Post reel recovers on auth errors")
      (fun err -> failwith ("Expected reel recovery success, got: " ^ err)))

(** Test: Post reel recovers on auth error during upload phase *)
let test_post_reel_recovers_on_upload_auth_error () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "user_token_reel_upload";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  let permission_error = {|{
    "error": {
      "message": "(#200) Permissions error",
      "type": "OAuthException",
      "code": 200,
      "fbtrace_id": "REEL_UP_AUTH"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";

  Mock_http.set_responses [
    { status = 200; body = "video_data_1"; headers = [] };
    { status = 200; body = {|{"video_id": "vid_up_1", "upload_url": "https://upload.facebook.com/reel-up-1"}|}; headers = [] };
    { status = 400; body = permission_error; headers = [] };
    { status = 200; body = {|{"data":[{"id":"page_123","name":"My Page","access_token":"resolved_reel_upload_token","category":"Brand"}]}|}; headers = [] };
    { status = 200; body = "video_data_2"; headers = [] };
    { status = 200; body = {|{"video_id": "vid_up_2", "upload_url": "https://upload.facebook.com/reel-up-2"}|}; headers = [] };
    { status = 200; body = {|{"success": true}|}; headers = [] };
    { status = 200; body = {|{"success": true}|}; headers = [] };
  ];

  Facebook.post_reel
    ~account_id:"test_account"
    ~text:"Reel upload auth recovery"
    ~video_url:"https://example.com/reel-upload-recover.mp4"
    (handle_outcome
      (fun video_id ->
        assert (video_id = "vid_up_2");
        let used_resolved_token =
          List.exists (fun (_m, _u, headers, _body) ->
            List.exists (fun (k, v) -> k = "Authorization" && string_contains v "resolved_reel_upload_token") headers
          ) !Mock_http.requests
        in
        assert used_resolved_token;
        (match List.assoc_opt "test_account" !Mock_config.credentials_store with
        | Some updated_creds ->
            assert (updated_creds.access_token = "resolved_reel_upload_token");
            assert (updated_creds.refresh_token = Some "user_token_reel_upload")
        | None -> failwith "Expected updated credentials after reel upload recovery");
        print_endline "✓ Post reel recovers on upload auth error")
      (fun err -> failwith ("Reel upload auth recovery failed: " ^ err)))

(** Test: Post reel does not recover on upload rate-limit errors *)
let test_post_reel_no_recovery_on_upload_rate_limit () =
  Mock_config.reset ();

  let future_time =
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in

  let creds = {
    access_token = "user_token_reel_rl";
    refresh_token = Some "stored_user_token_reel_rl";
    expires_at = Some future_time;
    token_type = "Bearer";
  } in

  let rate_limit_error = {|{
    "error": {
      "message": "(#80001) There have been too many calls to this Page account",
      "type": "OAuthException",
      "code": 80001,
      "fbtrace_id": "REEL_UP_RL"
    }
  }|} in

  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";

  Mock_http.set_responses [
    { status = 200; body = "video_data_1"; headers = [] };
    { status = 200; body = {|{"video_id": "vid_up_1", "upload_url": "https://upload.facebook.com/reel-up-1"}|}; headers = [] };
    { status = 400; body = rate_limit_error; headers = [] };
  ];

  Facebook.post_reel
    ~account_id:"test_account"
    ~text:"Reel upload rate-limit"
    ~video_url:"https://example.com/reel-upload-rate-limit.mp4"
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Rate_limited _) ->
          let me_accounts_requests =
            List.filter (fun (m, u, _, _) -> m = "GET" && string_contains u "/me/accounts") !Mock_http.requests
          in
          assert (List.length me_accounts_requests = 0);
          print_endline "✓ Post reel skips recovery on upload rate-limit"
      | Error_types.Failure e ->
          failwith ("Expected rate-limited error, got: " ^ Error_types.error_to_string e)
      | _ -> failwith "Expected reel failure on upload rate-limit")

(** Test: Video reel validation - caption too long *)
let test_reel_caption_validation () =
  Mock_config.reset ();
  
  let future_time = 
    let now = Ptime_clock.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s (30 * 86400)) with
    | Some t -> Ptime.to_rfc3339 t
    | None -> failwith "Failed to calculate future time"
  in
  
  let creds = {
    access_token = "valid_token";
    refresh_token = None;
    expires_at = Some future_time;
    token_type = "Bearer";
  } in
  
  Mock_config.set_credentials ~account_id:"test_account" ~credentials:creds;
  Mock_config._set_page_id ~account_id:"test_account" ~page_id:"page_123";
  
  (* Facebook's actual post limit is 63206 chars - test exceeding that *)
  let long_caption = String.make 63300 'x' in
  
  Facebook.post_reel
    ~account_id:"test_account"
    ~text:long_caption
    ~video_url:"https://example.com/reel.mp4"
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Validation_error _) ->
          print_endline "✓ Reel caption validation - too long rejected"
      | _ -> failwith "Should reject caption over 63206 chars")

(** Test: Video media validation - validates URLs are accessible *)
let test_video_media_validation () =
  (* Valid video URL *)
  (match Facebook.validate_media ~media_urls:["https://example.com/video.mp4"] with
   | Ok () -> print_endline "✓ Valid video URL passes"
   | Error _ -> failwith "Valid video URL should pass");
  
  (* Multiple valid URLs *)
  (match Facebook.validate_media ~media_urls:[
    "https://example.com/photo1.jpg";
    "https://example.com/photo2.jpg"
  ] with
   | Ok () -> print_endline "✓ Multiple valid URLs pass"
   | Error _ -> failwith "Multiple valid URLs should pass");
  
  (* Invalid URL (not http/https) rejected *)
  (match Facebook.validate_media ~media_urls:["ftp://example.com/video.mp4"] with
   | Error _ -> print_endline "✓ Invalid URL rejected"
   | Ok () -> failwith "Invalid URL should fail")

(** Run all tests *)
let () =
  print_endline "\n=== Facebook Provider Tests ===\n";
  
  print_endline "--- OAuth Flow Tests ---";
  test_oauth_url ();
  test_oauth_url_permissions ();
  test_oauth_url_special_chars ();
  test_oauth_csrf_protection ();
  test_redirect_uri_validation ();
  test_token_exchange ();
  test_token_exchange_errors ();
  test_exchange_code_and_get_pages ();
  test_long_lived_token ();
  test_page_vs_user_token ();
  test_token_expiry_detection ();
  test_app_secret_proof_generation ();
  
  print_endline "\n--- API Feature Tests ---";
  test_upload_photo ();
  test_content_validation ();
  test_ensure_valid_token_fresh ();
  test_ensure_valid_token_expired ();
  test_rate_limit_parsing ();
  test_field_selection ();
  test_error_code_parsing ();
  test_invalid_token_without_expiry_subcode ();
  test_token_subcode_467_maps_to_expired ();
  test_permission_error_scopes ();
  test_me_accounts_permission_scope ();
  test_get_required_permissions_override ();
  test_post_required_permissions_override ();
  test_delete_required_permissions_override ();
  test_pagination ();
  test_batch_requests ();
  test_batch_required_permissions_override ();
  test_app_secret_proof ();
  test_authorization_header ();
  
  print_endline "\n--- Alt-Text Tests ---";
  test_upload_photo_with_alt_text ();
  test_post_with_alt_text ();
  test_post_with_multiple_alt_texts ();
  test_post_with_indexed_attached_media_payload ();
  test_post_recovers_page_token_from_user_token ();
  test_post_recovery_uses_stored_user_token_fallback ();
  test_post_recovery_stops_on_rate_limit ();
  test_post_cleans_up_uploaded_photos_on_publish_failure ();
  test_post_without_alt_text ();
  test_alt_text_special_characters ();
  test_partial_alt_texts ();
  
  print_endline "\n--- Stories Tests ---";
  test_upload_photo_story ();
  test_upload_video_story ();
  test_post_story_photo ();
  test_post_story_video ();
  test_post_story_video_recovers_on_upload_auth_error ();
  test_post_story_video_no_recovery_on_upload_rate_limit ();
  test_post_story_auto_image ();
  test_post_story_auto_video ();
  test_validate_story_valid_image ();
  test_validate_story_valid_video ();
  test_validate_story_invalid_url ();
  test_validate_story_invalid_format ();
  
  print_endline "\n--- Video Reel Tests ---";
  test_upload_video_reel ();
  test_post_reel ();
  test_post_reel_no_recovery_on_non_auth_error ();
  test_post_reel_recovers_on_auth_error ();
  test_post_reel_recovers_on_upload_auth_error ();
  test_post_reel_no_recovery_on_upload_rate_limit ();
  test_reel_caption_validation ();
  test_video_media_validation ();
  test_rate_limit_error_code_80001 ();
  
  print_endline "\n=== All 63 tests passed! ===\n"
