(** Tests for Reddit API v1 Provider 
    
    Test patterns adapted from go-reddit (https://github.com/vartanbeno/go-reddit)
    and PRAW (https://github.com/praw-dev/praw)
*)

open Social_core
open Social_reddit_v1

(** {1 Test Helpers} *)

(** Helper to check if string contains substring *)
let string_contains s substr =
  try
    ignore (Str.search_forward (Str.regexp_string substr) s 0);
    true
  with Not_found -> false

(** Helper to handle outcome results in tests *)
let handle_outcome on_success on_error outcome =
  match outcome with
  | Error_types.Success result -> on_success result
  | Error_types.Partial_success { result; _ } -> on_success result
  | Error_types.Failure err -> on_error (Error_types.error_to_string err)

(** Helper to handle api_result type for tests *)
let _handle_api_result on_success on_error result =
  match result with
  | Ok value -> on_success value
  | Error err -> on_error (Error_types.error_to_string err)

(** Helper to create future expiration time *)
let future_expiry () =
  match Ptime.add_span (Ptime_clock.now ()) (Ptime.Span.of_int_s 3600) with
  | Some t -> Ptime.to_rfc3339 t
  | None -> ""

(** Helper to create past expiration time (expired) *)
let past_expiry () =
  match Ptime.sub_span (Ptime_clock.now ()) (Ptime.Span.of_int_s 3600) with
  | Some t -> Ptime.to_rfc3339 t
  | None -> ""



(** {1 Mock HTTP Client} *)

module Mock_http = struct
  let requests = ref []
  let response_queue = ref []
  
  let reset () =
    requests := [];
    response_queue := []
  
  (** Set a single response (for backward compatibility) *)
  let set_response response =
    response_queue := [response]
  
  (** Set multiple responses to be returned in order *)
  let set_responses responses =
    response_queue := responses
  
  (** Get next response from queue *)
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
      let _ = parts in
      requests := ("POST_MULTIPART", url, headers, "") :: !requests;
      match get_next_response () with
      | Some response -> on_success response
      | None -> on_error "No mock response set"
  end : HTTP_CLIENT)
end

(** {1 Mock Config} *)

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
    if String.length data > 10 && String.sub data 0 10 = "encrypted:" then
      on_success (String.sub data 10 (String.length data - 10))
    else
      on_error "Invalid encrypted data"
  
  let update_health_status ~account_id ~status ~error_message on_success _on_error =
    health_statuses := (account_id, status, error_message) :: !health_statuses;
    on_success ()
  
  (** Helper: Setup standard test environment with valid credentials *)
  let setup_valid_credentials ?(account_id="test_account") () =
    reset ();
    set_env "REDDIT_CLIENT_ID" "test_client";
    set_env "REDDIT_CLIENT_SECRET" "test_secret";
    set_credentials ~account_id ~credentials:{
      access_token = "test_token";
      refresh_token = Some "refresh_tok";
      expires_at = Some (future_expiry ());
      token_type = "Bearer";
    }
  
  (** Helper: Setup test environment with expired credentials *)
  let setup_expired_credentials ?(account_id="test_account") () =
    reset ();
    set_env "REDDIT_CLIENT_ID" "test_client";
    set_env "REDDIT_CLIENT_SECRET" "test_secret";
    set_credentials ~account_id ~credentials:{
      access_token = "old_token";
      refresh_token = Some "refresh_tok";
      expires_at = Some (past_expiry ());
      token_type = "Bearer";
    }
end

module Reddit = Make(Mock_config)

(** {1 OAuth Tests} *)

let test_oauth_url_generation () =
  let client_id = "test_client_id" in
  let redirect_uri = "https://example.com/callback" in
  let state = "test_state_123" in
  
  let url = OAuth.get_authorization_url ~client_id ~redirect_uri ~state () in
  
  assert (string_contains url "client_id=test_client_id");
  assert (string_contains url "redirect_uri=");
  assert (string_contains url "state=test_state_123");
  assert (string_contains url "response_type=code");
  assert (string_contains url "duration=permanent");
  assert (string_contains url "scope=");
  assert (string_contains url "submit");
  print_endline "✓ OAuth URL generation"

let test_oauth_url_custom_scopes () =
  let client_id = "test_client" in
  let redirect_uri = "https://example.com/cb" in
  let state = "state123" in
  let scopes = ["identity"; "submit"; "read"] in
  
  let url = OAuth.get_authorization_url ~client_id ~redirect_uri ~state ~scopes () in
  
  assert (string_contains url "identity");
  assert (string_contains url "submit");
  assert (string_contains url "read");
  print_endline "✓ OAuth URL with custom scopes"

let test_token_exchange () =
  Mock_config.reset ();
  Mock_config.set_env "REDDIT_CLIENT_ID" "test_client";
  Mock_config.set_env "REDDIT_CLIENT_SECRET" "test_secret";
  
  (* Mock response from Reddit - based on go-reddit testdata *)
  let response_body = {|{
    "access_token": "new_access_token_123",
    "refresh_token": "refresh_token_456",
    "token_type": "bearer",
    "expires_in": 3600,
    "scope": "submit read mysubreddits flair"
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.exchange_code 
    ~code:"test_auth_code"
    ~redirect_uri:"https://example.com/callback"
    (fun creds ->
      assert (creds.access_token = "new_access_token_123");
      assert (creds.refresh_token = Some "refresh_token_456");
      assert (creds.token_type = "bearer");
      assert (creds.expires_at <> None);
      print_endline "✓ Token exchange")
    (fun err -> failwith ("Token exchange failed: " ^ err))

let test_token_exchange_basic_auth () =
  Mock_config.reset ();
  Mock_config.set_env "REDDIT_CLIENT_ID" "my_client";
  Mock_config.set_env "REDDIT_CLIENT_SECRET" "my_secret";
  
  let response_body = {|{"access_token": "tok", "token_type": "bearer", "expires_in": 3600}|} in
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.exchange_code 
    ~code:"code123"
    ~redirect_uri:"https://example.com/cb"
    (fun _ ->
      (* Check that Basic Auth header was used *)
      let (_, _, headers, _) = List.hd !Mock_http.requests in
      let auth_header = List.assoc_opt "Authorization" headers in
      assert (auth_header <> None);
      let auth = Option.get auth_header in
      assert (String.sub auth 0 6 = "Basic ");
      print_endline "✓ Token exchange uses Basic Auth")
    (fun err -> failwith ("Token exchange failed: " ^ err))

(** {1 Validation Tests} *)

let test_validate_title_empty () =
  match Reddit.validate_content ~title:"" () with
  | Error msg when string_contains msg "empty" -> 
      print_endline "✓ Title validation - empty rejected"
  | _ -> failwith "Empty title should fail validation"

let test_validate_title_too_long () =
  let long_title = String.make 301 'x' in
  match Reddit.validate_content ~title:long_title () with
  | Error msg when string_contains msg "300" -> 
      print_endline "✓ Title validation - too long rejected"
  | _ -> failwith "Long title should fail validation"

let test_validate_title_valid () =
  match Reddit.validate_content ~title:"Test Post Title" () with
  | Ok () -> print_endline "✓ Title validation - valid passes"
  | Error e -> failwith ("Valid title should pass: " ^ e)

let test_validate_body_too_long () =
  let long_body = String.make 40001 'x' in
  match Reddit.validate_content ~title:"Title" ~body:long_body () with
  | Error msg when string_contains msg "40000" -> 
      print_endline "✓ Body validation - too long rejected"
  | _ -> failwith "Long body should fail validation"

let test_validate_body_valid () =
  let body = "This is a test post body with some content." in
  match Reddit.validate_content ~title:"Title" ~body () with
  | Ok () -> print_endline "✓ Body validation - valid passes"
  | Error e -> failwith ("Valid body should pass: " ^ e)

(** {1 Post Submission Tests} *)

let test_submit_self_post () =
  Mock_config.setup_valid_credentials ();
  
  (* Mock response - based on go-reddit testdata/post/submit.json *)
  let response_body = {|{
    "json": {
      "errors": [],
      "data": {
        "url": "https://www.reddit.com/r/test/comments/hw6l6a/test_title/",
        "drafts_count": 0,
        "id": "hw6l6a",
        "name": "t3_hw6l6a"
      }
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test Title"
    ~body:"Test Text"
    ~spoiler:true
    ()
    (handle_outcome
      (fun post_id ->
        assert (post_id = "hw6l6a");
        
        (* Verify request parameters - based on go-reddit test assertions *)
        let (method_, url, _, body) = List.hd !Mock_http.requests in
        assert (method_ = "POST");
        assert (string_contains url "/api/submit");
        assert (string_contains body "api_type=json");
        assert (string_contains body "kind=self");
        assert (string_contains body "sr=test");
        assert (string_contains body "title=Test%20Title");
        assert (string_contains body "text=Test%20Text");
        assert (string_contains body "spoiler=true");
        print_endline "✓ Submit self post")
      (fun err -> failwith ("Submit failed: " ^ err)))

let test_submit_link_post () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "json": {
      "errors": [],
      "data": {
        "url": "https://www.reddit.com/r/test/comments/hw6l6a/test_title/",
        "id": "hw6l6a",
        "name": "t3_hw6l6a"
      }
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_link_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test Title"
    ~url:"https://www.example.com"
    ~resubmit:true
    ~nsfw:true
    ()
    (handle_outcome
      (fun post_id ->
        assert (post_id = "hw6l6a");
        
        let (_, url, _, body) = List.hd !Mock_http.requests in
        assert (string_contains url "/api/submit");
        assert (string_contains body "kind=link");
        assert (string_contains body "url=https");
        assert (string_contains body "resubmit=true");
        assert (string_contains body "nsfw=true");
        print_endline "✓ Submit link post")
      (fun err -> failwith ("Submit link failed: " ^ err)))

let test_submit_post_with_flair () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "json": {
      "errors": [],
      "data": {
        "id": "flair123",
        "name": "t3_flair123",
        "url": "https://www.reddit.com/r/test/comments/flair123/test/"
      }
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test Post"
    ~flair_id:"flair_template_123"
    ~flair_text:"Discussion"
    ()
    (handle_outcome
      (fun _ ->
        let (_, _, _, body) = List.hd !Mock_http.requests in
        assert (string_contains body "flair_id=flair_template_123");
        assert (string_contains body "flair_text=Discussion");
        print_endline "✓ Submit post with flair")
      (fun err -> failwith ("Submit with flair failed: " ^ err)))

let test_delete_post () =
  Mock_config.setup_valid_credentials ();
  
  Mock_http.set_response { status = 200; body = "{}"; headers = [] };
  
  Reddit.delete_post
    ~account_id:"test_account"
    ~post_id:"t3_test"
    (fun outcome ->
      match outcome with
      | Error_types.Success () ->
          let (method_, url, _, body) = List.hd !Mock_http.requests in
          assert (method_ = "POST");
          assert (string_contains url "/api/del");
          assert (string_contains body "id=t3_test");
          print_endline "✓ Delete post"
      | Error_types.Failure err ->
          failwith ("Delete failed: " ^ Error_types.error_to_string err)
      | _ -> failwith "Unexpected outcome")

(** {1 Subreddit Tests} *)

let test_get_moderated_subreddits () =
  Mock_config.setup_valid_credentials ();
  
  (* Mock response - based on go-reddit testdata/subreddit/list.json *)
  let response_body = {|{
    "kind": "Listing",
    "data": {
      "children": [
        {
          "kind": "t5",
          "data": {
            "id": "2qs0k",
            "display_name": "Home",
            "subscribers": 15336,
            "over18": false,
            "link_flair_enabled": true,
            "submission_type": "any"
          }
        },
        {
          "kind": "t5",
          "data": {
            "id": "2qh1i",
            "display_name": "AskReddit",
            "subscribers": 28449174,
            "over18": false,
            "link_flair_enabled": true,
            "submission_type": "self"
          }
        }
      ],
      "after": "t5_2qh0u"
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.get_moderated_subreddits
    ~account_id:"test_account"
    (handle_outcome
      (fun (subreddits : Social_reddit_v1.subreddit list) ->
        assert (List.length subreddits = 2);
        let first = List.hd subreddits in
        assert (first.name = "Home");
        assert (first.subscribers = 15336);
        assert (first.flair_enabled = true);
        print_endline "✓ Get moderated subreddits")
      (fun err -> failwith ("Get moderated failed: " ^ err)))

let test_get_subreddit_flairs () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|[
    {
      "id": "flair_1",
      "text": "Discussion",
      "text_editable": false,
      "background_color": "#0055ff",
      "text_color": "light"
    },
    {
      "id": "flair_2",
      "text": "Question",
      "text_editable": true,
      "background_color": "#ff5500",
      "text_color": "dark"
    }
  ]|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.get_subreddit_flairs
    ~account_id:"test_account"
    ~subreddit:"test"
    (handle_outcome
      (fun (flairs : Social_reddit_v1.flair list) ->
        assert (List.length flairs = 2);
        let first = List.hd flairs in
        assert (first.flair_id = "flair_1");
        assert (first.flair_text = "Discussion");
        assert (first.flair_text_editable = false);
        print_endline "✓ Get subreddit flairs")
      (fun err -> failwith ("Get flairs failed: " ^ err)))

let test_check_flair_required () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "is_flair_required": true,
    "title_text_min_length": 0,
    "title_text_max_length": 300
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.check_flair_required
    ~account_id:"test_account"
    ~subreddit:"AskReddit"
    (handle_outcome
      (fun is_required ->
        assert (is_required = true);
        print_endline "✓ Check flair required")
      (fun err -> failwith ("Check flair required failed: " ^ err)))

let test_get_user_info () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "id": "abc123",
    "name": "testuser",
    "icon_img": "https://www.redditstatic.com/avatars/avatar.png",
    "total_karma": 1234,
    "created_utc": 1234567890.0
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.get_user_info
    ~account_id:"test_account"
    (handle_outcome
      (fun (user : Social_reddit_v1.user_info) ->
        assert (user.name = "testuser");
        assert (user.total_karma = 1234);
        print_endline "✓ Get user info")
      (fun err -> failwith ("Get user info failed: " ^ err)))

(** {1 Error Handling Tests} *)

let test_rate_limit_error () =
  Mock_config.setup_valid_credentials ();
  
  (* Rate limit error response - based on PRAW test cases *)
  let response_body = {|{
    "json": {
      "errors": [["RATELIMIT", "You are doing that too much. Try again in 5 minutes.", "ratelimit"]]
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Rate_limited info) ->
          assert (info.retry_after_seconds = Some 300);
          print_endline "✓ Rate limit error handling"
      | Error_types.Success _ ->
          failwith "Should have failed with rate limit"
      | Error_types.Failure err ->
          failwith ("Wrong error type: " ^ Error_types.error_to_string err)
      | _ -> failwith "Unexpected outcome")

let test_rate_limit_seconds () =
  Mock_config.setup_valid_credentials ();
  
  (* Rate limit with seconds - based on PRAW test case *)
  let response_body = {|{
    "json": {
      "errors": [["RATELIMIT", "You are doing that too much. Try again in 6 seconds.", "ratelimit"]]
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Rate_limited info) ->
          assert (info.retry_after_seconds = Some 6);
          print_endline "✓ Rate limit with seconds"
      | _ -> failwith "Should have failed with rate limit")

let test_rate_limit_singular_minute () =
  Mock_config.setup_valid_credentials ();
  
  (* Rate limit with singular minute - based on PRAW test case *)
  let response_body = {|{
    "json": {
      "errors": [["RATELIMIT", "You are doing that too much. Try again in 1 minute.", "ratelimit"]]
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Rate_limited info) ->
          assert (info.retry_after_seconds = Some 60);
          print_endline "✓ Rate limit with 1 minute"
      | _ -> failwith "Should have failed with rate limit")

let test_auth_error () =
  Mock_config.setup_valid_credentials ();
  
  Mock_http.set_response { status = 401; body = {|{"message": "Unauthorized", "error": 401}|}; headers = [] };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Auth_error Error_types.Token_invalid) ->
          print_endline "✓ Auth error handling"
      | Error_types.Success _ ->
          failwith "Should have failed with auth error"
      | Error_types.Failure err ->
          failwith ("Wrong error type: " ^ Error_types.error_to_string err)
      | _ -> failwith "Unexpected outcome")

let test_subreddit_not_allowed () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "json": {
      "errors": [["SUBREDDIT_NOTALLOWED", "you aren't allowed to post there.", "sr"]]
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"private_sub"
    ~title:"Test"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Auth_error (Error_types.Insufficient_permissions _)) ->
          print_endline "✓ Subreddit not allowed error"
      | Error_types.Success _ ->
          failwith "Should have failed"
      | Error_types.Failure err ->
          failwith ("Wrong error type: " ^ Error_types.error_to_string err)
      | _ -> failwith "Unexpected outcome")

let test_no_selfs_error () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "json": {
      "errors": [["NO_SELFS", "This subreddit doesn't allow text posts", "kind"]]
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"pics"
    ~title:"Test"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Api_error info) ->
          assert (string_contains info.message "text posts");
          print_endline "✓ NO_SELFS error handling"
      | _ -> failwith "Should have failed with API error")

let test_no_links_error () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "json": {
      "errors": [["NO_LINKS", "This subreddit only allows text posts", "kind"]]
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_link_post
    ~account_id:"test_account"
    ~subreddit:"askreddit"
    ~title:"Test"
    ~url:"https://example.com"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Api_error info) ->
          assert (string_contains info.message "link posts");
          print_endline "✓ NO_LINKS error handling"
      | _ -> failwith "Should have failed with API error")

let test_bad_subreddit_name () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "json": {
      "errors": [["BAD_SR_NAME", "that subreddit doesn't exist", "sr"]]
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"nonexistent_sub_12345"
    ~title:"Test"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Resource_not_found _) ->
          print_endline "✓ BAD_SR_NAME error handling"
      | _ -> failwith "Should have failed with resource not found")

let test_duplicate_post () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "json": {
      "errors": [["ALREADY_SUB", "that link has already been submitted", "url"]]
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_link_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test"
    ~url:"https://already-submitted.com"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure Error_types.Duplicate_content ->
          print_endline "✓ ALREADY_SUB error handling"
      | _ -> failwith "Should have failed with duplicate content")

(** {1 Crosspost Tests} *)

let test_submit_crosspost () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "json": {
      "errors": [],
      "data": {
        "id": "crosspost123",
        "name": "t3_crosspost123",
        "url": "https://www.reddit.com/r/target/comments/crosspost123/test/"
      }
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_crosspost
    ~account_id:"test_account"
    ~subreddit:"target_subreddit"
    ~title:"Crosspost Title"
    ~original_post_id:"t3_original123"
    ()
    (handle_outcome
      (fun post_id ->
        assert (post_id = "crosspost123");
        
        let (_, _, _, body) = List.hd !Mock_http.requests in
        assert (string_contains body "kind=crosspost");
        assert (string_contains body "crosspost_fullname=t3_original123");
        print_endline "✓ Submit crosspost")
      (fun err -> failwith ("Crosspost failed: " ^ err)))

(** {1 post_single Tests} *)

let test_post_single_self () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "json": {
      "errors": [],
      "data": {"id": "self123", "name": "t3_self123", "url": "..."}
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.post_single
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Self Post"
    ~body:"Body text"
    ()
    (handle_outcome
      (fun _ ->
        let (_, _, _, body) = List.hd !Mock_http.requests in
        assert (string_contains body "kind=self");
        print_endline "✓ post_single as self post")
      (fun err -> failwith ("post_single failed: " ^ err)))

let test_post_single_link () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "json": {
      "errors": [],
      "data": {"id": "link123", "name": "t3_link123", "url": "..."}
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.post_single
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Link Post"
    ~url:"https://example.com"
    ()
    (handle_outcome
      (fun _ ->
        let (_, _, _, body) = List.hd !Mock_http.requests in
        assert (string_contains body "kind=link");
        print_endline "✓ post_single as link post")
      (fun err -> failwith ("post_single failed: " ^ err)))

(** {1 Token Refresh Tests} *)

let test_token_refresh_expired () =
  Mock_config.setup_expired_credentials ();
  
  (* First response: token refresh, second: API call *)
  let refresh_response = {|{
    "access_token": "new_access_token",
    "refresh_token": "new_refresh_token",
    "token_type": "bearer",
    "expires_in": 3600,
    "scope": "submit"
  }|} in
  let submit_response = {|{
    "json": {
      "errors": [],
      "data": {"id": "refreshed123", "name": "t3_refreshed123", "url": "..."}
    }
  }|} in
  
  Mock_http.set_responses [
    { status = 200; body = refresh_response; headers = [] };
    { status = 200; body = submit_response; headers = [] };
  ];
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test Post After Refresh"
    ()
    (handle_outcome
      (fun post_id ->
        assert (post_id = "refreshed123");
        print_endline "✓ Token refresh when expired")
      (fun err -> failwith ("Token refresh failed: " ^ err)))

let test_token_refresh_no_refresh_token () =
  Mock_config.reset ();
  Mock_config.set_env "REDDIT_CLIENT_ID" "test_client";
  Mock_config.set_env "REDDIT_CLIENT_SECRET" "test_secret";
  
  (* Set up credentials that are expired but have no refresh token *)
  Mock_config.set_credentials 
    ~account_id:"test_account"
    ~credentials:{
      access_token = "old_token";
      refresh_token = None;
      expires_at = Some (past_expiry ());
      token_type = "Bearer";
    };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Auth_error Error_types.Token_expired) ->
          print_endline "✓ Token refresh fails without refresh token"
      | Error_types.Success _ ->
          failwith "Should have failed with expired token"
      | Error_types.Failure err ->
          failwith ("Wrong error type: " ^ Error_types.error_to_string err)
      | _ -> failwith "Unexpected outcome")

(** {1 Header Verification Tests} *)

let test_user_agent_header () =
  Mock_config.setup_valid_credentials ();
  
  let response_body = {|{
    "json": {
      "errors": [],
      "data": {"id": "ua123", "name": "t3_ua123", "url": "..."}
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test"
    ()
    (handle_outcome
      (fun _ ->
        let (_, _, headers, _) = List.hd !Mock_http.requests in
        let ua_header = List.assoc_opt "User-Agent" headers in
        assert (ua_header <> None);
        let ua = Option.get ua_header in
        assert (string_contains ua "ocaml");
        assert (string_contains ua "social-reddit");
        print_endline "✓ User-Agent header verification")
      (fun err -> failwith ("Request failed: " ^ err)))

let test_authorization_header () =
  Mock_config.reset ();
  Mock_config.set_env "REDDIT_CLIENT_ID" "test_client";
  Mock_config.set_env "REDDIT_CLIENT_SECRET" "test_secret";
  Mock_config.set_credentials 
    ~account_id:"test_account"
    ~credentials:{
      access_token = "my_access_token_123";
      refresh_token = Some "refresh_tok";
      expires_at = Some (future_expiry ());
      token_type = "Bearer";
    };
  
  let response_body = {|{
    "json": {
      "errors": [],
      "data": {"id": "auth123", "name": "t3_auth123", "url": "..."}
    }
  }|} in
  
  Mock_http.set_response { status = 200; body = response_body; headers = [] };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test"
    ()
    (handle_outcome
      (fun _ ->
        let (_, _, headers, _) = List.hd !Mock_http.requests in
        let auth_header = List.assoc_opt "Authorization" headers in
        assert (auth_header <> None);
        let auth = Option.get auth_header in
        assert (auth = "Bearer my_access_token_123");
        print_endline "✓ Authorization Bearer header verification")
      (fun err -> failwith ("Request failed: " ^ err)))

(** {1 Credentials Tests} *)

let test_missing_credentials () =
  Mock_config.reset ();
  (* Don't set REDDIT_CLIENT_ID or REDDIT_CLIENT_SECRET *)
  
  Mock_config.set_credentials 
    ~account_id:"test_account"
    ~credentials:{
      access_token = "old_token";
      refresh_token = Some "refresh_tok";
      expires_at = Some (past_expiry ());  (* expired, so refresh will be attempted *)
      token_type = "Bearer";
    };
  
  Reddit.submit_self_post
    ~account_id:"test_account"
    ~subreddit:"test"
    ~title:"Test"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Auth_error Error_types.Missing_credentials) ->
          print_endline "✓ Missing credentials error"
      | Error_types.Success _ ->
          failwith "Should have failed with missing credentials"
      | Error_types.Failure err ->
          failwith ("Wrong error type: " ^ Error_types.error_to_string err)
      | _ -> failwith "Unexpected outcome")

let test_account_not_found () =
  Mock_config.reset ();
  Mock_config.set_env "REDDIT_CLIENT_ID" "test_client";
  Mock_config.set_env "REDDIT_CLIENT_SECRET" "test_secret";
  (* Don't set any credentials for the account *)
  
  Reddit.submit_self_post
    ~account_id:"nonexistent_account"
    ~subreddit:"test"
    ~title:"Test"
    ()
    (fun outcome ->
      match outcome with
      | Error_types.Failure (Error_types.Auth_error _) ->
          print_endline "✓ Account not found error"
      | Error_types.Success _ ->
          failwith "Should have failed with account not found"
      | Error_types.Failure err ->
          failwith ("Wrong error type: " ^ Error_types.error_to_string err)
      | _ -> failwith "Unexpected outcome")

(** {1 Run All Tests} *)

let () =
  print_endline "\n=== Reddit Provider Tests ===\n";
  
  print_endline "--- OAuth Tests ---";
  test_oauth_url_generation ();
  test_oauth_url_custom_scopes ();
  test_token_exchange ();
  test_token_exchange_basic_auth ();
  
  print_endline "\n--- Validation Tests ---";
  test_validate_title_empty ();
  test_validate_title_too_long ();
  test_validate_title_valid ();
  test_validate_body_too_long ();
  test_validate_body_valid ();
  
  print_endline "\n--- Post Submission Tests ---";
  test_submit_self_post ();
  test_submit_link_post ();
  test_submit_post_with_flair ();
  test_delete_post ();
  
  print_endline "\n--- Subreddit Tests ---";
  test_get_moderated_subreddits ();
  test_get_subreddit_flairs ();
  test_check_flair_required ();
  
  print_endline "\n--- User Tests ---";
  test_get_user_info ();
  
  print_endline "\n--- Error Handling Tests ---";
  test_rate_limit_error ();
  test_rate_limit_seconds ();
  test_rate_limit_singular_minute ();
  test_auth_error ();
  test_subreddit_not_allowed ();
  test_no_selfs_error ();
  test_no_links_error ();
  test_bad_subreddit_name ();
  test_duplicate_post ();
  
  print_endline "\n--- Crosspost Tests ---";
  test_submit_crosspost ();
  
  print_endline "\n--- post_single Tests ---";
  test_post_single_self ();
  test_post_single_link ();
  
  print_endline "\n--- Token Refresh Tests ---";
  test_token_refresh_expired ();
  test_token_refresh_no_refresh_token ();
  
  print_endline "\n--- Header Verification Tests ---";
  test_user_agent_header ();
  test_authorization_header ();
  
  print_endline "\n--- Credentials Tests ---";
  test_missing_credentials ();
  test_account_not_found ();
  
  print_endline "\n=== All tests passed! ===";
  print_endline "\nTest Coverage Summary:";
  print_endline "  - OAuth 2.0 with Basic Auth (4 tests)";
  print_endline "  - Content validation (5 tests)";
  print_endline "  - Post submission (4 tests)";
  print_endline "  - Subreddit operations (3 tests)";
  print_endline "  - User operations (1 test)";
  print_endline "  - Error handling (9 tests)";
  print_endline "  - Crosspost (1 test)";
  print_endline "  - post_single interface (2 tests)";
  print_endline "  - Token refresh (2 tests)";
  print_endline "  - Header verification (2 tests)";
  print_endline "  - Credentials handling (2 tests)";
  print_endline "";
  print_endline "Total: 35 test functions\n"
