# social-pinterest-v5

Pinterest API v5 client for OCaml.

## Features

- OAuth 2.0 with Basic Auth token exchange
- Single-pin publishing via `post_single`
- Native image pin flow (download -> `/media` multipart upload -> `/pins` create)
- Native video pin flow (register upload -> upload binary -> processing poll -> `/pins` create)
- Strict media validation (size and content type)
- Unknown extension routing by downloaded `content-type` (video vs image)

## Platform Limits

| Type | Limit |
|------|-------|
| Description (`text`) | 500 characters |
| Pin title | 100 characters (derived from `text`) |
| Image | 20MB max |
| Video | 2GB max |

## Installation

```bash
opam install social-pinterest-v5
```

## Account Linking (OAuth)

```ocaml
open Social_pinterest_v5

(* 1) Generate the Pinterest OAuth URL *)
OAuth.get_authorization_url
  ~client_id:"your_client_id"
  ~redirect_uri:"https://your-app.com/callback"
  ~state:"random_state_string"
  ()

(* 2) Exchange callback code for credentials *)
module OAuthClient = OAuth.Make(Your_http_client)

OAuthClient.exchange_code
  ~client_id:"your_client_id"
  ~client_secret:"your_client_secret"
  ~redirect_uri:"https://your-app.com/callback"
  ~code:"authorization_code"
  (fun credentials ->
    (* Store credentials.access_token / credentials.refresh_token *)
    ())
  (fun err -> Printf.eprintf "OAuth error: %s\n" err)
```

## Posting

```ocaml
open Social_pinterest_v5

module Pinterest = Make(Your_config)

(* Image pin *)
Pinterest.post_single
  ~account_id:"account123"
  ~text:"Fresh design ideas for small offices"
  ~media_urls:["https://example.com/image.jpg"]
  ~alt_texts:[Some "A compact office setup with natural lighting"]
  (fun outcome ->
    match outcome with
    | Error_types.Success pin_id ->
        Printf.printf "Created pin: %s\n" pin_id
    | Error_types.Failure err ->
        Printf.eprintf "Error: %s\n" (Error_types.error_to_string err)
    | _ -> ())

(* Native video pin with optional cover image URL in media_urls[1] *)
Pinterest.post_single
  ~account_id:"account123"
  ~text:"Before/after room makeover walkthrough"
  ~media_urls:[
    "https://example.com/makeover.mp4";
    "https://example.com/makeover-cover.jpg";
  ]
  (fun outcome ->
    match outcome with
    | Error_types.Success pin_id ->
        Printf.printf "Created video pin: %s\n" pin_id
    | Error_types.Failure err ->
        Printf.eprintf "Error: %s\n" (Error_types.error_to_string err)
    | _ -> ())
```

Note: `post_single` resolves the first available board automatically; if the account has no boards, posting fails until a board exists.

## Video Flow Details

- Video pins use Pinterest native upload semantics:
  1. Register media upload (`POST /v5/media` with `media_type="video"`)
  2. Upload binary to returned upload URL
  3. Poll media processing status until ready
  4. Create pin with `media_source.source_type = "video_id"`
- If the primary media URL does not have a video extension, the provider still attempts video flow first and decides based on downloaded `content-type`
- If downloaded content type is `image/*`, provider routes to image flow
- If downloaded content type is unsupported (for example `application/pdf`), provider returns a clean structured error
- Optional cover image is only included when Pinterest sees an image content type

## Threads

Pinterest does not support thread posting. `post_thread` posts only the first item and returns a partial-success warning when multiple items are requested.

## Available Functions (Make)

- `post_single`
- `post_thread`
- `validate_media_file`
- `validate_content`
- `get_oauth_url`
- `exchange_code`
- `ensure_valid_token` (helper)
- `get_default_board` (helper)

## Required Environment Variables

- `PINTEREST_CLIENT_ID`
- `PINTEREST_CLIENT_SECRET`

## Testing

```bash
dune exec packages/social-pinterest-v5/test/test_pinterest.exe
```

## References

- https://developers.pinterest.com/docs/api/v5/
- https://developers.pinterest.com/docs/getting-started/authentication/
