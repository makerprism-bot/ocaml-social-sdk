# social-tiktok-v1

TikTok Content Posting API client for OCaml.

> **Warning:** This library was primarily built using LLMs and has not been tested. Expect breaking changes.

## Features

- **OAuth 2.0 Authentication**: Complete OAuth flow with PKCE support
- **Video Posting**: Upload videos via FILE_UPLOAD or PULL_FROM_URL
- **Photo Carousels**: Post multiple images as a carousel
- **Creator Info**: Query user's posting capabilities and limits
- **Status Tracking**: Monitor publish progress

## Installation

```bash
opam install social-tiktok-v1
```

## Usage

### OAuth Flow

```ocaml
open Social_tiktok_v1

(* Generate authorization URL *)
let auth_url = get_authorization_url
  ~client_id:"your_client_key"
  ~redirect_uri:"https://your-app.com/callback"
  ~scopes:["video.publish"]
  ~state:"random_state"

(* Exchange code for token *)
TikTok.exchange_code
  ~code:authorization_code
  ~redirect_uri:"https://your-app.com/callback"
  (fun credentials ->
    (* Store tokens securely *)
    Printf.printf "Access token: %s\n" credentials.access_token)
  (fun err -> print_endline err)
```

### Query Creator Info (Required Before Posting)

```ocaml
TikTok.get_creator_info ~account_id
  (function
    | Ok info ->
        Printf.printf "Max duration: %d seconds\n" info.max_video_post_duration_sec;
        Printf.printf "Privacy options: %d\n" (List.length info.privacy_level_options)
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Post a Video

```ocaml
(* Post a video using post_single - handles the upload process *)
TikTok.post_single
  ~account_id:"user123"
  ~text:""  (* TikTok uses video description, not separate text *)
  ~media_urls:["https://your-domain.com/video.mp4"]
  (function
    | Social_core.Error_types.Success publish_id ->
        Printf.printf "Published: %s\n" publish_id
    | Social_core.Error_types.Partial_success { result = publish_id; warnings } ->
        Printf.printf "Published: %s with %d warnings\n" publish_id (List.length warnings)
    | Social_core.Error_types.Failure err ->
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

### Check Publish Status

```ocaml
TikTok.check_publish_status ~account_id ~publish_id
  (function
    | Ok status ->
        (match status with
        | Processing -> print_endline "Still processing..."
        | Published video_id -> Printf.printf "Published! ID: %s\n" video_id
        | Failed { error_code; error_message } ->
            Printf.printf "Failed: %s - %s\n" error_code error_message)
    | Error err -> 
        Printf.printf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

## Video Constraints

| Specification | Default Limit | TikTok Limit |
|--------------|-------------------|--------------|
| Max file size | 50 MB | 4 GB |
| Duration | 3s - 600s | 3s - 600s |
| Resolution | 360-4096px | 360-4096px |
| Frame rate | 23-60 FPS | 23-60 FPS |
| Formats | MP4, WebM, MOV | MP4, WebM, MOV |

### Aspect Ratios

| Ratio | Type | Recommendation |
|-------|------|----------------|
| 9:16 | Vertical | Best for TikTok |
| 1:1 | Square | Works, less optimal |
| 16:9 | Horizontal | Works, letterboxed |

## Chunked Upload

For videos > 5MB, use chunked upload:

```ocaml
(* Calculate chunks *)
let chunk_size, total_chunks = calculate_chunks ~video_size

(* Chunk requirements:
   - Minimum: 5MB
   - Maximum: 64MB (final chunk can be 128MB)
   - Maximum 1000 chunks
   - Must upload sequentially
*)
```

## Error Handling

Posting functions use structured error handling with the `outcome` type:

```ocaml
TikTok.post_single
  ~account_id:"user123"
  ~text:""
  ~media_urls:["https://example.com/video.mp4"]
  (function
    | Social_core.Error_types.Success publish_id ->
        Printf.printf "Published: %s\n" publish_id
    | Social_core.Error_types.Partial_success { result = publish_id; warnings } ->
        Printf.printf "Published: %s with %d warnings\n" publish_id (List.length warnings)
    | Social_core.Error_types.Failure err ->
        (* Common errors:
           - Auth_error Token_expired - Token expired
           - Auth_error (Insufficient_permissions _) - Missing video.publish scope
           - Rate_limited _ - Too many requests
           - Validation_error [Media_required] - Video is required
        *)
        Printf.eprintf "Error: %s\n" (Social_core.Error_types.error_to_string err))
```

## Important Notes

1. **App Audit Required**: All content from unaudited apps is private
2. **Query Creator Info**: Must call before every post
3. **Domain Verification**: Required for PULL_FROM_URL method
4. **Sequential Chunks**: Chunked uploads must be sequential, not parallel

## License

MIT
