# Video Upload Constraints by Platform

This document provides comprehensive video upload requirements for each social media platform supported by the OCaml Social SDK. Use this as a reference when implementing video upload functionality.

## Quick Reference Table

| Platform | Max Size | Max Duration | Supported Formats | Resolution | Official Docs |
|----------|----------|--------------|-------------------|------------|---------------|
| **Twitter/X** | 512 MB | 140 sec | MP4 (H.264) | 1920x1200 | [Media Upload API](https://developer.x.com/en/docs/twitter-api/v1/media/upload-media/api-reference/post-media-upload) |
| **YouTube** | 256 GB | 12 hours | MP4, MOV, AVI, etc. | 8K | [Resumable Upload](https://developers.google.com/youtube/v3/guides/using_resumable_upload_protocol) |
| **Bluesky** | 50 MB | 60 sec | MP4, MPEG, MOV, WebM | - | [Upload Video](https://docs.bsky.app/docs/api/app-bsky-video-upload-video) |
| **Facebook Reels** | ~1 GB | 90 sec | MP4 | 1080x1920 | [Reels Publishing](https://developers.facebook.com/docs/video-api/guides/reels-publishing) |
| **Instagram Reels** | 1 GB | 90 sec | MP4, MOV | 1080x1920 | [Content Publishing](https://developers.facebook.com/docs/instagram-api/guides/content-publishing) |
| **LinkedIn** | 200 MB | 10 min | MP4 | 4096x2304 | [Videos API](https://learn.microsoft.com/en-us/linkedin/marketing/community-management/shares/videos-api) |
| **Mastodon** | 100 MB* | 2 hours* | MP4 | Instance-dependent | [Media API](https://docs.joinmastodon.org/methods/media/) |
| **TikTok** | 4 GB | 10 min | MP4, WebM, MOV | 4K | [Content Posting](https://developers.tiktok.com/doc/content-posting-api-media-transfer-guide) |
| **Pinterest** | - | - | - | - | Video pins via Media API (not implemented) |

*Mastodon limits vary by instance. Defaults shown.

---

## Twitter/X

### Video Constraints
- **Maximum file size**: 512 MB
- **Maximum duration**: 140 seconds (2 min 20 sec)
- **Minimum duration**: 0.5 seconds
- **Supported formats**: MP4 (H.264 video, AAC audio)
- **Maximum resolution**: 1920x1200 (or 1200x1920)
- **Frame rate**: 40 fps max
- **Chunk size**: 5 MB (for chunked upload)

### Upload Method
Uses chunked upload with INIT/APPEND/FINALIZE flow:
1. `POST media/upload` (INIT) - Initialize upload, get media_id
2. `POST media/upload` (APPEND) - Upload chunks (5MB each)
3. `POST media/upload` (FINALIZE) - Complete upload
4. `GET media/upload` (STATUS) - Poll until processing complete

### SDK Implementation
```ocaml
(* packages/social-twitter-v2/lib/twitter_v2.ml *)
let max_video_size_bytes = 512 * 1024 * 1024  (* 512 MB *)
let max_video_duration_seconds = 140
let video_chunk_size = 5 * 1024 * 1024  (* 5 MB *)
```

### Official Documentation
- [Media Upload API](https://developer.x.com/en/docs/twitter-api/v1/media/upload-media/api-reference/post-media-upload)
- [Chunked Upload](https://developer.x.com/en/docs/twitter-api/v1/media/upload-media/uploading-media/chunked-media-upload)

---

## YouTube

### Video Constraints
- **Maximum file size**: 256 GB (or 12 hours, whichever is less)
- **Maximum duration**: 12 hours (verified accounts), 15 minutes (unverified)
- **Shorts duration**: 60 seconds max
- **Supported formats**: MP4, MOV, AVI, WMV, FLV, 3GPP, MPEG-PS, WebM
- **Maximum resolution**: 8K (7680x4320)
- **Recommended codecs**: H.264 video, AAC audio

### Upload Method
Uses resumable upload protocol:
1. `POST /upload/youtube/v3/videos?uploadType=resumable` - Get upload URL
2. `PUT {upload_url}` - Upload video content (supports resume on failure)
3. Poll for processing status

### SDK Implementation
```ocaml
(* packages/social-youtube-data-v3/lib/youtube_data_v3.ml *)
(* YouTube doesn't enforce strict client-side limits - server validates *)
let shorts_max_duration_seconds = 60
```

### Official Documentation
- [Resumable Upload Protocol](https://developers.google.com/youtube/v3/guides/using_resumable_upload_protocol)
- [Videos: insert](https://developers.google.com/youtube/v3/docs/videos/insert)

---

## Bluesky (AT Protocol)

### Video Constraints
- **Maximum file size**: 50 MB
- **Maximum duration**: 60 seconds
- **Supported formats**: MP4, MPEG, QuickTime (MOV), WebM
- **Supported MIME types**: `video/mp4`, `video/mpeg`, `video/quicktime`, `video/webm`

### Upload Method
Uses direct upload (NOT chunked):
1. `POST /xrpc/app.bsky.video.uploadVideo` - Upload video, get job ID
2. Poll `app.bsky.video.getJobStatus` until processing complete
3. Use blob reference in post

### SDK Implementation
```ocaml
(* packages/social-bluesky-v1/lib/bluesky_v1.ml *)
let max_video_size_bytes = 50 * 1024 * 1024  (* 50 MB *)
let max_video_duration_seconds = 60
```

### Official Documentation
- [Upload Video](https://docs.bsky.app/docs/api/app-bsky-video-upload-video)
- [Video Processing](https://docs.bsky.app/docs/api/app-bsky-video-get-job-status)

---

## Facebook Reels

### Video Constraints
- **Maximum file size**: ~1 GB (varies)
- **Maximum duration**: 90 seconds (Reels), 240 minutes (regular video)
- **Minimum duration**: 3 seconds (Reels)
- **Supported formats**: MP4, MOV
- **Aspect ratio**: 9:16 (vertical) recommended for Reels
- **Resolution**: 1080x1920 recommended

### Upload Method
Uses 3-phase chunked upload for large files:
1. Initialize upload session
2. Upload video chunks
3. Finish upload and publish

### SDK Implementation
```ocaml
(* packages/social-facebook-graph-v21/lib/facebook_graph_v21.ml *)
(* Server-side validation - no strict client limits defined *)
```

### Official Documentation
- [Reels Publishing](https://developers.facebook.com/docs/video-api/guides/reels-publishing)
- [Video Upload](https://developers.facebook.com/docs/graph-api/guides/upload)

---

## Instagram

### Video Constraints (Reels)
- **Maximum file size**: 1 GB
- **Maximum duration**: 90 seconds (Reels), 60 seconds (Stories)
- **Minimum duration**: 3 seconds
- **Supported formats**: MP4, MOV
- **Aspect ratio**: 9:16 (1080x1920) for Reels/Stories
- **Frame rate**: 23-60 fps

### Video Constraints (Stories)
- **Maximum duration**: 60 seconds
- **Supported formats**: MP4, MOV
- **Supported codecs**: H.264 video, AAC audio

### Upload Method
Uses container-based publishing:
1. Create media container with video URL
2. Poll container status until ready
3. Publish container

### SDK Implementation
```ocaml
(* packages/social-instagram-graph-v21/lib/instagram_graph_v21.ml *)
(* Video validated server-side via publicly accessible URL *)
```

### Official Documentation
- [Content Publishing](https://developers.facebook.com/docs/instagram-api/guides/content-publishing)
- [Reels](https://developers.facebook.com/docs/instagram-api/guides/reels-publishing)

---

## LinkedIn

### Video Constraints
- **Maximum file size**: 200 MB (via API), 5 GB (LinkedIn native)
- **Maximum duration**: 10 minutes (600 seconds)
- **Supported formats**: MP4
- **Maximum resolution**: 4096x2304
- **Frame rate**: 60 fps max
- **Bitrate**: 30 Mbps max

### Upload Method
Uses chunked upload with ETags:
1. `POST /rest/videos` - Register upload, get upload URL
2. Upload chunks with ETag tracking
3. Finalize upload

### SDK Implementation
```ocaml
(* packages/social-linkedin-v2/lib/linkedin_v2.ml *)
let max_video_size_bytes = 200 * 1024 * 1024  (* 200 MB *)
let max_video_duration_seconds = 600  (* 10 minutes *)
```

### Official Documentation
- [Videos API](https://learn.microsoft.com/en-us/linkedin/marketing/community-management/shares/videos-api)
- [Video Upload](https://learn.microsoft.com/en-us/linkedin/marketing/integrations/community-management/shares/vector-asset-api)

---

## Mastodon

### Video Constraints (Default Instance Limits)
- **Maximum file size**: 100 MB (instance-dependent, some allow more)
- **Maximum duration**: 2 hours (instance-dependent)
- **Supported formats**: MP4, M4V, MOV, WebM
- **Supported codecs**: Various (instance-dependent)

### Important Notes
Mastodon is federated - each instance can set different limits. The SDK uses conservative defaults but actual limits should be fetched from `/api/v1/instance` endpoint.

### Upload Method
Uses API v2 media upload with async processing:
1. `POST /api/v2/media` - Upload media, get ID
2. Poll for processing status (202 = still processing)
3. Attach media_id to status

### SDK Implementation
```ocaml
(* packages/social-mastodon-v1/lib/mastodon_v1.ml *)
(* Default limits - actual limits vary by instance *)
let validate_media ~(media : Platform_types.post_media) =
  match media.Platform_types.media_type with
  | Platform_types.Video ->
      if media.file_size_bytes > 100 * 1024 * 1024 then
        Error "Video exceeds 100MB limit (default)"
      else if Option.value ~default:0.0 media.duration_seconds > 7200.0 then
        Error "Video exceeds 2 hour duration limit"
      else
        Ok ()
```

### Official Documentation
- [Media API](https://docs.joinmastodon.org/methods/media/)
- [Instance Information](https://docs.joinmastodon.org/methods/instance/)

---

## TikTok

### Video Constraints
- **Maximum file size**: 50 MB (default), up to 4 GB via API
- **Maximum duration**: 10 minutes (600 seconds, varies by user)
- **Minimum duration**: 3 seconds
- **Supported formats**: MP4, WebM, MOV
- **Resolution**: 360p to 4K
- **Frame rate**: 23-60 fps

### Upload Method
Uses FILE_UPLOAD source with chunked support:
1. `POST /post/publish/video/init/` - Initialize upload
2. Upload video to provided upload_url
3. Poll `/post/publish/status/fetch/` for completion

### SDK Implementation
```ocaml
(* packages/social-tiktok-v1/lib/tiktok_v1.ml *)
let max_video_duration_sec = 600  (* 10 minutes *)
let min_video_duration_sec = 3
let max_video_size_bytes = 50 * 1024 * 1024  (* 50MB default *)
let min_resolution = 360
let max_resolution = 4096
```

### Official Documentation
- [Content Posting API](https://developers.tiktok.com/doc/content-posting-api-get-started)
- [Media Transfer Guide](https://developers.tiktok.com/doc/content-posting-api-media-transfer-guide)

---

## Pinterest

### Current Status: NOT IMPLEMENTED

Pinterest supports video pins via their Media API, but video upload functionality is not currently implemented in this SDK.

### Future Implementation Notes
- Videos are uploaded via Media API
- Supports MP4 format
- Duration: 4 seconds to 15 minutes
- Aspect ratios: 1:1, 2:3, 9:16

### Official Documentation
- [Media API](https://developers.pinterest.com/docs/api/v5/#tag/media)
- [Pin Creation](https://developers.pinterest.com/docs/api/v5/#operation/pins/create)

---

## Common Video Best Practices

### Format Recommendations
1. **Container**: MP4 (most widely supported)
2. **Video Codec**: H.264 (AVC) for compatibility, H.265 (HEVC) for efficiency
3. **Audio Codec**: AAC
4. **Frame Rate**: 30 fps (or platform's native rate)
5. **Aspect Ratio**: 
   - 9:16 for Stories/Reels/Shorts (vertical)
   - 16:9 for YouTube/regular posts (horizontal)
   - 1:1 for feed posts (square)

### Error Handling
The SDK validates video constraints before upload where possible:
- File size limits enforced client-side
- Duration limits enforced client-side when metadata available
- Format/codec validation typically server-side

### Processing States
All platforms have asynchronous video processing. Common states:
- `PROCESSING` / `IN_PROGRESS` - Video being transcoded
- `FINISHED` / `COMPLETE` / `PUBLISHED` - Ready for viewing
- `FAILED` / `ERROR` - Processing failed

Always implement polling with exponential backoff when waiting for processing.

---

## SDK Implementation Summary

| Platform | Module | Upload Type | Async Processing |
|----------|--------|-------------|------------------|
| Twitter | `twitter_v2.ml` | Chunked (INIT/APPEND/FINALIZE) | Yes |
| YouTube | `youtube_data_v3.ml` | Resumable | Yes |
| Bluesky | `bluesky_v1.ml` | Direct upload | Yes |
| Facebook | `facebook_graph_v21.ml` | Chunked (3-phase) | Yes |
| Instagram | `instagram_graph_v21.ml` | Container-based | Yes |
| LinkedIn | `linkedin_v2.ml` | Chunked with ETags | Yes |
| Mastodon | `mastodon_v1.ml` | Direct (API v2) | Yes |
| TikTok | `tiktok_v1.ml` | FILE_UPLOAD | Yes |

---

*Last updated: December 2024*
