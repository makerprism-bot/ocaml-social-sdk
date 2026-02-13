# Twitter Differential Runtime Report

This report records runtime verification evidence for `social-twitter-v2` using deterministic mock-driven contract tests.

Notes:
- These checks validate request/response contracts and control-flow behavior in-process.
- Live comparison against external reference clients (`twurl`, official SDKs) remains pending.

## Executed Verification Command

```bash
dune test packages/social-twitter-v2/test
```

Result:
- Passed: 145 test functions

## Evidence by Area

### OAuth

- `test_oauth_url_pkce`: verifies PKCE S256 challenge generation and URL fields.
- `test_oauth_state_verification_helper`: verifies callback state match/mismatch helper behavior.
- `test_oauth_scopes`: verifies exact URL scopes + operation-derived scope sets.
- `test_oauth_exchange_code_request_contract`: verifies token exchange request contract (headers/body params).
- `test_oauth_refresh_request_contract`: verifies refresh request contract (headers/body params).
- `test_oauth_exchange_code_deterministic_failure`: verifies deterministic non-2xx error path.
- `test_oauth_redaction_helper`: verifies token-like values are redacted in diagnostic strings.
- `test_oauth_exchange_failure_redacts_sensitive_response`: verifies failed exchange errors do not leak token fields.
- `test_token_refresh_buffer_boundary`: verifies expiry boundary behavior at refresh buffer edges.
- `test_health_status_refresh_success`: verifies successful refresh updates health to `healthy`.
- `test_health_status_missing_refresh_token`: verifies missing refresh token triggers `token_expired` path.
- `test_health_status_refresh_failed`: verifies refresh failure sets `refresh_failed`.
- `test_refresh_rotation_uses_latest_refresh_token`: verifies rotated refresh token is used on subsequent refresh.
- `test_refresh_response_without_refresh_token_preserves_old_token`: verifies fallback preservation behavior.
- `test_user_context_uses_bearer_access_token_header`: verifies user-context requests use bearer access token.

### Read/API Error Handling

- `test_get_tweet_query_contract`: verifies encoded query params for `get_tweet`.
- `test_search_tweets_query_contract`: verifies encoded query params for recent search.
- `test_mixed_payload_data_and_errors_preserved`: verifies mixed payloads retain both `data` and `errors`.
- `test_read_unknown_fields_tolerance`: verifies unknown fields are preserved/tolerated without parse failure.
- `test_read_partial_response_data_null_with_errors`: verifies partial read payload handling with `data=null` and `errors`.
- `test_read_5xx_no_automatic_retry_policy`: verifies deterministic no-auto-retry behavior on read 5xx failures.
- `test_pagination_end_to_end_stop_condition`: verifies multi-page loop stops on missing `next_token` and does not over-fetch.
- `test_rate_limit_error_uses_reset_header_in_flow`: verifies reset-derived retry-after is propagated in read flow errors.
- `test_parse_api_error_401_maps_token_invalid`: verifies 401 mapping.
- `test_parse_api_error_403_forbidden_maps_insufficient_permissions`: verifies forbidden permission mapping.
- `test_parse_api_error_403_nonforbidden_maps_api_error`: verifies non-forbidden 403 mapping.
- `test_parse_api_error_429_maps_rate_limited`: verifies 429 mapping.
- `test_parse_api_error_429_uses_reset_header_retry_after`: verifies reset-header to retry-after conversion.
- `test_parse_api_error_500_prefers_detail_then_errors_then_default`: verifies fallback ordering.
- `test_parse_api_error_400_duplicate_maps_duplicate_content`: verifies duplicate-content mapping.
- `test_parse_api_error_400_invalid_media_maps_api_error`: verifies invalid-media detail mapping.

### Post API

- `test_post_single_payload_contract`: verifies text-only tweet payload shape.
- `test_reply_payload_contract`: verifies `reply.in_reply_to_tweet_id` payload shape.
- `test_quote_payload_contract`: verifies `quote_tweet_id` payload shape.
- `test_post_invalid_media_id_end_to_end_mapping`: verifies invalid media ID maps to API error on post path.
- `test_post_network_failure_does_not_retry_tweet_creation`: verifies no automatic retry on tweet creation network failure.
- `test_post_too_long_validates_before_api_call`: verifies content validation prevents over-limit post requests.
- `test_post_response_id_parse_failure_maps_internal_error`: verifies malformed post response maps to internal parse error.
- `test_alt_text_metadata_endpoint_contract`: verifies alt-text metadata endpoint URL and payload contract.

### Video Upload / Posting

- `test_chunked_upload_contract_init_append_finalize`: validates INIT/APPEND/FINALIZE sequence and fields.
- `test_video_processing_immediate_success_skips_status_polling`: validates immediate success does not trigger STATUS polling.
- `test_video_processing_pending_then_success_posts_once`: validates STATUS polling progression and posting gate.
- `test_video_processing_failed_blocks_post`: validates failed processing blocks post.
- `test_video_processing_timeout_blocks_post`: validates timeout behavior blocks post.
- `test_video_append_interruption_then_manual_retry_recovery`: validates interruption safety and manual retry recovery.
- `test_video_codec_container_failure_mapping`: validates unsupported codec/container failure mapping and post-block behavior.
- `test_video_uses_chunked_upload_path`: validates video route uses chunked upload.
- `test_image_uses_simple_upload_path`: validates images stay on simple upload path.
- `test_validate_media_before_upload_blocks_oversize_image`: validates pre-upload boundary enforcement prevents upload/post.

## Remaining Differential Work (Live References)

1. Execute equivalent OAuth/read/post/video flows with `twurl`.
2. Capture request/response traces from both implementations.
3. Compare semantic parity for endpoint contracts and error shapes.
4. Attach evidence links and fill matrix sign-off rows.
