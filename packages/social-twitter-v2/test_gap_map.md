# Twitter Conformance Test Gap Map

This file maps checklist IDs to current tests in `packages/social-twitter-v2/test/test_twitter.ml`.

Legend:
- `Covered` = existing test exists (may still be shallow)
- `Partial` = test exists but does not verify full contract
- `Missing` = no direct test found

## OAuth Coverage

| Checklist ID | Current Test(s) | Coverage | Notes |
|--------------|------------------|----------|-------|
| AUTH-001 | `test_oauth_url_pkce` | Covered | Validates `code_challenge_method=S256` and challenge derivation |
| AUTH-002 | `test_oauth_state`, `test_oauth_state_verification_helper` | Covered | Verifies state propagation and explicit callback-state verification helper |
| AUTH-003 | `test_oauth_scopes` | Covered | Asserts exact required scopes and operation-derived scope sets |
| AUTH-004 | `test_token_exchange`, `test_oauth_exchange_code_request_contract`, `test_oauth_refresh_request_contract` | Covered | Verifies token endpoint request body/headers and grant contract |
| AUTH-005 | `test_refresh_token_rotation`, `test_token_exchange_with_refresh`, `test_refresh_rotation_uses_latest_refresh_token`, `test_refresh_response_without_refresh_token_preserves_old_token` | Covered | Includes rotation propagation and preservation behavior when refresh token is omitted |
| AUTH-006 | `test_token_expiry`, `test_token_refresh_buffer_boundary` | Covered | Includes boundary checks around 30-minute refresh buffer |
| AUTH-007 | `test_oauth_error_handling`, `test_oauth_exchange_code_deterministic_failure` | Covered | Includes deterministic non-2xx token exchange failure assertion |
| AUTH-008 | `test_oauth_redaction_helper`, `test_oauth_exchange_failure_redacts_sensitive_response` | Covered | Verifies token-like fields are redacted from error/log strings |
| AUTH-009 | `test_user_context_uses_bearer_access_token_header` | Covered | Verifies user-context requests use Bearer access token from credentials |
| AUTH-010 | `test_health_status_refresh_success`, `test_health_status_missing_refresh_token`, `test_health_status_refresh_failed` | Covered | Verifies health-status transitions on refresh success/failure/token-expired paths |

## Read Coverage

| Checklist ID | Current Test(s) | Coverage | Notes |
|--------------|------------------|----------|-------|
| READ-001 | `test_get_tweet`, `test_search_tweets`, `test_get_user_by_username`, `test_get_me`, `test_get_tweet_query_contract` | Covered | Includes explicit `get_tweet` query contract assertions |
| READ-002 | `test_get_tweet`, `test_search_tweets`, `test_get_tweet_query_contract`, `test_search_tweets_query_contract` | Covered | Strict query parameter assertions for expansions/fields/pagination |
| READ-003 | `test_pagination_parsing`, `test_pagination_with_next_token`, `test_pagination_end_to_end_stop_condition` | Covered | Includes end-to-end stop condition when `next_token` is absent |
| READ-004 | `test_mixed_payload_data_and_errors_preserved` | Covered | Confirms response with both `data` and `errors` is preserved |
| READ-005 | `test_read_partial_response_data_null_with_errors` | Covered | Verifies successful 200 read responses can carry `data=null` plus `errors` without parser failure |
| READ-006 | `test_rate_limit_error`, `test_rate_limit_error_uses_reset_header_in_flow`, `test_parse_api_error_429_uses_reset_header_retry_after` | Covered | Verifies reset header is converted to retry_after and propagated through read-path rate-limit handling |
| READ-007 | `test_read_5xx_no_automatic_retry_policy` | Covered | Verifies read path does not auto-retry 5xx and returns failure deterministically |
| READ-008 | `test_rate_limit_parsing` | Covered | Parses limit/remaining/reset |
| READ-009 | `test_read_unknown_fields_tolerance` | Covered | Verifies unknown top-level and nested fields do not break read-path handling |
| READ-010 | `test_unauthorized_error`, `test_forbidden_error`, `test_server_error`, `test_parse_api_error_preserves_details`, `test_parse_api_error_401_maps_token_invalid`, `test_parse_api_error_403_forbidden_maps_insufficient_permissions`, `test_parse_api_error_403_nonforbidden_maps_api_error`, `test_parse_api_error_429_maps_rate_limited`, `test_parse_api_error_500_prefers_detail_then_errors_then_default` | Covered | Includes explicit 401/403/429/5xx mapping and message fallback assertions |

## Post Coverage (Non-Video)

| Checklist ID | Current Test(s) | Coverage | Notes |
|--------------|------------------|----------|-------|
| POST-001 | `test_post_single`, `test_post_single_payload_contract` | Covered | Includes explicit payload assertion for text-only post body |
| POST-002 | `test_reply_tweet`, `test_reply_payload_contract` | Covered | Includes explicit payload assertions for `reply.in_reply_to_tweet_id` |
| POST-003 | `test_quote_tweet`, `test_quote_payload_contract` | Covered | Includes explicit payload assertion for `quote_tweet_id` |
| POST-004 | `test_post_with_alt_text`, `test_post_with_multiple_alt_texts`, `test_alt_text_metadata_endpoint_contract` | Covered | Includes explicit metadata endpoint and payload contract assertions for alt text |
| POST-005 | `test_parse_api_error_400_duplicate_maps_duplicate_content` | Covered | Verifies duplicate-content error message maps to `Duplicate_content` |
| POST-006 | `test_content_validation`, `test_post_too_long_validates_before_api_call` | Covered | Validation guard blocks too-long content before API call and returns validation error |
| POST-007 | `test_parse_api_error_400_invalid_media_maps_api_error`, `test_post_invalid_media_id_end_to_end_mapping` | Covered | Includes end-to-end posting path assertion for invalid media ID mapping |
| POST-008 | `test_post_network_failure_does_not_retry_tweet_creation` | Covered | Verifies tweet creation is not retried automatically on network error |
| POST-009 | `test_post_single`, `test_post_response_id_parse_failure_maps_internal_error` | Covered | Includes malformed response path assertion for tweet ID parse failures |
| POST-010 | `test_oauth_scopes` | Covered | Scope checks are strict and include operation-derived scope assertions |

## Video Coverage (Priority)

| Checklist ID | Current Test(s) | Coverage | Notes |
|--------------|------------------|----------|-------|
| VID-001 | `test_chunked_upload_contract_init_append_finalize` | Covered | Verifies INIT includes command/media_type/total_bytes/media_category contract |
| VID-002 | `test_chunked_upload_contract_init_append_finalize` | Covered | Verifies segment indices and per-chunk size bound (<=5MB) |
| VID-003 | `test_chunked_upload_contract_init_append_finalize` | Covered | Verifies APPEND sequencing and FINALIZE command ordering |
| VID-004 | `test_video_processing_pending_then_success_posts_once`, `test_video_processing_immediate_success_skips_status_polling` | Covered | Covers async STATUS progression and immediate-success no-poll branch |
| VID-005 | `test_video_processing_pending_then_success_posts_once`, `test_video_processing_timeout_blocks_post` | Covered | Includes elapsed-time assertion confirming `check_after_secs` delay is applied |
| VID-006 | `test_video_processing_pending_then_success_posts_once`, `test_video_processing_failed_blocks_post` | Covered | Both success and failed terminal states asserted |
| VID-007 | `test_video_processing_failed_blocks_post`, `test_video_uses_chunked_upload_path` | Covered | Verifies failed processing blocks tweet creation and video routes through chunked upload |
| VID-008 | `test_video_processing_failed_blocks_post` | Covered | Failure path and surfaced failure diagnostics are asserted |
| VID-009 | `test_video_append_interruption_then_manual_retry_recovery` | Covered | Verifies interrupted APPEND fails safely and full manual retry recovers successfully |
| VID-010 | `test_media_validation`, `test_validate_media_before_upload_blocks_oversize_image` | Partial | Size boundary is exercised pre-upload; duration boundary remains validation-only |
| VID-011 | `test_video_codec_container_failure_mapping` | Covered | Verifies unsupported codec/container processing failure is surfaced and blocks posting |
| VID-012 | `test_post_with_alt_text`, `test_alt_text_metadata_endpoint_contract` | Covered | Alt text endpoint URL and payload contract are asserted |

Additional upload-mode checks:
- `test_video_uses_chunked_upload_path` confirms video upload path uses multipart chunked flow.
- `test_image_uses_simple_upload_path` confirms image upload path uses simple upload flow.

## Next Implementation Targets

1. Add live reference differential evidence and complete sign-off rows.
2. Add deterministic header-driven rate-limit reset/backoff tests (`READ-006`) if client-side retry policy is introduced.
3. Add explicit duration-boundary pre-upload enforcement test path if media duration parsing is introduced.
4. Add optional jitter/backoff strategy tuning tests for STATUS polling.
