# Twitter Conformance Matrix

This matrix tracks endpoint and flow parity between `social-twitter-v2` and reference implementations.

References:
- `packages/social-twitter-v2/REFERENCE_IMPLEMENTATIONS.md`
- Official X docs for OAuth 2.0 and API endpoints

Status legend:
- `Not Reviewed`
- `Pass`
- `Deviation`
- `Static Pass`
- `N/A`

## OAuth and Cross-Cutting Behavior

| ID | Area | Contract | Expected Source | Status | Evidence | Notes |
|----|------|----------|-----------------|--------|----------|-------|
| AUTH-MX-001 | OAuth | PKCE S256 challenge | X OAuth 2.0 docs | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:232` | PKCE S256 challenge derivation and URL contract are unit-tested |
| AUTH-MX-002 | OAuth | State validation | X OAuth 2.0 docs | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:269` | URL state propagation and callback state verification helper are unit-tested |
| AUTH-MX-003 | OAuth | Token exchange payload | X OAuth 2.0 docs | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:946` | OAuth token exchange and refresh request contract (headers/body/grant params) are unit-tested |
| AUTH-MX-004 | OAuth | Refresh flow and expiry handling | X OAuth 2.0 docs | Static Pass | `packages/social-twitter-v2/lib/twitter_v2.ml:559` | Boundary, health-status transitions, rotation propagation, and preservation behavior are unit-tested |
| AUTH-MX-005 | Scopes | Scope requirements per operation | X OAuth scopes docs | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:251` | Scope assertions now check exact scope values and operation-derived sets |
| XCUT-MX-001 | Errors | Error mapping consistency | refs + SDK policy | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:851` | Explicit parse-mapping matrix now covers 401/403/429/5xx and fallback message order |
| XCUT-MX-002 | Rate limit | Header parsing and behavior | X API docs | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:3157` | Header parsing, reset-to-retry conversion, and flow propagation are covered by unit tests |
| XCUT-MX-003 | Observability | Redaction in traces/logs | internal policy | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:1041` | Redaction helper and failure-path redaction are unit-tested |

## Read Endpoints

| ID | Endpoint | Method | Contract Focus | Status | Evidence | Notes |
|----|----------|--------|----------------|--------|----------|-------|
| READ-MX-001 | `/2/tweets/:id` | GET | Params, fields, includes parsing | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:1066` | Query contract assertions cover expansions and `tweet.fields` encoding; unknown-field tolerance is also tested |
| READ-MX-002 | `/2/tweets/search/recent` | GET | Query encoding + pagination | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:1093` | Query contract assertions cover `query`, `max_results`, `next_token`, and fields |
| READ-MX-003 | `/2/users/:id/tweets` | GET | Timeline params + pagination | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2209` | User timeline path is exercised in timeline operation tests |
| READ-MX-004 | `/2/users/:id/mentions` | GET | Mention timeline contract | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2298` | Basic endpoint coverage exists; full field/expansion contract parity still pending live refs |
| READ-MX-005 | `/2/users/:id/timelines/reverse_chronological` | GET | Home timeline contract | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2702` | Home timeline endpoint flow is exercised |
| READ-MX-006 | `/2/users/:id` | GET | User field parsing | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:1763` | User operation smoke tests cover decode/flow paths |
| READ-MX-007 | `/2/users/by/username/:username` | GET | Path and decode | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2159` | Username lookup path is exercised in tests |
| READ-MX-008 | `/2/users/me` | GET | Auth user context | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:1335` | Bearer user-context header and `/users/me` success path are tested |
| READ-MX-009 | `/2/users/:id/followers` | GET | Relationship pagination | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2366` | Follower relationship path is exercised |
| READ-MX-010 | `/2/users/:id/following` | GET | Relationship pagination | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2200` | `get_following` path exercised with result handling |
| READ-MX-011 | `/2/lists/:id` | GET | List object parsing | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2380` | List fetch decode path is exercised |
| READ-MX-012 | `/2/lists/:id/members` | GET | List members pagination | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2437` | Members fetch path exercised with pagination args |
| READ-MX-013 | `/2/lists/:id/tweets` | GET | List tweets parsing | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2491` | List tweet fetch path exercised |
| READ-MX-014 | `/2/users/search` | GET | User search query handling | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2274` | User search path and parameters are exercised |

## Post/Write Endpoints (non-video)

| ID | Endpoint | Method | Contract Focus | Status | Evidence | Notes |
|----|----------|--------|----------------|--------|----------|-------|
| POST-MX-001 | `/2/tweets` | POST | Text post payload | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:1307` | Payload shape, no-retry on network error, too-long preflight block, and malformed-response parse handling are unit-tested |
| POST-MX-002 | `/2/tweets` | POST | Reply payload (`reply`) | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:965` | Explicit payload contract test validates `reply.in_reply_to_tweet_id` |
| POST-MX-003 | `/2/tweets` | POST | Quote payload (`quote_tweet_id`) | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:990` | Explicit payload contract test validates `quote_tweet_id` |
| POST-MX-004 | `/2/tweets/:id` | DELETE | Delete behavior | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:1669` | Delete flow path exercised |
| POST-MX-005 | `/2/users/:id/likes` | POST | Like payload and decode | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2168` | Like flow is exercised in engagement tests |
| POST-MX-006 | `/2/users/:id/likes/:tweet_id` | DELETE | Unlike behavior | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2515` | Unlike path exercised |
| POST-MX-007 | `/2/users/:id/retweets` | POST | Retweet payload and decode | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2386` | Retweet flow is exercised in retweet operations tests |
| POST-MX-008 | `/2/users/:id/retweets/:tweet_id` | DELETE | Unretweet behavior | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2533` | Unretweet path exercised |
| POST-MX-009 | `/2/users/:id/bookmarks` | POST | Bookmark behavior | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2175` | Bookmark flow is exercised in operations tests |
| POST-MX-010 | `/2/users/:id/bookmarks/:tweet_id` | DELETE | Remove bookmark behavior | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2551` | Remove bookmark path exercised |
| POST-MX-011 | `/2/users/:id/following` | POST | Follow behavior | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2193` | Follow flow is exercised in operations tests |
| POST-MX-012 | `/2/users/:source_id/following/:target_id` | DELETE | Unfollow behavior | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2220` | Unfollow path exercised |
| POST-MX-013 | `/2/users/:id/blocking` | POST | Block behavior | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2211` | Block flow is exercised in operations tests |
| POST-MX-014 | `/2/users/:source_id/blocking/:target_id` | DELETE | Unblock behavior | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2238` | Unblock path exercised |
| POST-MX-015 | `/2/users/:id/muting` | POST | Mute behavior | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2229` | Mute flow is exercised in operations tests |
| POST-MX-016 | `/2/users/:source_id/muting/:target_id` | DELETE | Unmute behavior | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2256` | Unmute path exercised |
| POST-MX-017 | `/2/lists` | POST | Create list payload | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2068` | Create-list flow is exercised in list operations tests |
| POST-MX-018 | `/2/lists/:id` | PUT | Update list payload | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2340` | Update list flow exercised |
| POST-MX-019 | `/2/lists/:id` | DELETE | Delete list behavior | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2362` | Delete list flow exercised |
| POST-MX-020 | `/2/lists/:id/members` | POST | Add member payload | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2663` | Add-list-member flow is exercised |
| POST-MX-021 | `/2/lists/:id/members/:user_id` | DELETE | Remove member behavior | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2418` | Remove member flow exercised |
| POST-MX-022 | `/2/users/:id/followed_lists` | POST | Follow list behavior | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2721` | Follow-list flow is exercised |
| POST-MX-023 | `/2/users/:id/followed_lists/:list_id` | DELETE | Unfollow list behavior | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2474` | Unfollow list flow exercised |
| POST-MX-024 | `/2/users/:id/pinned_lists` | POST | Pin list behavior | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2757` | Pin-list flow is exercised |
| POST-MX-025 | `/2/users/:id/pinned_lists/:list_id` | DELETE | Unpin list behavior | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:2774` | Unpin-list flow is exercised |

## Media and Video (priority)

| ID | Endpoint/Flow | Method | Contract Focus | Status | Evidence | Notes |
|----|---------------|--------|----------------|--------|----------|-------|
| VID-MX-001 | `/2/media/upload` INIT | POST | Required fields (`total_bytes`, `media_type`, `media_category`) | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:4328` | Contract test validates INIT field set |
| VID-MX-002 | `/2/media/upload` APPEND | POST | Multipart chunk and `segment_index` correctness | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:4328` | Contract test validates segment index sequence and chunk size limit |
| VID-MX-003 | `/2/media/upload` FINALIZE | POST | Finalize handling | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:4328` | Contract test validates command ordering through FINALIZE |
| VID-MX-004 | `/2/media/upload` STATUS | GET | Polling and terminal state handling | Static Pass | `packages/social-twitter-v2/lib/twitter_v2.ml:699` | STATUS polling enforces `check_after_secs` and timeout/failure paths are unit-tested |
| VID-MX-005 | `/2/media/metadata` | POST | Alt text payload and limits | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:1594` | Endpoint URL and metadata payload contract are unit-tested |
| VID-MX-006 | Tweet attach after upload | POST `/2/tweets` | Attach only after media processing success | Static Pass | `packages/social-twitter-v2/lib/twitter_v2.ml:729` | Verified by tests that failed processing blocks posting |
| VID-MX-007 | Failure mapping | N/A | Processing/transcode failure diagnostics | Static Pass | `packages/social-twitter-v2/lib/twitter_v2.ml:639` | Parsed processing errors include code/name/message |
| VID-MX-008 | Boundary validation | N/A | Size/duration/format boundaries | Static Pass | `packages/social-twitter-v2/test/test_twitter.ml:1520` | Pre-upload oversize validation and unsupported codec/container failure behavior are unit-tested |

## Differential Runtime Status

- Mock-driven runtime contract verification: complete (`packages/social-twitter-v2/differential_runtime_report.md`).
- Live reference differential verification: pending (requires authenticated `twurl`/reference SDK execution).
- Record live runs in `packages/social-twitter-v2/live_differential_execution_log.md`.

## Initial Static Audit Notes

The following findings come from static code inspection and should be validated with differential runtime tests:

- Video chunked upload flow polls STATUS after FINALIZE for async processing states.
- STATUS polling now waits for `check_after_secs` before polling again.

## Sign-Off

| Category | Reviewer | Date | Result | Notes |
|----------|----------|------|--------|-------|
| OAuth | OpenCode | 2026-02-13 | Pending live refs | Track scenarios: `LIVE-OAUTH-REFRESH-001` |
| Read APIs | OpenCode | 2026-02-13 | Pending live refs | Track scenarios: `LIVE-READ-TWEET-001`, `LIVE-READ-SEARCH-001` |
| Post APIs | OpenCode | 2026-02-13 | Pending live refs | Track scenarios: `LIVE-POST-TEXT-001`, `LIVE-POST-REPLY-001`, `LIVE-POST-QUOTE-001` |
| Video | OpenCode | 2026-02-13 | Pending live refs | Track scenarios: `LIVE-VIDEO-CHUNKED-001`, `LIVE-VIDEO-CODEC-001` |
