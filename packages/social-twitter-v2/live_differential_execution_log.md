# Live Differential Execution Log

Use this file while running `live_differential_runbook.md`.

Status legend:
- `Pass`
- `Deviation`
- `Blocked`

## Quick Index (fill first)

| Scenario ID | Matrix Rows | Status | Evidence Ref |
|-------------|-------------|--------|--------------|
| LIVE-OAUTH-REFRESH-001 | AUTH-MX-003, AUTH-MX-004 | Blocked | Missing live authenticated execution context in current session |
| LIVE-READ-TWEET-001 | READ-MX-001 | Blocked | Missing live authenticated execution context in current session |
| LIVE-READ-SEARCH-001 | READ-MX-002 | Blocked | Missing live authenticated execution context in current session |
| LIVE-POST-TEXT-001 | POST-MX-001 | Blocked | Missing live authenticated execution context in current session |
| LIVE-POST-REPLY-001 | POST-MX-002 | Blocked | Missing live authenticated execution context in current session |
| LIVE-POST-QUOTE-001 | POST-MX-003 | Blocked | Missing live authenticated execution context in current session |
| LIVE-VIDEO-CHUNKED-001 | VID-MX-001, VID-MX-002, VID-MX-003, VID-MX-004, VID-MX-006 | Blocked | Missing live authenticated execution context in current session |
| LIVE-VIDEO-CODEC-001 | VID-MX-007, VID-MX-008 | Blocked | Missing live authenticated execution context in current session |

## OAuth

### Scenario: OAuth refresh boundary path
- Scenario ID: LIVE-OAUTH-REFRESH-001
- Date:
- Tester:
- SDK Command/Call: `get_tweet` (or another read call) with near-expiry token in credential store
- Reference Command: `twurl -H api.twitter.com "/2/users/me"`
- SDK Request Summary:
  - Expected token endpoint: `/2/oauth2/token`
  - Expected grant type: `refresh_token`
  - Expected auth header: `Basic ...`
- Reference Request Summary:
  - N/A for refresh internals; use reference call for final authenticated endpoint parity
- SDK Response Summary:
  - Refresh succeeds, then read call succeeds
  - Health status transitions to `healthy`
- Reference Response Summary:
  - Successful authenticated read response
- Parity Result: <Pass | Deviation | Blocked>
- Notes:

## Read

### Scenario: get_tweet with expansions and fields
- Scenario ID: LIVE-READ-TWEET-001
- Date:
- Tester:
- SDK Command/Call: `get_tweet ~tweet_id:"<tweet_id>" ~expansions:["author_id"] ~tweet_fields:["created_at";"public_metrics"]`
- Reference Command: `twurl -H api.twitter.com "/2/tweets/<tweet_id>?expansions=author_id&tweet.fields=created_at,public_metrics"`
- SDK Request Summary:
  - Query includes `expansions=author_id`
  - Query includes `tweet.fields=created_at,public_metrics`
- Reference Request Summary:
  - Same query shape
- SDK Response Summary:
  - `data` object parse succeeds
- Reference Response Summary:
  - Equivalent object shape
- Parity Result: <Pass | Deviation | Blocked>
- Notes:

### Scenario: recent search with pagination token
- Scenario ID: LIVE-READ-SEARCH-001
- Date:
- Tester:
- SDK Command/Call: `search_tweets ~query:"ocaml" ~max_results:25 ~next_token:(Some "<token>")`
- Reference Command: `twurl -H api.twitter.com "/2/tweets/search/recent?query=ocaml&max_results=25&next_token=<token>"`
- SDK Request Summary:
  - Query includes `query`, `max_results`, `next_token`
- Reference Request Summary:
  - Same query shape
- SDK Response Summary:
  - `meta.next_token` presence/absence preserved
- Reference Response Summary:
  - Equivalent pagination semantics
- Parity Result: <Pass | Deviation | Blocked>
- Notes:

## Post

### Scenario: text-only post
- Scenario ID: LIVE-POST-TEXT-001
- Date:
- Tester:
- SDK Command/Call: `post_single ~text:"sdk parity check" ~media_urls:[]`
- Reference Command: `twurl -H api.twitter.com -X POST "/2/tweets" -d '{"text":"sdk parity check"}'`
- SDK Request Summary:
  - JSON body contains `text` only
- Reference Request Summary:
  - Same payload shape
- SDK Response Summary:
  - Returns `data.id`
- Reference Response Summary:
  - Returns equivalent identifier
- Parity Result: <Pass | Deviation | Blocked>
- Notes:

### Scenario: reply post
- Scenario ID: LIVE-POST-REPLY-001
- Date:
- Tester:
- SDK Command/Call: `reply_to_tweet ~reply_to_tweet_id:"<tweet_id>" ~text:"reply"`
- Reference Command: `twurl -H api.twitter.com -X POST "/2/tweets" -d '{"text":"reply","reply":{"in_reply_to_tweet_id":"<tweet_id>"}}'`
- SDK Request Summary:
  - Body contains `reply.in_reply_to_tweet_id`
- Reference Request Summary:
  - Same payload shape
- SDK Response Summary:
  - Reply tweet created with returned ID
- Reference Response Summary:
  - Equivalent behavior
- Parity Result: <Pass | Deviation | Blocked>
- Notes:

### Scenario: quote post
- Scenario ID: LIVE-POST-QUOTE-001
- Date:
- Tester:
- SDK Command/Call: `quote_tweet ~quoted_tweet_id:"<tweet_id>" ~text:"quote"`
- Reference Command: `twurl -H api.twitter.com -X POST "/2/tweets" -d '{"text":"quote","quote_tweet_id":"<tweet_id>"}'`
- SDK Request Summary:
  - Body contains `quote_tweet_id`
- Reference Request Summary:
  - Same payload shape
- SDK Response Summary:
  - Quote tweet created with returned ID
- Reference Response Summary:
  - Equivalent behavior
- Parity Result: <Pass | Deviation | Blocked>
- Notes:

## Video

### Scenario: chunked upload + status polling + post
- Scenario ID: LIVE-VIDEO-CHUNKED-001
- Date:
- Tester:
- SDK Command/Call: `post_single ~media_urls:["<mp4_url>"]` (video path)
- Reference Command: chunked upload script (`INIT`/`APPEND`/`FINALIZE`/`STATUS`) + tweet create with `media_ids`
- SDK Request Summary:
  - INIT has `command`, `media_type`, `total_bytes`, `media_category=tweet_video`
  - APPEND uses increasing `segment_index`
  - FINALIZE followed by STATUS polling until terminal state
  - tweet create only after processing success
- Reference Request Summary:
  - Equivalent phase sequencing and payloads
- SDK Response Summary:
  - On success, tweet posted; on failed processing, no tweet posted
- Reference Response Summary:
  - Equivalent success/failure semantics
- Parity Result: <Pass | Deviation | Blocked>
- Notes:

### Scenario: unsupported codec/container failure
- Scenario ID: LIVE-VIDEO-CODEC-001
- Date:
- Tester:
- SDK Command/Call: `post_single ~media_urls:["<unsupported_video_url>"]`
- Reference Command: attempt upload via reference chunked flow
- SDK Request Summary:
  - Upload proceeds through chunked flow
  - STATUS terminal state `failed` is surfaced
- Reference Request Summary:
  - Equivalent upload path; processing failure expected
- SDK Response Summary:
  - Returns failure with processing diagnostics and does not post tweet
- Reference Response Summary:
  - Equivalent failure behavior
- Parity Result: <Pass | Deviation | Blocked>
- Notes:

## Summary

- Total scenarios run:
- Pass: 0
- Deviation: 0
- Blocked: 8
- Follow-up actions:
  - Provide live authenticated environment and run all `LIVE-*` scenarios.
  - Record concrete evidence refs and update `conformance_matrix.md` sign-off rows.
