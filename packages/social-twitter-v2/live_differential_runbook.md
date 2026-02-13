# Live Differential Runbook

Use this runbook to execute live parity checks between `social-twitter-v2` and reference clients (`twurl` and/or official SDKs).

Companion log:
- Record outcomes in `packages/social-twitter-v2/live_differential_execution_log.md` using the predefined scenario IDs.

Prerequisites:
- A test account with required scopes (`tweet.read`, `tweet.write`, `users.read`, `offline.access`, `media.write`)
- `twurl` configured for that account
- A short MP4 test asset and one JPEG asset

## Environment

- `TWITTER_CLIENT_ID`
- `TWITTER_CLIENT_SECRET`
- `TWITTER_LINK_REDIRECT_URI`
- valid account credentials in your integration store

## Scenarios to Run

1. OAuth refresh boundary path
- Trigger request with near-expiry token
- Verify refresh occurs and subsequent request succeeds
- Compare observed behavior with `twurl` authenticated request

2. Read contract parity
- Run `get_tweet` with expansions + fields
- Run recent search with `query`, `max_results`, `next_token`
- Compare final query shapes and response key preservation (`data/includes/meta/errors`)

3. Post contract parity
- Text-only post
- Reply post
- Quote post
- Confirm payload shape and resulting IDs

4. Video contract parity
- Upload MP4 with chunked flow (INIT/APPEND/FINALIZE/STATUS)
- Validate polling behavior and terminal-state handling
- Post tweet with uploaded media ID only after processing success

## Evidence Template (per scenario)

```md
### Scenario: <name>
- Scenario ID: <LIVE-...>
- Date:
- Tester:
- SDK Command/Call:
- Reference Command:
- SDK Request Summary:
- Reference Request Summary:
- SDK Response Summary:
- Reference Response Summary:
- Parity Result: <Pass | Deviation | Blocked>
- Notes:
```

## Suggested `twurl` Commands

```bash
# Read: get tweet by id
twurl -H api.twitter.com "/2/tweets/<tweet_id>?expansions=author_id&tweet.fields=created_at,public_metrics"

# Read: recent search
twurl -H api.twitter.com "/2/tweets/search/recent?query=ocaml&max_results=25"

# Post: text tweet
twurl -H api.twitter.com -X POST "/2/tweets" -d '{"text":"sdk parity check"}'
```

For media upload, use your existing chunked upload script/reference tool and record command/output summaries in the evidence template.

## Exit Criteria

- All P0/P1 rows in `conformance_matrix.md` have live evidence links.
- Any live mismatch is logged in `deviation_log.md`.
- Sign-off table completed in `conformance_matrix.md`.
