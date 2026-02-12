# social-threads-v1

OCaml library for Threads API v1.

Current status:
- OAuth helpers are implemented (`get_oauth_url`, `exchange_code`, long-lived token exchange, refresh).
- OAuth state validation helper is available via `validate_oauth_state`.
- OAuth state validation enforces exact string equality (no whitespace normalization).
- OAuth inputs (`state`, `redirect_uri`, auth code, token exchange inputs) reject leading/trailing whitespace.
- Explicit account-level refresh helper is available via `refresh_account_credentials`.
- Read operations are implemented for `get_me` and `get_posts`.
- Publish supports text and single-media posts in `post_single`.
- Thread publishing supports reply chains in `post_thread` (text/single-media per item).
- Optional idempotency key support is available via `~idempotency_key` for `post_single` and `post_thread`.
- Optional reply control can be forwarded via `~reply_control` for `post_single` and `post_thread`.
- Read retry attempts are configurable with `THREADS_MAX_READ_RETRY_ATTEMPTS` (default `3`, clamped `1..10`).
- Read retry status policy is configurable with:
  - `THREADS_READ_RETRY_ON_429` (default `false`)
  - `THREADS_READ_RETRY_ON_5XX` (default `true`)
  - `THREADS_READ_RETRY_ON_NETWORK` (default `true`)
- Token expiry precheck uses configurable skew:
  - `THREADS_TOKEN_EXPIRY_SKEW_SECONDS` (default `60`, clamped `0..3600`)
- Optional automatic token refresh on near-expiry:
  - `THREADS_AUTO_REFRESH_TOKEN` (default `false`)

This package is in active conformance hardening and should be treated as pre-production.

## Installation

```bash
cd packages/social-threads-v1
dune build
```

## Quick Start

```ocaml
open Social_threads_v1

module Threads = Threads_v1.Make(Your_config)

let () =
  Threads.get_oauth_url
    ~redirect_uri:"https://example.com/callback"
    ~state:"csrf-token"
    (fun url -> Printf.printf "Authorize here: %s\n" url)
    (fun err -> Printf.eprintf "OAuth URL error: %s\n" err)
```

## Configuration

Required:
- `THREADS_CLIENT_ID`
- `THREADS_CLIENT_SECRET`

Optional OAuth hardening:
- `THREADS_REDIRECT_URI` - when set, `get_oauth_url` and `exchange_code` require
  exact redirect URI equality.

Read behavior:
- `THREADS_MAX_READ_RETRY_ATTEMPTS` (default `3`, clamped `1..10`)
- `THREADS_READ_RETRY_ON_429` (default `false`)
- `THREADS_READ_RETRY_ON_5XX` (default `true`)
- `THREADS_READ_RETRY_ON_NETWORK` (default `true`)

Token lifecycle:
- `THREADS_TOKEN_EXPIRY_SKEW_SECONDS` (default `60`, clamped `0..3600`)
- `THREADS_AUTO_REFRESH_TOKEN` (default `false`)

## OAuth Scopes

Default authorization URL scopes:
- `threads_basic`
- `threads_content_publish`

Additional scopes you may need in your app setup (depending on features):
- `threads_manage_replies`
- `threads_read_replies`
- `threads_manage_insights`

## Known Gaps

- Media support is currently limited to one URL per post (no carousel support yet).
- Media-only posts are supported (empty/whitespace text with one media URL).
- Preflight media validation is extension-based and should be hardened with metadata checks.
- Publish calls do not auto-retry by default to avoid duplicate posts; use idempotency keys when you add external retries.
- Asynchronous container status polling is not implemented; publish flow is synchronous create->publish.
