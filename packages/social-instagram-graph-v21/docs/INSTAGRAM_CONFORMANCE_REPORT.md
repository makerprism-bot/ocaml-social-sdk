# Instagram Conformance Report

Date: 2026-02-12

Scope:
- OAuth flows and token lifecycle
- Read flows (`/me/accounts`, container status)
- Publish flows (single, carousel, reel, stories)
- Validation and error mapping behavior

## Result

- Overall status: **Conformant for the implemented surface**
- OAuth/read/publish/validation review items resolved and verified by tests

## Evidence Summary

- OAuth
  - Provider OAuth paths delegate to shared `OAuth.Make` implementation for exchange and long-lived exchange.
  - OAuth helper and provider share structured error mapping.
  - Query encoding is validated by round-trip query parsing tests.
  - Optional `appsecret_proof` policy implemented and tested where applicable.

- Read
  - Account discovery query encoding and `appsecret_proof` coverage tested.
  - Account parsing is resilient to malformed/partial page entries.
  - Rate-limit parser handles integer/float JSON values and is regression-tested.

- Publish
  - Container create/publish/status paths return structured API/network errors.
  - Polling no longer force-publishes on status-check failures.
  - Unsupported media formats are rejected before API calls for `post_single`, `post_reel`, and auto `post_story`.
  - Story/reel/single success and key failure paths are covered with tests.

- Validation + errors
  - `Media_required` semantics corrected and tested.
  - Structured mapping for key OAuth error codes (190/10/613) covered.
  - Friendly error layer remains intentionally separate for UX-facing messages.

## Remaining Notes

- Some media constraints (e.g., exact duration/ratio checks for reels/videos) are intentionally delegated to Instagram server-side validation.
- Scope delimiter behavior in authorization URLs remains tied to current implementation/reference behavior and should be rechecked if Meta changes OAuth expectations.
