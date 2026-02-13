# LinkedIn Deviation Report

Research date: 2026-02-12

## Fixed

| ID | Area | Severity | Deviation | Resolution |
| --- | --- | --- | --- | --- |
| DV-001 | OAuth exchange | P0 | Token exchange used query-string parameters with empty POST body | Switched to form-encoded POST body and explicit content type |
| DV-002 | OAuth input validation | P0 | No strict rejection of whitespace/blank OAuth values | Added strict validation for state, redirect URI, code, refresh token, and configured credentials |
| DV-003 | OAuth state validation | P0 | No dedicated strict state validator helper | Added `validate_oauth_state` with exact-match and whitespace rejection |
| DV-004 | Token parsing | P1 | Permissive token parsing (`token_type`/`expires_in`) | Added normalization for token type and expires bounds; reject empty access token |
| DV-005 | Batch read encoding | P1 | `ids` formatting did not follow Rest.li list style | Switched to `ids=List(...)` encoding path |
| DV-006 | Finder author filter | P1 | Author finder parameter was sent as scalar | Switched to Rest.li list-style `authors=List(...)` |
| DV-007 | Refresh path hardening | P1 | `refresh_access_token` had weaker validation/parsing than exchange path | Added whitespace/blank checks, empty token rejection, and expires normalization |
| DV-008 | Error payload handling | P1 | API raw response could leak token-like fields | Added sensitive-field redaction and non-JSON body redaction sentinel |
| DV-009 | Search finder author filter | P1 | Search author filter used scalar key/value | Switched to list-style `authors=List(...)` and added request-shape tests |
| DV-010 | Keyword search behavior | P1 | Implementation assumed unsupported `ugcPosts` keyword finder | Now explicitly rejects keyword finder usage and documents limitation |
| DV-011 | Rest.li method headers | P1 | Finder and batch calls did not always declare method intent | Added `X-RestLi-Method` (`FINDER`/`BATCH_GET`) and request-shape tests |

## Remaining review items

| ID | Area | Severity | Status | Next action |
| --- | --- | --- | --- | --- |
| DV-012 | Finder semantics | P1 | Open | Validate remaining finder criteria against latest docs for target endpoint version (`v2`/`rest`) |

## Validation

- Package tests pass with current changes (`dune test` in `packages/social-linkedin-v2`).
