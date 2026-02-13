# Twitter Deviation Log

Track all behavioral deviations found during conformance review.

Severity:
- `P0`: security/data-loss/corruption risk
- `P1`: contract mismatch causing incorrect runtime behavior
- `P2`: edge-case mismatch or degraded UX
- `P3`: minor inconsistency/documentation/tooling

Status:
- `Open`
- `In Progress`
- `Fixed`
- `Accepted`

## Active Deviations

| ID | Severity | Area | Endpoint/Flow | Summary | Status | Owner | Linked Test | Linked Fix |
|----|----------|------|---------------|---------|--------|-------|-------------|------------|

## Closed Deviations

| ID | Severity | Area | Endpoint/Flow | Resolution | Status | Linked Test | Linked Fix |
|----|----------|------|---------------|------------|--------|-------------|------------|
| DEV-010 | P2 | Docs | Media metadata endpoint naming | Updated README endpoint to `/2/media/metadata` to match implementation | Fixed | N/A | `packages/social-twitter-v2/README.md:576` |
| DEV-008 | P1 | Video | STATUS after FINALIZE | Duplicate of DEV-004; merged to avoid split tracking | Accepted | N/A | `packages/social-twitter-v2/lib/twitter_v2.ml:757` |
| DEV-011 | P1 | Video | Upload mode selection | Video media now routes through chunked upload path instead of simple upload path | Fixed | `packages/social-twitter-v2/test/test_twitter.ml:2257` | `packages/social-twitter-v2/lib/twitter_v2.ml:912` |
| DEV-005 | P1 | Video | Tweet attach timing | Tweet creation now blocked until media processing succeeds/fails and never proceeds on failed processing | Fixed | `packages/social-twitter-v2/test/test_twitter.ml:2346` | `packages/social-twitter-v2/lib/twitter_v2.ml:729` |
| DEV-009 | P1 | Video | Async processing failure mapping | Processing failure payloads now parsed and surfaced with code/name/message details | Fixed | `packages/social-twitter-v2/test/test_twitter.ml:2346` | `packages/social-twitter-v2/lib/twitter_v2.ml:639` |
| DEV-004 | P1 | Video | STATUS polling | STATUS polling now enforces `check_after_secs` delay between polls and timeout path is covered | Fixed | `packages/social-twitter-v2/test/test_twitter.ml:2262` | `packages/social-twitter-v2/lib/twitter_v2.ml:699` |
| DEV-001 | P1 | OAuth | Auth URL scope set | Scope tests now assert exact OAuth URL scopes and operation-specific scope requirements including `media.write` where required | Fixed | `packages/social-twitter-v2/test/test_twitter.ml:251` | `packages/social-twitter-v2/lib/twitter_v2.ml:53` |
| DEV-002 | P1 | OAuth | Refresh handling | Refresh boundary, health-status transitions, rotation propagation, token preservation, and missing-client-credential behavior are now covered | Fixed | `packages/social-twitter-v2/test/test_twitter.ml:840` | `packages/social-twitter-v2/lib/twitter_v2.ml:559` |
| DEV-003 | P1 | Video | INIT/APPEND/FINALIZE | Multipart contract now validated with command sequence, INIT fields, APPEND segment indices/chunk sizes, and FINALIZE ordering | Fixed | `packages/social-twitter-v2/test/test_twitter.ml:3086` | `packages/social-twitter-v2/lib/twitter_v2.ml:831` |
| DEV-006 | P2 | Read | Partial responses | Mixed `data` + `errors` response payload is now explicitly preserved and asserted in read-path tests | Fixed | `packages/social-twitter-v2/test/test_twitter.ml:822` | `packages/social-twitter-v2/lib/twitter_v2.ml:1406` |
| DEV-007 | P2 | Errors | Error mapping | Added explicit 401/403/429/5xx mapping matrix tests and improved `detail`/`errors` fallback parsing | Fixed | `packages/social-twitter-v2/test/test_twitter.ml:851` | `packages/social-twitter-v2/lib/twitter_v2.ml:467` |
| DEV-012 | P1 | OAuth | Callback state verification | Added explicit callback state verification helper and tests for match/mismatch behavior | Fixed | `packages/social-twitter-v2/test/test_twitter.ml:269` | `packages/social-twitter-v2/lib/twitter_v2.ml:2909` |
| DEV-013 | P2 | Rate limit | Header-driven reset/backoff | `x-rate-limit-reset` is now converted to `retry_after_seconds` and asserted in parser-level and flow-level tests | Fixed | `packages/social-twitter-v2/test/test_twitter.ml:3157` | `packages/social-twitter-v2/lib/twitter_v2.ml:477` |

## Deviation Entry Template

```md
### DEV-XXX
- Severity: <P0 | P1 | P2 | P3>
- Area: <OAuth | Read | Post | Video | Parser | Retry | Observability>
- Endpoint/Flow: <name>
- Summary: <one line>
- Expected:
  - <reference behavior>
- Observed:
  - <actual behavior>
- Impact:
  - <user impact>
- Root Cause:
  - <suspected or confirmed>
- Fix Plan:
  - <brief>
- Status: <Open | In Progress | Fixed | Accepted>
- Owner: <name>
- Linked Test: <path>
- Linked Fix: <path/PR>
```
