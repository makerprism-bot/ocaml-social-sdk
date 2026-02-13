# Twitter Conformance Review Checklist

Use this checklist to review `social-twitter-v2` against reference implementations and close deviations with tests.

Primary focus areas:
- OAuth correctness
- Read API contract correctness
- Post API contract correctness
- Video upload and posting correctness

References:
- `packages/social-twitter-v2/REFERENCE_IMPLEMENTATIONS.md`
- Official X/Twitter docs for v2 endpoints and v2 media upload (`/2/media/upload`)

## How to use this file

1. Copy this file into each PR description, or reference sections directly.
2. For each checked item, include at least one test case ID.
3. Every deviation must include a deviation record and a linked fix/test.
4. Do not close the review until all P0/P1 deviations are resolved.

## PR Header Template

```md
## Scope
- [ ] OAuth
- [ ] Read APIs
- [ ] Post APIs
- [ ] Video upload/posting

## References Used
- [ ] Official X docs
- [ ] `xdevplatform/twitter-api-typescript-sdk`
- [ ] `xdevplatform/twitter-api-java-sdk`
- [ ] `tweepy/tweepy`

## Risk Level
- [ ] Low
- [ ] Medium
- [ ] High

## Deviation Summary
- P0: <count>
- P1: <count>
- P2: <count>
- P3: <count>

## Result
- [ ] Ready to merge
- [ ] Needs follow-up
```

## Conformance Checklist

### 1) OAuth

- [ ] AUTH-001 OAuth 2.0 PKCE `S256` verifier/challenge correctness verified
- [ ] AUTH-002 OAuth 2.0 `state` mismatch is rejected
- [ ] AUTH-003 Authorization URL contains required scopes for each operation
- [ ] AUTH-004 Token exchange payload/headers match docs
- [ ] AUTH-005 Refresh token flow works and rotation behavior is handled safely
- [ ] AUTH-006 Expiry/clock-skew handling is correct near boundaries
- [ ] AUTH-007 Missing scope errors are surfaced with actionable message
- [ ] AUTH-008 Secrets/tokens are redacted in logs and traces
- [ ] AUTH-009 Endpoint auth context (OAuth 2.0 user context) matches docs
- [ ] AUTH-010 OAuth failures update health/status callbacks consistently

### 2) Read APIs

- [ ] READ-001 Request method/URL/params match docs for each reviewed endpoint
- [ ] READ-002 Expansions and fields are encoded correctly
- [ ] READ-003 Pagination token propagation and stop conditions verified
- [ ] READ-004 Parser handles `data/includes/meta/errors` combinations
- [ ] READ-005 Partial responses are handled without data corruption
- [ ] READ-006 429 handling respects reset headers and backoff
- [ ] READ-007 Retries happen only for safe retryable failures
- [ ] READ-008 Rate-limit metadata is parsed and exposed correctly
- [ ] READ-009 Unknown fields do not break decoding
- [ ] READ-010 Error mapping preserves useful server details

### 3) Post APIs (non-video)

- [ ] POST-001 Tweet creation payload matches docs (text)
- [ ] POST-002 Reply payload contract is correct
- [ ] POST-003 Quote payload contract is correct
- [ ] POST-004 Media attach contract is correct for image posts
- [ ] POST-005 Duplicate-content failures are mapped correctly
- [ ] POST-006 Text limit enforcement and error mapping are correct
- [ ] POST-007 Invalid media ID behavior is correct
- [ ] POST-008 Retry behavior avoids unsafe duplicate posts
- [ ] POST-009 Response parsing captures returned tweet ID reliably
- [ ] POST-010 Required scopes for posting are enforced/documented

### 4) Video Upload + Video Posting (priority)

- [ ] VID-001 `INIT` request fields are complete and correct
- [ ] VID-002 Chunk size and `segment_index` logic are correct
- [ ] VID-003 `APPEND` ordering and final chunk behavior are correct
- [ ] VID-004 `FINALIZE` handling supports sync and async paths
- [ ] VID-005 `STATUS` polling respects `check_after_secs`
- [ ] VID-006 Terminal processing states are handled (`succeeded`/`failed`)
- [ ] VID-007 Media is attached to tweet only after processing success
- [ ] VID-008 Transcode failure surfaces actionable diagnostics
- [ ] VID-009 Network interruption mid-upload has safe retry/recovery behavior
- [ ] VID-010 Max-size and max-duration boundaries are tested
- [ ] VID-011 Unsupported container/codec behavior is tested and mapped
- [ ] VID-012 Alt text endpoint behavior for uploaded video is correct

### 5) Differential Testing + Trace Validation

- [ ] DIFF-001 Equivalent scenario run against SDK and reference client
- [ ] DIFF-002 Wire trace normalized and comparable
- [ ] DIFF-003 Request contract differences are documented per endpoint
- [ ] DIFF-004 Response semantic differences are documented per endpoint
- [ ] DIFF-005 Nondeterministic fields are redacted/ignored consistently
- [ ] DIFF-006 Final diff report attached to PR

### 6) Fix Quality Gates

- [ ] FIX-001 Every fixed deviation has at least one regression test
- [ ] FIX-002 Every fixed P0/P1 has at least one negative test
- [ ] FIX-003 No unrelated behavior regressions in touched areas
- [ ] FIX-004 Docs updated when behavior or constraints changed
- [ ] FIX-005 Remaining deviations are intentional and approved

## Test Case Record (copy/paste)

```md
### Test Case: <ID>
- Area: <OAuth | Read | Post | Video | Diff>
- Endpoint/Flow: <endpoint or flow name>
- Priority: <P0 | P1 | P2 | P3>
- Reference: <doc URL or reference SDK file>
- Preconditions:
  - <tokens/scopes/test media/etc>
- Input:
  - <request payload/params>
- Expected:
  - <status, response shape, behavior>
- Observed:
  - <actual behavior>
- Result: <Pass | Fail>
- Evidence:
  - <trace/test output path>
- Notes:
  - <optional>
```

## Deviation Record (copy/paste)

```md
### Deviation: <DEV-XXX>
- Severity: <P0 | P1 | P2 | P3>
- Area: <OAuth | Read | Post | Video | Parser | Retry>
- Endpoint/Flow: <endpoint or flow name>
- Type: <Contract | Auth | Parsing | Retry | Validation | Observability>
- Expected:
  - <reference behavior>
- Observed:
  - <current behavior>
- Impact:
  - <user-visible risk>
- Root Cause (suspected/confirmed):
  - <brief>
- Fix Plan:
  - <brief>
- Linked PR/Test:
  - <link or path>
- Status: <Open | In Progress | Fixed | Won't Fix>
- Owner: <name>
```

## Video Codec/Container Matrix (fill in during review)

```md
| Case | Container | Video Codec | Audio Codec | Resolution | Duration | Size | Expected | Observed | Pass/Fail |
|------|-----------|-------------|-------------|------------|----------|------|----------|----------|-----------|
| VCM-001 | MP4 | H.264 | AAC | 1280x720 | 30s | 20MB | Success | | |
| VCM-002 | MP4 | H.264 | AAC | 1920x1080 | 120s | 300MB | Success/Async | | |
| VCM-003 | MP4 | H.265 | AAC | 1920x1080 | 30s | 20MB | Reject/Fail processing | | |
| VCM-004 | MOV | H.264 | AAC | 1280x720 | 30s | 20MB | Verify support | | |
| VCM-005 | WebM | VP9 | Opus | 1280x720 | 30s | 20MB | Likely reject | | |
| VCM-006 | MP4 | H.264 | AAC | 1920x1080 | 145s | 200MB | Duration rejection | | |
| VCM-007 | MP4 | H.264 | AAC | 1920x1080 | 120s | 530MB | Size rejection | | |
```

## Exit Criteria

- [ ] No open P0 deviations
- [ ] No open P1 deviations
- [ ] OAuth checklist complete
- [ ] Read checklist complete
- [ ] Post checklist complete
- [ ] Video checklist complete
- [ ] Differential report attached
- [ ] Conformance matrix updated
- [ ] Remaining P2/P3 deviations accepted with rationale
