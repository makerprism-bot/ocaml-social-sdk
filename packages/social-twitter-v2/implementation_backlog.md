# Twitter Conformance Implementation Backlog

This backlog translates current deviations and test gaps into concrete implementation batches.

## Batch 1: Video Processing Correctness (P1)

Objective:
- Ensure chunked video upload waits for processing completion before returning media usable for tweet creation.

Tasks:
- [x] Add STATUS polling helper for `/2/media/upload` with terminal state handling.
- [x] Parse `processing_info` fields including `state`, `error`, and `check_after_secs`.
- [x] Respect server-provided polling interval (`check_after_secs`) and enforce max timeout budget.
- [x] Update chunked upload flow to poll after FINALIZE when processing is async.
- [x] Return structured error when processing fails (include server code/name/message if present).

Acceptance:
- [x] Resolves `DEV-004` and `DEV-009`.
- [x] Adds tests for `VID-004`, `VID-005`, `VID-006`, `VID-008`.

## Batch 2: Tweet Attach Safety for Video (P1)

Objective:
- Prevent posting tweets with media IDs that are not yet in `succeeded` processing state.

Tasks:
- [x] Ensure post path for uploaded videos is gated on successful processing completion.
- [x] Add explicit failure path for processing timeout.
- [x] Ensure warning/error messaging is actionable and includes upload phase.

Acceptance:
- [x] Resolves `DEV-005`.
- [x] Adds tests for `VID-007` and timeout branch.

## Batch 3: OAuth Scope and Expiry Edge Cases (P1/P2)

Objective:
- Tighten scope correctness and refresh boundary behavior.

Tasks:
- [x] Add strict scope assertion tests for write/media operation sets.
- [x] Add refresh-buffer boundary tests around token expiry.
- [x] Add deterministic error test for missing refresh token + expired access token.
- [x] Validate health-status callback transitions for refresh success/failure.

Acceptance:
- [x] Closes `DEV-001` and `DEV-002`.
- [x] Improves `AUTH-003`, `AUTH-006`, `AUTH-010` from Partial/Missing.

## Batch 4: Parser and Error Mapping Hardening (P2)

Objective:
- Improve correctness for mixed payloads and actionable errors.

Tasks:
- [x] Add parser tests for payloads containing `data` and `errors` simultaneously.
- [x] Add explicit test vectors for 401/403/429/5xx mapping behavior.
- [x] Preserve provider error details in surfaced SDK errors.

Acceptance:
- [x] Addresses `DEV-006` and `DEV-007`.
- [x] Improves `READ-004`, `READ-010` from Missing/Partial.

## Batch 5: Differential Runtime Verification (Final Gate)

Objective:
- Validate behavior against selected references with evidence traces.

Tasks:
- [x] Run representative OAuth/read/post/video scenarios with trace capture (mock-driven in test suite).
- [ ] Compare SDK request/response contracts with live references.
- [x] Record current outcomes in `conformance_matrix.md`.
- [x] Close/update deviations with linked unit-test evidence.
- [x] Prepare live execution artifacts (`live_differential_runbook.md`, `live_differential_execution_log.md`).

Acceptance:
- [x] No open P0/P1 deviations.
- [ ] Final sign-off entries complete in `conformance_matrix.md`.

Remaining blocker for final sign-off:
- Live authenticated differential execution against references (`twurl`/official SDK) per `live_differential_runbook.md`.
- Required live scenario IDs: `LIVE-OAUTH-REFRESH-001`, `LIVE-READ-TWEET-001`, `LIVE-READ-SEARCH-001`, `LIVE-POST-TEXT-001`, `LIVE-POST-REPLY-001`, `LIVE-POST-QUOTE-001`, `LIVE-VIDEO-CHUNKED-001`, `LIVE-VIDEO-CODEC-001`.
- Current status: all required `LIVE-*` scenarios are marked `Blocked` until authenticated runtime context is provided.

Additional non-blocking follow-up:
- If client-side retry policy is expanded, add jitter/backoff strategy assertions beyond current retry-after derivation checks.

Recently completed additions beyond baseline batches:
- Added interruption/recovery validation for chunked APPEND failure and manual retry success.
- Added unsupported codec/container failure mapping test coverage.
