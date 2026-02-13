# Agent Workspace Rules

These rules apply to all coding agents working in this repository.

## Dune Lockfile Workflow

- If dependencies may have changed, the lockfile may be stale, or before running broader Dune operations after dependency-related edits, run:
  - `dune pkg lock`
- Treat `dune pkg lock` as the first recovery/sync step in that situation.

## Build Directory Safety

- Never run `dune clean`.
- Never delete or modify `_build/` manually (including `rm -rf _build`, partial deletes, or scripted cleanup).
- If build artifacts seem inconsistent, do not clean `_build/`; use non-destructive steps instead (for example, re-run targeted commands and report issues).
