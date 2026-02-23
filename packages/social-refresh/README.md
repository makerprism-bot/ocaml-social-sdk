# social-refresh

Optional shared token refresh orchestration package for `ocaml-social-sdk`.

This package introduces provider-neutral refresh types and API surface so refresh
decision and orchestration logic can be centralized without changing provider
public APIs.

## Modules

- `Social_refresh.Types`: shared refresh policy, decisions, and outcomes.
- `Social_refresh.Time`: RFC3339 parsing and refresh-window calculations.
- `Social_refresh.Decision`: token refresh decision engine (`Skip` vs `Refresh_required`).
- `Social_refresh.Orchestrator`: load/decide/refresh/persist/health pipeline.

## Current integrations

- `social-twitter-v2`
- `social-threads-v1`

## License

MIT
