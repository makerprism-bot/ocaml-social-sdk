# Reference Implementations

These repositories are used as reference points for Threads API behavior and Meta Graph patterns.

Research snapshot date: 2026-02-12

## Repositories

- `fbsamples/threads_api` (Node.js, official sample)
  https://github.com/fbsamples/threads_api
  Primary Threads-specific reference for OAuth URL, token lifecycle endpoints,
  read endpoint shapes, and publish sequencing (`/me/threads` then `/me/threads_publish`).

- `facebook/facebook-python-business-sdk` (Python, official)
  https://github.com/facebook/facebook-python-business-sdk
  Useful for Graph auth, paging, and error mapping patterns used across Meta APIs.

- `facebook/facebook-nodejs-business-sdk` (Node.js, official)
  https://github.com/facebook/facebook-nodejs-business-sdk
  Useful for Graph request construction and response handling conventions.

- `facebook/facebook-java-business-sdk` (Java, official)
  https://github.com/facebook/facebook-java-business-sdk
  Useful for typed request modeling and structured API error handling.

Notes:
- Threads-specific behavior should always be validated against official Threads documentation and live API behavior.
