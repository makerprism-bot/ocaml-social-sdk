# social-telegram-bot-v1

Telegram Bot API connector for **broadcast posting** to channels and groups.

This package intentionally focuses on publishing workflows only:

- Text posting via `sendMessage`
- Photo posting via `sendPhoto`
- Video posting via `sendVideo`
- Sequential `post_thread` orchestration with deterministic partial-success behavior

Out of scope:

- Direct-message workflows
- Webhooks or update polling
- Command bots and moderation tooling

## Usage

```ocaml
open Social_telegram_bot_v1

module Telegram = Telegram_bot_v1.Make (Your_config)

let () =
  Telegram.post_single
    ~account_id:"acct-1"
    ~target:"@your_channel"
    ~text:"Hello from OCaml"
    ~media_urls:[]
    (function
      | Error_types.Success message_id ->
          Printf.printf "Posted message id: %s\n" message_id
      | Error_types.Partial_success _
      | Error_types.Failure _ ->
          Printf.printf "Post failed\n")
```

Your config module must provide:

- standard `social-core` credential and health callbacks
- `get_chat_id` resolver to map a target key (for example `@channel`) to a Telegram chat ID

## Notes

- Bot token is read from `credentials.access_token`
- Broadcast guard rejects likely DM targets (positive numeric IDs)
- Token-like values are redacted from error surfaces
