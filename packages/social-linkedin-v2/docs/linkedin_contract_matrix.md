# LinkedIn Contract Matrix

Research date: 2026-02-12

Primary references:
- https://github.com/linkedin-developers/linkedin-api-python-client
- https://github.com/linkedin-developers/linkedin-api-js-client

## OAuth

| Operation | Endpoint | Method | Request format | Required headers | Expected fields |
| --- | --- | --- | --- | --- | --- |
| Authorization URL | `/oauth/v2/authorization` | Redirect | Query params: `response_type=code`, `client_id`, `redirect_uri`, `scope`, `state` | n/a | n/a |
| Exchange auth code | `/oauth/v2/accessToken` | POST | `application/x-www-form-urlencoded` body with `grant_type=authorization_code`, `code`, `client_id`, `client_secret`, `redirect_uri` | `Content-Type: application/x-www-form-urlencoded` | `access_token`, `expires_in`, optional `refresh_token`, optional `token_type` |
| Exchange refresh token | `/oauth/v2/accessToken` | POST | `application/x-www-form-urlencoded` body with `grant_type=refresh_token`, `refresh_token`, `client_id`, `client_secret` | `Content-Type: application/x-www-form-urlencoded` | `access_token`, `expires_in`, optional `refresh_token`, optional `token_type` |

## Rest.li read/write shape

| Operation | Endpoint | Method | Key query/path expectations | Headers |
| --- | --- | --- | --- | --- |
| Get post | `/v2/ugcPosts/{urn}` | GET | URN path key encoded | `Authorization`, `X-Restli-Protocol-Version: 2.0.0` |
| Get own posts | `/v2/ugcPosts` | GET (finder) | includes `q=authors`, `authors=List(<person_urn>)`, paging | `Authorization`, `X-Restli-Protocol-Version: 2.0.0`, `X-RestLi-Method: FINDER` |
| Batch get posts | `/v2/ugcPosts` | GET (batch get) | `ids=List(<urn1>,<urn2>,...)` | `Authorization`, `X-Restli-Protocol-Version: 2.0.0`, `X-RestLi-Method: BATCH_GET` |
| Search posts | `/v2/ugcPosts` | GET (finder) | current implementation supports author-based finder only (`q=authors`, `authors=List(...)`); keyword finder is intentionally rejected | `Authorization`, `X-Restli-Protocol-Version: 2.0.0`, `X-RestLi-Method: FINDER` |
| Create post | `/v2/ugcPosts` | POST | JSON body with `author`, `specificContent`, `visibility` | `Authorization`, `Content-Type: application/json`, `X-Restli-Protocol-Version: 2.0.0` |

## Notes

- This matrix is used as the request/response contract baseline for provider tests.
