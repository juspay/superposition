# `auth_n` — authentication middleware

This module authenticates incoming requests. Superposition does **not** store
users or do user management — every identity decision is delegated to an
external OIDC provider or an external RFC 7662 token-introspection service, and
verified per request.

Full documentation — providers, credential schemes (`Internal`, `Bearer`
OIDC id-token, `Basic` client-credentials/password with `X-Grant-Type`, and
API-key introspection), caching, error semantics, the environment-variable
reference, and the rationale behind each decision — lives in:

**[docs/docs/self-hosting/authentication.md](../../../../../docs/docs/self-hosting/authentication.md)**
(also published on the docs site under Self-Hosting → Authentication).

Module layout:

| File | Responsibility |
| --- | --- |
| `authentication.rs` | The `Authenticator` trait, `Login` scope, `BasicAuthGrant` |
| `oidc/simple_authenticator.rs` | Single-realm OIDC provider |
| `oidc/saas_authenticator.rs` | Per-organisation (SaaS) OIDC provider |
| `oidc/api_token.rs` | API-token flow: static tokens + RFC 7662 introspection client |
| `oidc/token_cache.rs` | In-memory cache for token-exchange / introspection results |
| `oidc/utils.rs` | Shared OIDC client + grant-exchange helpers |
| `no_auth.rs` | `DISABLED` provider |
