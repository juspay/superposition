---
title: Authentication
description: How Superposition authenticates requests — providers, schemes, configuration, and the rationale behind the design.
---

# Authentication

This page documents Superposition's authentication (`auth_n`) subsystem end to
end: the providers you can choose, the credential schemes accepted on each
request, how to configure them, and — importantly — **why** the design is the
way it is.

> Authentication answers "who is making this request". It is distinct from
> **authorization** (what they are allowed to do), which is handled separately
> by the Casbin layer. See [Environment Variables → Authorization](./environment-variables.md#authorization).

## Design philosophy: Superposition does not store users

A deliberate stance shapes this whole subsystem: **Superposition does not store
users and does not do any user management.** It never owns a password table, a
user directory, or a session store. Every identity decision is delegated to an
external identity provider (IdP) or an external token service, and Superposition
only ever *verifies* what it is handed on each request.

This keeps Superposition stateless with respect to identity and lets you bring
your own IdP (Keycloak, Okta, Auth0, Google, …) and your own API-key/token
management system. The trade-offs made throughout the design flow from this
stance.

## Providers

The provider is selected by the `AUTH_PROVIDER` environment variable, whose
value is a `+`-separated string of `PROVIDER[+ARG...]`.

| `AUTH_PROVIDER` | Meaning |
| --- | --- |
| `DISABLED` | No authentication. Every request is treated as a default user. **Development / private evaluation only.** |
| `OIDC+<issuer_url>` | Single-realm OIDC. One IdP issuer authenticates everyone; organisations are a Superposition concept, not separate realms. |
| `OIDC_SAAS+<issuer_url>` | Per-organisation OIDC. Each org has its own realm/endpoints; the first issuer also acts as the global identity provider. |

```bash
# Disabled (dev only)
AUTH_PROVIDER=DISABLED

# Single-realm OIDC
AUTH_PROVIDER=OIDC+https://issuer.example.com/realms/users

# Per-org (SaaS) OIDC
AUTH_PROVIDER=OIDC_SAAS+https://issuer.example.com/realms/users
```

The key difference between `OIDC` and `OIDC_SAAS`:

- **`OIDC` (Simple)** validates every credential against the **single**
  configured realm. Organisations exist in Superposition's own data, but they
  all share one IdP realm.
- **`OIDC_SAAS`** resolves a **per-organisation** realm/endpoint (via
  `<organisation>` placeholder formats) so each tenant's credentials are
  validated against that tenant's own realm. This is what gives SaaS deployments
  tenant isolation at the identity layer.

## Credential schemes

The middleware inspects the `Authorization` header and dispatches by scheme.
All schemes resolve to a single `User` that the rest of the request pipeline
consumes.

| Scheme on `Authorization` | Used by | Validated how |
| --- | --- | --- |
| `Internal <token>` + `x-user` header | Trusted internal services | Shared secret equals `SUPERPOSITION_TOKEN`; identity taken from `x-user` |
| `Bearer <id_token>` | Interactive users / frontends | OIDC id-token: signature (JWKS), `iss`, `aud`, `exp` |
| `Bearer <prefix><delim><api_key>` | Machine-to-machine (API keys) | **Static tokens** (operator-configured list), then **RFC 7662 token introspection** against an external endpoint |
| `Basic base64(id:secret)` (+ `X-Grant-Type`) | Machine-to-machine / non-interactive | OAuth `client_credentials` (default) or `password`/ROPC against the IdP |

Positive classification is used throughout: each token type is identified by an
explicit marker (scheme, or a configured prefix), never by "it wasn't any of the
others". A token that matches nothing is rejected, not guessed.

### `Bearer` — OIDC id-token

The standard path. The frontend obtains an id-token from the IdP via the
interactive authorization-code login flow and presents it as
`Authorization: Bearer <id_token>`. Superposition validates it against the IdP's
JWKS (`iss`, `aud`, `exp`, signature) and derives the `User` from the token's
`email` / `preferred_username` claims.

### `Basic` — client credentials & password grants

`Basic` carries a `base64(id:secret)` pair. **What the pair means is chosen by
the `X-Grant-Type` header** — Basic is only the transport; the grant is stated
explicitly, never inferred:

| `X-Grant-Type` | Grant | `id:secret` is | Result |
| --- | --- | --- | --- |
| *(absent)* or `client_credentials` | OAuth Client Credentials | `client_id:client_secret` | Machine principal |
| `password` | Resource Owner Password Credentials (ROPC) | `username:password` | The user |
| *anything else* | — | — | **400 Bad Request** |

**Client credentials (`client_credentials`)** — the default. Superposition
performs an OAuth `client_credentials` exchange against the IdP using the
provided machine credentials. A successful exchange proves the credentials are
valid; the resulting principal is a synthetic, namespaced service account:
`service-account-<client_id>` (matching Keycloak's own convention). This is the
recommended machine-to-machine mechanism when your IdP issues client
credentials.

- In **`OIDC_SAAS`**, client-credentials requests are **org-scoped**: the
  machine's credentials are validated against the **organisation's own realm**
  token endpoint, and a request that is not org-scoped is rejected. This is the
  isolation boundary — a machine registered in org A's realm cannot authenticate
  against org B.

**Password (`password`)** — ROPC. Superposition exchanges the user's
username/password with the IdP for an id-token. Note that ROPC is **deprecated**
and **not supported by all IdPs**: Keycloak supports it; Google does not.
Against an IdP that rejects the grant you will receive **501 Not Implemented**
(see [Error semantics](#error-semantics)).

### `Bearer <prefix>` — API tokens (static tokens & RFC 7662 introspection)

This is the mechanism for **API-key style** access. A configured `<prefix>`
(and optional `<delimiter>`) marks a bearer token as an API key rather than an
OIDC id-token; Superposition strips the prefix and validates the remaining
`<api_key>` by one of **two mechanisms**, tried in order:

1. **Static tokens** — an operator-configured list of tokens, each bound to a
   fixed principal. Validated locally, no network call.
2. **RFC 7662 token introspection** — the key is forwarded to an external system
   that owns it, which answers "is this token active, and who is it?".

When both are configured, a presented key is checked against the **static tokens
first**, then introspected. At least one mechanism must be configured for the
flow to be enabled.

**This flow is optional.** It is available only under an OIDC provider and only
when its environment variables are configured. If unconfigured, prefixed tokens
are simply treated as ordinary bearer tokens (and will fail id-token validation).

#### Static tokens

For a small, fixed set of machine credentials — or a deployment with no
introspection endpoint — you can configure the tokens directly. This still fits
the "Superposition does not store users" stance: the tokens live in your secret
manager (KMS), not in a Superposition-managed user store, and each simply names
the principal it authenticates as.

`OIDC_API_STATIC_TOKENS` is a **KMS-encrypted** (like `OIDC_CLIENT_SECRET`) JSON
array; each entry maps a token to an identity:

```json
[
  { "token": "<high-entropy-secret>", "principal": "svc-ci", "email": "ci@example.com" },
  { "token": "<another-secret>", "principal": "svc-billing", "org": "acme" }
]
```

- `token` (required) — the secret presented as `<api_key>`. Compared in
  **constant time**; use high-entropy values.
- `principal` (required) — the identity to authenticate as (used as the
  username, and the email when `email` is omitted). Authorization (Casbin) keys
  off this.
- `email` (optional) — defaults to `principal`.
- `org` (optional, **SaaS only**) — binds the token to one organisation: it
  authenticates only `Login::Org(<org>)` requests. Omit `org` to make the token
  **global-scoped** (`Login::Global`). In single-realm OIDC, `org` is ignored.

Because static tokens are matched locally, a match returns immediately with **no
introspection round-trip and no caching** needed. A key that matches no static
token falls through to introspection (if configured), otherwise the request is
rejected (`401`).

#### RFC 7662 introspection

This path is built entirely around
**[RFC 7662 OAuth 2.0 Token Introspection](https://datatracker.ietf.org/doc/html/rfc7662)**:
rather than managing API keys itself, Superposition forwards the key to an
**external** system that owns it, and asks — per RFC 7662 — "is this token
active, and who is it?".

How it works:

1. A caller sends `Authorization: Bearer <prefix><delimiter><api_key>`, where
   `<prefix>` and `<delimiter>` are configured (`OIDC_API_TOKEN_PREFIX`,
   `OIDC_API_TOKEN_DELIMITER`). The prefix distinguishes an API key from a
   normal JWT id-token (which begins with `eyJ`).
2. Superposition strips the prefix and `POST`s the key to the introspection
   endpoint (configured via env, or discovered from provider metadata) as
   `application/x-www-form-urlencoded` `token=<api_key>`, authenticating itself
   with the `OIDC_INTROSPECTION_AUTH_HEADER`.
3. The endpoint returns a standard RFC 7662 JSON response. If `active` is
   `false` the request is rejected; otherwise the `User` is derived from the
   standard `username` / `sub` / `email` claims.

The endpoint is protected (RFC 7662 §2.1), so Superposition authenticates itself
to it. **`OIDC_INTROSPECTION_AUTH_HEADER` is the verbatim `Authorization` header
value** Superposition sends — e.g. `Bearer <token>` for a custom service, or
`Basic <base64(client_id:client_secret)>` for a provider's native endpoint (such
as Keycloak, which requires client authentication). It is a secret and is
KMS-decrypted in non-dev environments, exactly like `OIDC_CLIENT_SECRET`.

The response is 100% standard RFC 7662 — **no Superposition-specific claims are
required.** In many cases you do not even need to build an endpoint: Keycloak,
Okta, Hydra, Auth0 and others already expose an introspection endpoint you can
point at.

> **Introspection endpoint URL.** RFC 7662 does not define a fixed path — the
> endpoint is provider-specific and advertised as `introspection_endpoint` in
> the provider's `.well-known/openid-configuration`. For Keycloak it is
> `.../realms/{realm}/protocol/openid-connect/token/introspect`. Configure the
> exact URL from your provider's discovery document.

**Global-scope API tokens (SaaS).** By default SaaS API tokens are org-scoped
(above). API tokens under a **global** (`Login::Global`) request — mirroring how
the SaaS provider keeps a global client alongside its per-org clients — are
supported by resolving the global introspection endpoint in this order:

1. `OIDC_TOKEN_INTROSPECTION_URL` if set (explicit override), else
2. the `introspection_endpoint` **discovered** from the global provider's
   metadata (which Superposition already fetches), else
3. rejected.

So in the common case you need **no** extra env — the global endpoint is
discovered automatically. Note that `introspection_endpoint` is *optional* in
provider metadata (RFC 8414): an IdP can support introspection without
advertising it, in which case set `OIDC_TOKEN_INTROSPECTION_URL` explicitly.

**Tenant isolation (SaaS).** For `OIDC_SAAS`, the introspection endpoint is a
per-organisation URL format
(`OIDC_ORG_TOKEN_INTROSPECTION_URL_FORMAT`, with `<organisation>`). API-token
requests must be org-scoped, and the token is validated against **that org's
introspection endpoint**. A token minted for another org's realm comes back
`active: false`.

> **Design note — why no org claim.** We deliberately do **not** require a
> custom org claim in the introspection response, to keep the contract pure
> RFC 7662. Isolation therefore rests on the endpoint genuinely being per-org.
> If an operator configures a **single shared** introspection endpoint for all
> orgs in a SaaS deployment, there is **no tenant isolation on API keys** — that
> is a configuration responsibility, made in exchange for a standard,
> zero-custom-claim contract.

A minimal reference implementation of an introspection endpoint is provided —
see [Reference introspection service](#reference-introspection-service).

### `Internal` — application-internal only

:::danger Not for external use
The `Internal` scheme is **exclusively for Superposition's own internal
service-to-service calls**. It is **not** an authentication method for users or
integrations, and must never be documented to, or handed to, any caller.

`Authorization: Internal <token>` (where `<token>` equals `SUPERPOSITION_TOKEN`)
with an `x-user` header lets the caller **assert any identity it wants** — the
`User` is taken verbatim from `x-user` with no verification. `SUPERPOSITION_TOKEN`
is a single, deployment-global secret. Anyone who obtains it can impersonate any
user in any organisation — i.e. **full access**. Treat it like a root credential:
never expose the scheme through a gateway, never log it, and rotate it if
leaked.
:::

## Caching

Every `Basic` and API-token request would otherwise round-trip to the IdP or
introspection endpoint on **each** call. An in-memory cache short-circuits
repeat requests bearing the same credentials:

- **Key**: `(org_scope, grant_kind, principal_id, hash(secret))`. The secret is
  stored only as a hash, never in plaintext — which also makes **credential
  rotation an automatic cache miss** (a new secret hashes differently).
- **Value**: the resolved `User` plus an expiry.
- **TTL**: derived from the token's `expires_in` / `exp`, minus a 30s safety
  margin (60s fallback when the IdP omits expiry). Introspection results are
  additionally **capped at 5 minutes** so revocation lag stays small. All three
  bounds are overridable via env (`OIDC_CACHE_REFRESH_SAFETY_MARGIN_SECS`,
  `OIDC_FALLBACK_TTL_SECS`, `OIDC_MAX_INTROSPECTION_CACHE_TTL_SECS`; seconds) —
  see [Environment variables](./environment-variables.md).
- Expired entries are pruned lazily on write, so the cache cannot grow unbounded.

**Revocation trade-off:** because results are cached, a revoked credential can
remain accepted until its cache entry expires (bounded by the TTL / the 5-minute
introspection cap). This is the standard cost of caching introspection; the cap
keeps it small.

## Error semantics

Failures are mapped to distinct HTTP statuses so callers can tell "your
credential is bad" from "we couldn't verify it":

| Situation | Status |
| --- | --- |
| Valid credential | `200` (request proceeds) |
| Bad username/password or client credentials (`invalid_grant` / `invalid_client`) | `401 Unauthorized` |
| API token `active: false`, or active but no identity claim | `401 Unauthorized` |
| IdP does not support the requested grant (e.g. ROPC on Google → `unsupported_grant_type`) | `501 Not Implemented` |
| Unknown `X-Grant-Type` value | `400 Bad Request` |
| Introspection endpoint unreachable / errored / unparseable | `503 Service Unavailable` |

For diagnosis, the raw IdP/introspection error (error code + description) is
logged server-side even though the client-facing message is intentionally
coarse.

## Environment variables

### Core OIDC

| Variable | Applies to | Notes |
| --- | --- | --- |
| `AUTH_PROVIDER` | all | `DISABLED` \| `OIDC+<issuer>` \| `OIDC_SAAS+<issuer>` |
| `OIDC_CLIENT_ID` | OIDC | Superposition's own client id at the IdP |
| `OIDC_CLIENT_SECRET` | OIDC | Client secret (plaintext or KMS ciphertext) |
| `OIDC_REDIRECT_HOST` | OIDC | Base URL for the interactive login redirect |
| `OIDC_ORG_ISSUER_ENDPOINT_FORMAT` | SaaS | Per-org issuer, with `<organisation>` |
| `OIDC_ORG_TOKEN_ENDPOINT_FORMAT` | SaaS | Per-org token endpoint, with `<organisation>` |

### API-token authentication (optional)

**Required to enable the flow:** `OIDC_API_TOKEN_PREFIX` plus **at least one**
validation mechanism — static tokens (`OIDC_API_STATIC_TOKENS`) and/or RFC 7662
introspection (`OIDC_INTROSPECTION_AUTH_HEADER`). A prefix set with neither
mechanism logs a warning and stays disabled. When both are set, a key is matched
against static tokens first, then introspected.

For **introspection under SaaS**, the per-org
`OIDC_ORG_TOKEN_INTROSPECTION_URL_FORMAT` is also required (the per-org endpoint
can't be discovered, and a per-org setup is the whole point of SaaS). The
single/global endpoint URL is otherwise optional — when unset it is taken from
the `introspection_endpoint` discovered in your provider's metadata.

**Startup validation** (fail-fast — the misconfiguration surfaces at boot, not
on the first request). A malformed `OIDC_API_STATIC_TOKENS` JSON fails startup.
When **introspection** is enabled:

- **Simple**: fails to start if no endpoint can be resolved — neither
  `OIDC_TOKEN_INTROSPECTION_URL` nor a discovered `introspection_endpoint`.
- **SaaS**: fails to start if `OIDC_ORG_TOKEN_INTROSPECTION_URL_FORMAT` is
  missing. A **global-only** SaaS configuration is rejected — it contradicts the
  per-org isolation model.

(A **static-only** setup needs no introspection endpoint, so these endpoint
checks don't apply.)

| Variable | Applies to | Notes |
| --- | --- | --- |
| `OIDC_API_TOKEN_PREFIX` | OIDC / SaaS | Arbitrary, operator-chosen prefix that marks a bearer token as an API key (e.g. `apikey`). Only needs to be distinguishable from a JWT. **Required** to enable the flow. |
| `OIDC_API_TOKEN_DELIMITER` | OIDC / SaaS | Optional separator between prefix and key (e.g. `_`). Defaults to `_`. |
| `OIDC_API_STATIC_TOKENS` | OIDC / SaaS | KMS-encrypted (like `OIDC_CLIENT_SECRET`) JSON array of `{token, principal[, email, org]}`. Enables the static-token mechanism. In SaaS, `org` binds a token to an organisation; omit for global scope. |
| `OIDC_INTROSPECTION_AUTH_HEADER` | OIDC / SaaS | Verbatim `Authorization` header Superposition sends to the endpoint (`Bearer <token>` or `Basic <base64>`). Secret — KMS-decrypted in non-dev, like `OIDC_CLIENT_SECRET`. Enables the introspection mechanism. |
| `OIDC_TOKEN_INTROSPECTION_URL` | OIDC (Simple) / SaaS (global) | Explicit single (Simple) / global (SaaS `Login::Global`) endpoint. **Optional** — falls back to the discovered `introspection_endpoint`. Set it only when the IdP doesn't advertise one, or to override. |
| `OIDC_ORG_TOKEN_INTROSPECTION_URL_FORMAT` | SaaS | Per-org endpoint, with `<organisation>`. Not discoverable. **Required** when introspection is enabled under SaaS (a global-only SaaS introspection config is rejected at startup). |

The endpoint URL is provider-specific (`introspection_endpoint` in the
provider's discovery document); for Keycloak it is
`.../realms/{realm}/protocol/openid-connect/token/introspect`.

Example (single-realm OIDC with API tokens enabled):

```bash
AUTH_PROVIDER=OIDC+https://issuer.example.com/realms/users
OIDC_CLIENT_ID=superposition
OIDC_CLIENT_SECRET='<client-secret-or-kms-ciphertext>'
OIDC_REDIRECT_HOST=https://superposition.example.com

# Optional API-token flow. Prefix + at least one mechanism (static and/or introspection).
OIDC_API_TOKEN_PREFIX=apikey
OIDC_API_TOKEN_DELIMITER=_
OIDC_INTROSPECTION_AUTH_HEADER='Bearer <token>'   # or 'Basic <base64(id:secret)>'; KMS ciphertext in non-dev
# Optional: omit to use the endpoint discovered from provider metadata.
OIDC_TOKEN_INTROSPECTION_URL=https://tokens.example.com/introspect
```

Example (single-realm OIDC, **static tokens only** — no introspection endpoint):

```bash
AUTH_PROVIDER=OIDC+https://issuer.example.com/realms/users
OIDC_CLIENT_ID=superposition
OIDC_CLIENT_SECRET='<client-secret-or-kms-ciphertext>'
OIDC_REDIRECT_HOST=https://superposition.example.com

OIDC_API_TOKEN_PREFIX=apikey
OIDC_API_TOKEN_DELIMITER=_
# KMS ciphertext in non-dev; each token maps to a fixed principal.
OIDC_API_STATIC_TOKENS='[{"token":"<key>","principal":"svc-ci","email":"ci@example.com"}]'
```

Example (SaaS with per-org API-token introspection):

```bash
AUTH_PROVIDER=OIDC_SAAS+https://issuer.example.com/realms/users
OIDC_CLIENT_ID=superposition
OIDC_CLIENT_SECRET='<client-secret-or-kms-ciphertext>'
OIDC_REDIRECT_HOST=https://superposition.example.com
OIDC_ORG_TOKEN_ENDPOINT_FORMAT='https://issuer.example.com/realms/<organisation>/protocol/openid-connect/token'
OIDC_ORG_ISSUER_ENDPOINT_FORMAT='https://issuer.example.com/realms/<organisation>'

OIDC_API_TOKEN_PREFIX=apikey
OIDC_API_TOKEN_DELIMITER=_
OIDC_INTROSPECTION_AUTH_HEADER='Bearer <token>'   # or 'Basic <base64(id:secret)>'; KMS ciphertext in non-dev
OIDC_ORG_TOKEN_INTROSPECTION_URL_FORMAT='https://issuer.example.com/realms/<organisation>/protocol/openid-connect/token/introspect'
# Global (Login::Global) endpoint is auto-discovered from provider metadata;
# set OIDC_TOKEN_INTROSPECTION_URL only to override it.
```

## Design decisions & rationale

- **No user storage.** Superposition delegates all identity to an IdP or an
  external token service, staying stateless. Every other decision follows from
  this.
- **RFC 7662 for API tokens.** Choosing a standard (rather than a bespoke
  contract) means external teams can often reuse an existing introspection
  endpoint with zero custom code, and the response needs no Superposition-specific
  claims.
- **Static tokens as a no-infra alternative.** Not every deployment wants to
  stand up (or point at) an introspection endpoint for a handful of machine
  credentials. A KMS-encrypted list keeps the secrets in your secret manager
  rather than a Superposition user store, is matched in constant time, and
  — being checked before introspection — lets a fixed "master" key coexist with
  dynamically introspected ones.
- **Explicit grant selection (`X-Grant-Type`).** A `Basic` pair is ambiguous
  between a user's password and a client's credentials. Rather than inferring
  intent from the credential, the grant is stated in a header and unknown values
  fail closed — auth intent is never guessed.
- **Client credentials over ROPC for machines.** ROPC is deprecated and
  IdP-dependent (Google rejects it). Client credentials is the correct M2M grant
  and works wherever the IdP issues client credentials.
- **Org-scoped validation in SaaS.** Machine credentials and API tokens are
  validated against the organisation's own realm/endpoint, so a credential for
  one tenant cannot authenticate against another.
- **Positive classification.** Every token type is identified by an explicit
  marker; anything unrecognised is rejected, not routed to a default validator.
- **Caching keyed by hashed secrets.** Repeat requests avoid IdP round-trips,
  rotation is an automatic miss, and plaintext secrets are never held in memory —
  at the cost of a bounded revocation lag.

## Reference introspection service

A minimal reference implementation of an RFC 7662 introspection endpoint is
provided under [`examples/token-introspection`](https://github.com/juspay/superposition/tree/main/examples/token-introspection)
in the repository. It shows the exact request/response contract Superposition
expects and can be used as a starting point for your own token-management
service. It is illustrative only and not production-hardened.
