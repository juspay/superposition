---
title: Environment Variables
description: Runtime configuration for self-hosted Superposition deployments.
---

# Environment Variables

These are the main environment variables used by a self-hosted Superposition
deployment.

## Core Runtime Settings

| Variable | Example | Notes |
| --- | --- | --- |
| `PORT` | `8080` | HTTP listen port. |
| `APP_ENV` | `PROD`, `DEV`, `TEST`, or `SANDBOX` | Self-hosted deployments should typically use `DEV` or `PROD`. The practical behavior differences for self-hosting are documented below in Secret Loading Modes. |
| `SERVICE_PREFIX` | empty or `sp` | Set empty for root deployment. A value like `sp` serves the app under `/sp`. |
| `CAC_HOST` | `https://superposition.example.com` | Public base URL used by generated links and internal callbacks. |
| `API_HOSTNAME` | `https://superposition.example.com` | Public API host exposed to the UI. |
| `SUPERPOSITION_VERSION` | `v0.112.0` | Sent in the `X-SERVER-VERSION` response header. |
| `WORKER_ID` | `1` | Unique ID generator worker. Use a unique value per replica, from `0` to `255`. |
| `RUST_LOG` | `info,superposition=info` | Logging filter for JSON logs. |
| `ACTIX_WORKER_COUNT` | `5` | Number of Actix workers. |
| `ACTIX_KEEP_ALIVE` | `120` | Keep-alive timeout in seconds. |
| `SUPERPOSITION_TOKEN` | secret value | Internal service token. In `DEV`, `TEST`, and `SANDBOX`, it defaults to `123456` if unset. In `PROD`, provide a KMS ciphertext. |

## Database And Cache Settings

| Variable | Example | Notes |
| --- | --- | --- |
| `DB_USER` | `superposition` | PostgreSQL user. |
| `DB_PASSWORD` | secret value | Plain value in `DEV` or `TEST`. In `PROD` and `SANDBOX`, provide a KMS ciphertext. |
| `DB_HOST` | `postgres:5432` | Host and port without scheme. |
| `DB_NAME` | `superposition` | PostgreSQL database name. |
| `MAX_DB_CONNECTION_POOL_SIZE` | `10` | PostgreSQL pool size per app process. |
| `REDIS_URL` | `redis://redis:6379` | Optional. Leave unset to disable Redis caching. |
| `REDIS_POOL_SIZE` | `10` | Optional Redis pool size override. |
| `REDIS_MAX_ATTEMPTS` | `10` | Redis reconnect attempts. |
| `REDIS_CONN_TIMEOUT` | `1000` | Redis connect timeout in milliseconds. |

## Required Validation Flags

These flags are required during startup:

```bash
ALLOW_SAME_KEYS_OVERLAPPING_CTX=true
ALLOW_DIFF_KEYS_OVERLAPPING_CTX=true
ALLOW_SAME_KEYS_NON_OVERLAPPING_CTX=true
```

`TENANT_MIDDLEWARE_EXCLUSION_LIST` is also required. Start with the default
list from `.env.example` and only change it if you are modifying route prefixes
or frontend asset paths:

```bash
TENANT_MIDDLEWARE_EXCLUSION_LIST="/health,/assets/favicon.ico,/pkg/frontend.js,/pkg,/pkg/frontend_bg.wasm,/pkg/tailwind.css,/pkg/style.css,/assets,/admin,/oidc/login,/admin/organisations,/organisations,/organisations/switch/{organisation_id},/"
```

## Secret Loading Modes

Superposition currently has two practical deployment modes:

- `APP_ENV=PROD`: the app initializes AWS KMS and expects encrypted, base64 KMS
  ciphertext values for sensitive settings such as `DB_PASSWORD`,
  `SUPERPOSITION_TOKEN`, `OIDC_CLIENT_SECRET`, `OIDC_INTROSPECTION_AUTH_HEADER`
  and `OIDC_API_STATIC_TOKENS` (when API-token auth is enabled),
  `CASBIN_DB_PASSWORD`, and `MASTER_ENCRYPTION_KEY`.
- `APP_ENV=DEV`: the app reads those values directly from environment variables.
  This is useful for local and non-AWS self-hosted deployments. If you use this
  mode outside local development, inject secrets from your platform secret
  manager and enable real authentication.

If you want to use Superposition secrets, set `MASTER_ENCRYPTION_KEY` to a
base64-encoded 32-byte key:

```bash
openssl rand -base64 32
```

Optional key rotation support:

| Variable | Required | Notes |
| --- | --- | --- |
| `MASTER_ENCRYPTION_KEY` | Yes, for secrets | Current base64-encoded 32-byte key. |
| `PREVIOUS_MASTER_ENCRYPTION_KEY` | No | Used during key rotation fallback. |

See [Secrets](../basic-concepts/safety/secrets.md) for workspace and master key
rotation behavior.

## Authentication

For private evaluation, you can disable authentication and authorization:

```bash
AUTH_PROVIDER=DISABLED
AUTH_Z_PROVIDER=DISABLED
```

For production, configure OIDC authentication:

```bash
AUTH_PROVIDER=OIDC+https://issuer.example.com/realms/users
OIDC_CLIENT_ID=superposition
OIDC_CLIENT_SECRET='<client-secret-or-kms-ciphertext>'
OIDC_REDIRECT_HOST=https://superposition.example.com
```

For organisation-scoped OIDC:

```bash
AUTH_PROVIDER=OIDC_SAAS+https://issuer.example.com/realms/users
OIDC_CLIENT_ID=superposition
OIDC_CLIENT_SECRET='<client-secret-or-kms-ciphertext>'
OIDC_REDIRECT_HOST=https://superposition.example.com
OIDC_ORG_TOKEN_ENDPOINT_FORMAT='https://issuer.example.com/realms/<organisation>/protocol/openid-connect/token'
OIDC_ORG_ISSUER_ENDPOINT_FORMAT='https://issuer.example.com/realms/<organisation>'
```

Optional API-token authentication (OIDC only) is enabled with a prefix plus at
least one validation mechanism: **static tokens** and/or **RFC 7662 token
introspection**. When both are configured, a presented key is matched against
static tokens first, then introspected.

```bash
OIDC_API_TOKEN_PREFIX=apikey                      # arbitrary, operator-chosen; marks a bearer token as an API key
OIDC_API_TOKEN_DELIMITER=_                         # optional; separates prefix and key (defaults to "_")

# Mechanism 1 — static tokens. KMS-encrypted (like OIDC_CLIENT_SECRET) JSON array;
# each entry maps a token to a fixed principal. In SaaS, "org" binds a token to an
# organisation; omit "org" for a global-scoped token. "email" is optional.
OIDC_API_STATIC_TOKENS='[{"token":"<key>","principal":"svc-ci","org":"acme"}]'

# Mechanism 2 — RFC 7662 introspection. Verbatim Authorization header sent to the
# endpoint (Bearer or Basic). Secret — KMS-encrypted in non-dev.
OIDC_INTROSPECTION_AUTH_HEADER='Bearer <token>'   # or 'Basic <base64(id:secret)>'

# Introspection endpoint URLs are OPTIONAL — when unset, the endpoint is
# discovered from provider metadata. Simple: single endpoint (optional).
OIDC_TOKEN_INTROSPECTION_URL='https://issuer.example.com/realms/users/protocol/openid-connect/token/introspect'

# SaaS OIDC: the per-org format is REQUIRED when introspection is enabled (a
# global-only SaaS introspection config is rejected at startup); the global
# (Login::Global) endpoint is discovered unless OIDC_TOKEN_INTROSPECTION_URL
# overrides it.
OIDC_ORG_TOKEN_INTROSPECTION_URL_FORMAT='https://issuer.example.com/realms/<organisation>/protocol/openid-connect/token/introspect'
```

Optional cache-tuning knobs (all in seconds). Defaults are sensible; override
only if you need to. Values are read at startup, so a malformed value fails the
process at boot rather than on a request.

```bash
# Cap on how long an RFC 7662 introspection result is cached, regardless of the
# token's own expiry — keeps revocation lag small for long-lived tokens. (300)
OIDC_MAX_INTROSPECTION_CACHE_TTL_SECS=300

# For Basic-auth (password / client_credentials) token-exchange caching:
# how far before the exchanged token's expiry a cached principal is dropped,
# to stay ahead of clock skew and in-flight latency. (30)
OIDC_CACHE_REFRESH_SAFETY_MARGIN_SECS=30

# Fallback cache TTL used when the IdP omits `expires_in` from a Basic-auth
# token response (`expires_in` is only RECOMMENDED by RFC 6749, not required). (60)
OIDC_FALLBACK_TTL_SECS=60
```

For the complete authentication picture — all credential schemes (including
`Basic` machine-to-machine access and API-key introspection), how they behave,
caching, and error semantics — see the dedicated
[Authentication](./authentication.md) page.

## Authorization

To enable Casbin authorization policies:

```bash
AUTH_Z_PROVIDER=CASBIN
CASBIN_DB_USER=superposition
CASBIN_DB_PASSWORD='<database-password-or-kms-ciphertext>'
CASBIN_DB_HOST='postgres.example.com:5432'
CASBIN_DB_NAME=superposition
CASBIN_DB_POOL_SIZE=3
CASBIN_POLICY_REFRESH_INTERVAL=20
ROOT_ADMIN_EMAIL=admin@example.com
```

Initialize the Casbin policy table once:

```bash
export CASBIN_DATABASE_URL="postgres://${CASBIN_DB_USER}:${CASBIN_DB_PASSWORD}@${CASBIN_DB_HOST}/${CASBIN_DB_NAME}"
psql "$CASBIN_DATABASE_URL" -f crates/service_utils/src/middlewares/auth_z/casbin/casbin.sql
```
