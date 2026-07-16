# Reference token introspection service

A minimal, **illustrative** [RFC 7662](https://datatracker.ietf.org/doc/html/rfc7662)
OAuth 2.0 Token Introspection endpoint for Superposition's optional API-token
authentication flow.

Superposition takes the stance that it does **not** store users or manage
tokens. For API-key access it forwards the key to an external introspection
endpoint (this one) and builds a principal from the standard response. See the
[Authentication docs](../../docs/docs/self-hosting/authentication.md) for the
full flow.

> This is a reference only — an in-memory key store, no persistence, no real
> crypto. Use it to understand the contract, not in production.

## Run

```bash
npm install
INTROSPECTION_AUTH_TOKEN=my-caller-secret npm start
# listening on :4000/introspect
```

## The contract

**Request** (sent by Superposition):

```http
POST /introspect
Authorization: <OIDC_INTROSPECTION_AUTH_HEADER, verbatim — this example expects "Bearer <token>">
Content-Type: application/x-www-form-urlencoded

token=<api_key>
```

**Response** — active token:

```json
{ "active": true, "sub": "svc-billing", "username": "svc-billing", "email": "svc-billing@example.com", "exp": 1893456000 }
```

**Response** — unknown / revoked / expired token:

```json
{ "active": false }
```

Superposition derives identity from `username` → `sub` → `email`, and uses `exp`
to bound its cache. Any additional standard claims are ignored.

## Try it

```bash
# Valid key
curl -s -X POST http://localhost:4000/introspect \
  -H "Authorization: Bearer my-caller-secret" \
  -d token=live-abc123

# Revoked key -> { "active": false }
curl -s -X POST http://localhost:4000/introspect \
  -H "Authorization: Bearer my-caller-secret" \
  -d token=live-revoked
```

## Point Superposition at it

```bash
AUTH_PROVIDER=OIDC+https://issuer.example.com/realms/users
# ... other OIDC vars ...
OIDC_API_TOKEN_PREFIX=apikey
OIDC_API_TOKEN_DELIMITER=_
OIDC_INTROSPECTION_AUTH_HEADER='Bearer my-caller-secret'
OIDC_TOKEN_INTROSPECTION_URL=http://localhost:4000/introspect
```

The prefix is arbitrary and operator-chosen — it only needs to be
distinguishable from a JWT. A caller then authenticates with:

```http
Authorization: Bearer apikey_live-abc123
```

(`apikey` + `_` + the api key `live-abc123`.)
