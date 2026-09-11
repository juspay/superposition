# Migrating superposition onto `authn_kit`

Blocking items for the Phase 8 cutover. Each is a place where a default in this
crate differs from what `service_utils::middlewares::auth_n` does today, so
leaving it unset changes production behaviour.

## Blocking

| # | Item | Why it blocks | Required action |
|---|------|---------------|-----------------|
| 1 | **Cookie names** | The crate defaults to `session` / `authn_protection`; the original uses `user`, `org_<id>` and `protection`. A mismatch means every browser presents a cookie nothing reads, and **every logged-in user is silently signed out** on deploy. | Implement `AuthScope::session_cookie_name` to return `none` / `user` / `org_<id>`, and set `CookieSettings::protection("protection")` on the `LoginFlow`. |
| 2 | **`client_credentials` principal** | The original derives `service-account-<client_id>`; the crate emits the raw `client_id` in `IdentityClaims`. The principal string is the Casbin subject, so **existing M2M authorization rules stop matching**. | In `TryFrom<IdentityClaims> for User`, prefix with `service-account-` when `source == ClaimSource::ClientCredentials`. |
| 3 | **Password grant (ROPC)** | Off by default in the crate, on in the original. Any caller sending `X-Grant-Type: password` gets `501`. | Confirm whether any caller uses it. If so, `BasicAuthenticator::new(..).with_password_grant(true)`. |
| 4 | **Public-scope semantics** | `Login::None` returned `User::default()` (a real identity); the crate returns `Outcome::Anonymous` and inserts no principal. Handlers on excluded routes that extract a `User` will now fail. | Either audit those handlers, or set `PublicScopePolicy::AlwaysAnonymous` and give the scope a default identity, to reproduce the original exactly. |

## Identity key: staying with email

Authorization keys off `email`, unchanged. `IdentityClaims` also carries `subject`
(the IdP's `sub`), and the tradeoff is worth recording:

- `sub` is guaranteed stable and never reassigned within an issuer; email is not,
  so a user changing their address silently loses their permissions.
- But `sub` is opaque — `110248495921238986420` (Google),
  `auth0|507f1f77bcf86cd799439011` (Auth0), a UUID (Keycloak) — and **policies
  written against opaque strings are unmaintainable by hand**. That objection
  outweighs the stability benefit.
- Some providers (Entra ID) issue *pairwise* subjects, different per registered
  client, so `sub`-keyed policies would break if a second client were ever added.

If email churn does become a problem, the resolution is a local `users` table
keyed by `(issuer, sub)` with email as an attribute: `sub` becomes a join key
that never appears in a policy, and policies keep referencing email. That needs a
user table this codebase does not currently have, so it is out of scope for the
migration.

## Non-blocking behaviour changes

- **Malformed `Basic` credentials.** The original fell through to cookie
  authentication, so junk in `Authorization` alongside a valid session cookie
  succeeded as the cookie's user. Decide whether the Basic authenticator should
  return `NotApplicable` (parity) or reject.
- **Mutations are no longer redirected into login.** A `POST` with an expired
  session now gets `401` instead of a redirect that silently discarded the body.
- **Post-login redirect preserves the query string.** The original used
  `request.path()`, dropping it.
- **Scheme matching is case-insensitive**, so `authorization: bearer <token>`
  now works where it previously fell through to cookie auth.
- **The session-clearing cookie now carries a `Path`.** The original built it
  with neither `path` nor `http_only`
  (`crates/service_utils/src/middlewares/auth_n/oidc.rs:93-98`), so the browser
  defaulted the path to the request's directory. RFC 6265 requires a matching
  `Path` to overwrite a cookie, meaning that with a non-empty `path_prefix` the
  clear silently failed and a stale session survived the start of a new login.
- **The login-protection cookie is now `HttpOnly`.** It was commented out in the
  original (`oidc.rs:82`), leaving it readable from JavaScript. This matters more
  now, since the cookie carries the PKCE verifier.
