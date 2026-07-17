// Reference RFC 7662 (OAuth 2.0 Token Introspection) endpoint for Superposition's
// optional API-token authentication flow.
//
// Superposition delegates API-key validation to an external service (it does not
// store users or manage tokens itself). When a request arrives as
// `Authorization: Bearer <prefix><delimiter><api_key>`, Superposition strips the
// prefix and POSTs the key to this endpoint per RFC 7662, then builds a principal
// from the standard response claims.
//
// This is an ILLUSTRATIVE reference only — an in-memory key store, no real
// crypto, no persistence. It exists to show the exact request/response contract.
//
//   npm install && npm start
//
// Then, from Superposition's config:
//   OIDC_TOKEN_INTROSPECTION_URL=http://localhost:4000/introspect
//   OIDC_INTROSPECTION_AUTH_HEADER="Bearer <INTROSPECTION_AUTH_TOKEN below>"
//   OIDC_API_TOKEN_PREFIX=apikey   (arbitrary, operator-chosen)
//   OIDC_API_TOKEN_DELIMITER=_
//
// OIDC_INTROSPECTION_AUTH_HEADER is sent to this endpoint verbatim as the
// Authorization header. This example expects the `Bearer <token>` form.

const express = require("express");

const app = express();
// RFC 7662 requests are application/x-www-form-urlencoded.
app.use(express.urlencoded({ extended: false }));

// The bearer token Superposition presents (the value inside
// OIDC_INTROSPECTION_AUTH_HEADER="Bearer <this>").
// The introspection endpoint MUST be protected (RFC 7662 §2.1).
const INTROSPECTION_AUTH_TOKEN =
  process.env.INTROSPECTION_AUTH_TOKEN || "change-me-caller-secret";

// A stand-in "token store". In a real service this is your API-key database.
// Each entry maps an api_key -> the identity + expiry to report.
const now = () => Math.floor(Date.now() / 1000);
const TOKENS = {
  // A valid, active key.
  "live-abc123": {
    active: true,
    sub: "svc-billing",
    username: "svc-billing",
    email: "svc-billing@example.com",
    exp: now() + 3600, // seconds since the Unix epoch
  },
  // A revoked key — reported as inactive.
  "live-revoked": { active: false },
};

// Verify the caller (Superposition) is authorized to introspect.
function callerAuthorized(req) {
  const auth = req.get("authorization") || "";
  const [scheme, value] = auth.split(" ");
  return scheme === "Bearer" && value === INTROSPECTION_AUTH_TOKEN;
}

app.post("/introspect", (req, res) => {
  if (!callerAuthorized(req)) {
    // The CALLER is unauthorized (not the token being introspected).
    return res.status(401).json({ error: "invalid_client" });
  }

  const token = req.body.token;
  const entry = token && TOKENS[token];

  // RFC 7662: an unknown/expired/revoked token is simply `{ "active": false }`.
  if (!entry || !entry.active || (entry.exp && entry.exp < now())) {
    return res.json({ active: false });
  }

  // Active: return the standard claims. Superposition reads username/sub/email
  // for identity and exp to bound its cache.
  return res.json({
    active: true,
    sub: entry.sub,
    username: entry.username,
    email: entry.email,
    exp: entry.exp,
  });
});

const port = process.env.PORT || 4000;
app.listen(port, () => {
  console.log(`introspection endpoint listening on :${port}/introspect`);
});
