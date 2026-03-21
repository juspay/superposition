# Implementation Plan: HMAC-SHA256 Webhook Authentication

## Overview

Add HMAC-SHA256 signing to outbound webhook calls so receivers can verify payload integrity and authenticate that the request originated from Superposition. This follows the same pattern used by GitHub, Stripe, and Slack.

## Signing Scheme

**Headers added to every webhook request:**
- `X-Superposition-Signature-256`: `sha256=<hex-encoded HMAC-SHA256>`
- `X-Superposition-Timestamp`: Unix epoch seconds (e.g., `1711036800`)

**Signed content:** `<timestamp>.<json_body>`
- Concatenate timestamp + "." + raw JSON body, then compute HMAC-SHA256 with the webhook's signing secret.
- Including the timestamp prevents replay attacks — receivers should reject requests older than ~5 minutes.

**Secret management:**
- Each webhook gets an optional `signing_secret` column (encrypted at rest using the existing workspace encryption key infrastructure from `encryption.rs`).
- When `signing_secret` is NULL, no signature headers are sent (backward compatible).

---

## Step-by-Step Plan

### Step 1: Database Migration

**New migration:** `crates/superposition_types/migrations/2026-03-21-000000_webhook_signing_secret/up.sql`

```sql
ALTER TABLE public.webhooks ADD COLUMN signing_secret TEXT;
```

**down.sql:**
```sql
ALTER TABLE public.webhooks DROP COLUMN signing_secret;
```

- Column is nullable — existing webhooks continue to work without signing.
- Value stored is the **encrypted** secret (same AES-256-GCM as existing secrets).

### Step 2: Update Diesel Schema

**File:** `crates/superposition_types/src/database/schema.rs`

Add `signing_secret -> Nullable<Text>` to the `webhooks` table definition.

### Step 3: Update Webhook Database Model

**File:** `crates/superposition_types/src/database/models/others.rs`

Add to `Webhook` struct:
```rust
pub signing_secret: Option<String>,  // encrypted HMAC secret
```

### Step 4: Update API Types (Create/Update Requests)

**File:** `crates/superposition_types/src/api/webhook.rs`

- `CreateWebhookRequest`: Add `pub signing_secret: Option<String>` (plaintext from user).
- `UpdateWebhookRequest`: Add `pub signing_secret: Option<String>` (plaintext from user).
- `HeadersEnum`: Add `Signature` and `Timestamp` variants.

### Step 5: Update Smithy API Definition

**File:** `smithy/models/webhook.smithy`

- Add `signing_secret: String` (optional) to the `Webhook` resource properties.
- Add it to `CreateWebhook` and `UpdateWebhook` input structures.
- **Exclude** it from `WebhookResponse` (never return the secret in API responses).

### Step 6: Update CRUD Handlers (Encrypt on Write)

**File:** `crates/superposition/src/webhooks/handlers.rs`

- **create_handler**: If `signing_secret` is provided, encrypt it using the workspace encryption key before storing.
- **update_handler**: Same — encrypt if provided. Use the `AsChangeset` derive to handle optional update.
- **GET/List handlers**: Strip `signing_secret` from responses (always return `None`) so it's never leaked.

### Step 7: Core — Sign Outbound Webhook Requests

**File:** `crates/service_utils/src/helpers.rs` — `execute_webhook_call()`

After building the JSON body but **before** sending:

1. Check if `webhook.signing_secret` is `Some(encrypted_secret)`.
2. If yes:
   a. Decrypt using workspace encryption key (reuse `fetch_secrets` / `decrypt_secret` pattern).
   b. Get current Unix timestamp.
   c. Serialize the `WebhookResponse` to a JSON string (use `serde_json::to_string` instead of `.json()` on the request builder).
   d. Compute: `HMAC-SHA256(decrypted_secret, "{timestamp}.{json_body}")`.
   e. Add headers:
      - `X-Superposition-Signature-256: sha256=<hex>`
      - `X-Superposition-Timestamp: <timestamp>`
   f. Send body using `.body()` with `Content-Type: application/json` instead of `.json()`.
3. If no signing secret, send as before (no behavior change).

**New dependency:** Add `hmac` and `sha2` crates to `crates/service_utils/Cargo.toml`.

### Step 8: Regenerate SDK (if needed)

The Smithy model change may require regenerating the SDK in `crates/superposition_sdk/`. Follow existing codegen process.

---

## Files Changed (Summary)

| File | Change |
|------|--------|
| `migrations/2026-03-21-000000_webhook_signing_secret/up.sql` | **New** — add column |
| `migrations/2026-03-21-000000_webhook_signing_secret/down.sql` | **New** — drop column |
| `superposition_types/src/database/schema.rs` | Add `signing_secret` column |
| `superposition_types/src/database/models/others.rs` | Add field to `Webhook` |
| `superposition_types/src/api/webhook.rs` | Add field to request types, new header variants |
| `smithy/models/webhook.smithy` | Add `signing_secret` to resource/operations |
| `superposition/src/webhooks/handlers.rs` | Encrypt on create/update, strip on read |
| `service_utils/src/helpers.rs` | HMAC signing in `execute_webhook_call()` |
| `service_utils/Cargo.toml` | Add `hmac`, `sha2` dependencies |

## Security Considerations

- **Secret never returned in API responses** — GET/List always returns `signing_secret: null`.
- **Encrypted at rest** — uses existing AES-256-GCM workspace encryption (same as Secrets feature).
- **Replay protection** — timestamp included in signed content; receivers should enforce a window.
- **Key rotation** — workspace key rotation (`rotate_workspace_encryption_key_helper`) should be extended to re-encrypt webhook signing secrets alongside regular secrets.
- **Backward compatible** — existing webhooks without a signing secret continue to work unchanged.

## Consumer Verification (Documentation Note)

Receivers verify like this (pseudocode):
```python
import hmac, hashlib

expected = hmac.new(
    signing_secret.encode(),
    f"{timestamp}.{raw_body}".encode(),
    hashlib.sha256
).hexdigest()

if not hmac.compare_digest(f"sha256={expected}", signature_header):
    reject()
if abs(time.time() - int(timestamp)) > 300:
    reject()  # replay protection
```
