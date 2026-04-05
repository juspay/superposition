---
sidebar_position: 3
title: Secrets
---
Secrets are encrypted key-value pairs that store sensitive information (API keys, tokens, passwords) for use in [Functions](./functions.mdx) and Webhooks. Superposition encrypts secrets at rest and decrypts them only at runtime when needed.

## Encryption Architecture

Superposition uses a two-layer encryption model:

```
Master Encryption Key (MEK)
         │
         ▼
┌─────────────────────┐
│ Workspace Encryption│  ← One per workspace
│ Key (WEK)           │    stored encrypted in DB
└─────────────────────┘
         │
         ▼
┌─────────────────────┐
│     Secrets         │  ← Encrypted with WEK
└─────────────────────┘
```

- **Master Encryption Key (MEK)**: Stored in environment variable `MASTER_ENCRYPTION_KEY` (or AWS KMS in production). Never stored in the database.
- **Workspace Encryption Key (WEK)**: Auto-generated per workspace, stored encrypted in the `workspaces` table.
- **Secrets**: Encrypted with AES-256-GCM and stored in the `secrets` table.

This architecture ensures:
- Each workspace has isolated encryption
- Compromising one workspace doesn't affect others
- Key rotation can happen per-workspace or globally

## Managing Secrets

### Create a Secret

```bash
POST /secrets
{
  "name": "API_KEY",
  "value": "sk_live_xxxxx",
  "description": "Production API key for payment service",
  "change_reason": "Initial setup for payment integration"
}
```

### Update a Secret

```bash
PATCH /secrets/API_KEY
{
  "value": "sk_live_yyyyy",
  "change_reason": "Rotated expired key"
}
```

### List Secrets

```bash
GET /secrets?name=API_KEY,PASSWORD&sort_on=last_modified_at&sort_by=desc
```

### Delete a Secret

```bash
DELETE /secrets/API_KEY
```

:::note
The actual secret value is never returned by the API. Only metadata (name, description, timestamps) is exposed.
:::

## Using Secrets

### In Functions

Secrets are automatically injected into function code as a JavaScript object named `SECRETS`:

```javascript
async function execute(payload) {
    const { value_validate } = payload;
    
    // Access secrets directly
    const apiKey = SECRETS.API_KEY;
    const dbPassword = SECRETS.DB_PASSWORD;
    
    // Use in HTTP calls
    const response = await axios.get('https://api.example.com/data', {
        headers: { 'Authorization': `Bearer ${apiKey}` }
    });
    
    return true;
}
```

The `SECRETS` object is injected at runtime before function execution. Only secrets defined in your workspace are available.

### In Webhooks

Secrets can be referenced in webhook custom headers using template syntax:

```json
{
  "custom_headers": {
    "Authorization": "Bearer {{secret:API_KEY}}",
    "X-Api-Secret": "{{secret:WEBHOOK_SECRET}}"
  }
}
```

At webhook execution time, `{{secret:NAME}}` templates are replaced with decrypted values.

## Key Rotation

Superposition supports rotating encryption keys without downtime:

### Master Key Rotation

1. Set `PREVIOUS_MASTER_ENCRYPTION_KEY` to the current key value
2. Set `MASTER_ENCRYPTION_KEY` to the new key value
3. Call the rotation endpoint:

```bash
POST /secrets/rotate
```

This re-encrypts all workspace keys and secrets across all workspaces.

### How Fallback Works

During rotation, the system uses both keys:
- **Decryption**: Tries current key first, falls back to previous key
- **Encryption**: Always uses the current key

This allows gradual migration without service interruption.

## Configuration

### Environment Variables

| Variable | Required | Description |
|----------|----------|-------------|
| `MASTER_ENCRYPTION_KEY` | Yes* | Base64-encoded 32-byte key for encrypting workspace keys |
| `PREVIOUS_MASTER_ENCRYPTION_KEY` | No | Previous key for rotation fallback |

*Required to use secrets functionality. If not set, secrets APIs return an error and `SECRETS` object will be empty in functions.

### Generating a Key

```bash
# Generate a new 32-byte key (base64 encoded)
openssl rand -base64 32
```

## Security Considerations

- **Never log secret values**: The system masks secrets in logs
- **Limit secret access**: Use workspace-level isolation for different teams/environments
- **Rotate keys regularly**: Use the rotation endpoint after updating environment variables
- **Audit changes**: All secret operations are logged with user and timestamp
- **Value never exposed**: API responses only include metadata, never the decrypted value


## Related Topics

- [Functions](./functions.mdx) - Use secrets in validation/compute functions
- **Webhooks** - Use secrets in webhook headers via `{{secret:NAME}}` template syntax
- [Variables](./variables.md) - Non-sensitive configuration values