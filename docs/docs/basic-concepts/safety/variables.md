---
sidebar_position: 4
title: Variables
---

Variables are key-value pairs that store non-sensitive configuration values for use in [Functions](./functions.mdx). Unlike [Secrets](./secrets.md), variables are stored in plaintext and their values are visible via the API — making them ideal for configuration that doesn't require encryption.

## When to Use Variables vs Secrets

| Use Variables For | Use Secrets For |
|-------------------|-----------------|
| API endpoints (URLs) | API keys and tokens |
| Feature flags | Database passwords |
| Thresholds and limits | Private certificates |
| Default timeout values | OAuth credentials |
| Environment names | Sensitive configuration |

## Naming Conventions

Variable names must follow these rules:

- Start with an **uppercase letter** (A-Z)
- Contain only **uppercase letters, digits, and underscores**
- Maximum **50 characters**

Valid examples: `API_ENDPOINT`, `MAX_RETRIES`, `TIMEOUT_MS`, `ENVIRONMENT_NAME`

## Using Variables in Functions

Variables are automatically injected into function code as a JavaScript object named `VARS`:

```javascript
async function execute(payload) {
    const { value_validate } = payload;
    
    // Access variables directly
    const apiEndpoint = VARS.API_ENDPOINT;
    const maxRetries = parseInt(VARS.MAX_RETRIES);
    const timeoutMs = parseInt(VARS.TIMEOUT_MS);
    
    // Use in validation logic
    if (someCondition) {
        const response = await axios.get(apiEndpoint, {
            timeout: timeoutMs
        });
    }
    
    return true;
}
```

The `VARS` object is injected at runtime before function execution. Only variables defined in your workspace are available.

## Audit Trail

All variable operations are automatically logged:

- Every create, update, and delete triggers an audit log entry
- The `change_reason` field is mandatory for tracking why changes were made
- Timestamps (`created_at`, `last_modified_at`) and actor information (`created_by`, `last_modified_by`) are maintained automatically

## Related Topics

- [Functions](./functions.mdx) - Use variables in validation/compute functions
- [Secrets](./secrets.md) - For sensitive configuration that requires encryption