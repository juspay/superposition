# TOML Response Format

## Overview

The `/config` endpoint now supports TOML response format via HTTP content negotiation. Clients can request configuration data in TOML format by including the appropriate `Accept` header.

## Requesting TOML Format

### HTTP Request

```http
GET /config HTTP/1.1
Accept: application/toml
```

### cURL Example

```bash
curl -X GET http://localhost:8080/config \
  -H "Accept: application/toml"
```

### Response

```http
HTTP/1.1 200 OK
Content-Type: application/toml
Last-Modified: <timestamp>
X-Audit-Id: <audit-id>
X-Config-Version: <version>

[default-config]
timeout = { value = 30, schema = { type = "integer" } }
max_retries = { value = 3, schema = { type = "integer" } }

[dimensions]
os = { position = 1, schema = { type = "string" } }
environment = { position = 2, schema = { type = "string" } }

[context."os=linux"]
timeout = 60

[context."os=linux; environment=production"]
max_retries = 5
```

## Backward Compatibility

The endpoint defaults to JSON format for backward compatibility. If no `Accept` header is provided, or if the header doesn't specify a supported format, JSON is returned.

### Default JSON Response

```http
GET /config HTTP/1.1
```

```http
HTTP/1.1 200 OK
Content-Type: application/json

{
  "default_configs": {
    "timeout": 30,
    "max_retries": 3
  },
  "dimensions": {
    "os": {
      "position": 1,
      "schema": {
        "type": "string"
      }
    },
    "environment": {
      "position": 2,
      "schema": {
        "type": "string"
      }
    }
  },
  "contexts": [...]
}
```

## Accept Header Behavior

| Accept Header | Response Format |
|---------------|-----------------|
| `application/toml` | TOML |
| `application/json` | JSON |
| `*/*` | JSON (default) |
| (not specified) | JSON (default) |

## Content Negotiation Priority

The endpoint checks the `Accept` header in the following order:

1. If `application/toml` is present → Return TOML
2. If `application/json` is present → Return JSON
3. Otherwise → Return JSON (default)

## Error Handling

If TOML serialization fails, the endpoint returns a 500 Internal Server Error with an error message in JSON format.

### Error Response

```http
HTTP/1.1 500 Internal Server Error
Content-Type: application/json

{
  "message": "Failed to serialize config to TOML: <error details>"
}
```

## Schema Inference

When serializing to TOML, the function infers basic schema types based on the value:

| Value Type | Inferred Schema |
|------------|-----------------|
| String | `{ type = "string" }` |
| Integer | `{ type = "integer" }` |
| Float | `{ type = "number" }` |
| Boolean | `{ type = "boolean" }` |
| Array | `{ type = "array" }` |
| Object | `{ type = "object" }` |

**Note:** This is a simplified schema inference. Original schema details (like enum values, minimum/maximum constraints) are not preserved during serialization.

## Implementation Notes

- The `serialize_to_toml` function in `superposition_core` handles the TOML generation
- The `determine_response_format` function in `handlers.rs` parses the Accept header
- TOML output is deterministic: dimensions are sorted by position, context conditions are sorted alphabetically