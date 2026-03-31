---
name: superposition-api
description: REST API reference for Superposition. Use when making direct HTTP calls to Superposition endpoints, integrating with custom HTTP clients, or when SDK is not available in your language.
license: Apache-2.0
compatibility: Requires HTTP client and Superposition instance access
metadata:
  author: juspay
  version: "1.0"
---

# Superposition REST API Reference

This skill provides a quick reference for the Superposition REST API - useful when you need to make direct HTTP calls instead of using SDKs.

## Authentication

All API requests require headers:

```http
x-org-id: your-org-id
x-workspace: your-workspace-id  # or x-tenant (both accepted)
Authorization: Bearer your-api-token  # For write operations
```

## Base URL

```
http://localhost:8080  # Local development
https://superposition.example.com  # Production
```

## Quick Reference

### Configuration Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| POST | `/dimension` | Create dimension |
| GET | `/dimension` | List dimensions |
| GET | `/dimension/{name}` | Get dimension |
| PATCH | `/dimension/{name}` | Update dimension |
| DELETE | `/dimension/{name}` | Delete dimension |
| POST | `/default-config` | Create default config |
| GET | `/default-config` | List default configs |
| GET | `/default-config/{key}` | Get default config |
| PATCH | `/default-config/{key}` | Update default config |
| DELETE | `/default-config/{key}` | Delete default config |
| POST | `/context` | Create context with override |
| GET | `/context` | List contexts |
| GET | `/context/{id}` | Get context |
| DELETE | `/context/{id}` | Delete context |
| POST | `/config` | Resolve configuration |

### Experiment Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| POST | `/experiments` | Create experiment |
| GET | `/experiments` | List experiments |
| GET | `/experiments/{id}` | Get experiment |
| PUT | `/experiments/{id}/ramp` | Ramp traffic |
| PUT | `/experiments/{id}/conclude` | Conclude experiment |
| PUT | `/experiments/{id}/discard` | Discard experiment |
| PUT | `/experiments/{id}/pause` | Pause experiment |
| PUT | `/experiments/{id}/resume` | Resume experiment |

### Function Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| POST | `/function` | Create function |
| GET | `/function` | List functions |
| GET | `/function/{name}` | Get function |
| PATCH | `/function/{name}` | Update function |
| DELETE | `/function/{name}` | Delete function |
| POST | `/function/{name}/test` | Test function |

### Webhook Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| POST | `/webhook` | Create webhook |
| GET | `/webhook` | List webhooks |
| GET | `/webhook/{name}` | Get webhook |
| PATCH | `/webhook/{name}` | Update webhook |
| DELETE | `/webhook/{name}` | Delete webhook |

### Type Template Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| POST | `/types` | Create type template |
| GET | `/types` | List type templates |
| GET | `/types/{name}` | Get type template |
| PATCH | `/types/{name}` | Update type template |
| DELETE | `/types/{name}` | Delete type template |

## Common API Patterns

### Resolve Configuration

```bash
curl -X POST http://localhost:8080/config \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "context": {
      "city": "Delhi",
      "vehicle_type": "cab",
      "hour_of_day": 20
    }
  }'
```

**Response:**
```json
{
  "per_km_rate": 25.0,
  "base_fare": 50.0,
  "surge_factor": 2.0
}
```

### Create Dimension

```bash
curl -X POST http://localhost:8080/dimension \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "dimension": "user_tier",
    "description": "User loyalty tier",
    "position": 2,
    "schema": {
      "type": "string",
      "enum": ["bronze", "silver", "gold", "platinum"]
    },
    "change_reason": "Adding user tier dimension"
  }'
```

### Create Experiment

```bash
curl -X POST http://localhost:8080/experiments \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "name": "checkout-test",
    "description": "Test simplified checkout",
    "context": {"platform": "web"},
    "variants": [
      {
        "id": "control",
        "variant_type": "CONTROL",
        "overrides": {"checkout_steps": 5}
      },
      {
        "id": "simplified",
        "variant_type": "EXPERIMENTAL",
        "overrides": {"checkout_steps": 3}
      }
    ],
    "change_reason": "A/B test for checkout"
  }'
```

### Ramp Experiment Traffic

```bash
curl -X PUT http://localhost:8080/experiments/checkout-test/ramp \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "traffic_percentage": 25,
    "change_reason": "Ramping to 25%"
  }'
```

### Get Applicable Variants (for Providers)

```bash
curl -X POST http://localhost:8080/experiments/applicable-variants \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "context": {
      "platform": "web",
      "user_id": "user-123"
    }
  }'
```

**Response:**
```json
{
  "data": [
    {
      "experiment_id": "checkout-test",
      "variant_id": "simplified",
      "variant_type": "EXPERIMENTAL",
      "overrides": {
        "checkout_steps": 3
      }
    }
  ]
}
```

## Response Codes

| Code | Meaning |
|------|---------|
| 200 | Success |
| 201 | Created |
| 400 | Bad Request - Invalid request body |
| 401 | Unauthorized - Missing or invalid token |
| 403 | Forbidden - Insufficient permissions |
| 404 | Not Found - Resource doesn't exist |
| 409 | Conflict - Resource already exists |
| 422 | Unprocessable - Validation failed |
| 500 | Internal Server Error |

## Error Response Format

```json
{
  "message": "Validation failed: dimension 'city' already exists",
  "code": "VALIDATION_ERROR",
  "details": {
    "field": "dimension",
    "value": "city"
  }
}
```

## Pagination

List endpoints support pagination:

```bash
GET /experiments?limit=20&offset=40
```

**Response:**
```json
{
  "data": [...],
  "total": 100,
  "limit": 20,
  "offset": 40
}
```

## Related Skills

- [superposition-config](../superposition-config/) - Configuration concepts and examples
- [superposition-experiments](../superposition-experiments/) - Experiment lifecycle
- [superposition-sdk](../superposition-sdk/) - SDK methods (preferred over direct API)

See [references/REFERENCE.md](references/REFERENCE.md) for complete API endpoint documentation.
