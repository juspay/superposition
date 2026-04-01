# REST API Complete Reference

Complete REST API documentation for Superposition.

## Table of Contents

- [Authentication](#authentication)
- [Dimensions API](#dimensions-api)
- [Default Config API](#default-config-api)
- [Context API](#context-api)
- [Override API](#override-api)
- [Function API](#function-api)
- [Type Templates API](#type-templates-api)
- [Experiments API](#experiments-api)
- [Webhooks API](#webhooks-api)
- [Configuration Resolution API](#configuration-resolution-api)
- [Organization & Workspace API](#organization--workspace-api)
- [Audit & Versioning API](#audit--versioning-api)

---

## Authentication

### Required Headers

All requests must include:

```http
x-org-id: your-organization-id
x-workspace: your-workspace-id
```

Write operations require authentication:

```http
Authorization: Bearer your-api-token
```

---

## Dimensions API

### Create Dimension

**POST /dimension**

Creates a new dimension.

**Request:**
```bash
curl -X POST http://localhost:8080/dimension \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "dimension": "city",
    "description": "City where user is located",
    "position": 1,
    "schema": {
      "type": "string",
      "enum": ["Bangalore", "Delhi", "Mumbai", "Chennai"]
    },
    "dependencies": [],
    "dimension_type": {"REGULAR": {}},
    "value_validation_function_name": null,
    "value_compute_function_name": null,
    "change_reason": "Adding city dimension"
  }'
```

**Request Body:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| dimension | string | Yes | Dimension name (unique) |
| description | string | Yes | Human-readable description |
| position | number | Yes | Evaluation order (0 reserved for variantIds) |
| schema | object | Yes | JSON Schema for validation |
| dependencies | string[] | No | Dimensions this depends on |
| dimension_type | object | No | REGULAR, LOCAL_COHORT, or REMOTE_COHORT |
| value_validation_function_name | string | No | Function for custom validation |
| value_compute_function_name | string | No | Function for value computation |
| change_reason | string | Yes | Audit trail reason |

**Response (200):**
```json
{
  "dimension": "city",
  "description": "City where user is located",
  "position": 1,
  "schema": {
    "type": "string",
    "enum": ["Bangalore", "Delhi", "Mumbai", "Chennai"]
  },
  "dimension_type": {"REGULAR": {}},
  "mandatory": false,
  "dependency_graph": {},
  "created_at": "2024-01-15T10:30:00Z",
  "created_by": "admin",
  "last_modified_at": "2024-01-15T10:30:00Z",
  "last_modified_by": "admin"
}
```

### List Dimensions

**GET /dimension**

```bash
curl -X GET "http://localhost:8080/dimension" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

**Response:**
```json
{
  "data": [
    {
      "dimension": "city",
      "position": 1,
      "description": "City where user is located",
      "schema": {"type": "string", "enum": ["Bangalore", "Delhi"]}
    },
    {
      "dimension": "user_tier",
      "position": 2,
      "description": "User loyalty tier",
      "schema": {"type": "string", "enum": ["bronze", "silver", "gold"]}
    }
  ]
}
```

### Get Dimension

**GET /dimension/{dimension_name}**

```bash
curl -X GET http://localhost:8080/dimension/city \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Update Dimension

**PATCH /dimension/{dimension_name}**

```bash
curl -X PATCH http://localhost:8080/dimension/city \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "schema": {
      "type": "string",
      "enum": ["Bangalore", "Delhi", "Mumbai", "Chennai", "Hyderabad"]
    },
    "change_reason": "Added Hyderabad"
  }'
```

### Delete Dimension

**DELETE /dimension/{dimension_name}**

```bash
curl -X DELETE http://localhost:8080/dimension/old_dimension \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

---

## Default Config API

### Create Default Config

**POST /default-config**

```bash
curl -X POST http://localhost:8080/default-config \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "key": "checkout_timeout_seconds",
    "value": {"value": 300},
    "schema": {"type": "integer", "minimum": 60, "maximum": 900},
    "function_name": null,
    "change_reason": "Default checkout timeout"
  }'
```

**Request Body:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| key | string | Yes | Configuration key name |
| value | object | Yes | Object with `value` property |
| schema | object | Yes | JSON Schema for the value |
| function_name | string | No | Validation function name |
| change_reason | string | Yes | Audit trail reason |

**Response (200):**
```json
{
  "key": "checkout_timeout_seconds",
  "value": {"value": 300},
  "schema": {"type": "integer", "minimum": 60, "maximum": 900},
  "function_name": null,
  "created_at": "2024-01-15T10:30:00Z",
  "created_by": "admin",
  "last_modified_at": "2024-01-15T10:30:00Z",
  "last_modified_by": "admin"
}
```

### List Default Configs

**GET /default-config**

```bash
curl -X GET http://localhost:8080/default-config \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Get Default Config

**GET /default-config/{key}**

```bash
curl -X GET http://localhost:8080/default-config/checkout_timeout_seconds \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Update Default Config

**PATCH /default-config/{key}**

```bash
curl -X PATCH http://localhost:8080/default-config/checkout_timeout_seconds \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "value": {"value": 600},
    "change_reason": "Increased timeout"
  }'
```

### Delete Default Config

**DELETE /default-config/{key}**

```bash
curl -X DELETE http://localhost:8080/default-config/old_key \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

---

## Context API

### Create Context

**POST /context**

Creates a context with associated overrides.

```bash
curl -X POST http://localhost:8080/context \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "context": {
      "city": "Delhi",
      "vehicle_type": "cab"
    },
    "override": {
      "per_km_rate": 25.0,
      "base_fare": 50.0
    },
    "change_reason": "Delhi cab pricing"
  }'
```

**Request Body:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| context | object | Yes | Key-value map matching dimension values |
| override | object | Yes | Key-value overrides |
| change_reason | string | Yes | Audit trail reason |

**Response (200):**
```json
{
  "id": "ctx-abc123",
  "context": {
    "city": "Delhi",
    "vehicle_type": "cab"
  },
  "override": {
    "per_km_rate": 25.0,
    "base_fare": 50.0
  },
  "created_at": "2024-01-15T10:30:00Z",
  "created_by": "admin"
}
```

### List Contexts

**GET /context**

```bash
curl -X GET http://localhost:8080/context \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Get Context

**GET /context/{context_id}**

```bash
curl -X GET http://localhost:8080/context/ctx-abc123 \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Delete Context

**DELETE /context/{context_id}**

```bash
curl -X DELETE http://localhost:8080/context/ctx-abc123 \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Move Context

**PUT /context/{context_id}/move**

Change context position in evaluation order.

```bash
curl -X PUT http://localhost:8080/context/ctx-abc123/move \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "position": 5,
    "change_reason": "Moving context for correct evaluation order"
  }'
```

---

## Override API

### Update Override

**PATCH /override/{override_id}**

Update the override values for an existing context.

```bash
curl -X PATCH http://localhost:8080/override/ovr-xyz789 \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "value": {
      "per_km_rate": 28.0,
      "base_fare": 55.0
    },
    "change_reason": "Updated pricing"
  }'
```

---

## Function API

### Create Function

**POST /function**

```bash
curl -X POST http://localhost:8080/function \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "function_name": "validate_discount",
    "code": "async function validate(key, value) {\n  const maxDiscount = { platinum: 50, gold: 30, silver: 20, bronze: 10 };\n  const tier = context.user_tier || \"bronze\";\n  return value <= maxDiscount[tier];\n}",
    "change_reason": "Discount validation by tier"
  }'
```

**Response (200):**
```json
{
  "function_name": "validate_discount",
  "code": "async function validate(key, value) {...}",
  "published_code": null,
  "created_at": "2024-01-15T10:30:00Z",
  "created_by": "admin",
  "last_modified_at": "2024-01-15T10:30:00Z",
  "last_modified_by": "admin"
}
```

### List Functions

**GET /function**

```bash
curl -X GET http://localhost:8080/function \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Get Function

**GET /function/{function_name}**

```bash
curl -X GET http://localhost:8080/function/validate_discount \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Update Function

**PATCH /function/{function_name}**

```bash
curl -X PATCH http://localhost:8080/function/validate_discount \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "code": "async function validate(key, value) {\n  // Updated logic\n  return true;\n}",
    "change_reason": "Updated validation logic"
  }'
```

### Test Function

**POST /function/{function_name}/test**

```bash
curl -X POST http://localhost:8080/function/validate_discount/test \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "test_case": {
      "key": "discount_percentage",
      "value": 25
    },
    "context": {
      "user_tier": "gold"
    }
  }'
```

**Response:**
```json
{
  "valid": true,
  "execution_time_ms": 5
}
```

### Delete Function

**DELETE /function/{function_name}**

```bash
curl -X DELETE http://localhost:8080/function/old_function \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

---

## Type Templates API

### Create Type Template

**POST /types**

```bash
curl -X POST http://localhost:8080/types \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "type_name": "CurrencyAmount",
    "type_schema": {
      "type": "object",
      "properties": {
        "amount": {"type": "number", "minimum": 0},
        "currency": {"type": "string", "enum": ["USD", "EUR", "INR"]}
      },
      "required": ["amount", "currency"]
    },
    "change_reason": "Standard currency type"
  }'
```

### List Type Templates

**GET /types**

```bash
curl -X GET http://localhost:8080/types \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Get Type Template

**GET /types/{type_name}**

```bash
curl -X GET http://localhost:8080/types/CurrencyAmount \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Update Type Template

**PATCH /types/{type_name}**

```bash
curl -X PATCH http://localhost:8080/types/CurrencyAmount \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "type_schema": {
      "type": "object",
      "properties": {
        "amount": {"type": "number", "minimum": 0},
        "currency": {"type": "string", "enum": ["USD", "EUR", "INR", "GBP"]}
      },
      "required": ["amount", "currency"]
    },
    "change_reason": "Added GBP support"
  }'
```

### Delete Type Template

**DELETE /types/{type_name}**

```bash
curl -X DELETE http://localhost:8080/types/OldType \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

---

## Experiments API

### Create Experiment

**POST /experiments**

```bash
curl -X POST http://localhost:8080/experiments \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "name": "checkout-optimization",
    "description": "Test simplified checkout flow",
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
    "experiment_type": "DEFAULT",
    "experiment_group_id": null,
    "metrics": null,
    "change_reason": "A/B test for checkout"
  }'
```

**Response (200):**
```json
{
  "id": "checkout-optimization",
  "name": "checkout-optimization",
  "description": "Test simplified checkout flow",
  "context": {"platform": "web"},
  "status": "CREATED",
  "traffic_percentage": 0,
  "variants": [
    {
      "id": "control",
      "variant_type": "CONTROL",
      "context_id": "ctx-1",
      "override_id": "ovr-1",
      "overrides": {"checkout_steps": 5}
    },
    {
      "id": "simplified",
      "variant_type": "EXPERIMENTAL",
      "context_id": "ctx-2",
      "override_id": "ovr-2",
      "overrides": {"checkout_steps": 3}
    }
  ],
  "override_keys": ["checkout_steps"],
  "created_at": "2024-01-15T10:30:00Z",
  "created_by": "admin"
}
```

### List Experiments

**GET /experiments**

Query parameters:
- `status` - Filter by status
- `limit` - Number of results
- `offset` - Pagination offset

```bash
curl -X GET "http://localhost:8080/experiments?status=INPROGRESS" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Get Experiment

**GET /experiments/{experiment_id}**

```bash
curl -X GET http://localhost:8080/experiments/checkout-optimization \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Ramp Experiment

**PUT /experiments/{experiment_id}/ramp**

```bash
curl -X PUT http://localhost:8080/experiments/checkout-optimization/ramp \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "traffic_percentage": 25,
    "change_reason": "Ramping to 25%"
  }'
```

### Conclude Experiment

**PUT /experiments/{experiment_id}/conclude**

```bash
curl -X PUT http://localhost:8080/experiments/checkout-optimization/conclude \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "chosen_variant": "simplified",
    "change_reason": "Simplified showed 15% improvement"
  }'
```

### Discard Experiment

**PUT /experiments/{experiment_id}/discard**

```bash
curl -X PUT http://localhost:8080/experiments/checkout-optimization/discard \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "change_reason": "Negative results in testing"
  }'
```

### Pause Experiment

**PUT /experiments/{experiment_id}/pause**

```bash
curl -X PUT http://localhost:8080/experiments/checkout-optimization/pause \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "change_reason": "Pausing for analysis"
  }'
```

### Resume Experiment

**PUT /experiments/{experiment_id}/resume**

```bash
curl -X PUT http://localhost:8080/experiments/checkout-optimization/resume \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "change_reason": "Resuming after analysis"
  }'
```

### Get Applicable Variants

**POST /experiments/applicable-variants**

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
      "experiment_id": "checkout-optimization",
      "variant_id": "simplified",
      "variant_type": "EXPERIMENTAL",
      "overrides": {"checkout_steps": 3}
    }
  ]
}
```

---

## Webhooks API

### Create Webhook

**POST /webhook**

```bash
curl -X POST http://localhost:8080/webhook \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "name": "experiment-webhook",
    "description": "Notify on experiment events",
    "url": "https://your-service.com/webhook",
    "method": "POST",
    "enabled": true,
    "events": ["ExperimentStarted", "ExperimentConcluded"],
    "custom_headers": {
      "Authorization": "Bearer token"
    },
    "change_reason": "Setup notifications"
  }'
```

### List Webhooks

**GET /webhook**

```bash
curl -X GET http://localhost:8080/webhook \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Get Webhook

**GET /webhook/{webhook_name}**

```bash
curl -X GET http://localhost:8080/webhook/experiment-webhook \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Update Webhook

**PATCH /webhook/{webhook_name}**

```bash
curl -X PATCH http://localhost:8080/webhook/experiment-webhook \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "enabled": false,
    "change_reason": "Temporarily disabling"
  }'
```

### Delete Webhook

**DELETE /webhook/{webhook_name}**

```bash
curl -X DELETE http://localhost:8080/webhook/experiment-webhook \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Get Webhooks by Event

**GET /webhook/event/{event_type}**

```bash
curl -X GET http://localhost:8080/webhook/event/ExperimentConcluded \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

---

## Configuration Resolution API

### Resolve Configuration

**POST /config**

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
  "surge_factor": 2.0,
  "checkout_timeout_seconds": 300
}
```

### Get Config (with prefix filter)

**POST /config?prefix=checkout_**

```bash
curl -X POST "http://localhost:8080/config?prefix=checkout_" \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{"city": "Delhi"}'
```

### Get Config as TOML

**GET /config/toml**

```bash
curl -X GET http://localhost:8080/config/toml \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Get Config as JSON

**GET /config/json**

```bash
curl -X GET http://localhost:8080/config/json \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Import Config from TOML

**PUT /config**

```bash
curl -X PUT http://localhost:8080/config \
  -H "Content-Type: application/toml" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  --data-binary @config.toml
```

### Get Config Version

**GET /config/versions**

```bash
curl -X GET http://localhost:8080/config/versions \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

**Response:**
```json
{
  "version": 42
}
```

### Validate Context

**POST /context/validate**

```bash
curl -X POST http://localhost:8080/context/validate \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "city": "Delhi",
    "vehicle_type": "cab"
  }'
```

**Response:**
```json
{
  "valid": true,
  "errors": []
}
```

---

## Organization & Workspace API

### Create Organization

**POST /superposition/organisations**

```bash
curl -X POST http://localhost:8080/superposition/organisations \
  -H "Content-Type: application/json" \
  -d '{
    "org_id": "acme-corp",
    "name": "Acme Corporation"
  }'
```

### List Organizations

**GET /superposition/organisations**

```bash
curl -X GET http://localhost:8080/superposition/organisations
```

### Get Organization

**GET /superposition/organisation/{org_id}**

```bash
curl -X GET http://localhost:8080/superposition/organisation/acme-corp
```

### Create Workspace

**POST /superposition/workspace**

```bash
curl -X POST http://localhost:8080/superposition/workspace \
  -H "Content-Type: application/json" \
  -H "x-org-id: acme-corp" \
  -d '{
    "workspace_id": "production",
    "name": "Production Environment"
  }'
```

### List Workspaces

**GET /superposition/workspaces**

```bash
curl -X GET http://localhost:8080/superposition/workspaces \
  -H "x-org-id: acme-corp"
```

### Get Workspace

**GET /superposition/workspace/{workspace_id}**

```bash
curl -X GET http://localhost:8080/superposition/workspace/production \
  -H "x-org-id: acme-corp"
```

---

## Audit & Versioning API

### List Audit Logs

**GET /audit-logs**

```bash
curl -X GET "http://localhost:8080/audit-logs?limit=50" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

**Response:**
```json
{
  "data": [
    {
      "id": "log-123",
      "action": "UPDATE",
      "resource_type": "dimension",
      "resource_id": "city",
      "change_reason": "Added Hyderabad",
      "performed_by": "admin",
      "performed_at": "2024-01-15T10:30:00Z"
    }
  ]
}
```

### List Versions

**GET /versions**

```bash
curl -X GET "http://localhost:8080/versions?limit=20" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Get Version

**GET /version/{version}**

```bash
curl -X GET http://localhost:8080/version/42 \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

**Response:**
```json
{
  "version": 42,
  "created_at": "2024-01-15T10:30:00Z",
  "created_by": "admin",
  "change_reason": "Updated city dimension",
  "changes": [
    {
      "resource_type": "dimension",
      "resource_id": "city",
      "action": "UPDATE"
    }
  ]
}
```

### Publish Configuration

**POST /publish**

Publish all draft changes.

```bash
curl -X POST http://localhost:8080/publish \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "change_reason": "Publishing Q1 configuration"
  }'
```

---

## Health & Info Endpoints

### Health Check

**GET /health**

```bash
curl -X GET http://localhost:8080/health
```

**Response:**
```
Health is good :D
```

### Readiness Check

**GET /ready**

```bash
curl -X GET http://localhost:8080/ready
```

### Server Info

**GET /info**

```bash
curl -X GET http://localhost:8080/info
```

**Response:**
```json
{
  "version": "0.100.0",
  "commit": "abc123",
  "build_date": "2024-01-15"
}
```
