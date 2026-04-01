# Configuration Management API Reference

Complete API reference for managing dimensions, default configs, contexts, overrides, functions, and type templates.

## Table of Contents

- [Dimensions](#dimensions)
- [Default Configs](#default-configs)
- [Contexts & Overrides](#contexts--overrides)
- [Functions](#functions)
- [Type Templates](#type-templates)
- [Configuration Resolution](#configuration-resolution)

---

## Dimensions

### Create Dimension

**POST /dimension**

Creates a new dimension with JSON Schema validation.

```bash
curl -X POST http://localhost:8080/dimension \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "dimension": "user_tier",
    "description": "User membership tier for loyalty program",
    "position": 2,
    "schema": {
      "type": "string",
      "enum": ["bronze", "silver", "gold", "platinum"]
    },
    "change_reason": "Adding user tier for loyalty discounts"
  }'
```

**Response:**
```json
{
  "dimension": "user_tier",
  "description": "User membership tier for loyalty program",
  "position": 2,
  "schema": { "type": "string", "enum": ["bronze", "silver", "gold", "platinum"] },
  "dimension_type": { "REGULAR": {} },
  "mandatory": false,
  "dependency_graph": {},
  "created_at": "2024-01-15T10:30:00Z",
  "created_by": "admin",
  "last_modified_at": "2024-01-15T10:30:00Z",
  "last_modified_by": "admin"
}
```

### Create Dimension with Dependencies

```bash
curl -X POST http://localhost:8080/dimension \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "dimension": "zone",
    "description": "Delivery zone within city",
    "position": 3,
    "schema": { "type": "string" },
    "dependencies": ["city"],
    "change_reason": "Zone depends on city being set"
  }'
```

### Create Dimension with Validation Function

```bash
curl -X POST http://localhost:8080/dimension \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "dimension": "promo_code",
    "description": "Promotional code for discounts",
    "position": 4,
    "schema": { "type": "string", "pattern": "^[A-Z]{3,6}[0-9]{2,4}$" },
    "value_validation_function_name": "validate_promo_code",
    "change_reason": "Promo codes need business validation"
  }'
```

### List Dimensions

**GET /dimension**

```bash
curl -X GET http://localhost:8080/dimension \
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
      "schema": { "type": "string", "enum": ["Bangalore", "Delhi"] },
      "dependencies": []
    },
    {
      "dimension": "user_tier",
      "position": 2,
      "schema": { "type": "string", "enum": ["bronze", "silver", "gold"] },
      "dependencies": []
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
    "change_reason": "Added Hyderabad and Chennai to supported cities"
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

## Default Configs

### Create Default Config

**POST /default-config**

```bash
curl -X POST http://localhost:8080/default-config \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "key": "checkout_timeout_seconds",
    "value": { "value": 300 },
    "schema": { "type": "integer", "minimum": 60, "maximum": 900 },
    "change_reason": "Default checkout timeout of 5 minutes"
  }'
```

### Create Default Config with Type Template

```bash
curl -X POST http://localhost:8080/default-config \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "key": "transaction_limit",
    "value": { "value": { "amount": 10000, "currency": "INR" } },
    "schema": { "$ref": "#/types/CurrencyAmount" },
    "change_reason": "Using CurrencyAmount type template"
  }'
```

### Create Default Config with Function Validation

```bash
curl -X POST http://localhost:8080/default-config \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "key": "discount_percentage",
    "value": { "value": 10 },
    "schema": { "type": "number", "minimum": 0, "maximum": 100 },
    "function_name": "validate_discount_rules",
    "change_reason": "Discount needs business rule validation"
  }'
```

### List Default Configs

**GET /default-configs**

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
    "value": { "value": 600 },
    "change_reason": "Increased timeout to 10 minutes"
  }'
```

### Delete Default Config

**DELETE /default-config/{key}**

```bash
curl -X DELETE http://localhost:8080/default-config/old_config_key \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

---

## Contexts & Overrides

### Create Context with Override

**POST /context**

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
    "change_reason": "Higher rates for cabs in Delhi"
  }'
```

### Context Condition Examples

Contexts use simple key-value maps that match against dimension values:

**Simple equality:**
```json
{"city": "Delhi"}
```

**Multiple conditions (AND logic):**
```json
{
  "city": "Delhi",
  "vehicle_type": "cab"
}
```

**Multiple dimension values:**
```json
{
  "city": "Delhi",
  "user_tier": "gold"
}
```

**Context with all dimensions specified:**
```json
{
  "city": "Delhi",
  "vehicle_type": "cab",
  "hour_of_day": 20
}
```

**Note:** For OR logic (e.g., city is Delhi OR Mumbai), create separate contexts for each condition.

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
curl -X GET http://localhost:8080/context/123 \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Update Override

**PATCH /override/{override_id}**

```bash
curl -X PATCH http://localhost:8080/override/456 \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "value": {
      "per_km_rate": 28.0
    },
    "change_reason": "Updated rate after fuel price increase"
  }'
```

### Delete Context

**DELETE /context/{context_id}**

```bash
curl -X DELETE http://localhost:8080/context/123 \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

---

## Functions

Functions are JavaScript code snippets for custom validation and autocomplete.

### Create Validation Function

**POST /function**

```bash
curl -X POST http://localhost:8080/function \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "function_name": "validate_discount_tier",
    "code": "async function validate(key, value) {\n  // Platinum users can get up to 50% discount\n  // Gold users up to 30%\n  // Others up to 20%\n  const tier = context.user_tier || \"bronze\";\n  const maxDiscount = {\n    \"platinum\": 50,\n    \"gold\": 30,\n    \"silver\": 20,\n    \"bronze\": 10\n  };\n  \n  if (key === \"discount_percentage\") {\n    return value <= maxDiscount[tier];\n  }\n  return true;\n}",
    "change_reason": "Validate discount based on user tier"
  }'
```

### Create Autocomplete Function

```bash
curl -X POST http://localhost:8080/function \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "function_name": "autocomplete_city",
    "code": "async function autocomplete(name, prefix, environment) {\n  const cities = [\n    \"Bangalore\", \"Delhi\", \"Mumbai\", \"Chennai\",\n    \"Hyderabad\", \"Kolkata\", \"Pune\", \"Ahmedabad\"\n  ];\n  \n  if (name === \"city\" && prefix.length >= 2) {\n    return cities.filter(city =>\n      city.toLowerCase().startsWith(prefix.toLowerCase())\n    );\n  }\n  return [];\n}",
    "change_reason": "City autocomplete suggestions"
  }'
```

### Test Function

**POST /function/{function_name}/test**

```bash
curl -X POST http://localhost:8080/function/validate_discount_tier/test \
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

### Publish Function

**PATCH /function/{function_name}**

```bash
curl -X PATCH http://localhost:8080/function/validate_discount_tier \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "code": "async function validate(key, value) {\n  const tier = context.user_tier || \"bronze\";\n  const maxDiscount = { \"platinum\": 50, \"gold\": 30, \"silver\": 20, \"bronze\": 10 };\n  if (key === \"discount_percentage\") return value <= maxDiscount[tier];\n  return true;\n}",
    "change_reason": "Publishing for production use"
  }'
```

### List Functions

**GET /function**

```bash
curl -X GET http://localhost:8080/function \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Delete Function

**DELETE /function/{function_name}**

```bash
curl -X DELETE http://localhost:8080/function/old_function \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

---

## Type Templates

### Create Type Template

**POST /types**

```bash
curl -X POST http://localhost:8080/types \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "type_name": "GeoLocation",
    "type_schema": {
      "type": "object",
      "properties": {
        "latitude": { "type": "number", "minimum": -90, "maximum": 90 },
        "longitude": { "type": "number", "minimum": -180, "maximum": 180 }
      },
      "required": ["latitude", "longitude"]
    },
    "change_reason": "Standard geographic coordinate type"
  }'
```

### Built-in Type Templates

| Name | Schema |
|------|--------|
| Number | `{ "type": "integer" }` |
| Decimal | `{ "type": "number" }` |
| Boolean | `{ "type": "boolean" }` |
| Enum | `{ "type": "string", "enum": [...] }` |
| Pattern | `{ "type": "string", "pattern": "..." }` |

### Use Type Template in Config

```bash
curl -X POST http://localhost:8080/default-config \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "key": "store_location",
    "value": { "value": { "latitude": 12.9716, "longitude": 77.5946 } },
    "schema": { "$ref": "#/types/GeoLocation" },
    "change_reason": "Store location using GeoLocation type"
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
curl -X GET http://localhost:8080/types/GeoLocation \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Delete Type Template

**DELETE /types/{type_name}**

```bash
curl -X DELETE http://localhost:8080/types/OldType \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

---

## Configuration Resolution

### Get Resolved Config

**POST /config**

Returns the fully resolved configuration for a given context.

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

### Get Config with Prefix

**POST /config?prefix=checkout_**

```bash
curl -X POST "http://localhost:8080/config?prefix=checkout_" \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "context": {
      "city": "Delhi"
    }
  }'
```

### Get Config Version

**GET /config/versions**

```bash
curl -X GET http://localhost:8080/config/versions \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
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

---

## Bulk Operations

### Import TOML Configuration

**PUT /config**

```bash
curl -X PUT http://localhost:8080/config \
  -H "Content-Type: application/toml" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  --data-binary @config.toml
```

### Export TOML Configuration

**GET /config/toml**

```bash
curl -X GET http://localhost:8080/config/toml \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Export JSON Configuration

**GET /config/json**

```bash
curl -X GET http://localhost:8080/config/json \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```
