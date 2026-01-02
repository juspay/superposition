# TOML Response Format for get_config API

**Date:** 2026-01-02
**Status:** Design Complete
**Author:** Claude Sonnet 4.5

## Overview

This design document outlines the addition of TOML response format support to the `get_config` API endpoint. The feature enables clients to request configuration in TOML format through HTTP content negotiation, providing round-trip compatibility with TOML configuration files.

## Background

The superposition system currently supports TOML parsing for configuration input through `parse_toml_config()`. The `get_config` API endpoint returns configurations exclusively in JSON format. Adding TOML response support provides:

1. **Round-trip compatibility**: Clients can receive configs in the same format they submit
2. **Human readability**: TOML is often more readable than JSON for configuration
3. **Tooling integration**: Config management tools that work with TOML can consume API responses directly

## Goals

1. Add TOML serialization capability to mirror existing parsing
2. Implement content negotiation in `get_config` API handler
3. Maintain backwards compatibility with existing JSON clients
4. Provide round-trip compatibility (parse → API → serialize → parse)
5. Follow HTTP standards for content negotiation

## Non-Goals

- Supporting TOML format for other API endpoints
- Adding TOML input support to endpoints (already exists via parsing)
- Supporting other formats (XML, YAML, etc.) - but design should allow future additions
- Modifying the Config structure or database schema

---

## Architecture

### High-Level Design

```
┌─────────────────────────────────────────────────────────┐
│  Client Request                                         │
│  GET /config                                            │
│  Accept: application/toml                               │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│  API Handler (handlers.rs)                              │
│  - Parse Accept header                                  │
│  - Fetch Config from database                           │
│  - Determine response format                            │
└─────────────────────────────────────────────────────────┘
                          ↓
                    ┌─────┴──────┐
                    │            │
              ┌─────▼─────┐  ┌──▼──────┐
              │   TOML    │  │  JSON   │
              │ Serialize │  │ Serialize│
              └─────┬─────┘  └──┬──────┘
                    │            │
                    └─────┬──────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│  HTTP Response                                          │
│  Content-Type: application/toml                         │
│  + custom headers (x-config-version, etc.)              │
└─────────────────────────────────────────────────────────┘
```

### Components

**1. Content Negotiation (API Layer)**
- **Location:** `crates/context_aware_config/src/api/config/handlers.rs`
- **Responsibility:** Parse Accept header and route to appropriate serializer
- **Integration Point:** Modify existing `get_config()` handler

**2. TOML Serialization (Core Layer)**
- **Location:** `crates/superposition_core/src/toml_parser.rs`
- **Responsibility:** Convert Config structure to TOML string
- **New Function:** `serialize_to_toml(config: &Config) -> Result<String, TomlError>`

**3. Response Building (API Layer)**
- **Location:** `crates/context_aware_config/src/api/config/handlers.rs`
- **Responsibility:** Set appropriate Content-Type and headers
- **Integration:** Extend existing response building logic

---

## Detailed Design

### 1. TOML Serialization Module

**Location:** `crates/superposition_core/src/toml_parser.rs`

#### Function Signature

```rust
/// Serialize Config structure to TOML format
///
/// Converts a Config object back to TOML string format matching the input specification.
/// The output can be parsed by `parse()` to recreate an equivalent Config.
///
/// # Arguments
/// * `config` - The Config structure to serialize
///
/// # Returns
/// * `Ok(String)` - TOML formatted string
/// * `Err(TomlError)` - Serialization error
///
/// # Example
/// ```rust
/// let config = Config { /* ... */ };
/// let toml_string = serialize_to_toml(&config)?;
/// println!("{}", toml_string);
/// ```
pub fn serialize_to_toml(config: &Config) -> Result<String, TomlError>
```

#### Output Format

The serialized TOML matches the input format exactly:

```toml
[default-config]
per_km_rate = { value = 20.0, schema = { "type" = "number" } }
surge_factor = { value = 0.0, schema = { "type" = "number" } }

[dimensions]
city = { position = 1, schema = { "type" = "string", "enum" = ["Bangalore", "Delhi"] } }
vehicle_type = { position = 2, schema = { "type" = "string", "enum" = ["auto", "cab", "bike"] } }

[context."vehicle_type=cab"]
per_km_rate = 25.0

[context."city=Bangalore; vehicle_type=cab"]
per_km_rate = 22.0
```

#### Implementation Algorithm

```rust
pub fn serialize_to_toml(config: &Config) -> Result<String, TomlError> {
    let mut output = String::new();

    // 1. Serialize [default-config] section
    output.push_str("[default-config]\n");
    for (key, value) in &config.default_configs.0 {
        // Get schema from dimensions if available, or infer
        let schema = get_schema_for_key(key, &config.dimensions);
        let toml_entry = format!(
            "{} = {{ value = {}, schema = {} }}\n",
            key,
            value_to_toml(value),
            schema_to_toml(&schema)
        );
        output.push_str(&toml_entry);
    }
    output.push('\n');

    // 2. Serialize [dimensions] section
    output.push_str("[dimensions]\n");
    for (name, info) in &config.dimensions {
        let toml_entry = format!(
            "{} = {{ position = {}, schema = {} }}\n",
            name,
            info.position,
            schema_to_toml(&info.schema)
        );
        output.push_str(&toml_entry);
    }
    output.push('\n');

    // 3. Serialize [context.*] sections
    for context in &config.contexts {
        // Convert condition to string format
        let condition_str = condition_to_string(&context.condition)?;

        output.push_str(&format!("[context.\"{}\"]\n", condition_str));

        // Get overrides for this context
        if let Some(overrides) = config.overrides.get(&context.id) {
            for (key, value) in &overrides.0 {
                output.push_str(&format!(
                    "{} = {}\n",
                    key,
                    value_to_toml(value)
                ));
            }
        }
        output.push('\n');
    }

    Ok(output)
}
```

#### Helper Functions

```rust
/// Convert serde_json::Value to TOML representation
fn value_to_toml(value: &Value) -> String {
    match value {
        Value::String(s) => format!("\"{}\"", s),
        Value::Number(n) => n.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Array(arr) => {
            let items: Vec<String> = arr.iter()
                .map(|v| value_to_toml(v))
                .collect();
            format!("[{}]", items.join(", "))
        }
        Value::Object(obj) => {
            let items: Vec<String> = obj.iter()
                .map(|(k, v)| format!("{} = {}", k, value_to_toml(v)))
                .collect();
            format!("{{ {} }}", items.join(", "))
        }
        Value::Null => "null".to_string(),
    }
}

/// Convert ExtendedMap schema to TOML representation
fn schema_to_toml(schema: &ExtendedMap) -> String {
    // Schema is already a JSON-like structure
    value_to_toml(&serde_json::to_value(schema).unwrap())
}

/// Convert Condition to context expression string
fn condition_to_string(condition: &Cac<Condition>) -> Result<String, TomlError> {
    // Extract dimension key-value pairs
    let pairs: Vec<String> = condition.0.iter()
        .map(|(key, value)| {
            format!("{}={}", key, value_to_string_simple(value))
        })
        .collect();

    Ok(pairs.join("; "))
}

/// Simple value to string for context expressions
fn value_to_string_simple(value: &Value) -> String {
    match value {
        Value::String(s) => s.clone(),
        Value::Number(n) => n.to_string(),
        Value::Bool(b) => b.to_string(),
        _ => value.to_string(),
    }
}

/// Get schema for a config key from dimensions
fn get_schema_for_key(
    key: &str,
    dimensions: &HashMap<String, DimensionInfo>
) -> ExtendedMap {
    // Try to find schema from dimensions
    // If not found, infer from value or use default
    // This handles cases where schema info isn't in DB
    ExtendedMap::default() // Simplified for design doc
}
```

#### Error Handling

```rust
/// Rename existing TomlParseError to TomlError and add serialization variants
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TomlError {
    // Existing parse errors
    FileReadError(String),
    TomlSyntaxError(String),
    MissingSection(String),
    MissingField { section: String, key: String, field: String },
    InvalidContextExpression { expression: String, reason: String },
    UndeclaredDimension { dimension: String, context: String },
    InvalidOverrideKey { key: String, context: String },
    ConversionError(String),
    DuplicatePosition { position: i32, dimensions: Vec<String> },

    // New serialization errors
    SerializationError(String),
    InvalidContextCondition(String),
}

impl Display for TomlError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::SerializationError(msg) =>
                write!(f, "TOML serialization error: {}", msg),
            Self::InvalidContextCondition(cond) =>
                write!(f, "Cannot serialize context condition: {}", cond),
            // ... existing variants
        }
    }
}
```

---

### 2. Content Negotiation Implementation

**Location:** `crates/context_aware_config/src/api/config/handlers.rs`

#### Accept Header Parsing

```rust
/// Supported response formats for get_config
#[derive(Debug, Clone, Copy, PartialEq)]
enum ResponseFormat {
    Json,
    Toml,
}

/// Determine response format from Accept header
///
/// Implements content negotiation following HTTP standards:
/// - application/toml → TOML format
/// - application/json → JSON format
/// - */* or no header → JSON (default for backwards compatibility)
fn determine_response_format(req: &HttpRequest) -> ResponseFormat {
    let accept_header = req.headers()
        .get(actix_web::http::header::ACCEPT)
        .and_then(|h| h.to_str().ok())
        .unwrap_or("*/*");

    // Simple prefix matching for content types
    // Supports patterns like:
    // - "application/toml"
    // - "application/toml, application/json;q=0.9"
    // - "*/*"
    if accept_header.contains("application/toml") {
        ResponseFormat::Toml
    } else if accept_header.contains("application/json") {
        ResponseFormat::Json
    } else {
        // Default to JSON for backwards compatibility
        // Handles: */* , text/*, or no Accept header
        ResponseFormat::Json
    }
}
```

#### Modified Handler

```rust
#[routes]
#[get("")]
#[post("")]
async fn get_config(
    req: HttpRequest,
    body: Option<Json<ContextPayload>>,
    db_conn: DbConnection,
    dimension_params: DimensionQuery<QueryMap>,
    query_filters: superposition_query::Query<ConfigQuery>,
    workspace_context: WorkspaceContext,
) -> superposition::Result<HttpResponse> {
    // ... existing logic to fetch config (unchanged) ...
    let config: Config = /* database fetch logic */;
    let max_created_at = /* timestamp logic */;
    let version = /* version logic */;

    // Determine response format
    let format = determine_response_format(&req);

    // Build response headers (common to both formats)
    let mut response = HttpResponse::Ok();
    add_last_modified_to_header(max_created_at, is_smithy, &mut response);
    add_audit_id_to_header(&mut conn, &mut response, &workspace_context.schema_name);
    add_config_version_to_header(&version, &mut response);

    // Serialize and return based on format
    match format {
        ResponseFormat::Toml => {
            let toml_string = superposition_core::serialize_to_toml(&config)
                .map_err(|e| {
                    log::error!(
                        "TOML serialization failed for workspace {}: {}",
                        workspace_context.schema_name,
                        e
                    );
                    superposition::AppError::InternalServerError
                })?;

            Ok(response
                .content_type("application/toml")
                .body(toml_string))
        },
        ResponseFormat::Json => {
            // Existing JSON response (unchanged)
            Ok(response.json(config))
        }
    }
}
```

#### Response Headers

**TOML Response:**
```
HTTP/1.1 200 OK
Content-Type: application/toml
x-config-version: 123
x-audit-id: 550e8400-e29b-41d4-a716-446655440000
Last-Modified: Thu, 02 Jan 2026 10:30:00 GMT

[default-config]
...
```

**JSON Response (unchanged):**
```
HTTP/1.1 200 OK
Content-Type: application/json
x-config-version: 123
x-audit-id: 550e8400-e29b-41d4-a716-446655440000
Last-Modified: Thu, 02 Jan 2026 10:30:00 GMT

{
  "contexts": [...],
  ...
}
```

---

### 3. Error Handling & Edge Cases

#### Serialization Errors

**Scenario:** Config structure cannot be serialized to valid TOML

**Response:**
```
HTTP/1.1 500 Internal Server Error
Content-Type: application/json

{
  "error": "Internal server error"
}
```

**Logging:**
```rust
log::error!(
    "TOML serialization failed for workspace {}: {}",
    workspace_context.schema_name,
    error
);
```

**Rationale:**
- Serialization failure is a genuine server error, not a negotiation failure
- Indicates a bug in serialization logic that needs fixing
- Same Config that serializes to JSON should serialize to TOML

#### Missing Schema Information

**Scenario:** Config has values but no corresponding schema metadata

**Solution:** Use type inference or default schema

```rust
fn get_schema_for_key(
    key: &str,
    dimensions: &HashMap<String, DimensionInfo>
) -> ExtendedMap {
    // Try to find in dimensions
    for dim_info in dimensions.values() {
        if let Some(schema) = dim_info.schema.get(key) {
            return schema.clone();
        }
    }

    // Fallback: use generic schema
    ExtendedMap::from([
        ("type".to_string(), "any".into())
    ])
}
```

#### Complex Context Conditions

**Scenario:** Context conditions that can't be represented as "key=value" pairs

**Current Handling:** The Config structure stores conditions as `Cac<Condition>` which is a map of dimension name to value. This naturally maps to "key=value" format.

**Edge Case:** If future versions support complex conditions (AND/OR logic, ranges, etc.)

**Future Solution:**
```toml
# Simple condition (current)
[context."city=Bangalore"]

# Complex condition (future - if needed)
[context.'{"$and": [{"city": "Bangalore"}, {"region": "South"}]}']
```

#### Special Characters in Values

**Handling:** TOML quoted keys handle special characters

```toml
[context."city=San Francisco; state=CA"]
per_km_rate = 30.0

[context."name=O'Brien"]
enabled = true
```

**Implementation:** Ensure proper escaping in `condition_to_string()`

```rust
fn condition_to_string(condition: &Cac<Condition>) -> Result<String, TomlError> {
    let pairs: Vec<String> = condition.0.iter()
        .map(|(key, value)| {
            let value_str = value_to_string_simple(value);
            // Escape special characters if needed
            format!("{}={}", escape_toml_key(key), escape_toml_value(&value_str))
        })
        .collect();

    Ok(pairs.join("; "))
}
```

#### Empty Sections

**Behavior:** Serialize all sections even if empty

```toml
[default-config]
# Empty but present

[dimensions]
# Empty but present

# No context sections if none exist
```

**Rationale:** Matches input format specification and makes structure clear

#### Large Configurations

**Current Approach:** No special handling (same as JSON)

**Future Considerations:**
- Add size warnings in logs if TOML exceeds threshold
- Consider pagination or streaming for very large configs
- Not a problem for initial implementation

---

## Testing Strategy

### Unit Tests

**Location:** `crates/superposition_core/src/toml_parser.rs`

#### Test: Round-trip Compatibility

```rust
#[cfg(test)]
mod serialization_tests {
    use super::*;

    #[test]
    fn test_toml_round_trip_simple() {
        let original_toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }
            enabled = { value = true, schema = { type = "boolean" } }

            [dimensions]
            os = { position = 1, schema = { "type" = "string", "enum" = ["linux", "windows"] } }

            [context."os=linux"]
            timeout = 60
        "#;

        // Parse TOML → Config
        let config = parse(original_toml).unwrap();

        // Serialize Config → TOML
        let serialized = serialize_to_toml(&config).unwrap();

        // Parse again
        let reparsed = parse(&serialized).unwrap();

        // Configs should be functionally equivalent
        assert_eq!(config.default_configs, reparsed.default_configs);
        assert_eq!(config.dimensions.len(), reparsed.dimensions.len());
        assert_eq!(config.contexts.len(), reparsed.contexts.len());
    }

    #[test]
    fn test_toml_round_trip_complex() {
        let toml = include_str!("../../tests/fixtures/complex_config.toml");
        let config = parse(toml).unwrap();
        let serialized = serialize_to_toml(&config).unwrap();
        let reparsed = parse(&serialized).unwrap();
        assert_eq!(config, reparsed);
    }

    #[test]
    fn test_serialize_empty_config() {
        let config = Config {
            default_configs: Overrides(Map::new()),
            dimensions: HashMap::new(),
            contexts: Vec::new(),
            overrides: HashMap::new(),
        };

        let result = serialize_to_toml(&config);
        assert!(result.is_ok());

        let toml = result.unwrap();
        assert!(toml.contains("[default-config]"));
        assert!(toml.contains("[dimensions]"));
    }

    #[test]
    fn test_serialize_special_characters() {
        // Test context with spaces, quotes, semicolons
        let config = /* build config with special chars */;
        let toml = serialize_to_toml(&config).unwrap();

        // Should be valid TOML
        assert!(toml::from_str::<toml::Value>(&toml).is_ok());

        // Should round-trip
        let reparsed = parse(&toml).unwrap();
        assert_eq!(config, reparsed);
    }

    #[test]
    fn test_serialize_all_value_types() {
        // Test integer, float, boolean, string, array, object
        let config = /* build config with all types */;
        let toml = serialize_to_toml(&config).unwrap();
        let reparsed = parse(&toml).unwrap();
        assert_eq!(config, reparsed);
    }
}
```

### Integration Tests

**Location:** `crates/context_aware_config/tests/config_api_tests.rs`

#### Test: Accept Header Negotiation

```rust
#[cfg(test)]
mod api_tests {
    use super::*;
    use actix_web::{test, App};

    #[actix_web::test]
    async fn test_get_config_with_toml_accept_header() {
        let app = test::init_service(
            App::new().service(config::endpoints())
        ).await;

        let req = test::TestRequest::get()
            .uri("/config")
            .insert_header(("Accept", "application/toml"))
            .to_request();

        let resp = test::call_service(&app, req).await;

        assert_eq!(resp.status(), 200);
        assert_eq!(
            resp.headers()
                .get("content-type")
                .unwrap()
                .to_str()
                .unwrap(),
            "application/toml"
        );

        let body = test::read_body(resp).await;
        let toml_str = std::str::from_utf8(&body).unwrap();

        // Verify valid TOML
        assert!(toml::from_str::<toml::Value>(toml_str).is_ok());

        // Verify structure
        assert!(toml_str.contains("[default-config]"));
        assert!(toml_str.contains("[dimensions]"));
    }

    #[actix_web::test]
    async fn test_get_config_with_json_accept_header() {
        let app = test::init_service(
            App::new().service(config::endpoints())
        ).await;

        let req = test::TestRequest::get()
            .uri("/config")
            .insert_header(("Accept", "application/json"))
            .to_request();

        let resp = test::call_service(&app, req).await;

        assert_eq!(resp.status(), 200);
        assert!(resp.headers()
            .get("content-type")
            .unwrap()
            .to_str()
            .unwrap()
            .contains("application/json"));
    }

    #[actix_web::test]
    async fn test_get_config_with_wildcard_accept() {
        let app = test::init_service(
            App::new().service(config::endpoints())
        ).await;

        let req = test::TestRequest::get()
            .uri("/config")
            .insert_header(("Accept", "*/*"))
            .to_request();

        let resp = test::call_service(&app, req).await;

        assert_eq!(resp.status(), 200);
        // Should default to JSON
        assert!(resp.headers()
            .get("content-type")
            .unwrap()
            .to_str()
            .unwrap()
            .contains("application/json"));
    }

    #[actix_web::test]
    async fn test_get_config_without_accept_header() {
        // Backwards compatibility test
        let app = test::init_service(
            App::new().service(config::endpoints())
        ).await;

        let req = test::TestRequest::get()
            .uri("/config")
            .to_request();

        let resp = test::call_service(&app, req).await;

        assert_eq!(resp.status(), 200);
        // Should default to JSON
        assert!(resp.headers()
            .get("content-type")
            .unwrap()
            .to_str()
            .unwrap()
            .contains("application/json"));
    }

    #[actix_web::test]
    async fn test_toml_response_includes_custom_headers() {
        let app = test::init_service(
            App::new().service(config::endpoints())
        ).await;

        let req = test::TestRequest::get()
            .uri("/config")
            .insert_header(("Accept", "application/toml"))
            .to_request();

        let resp = test::call_service(&app, req).await;

        // Verify custom headers present
        assert!(resp.headers().get("x-config-version").is_some());
        assert!(resp.headers().get("x-audit-id").is_some());
        assert!(resp.headers().get("last-modified").is_some());
    }

    #[actix_web::test]
    async fn test_toml_response_with_post_request() {
        // Smithy clients use POST
        let app = test::init_service(
            App::new().service(config::endpoints())
        ).await;

        let req = test::TestRequest::post()
            .uri("/config")
            .insert_header(("Accept", "application/toml"))
            .set_json(&json!({
                "context": {"os": "linux"}
            }))
            .to_request();

        let resp = test::call_service(&app, req).await;

        assert_eq!(resp.status(), 200);
        assert_eq!(
            resp.headers()
                .get("content-type")
                .unwrap()
                .to_str()
                .unwrap(),
            "application/toml"
        );
    }
}
```

### Manual Testing Checklist

- [ ] Test with `curl` using Accept header
- [ ] Test with Postman/Insomnia
- [ ] Verify large config performance
- [ ] Test with existing client applications (ensure JSON still works)
- [ ] Test with different dimension combinations
- [ ] Verify TOML output can be used as input (round-trip)

```bash
# Example curl commands
curl -H "Accept: application/toml" http://localhost:8080/config
curl -H "Accept: application/json" http://localhost:8080/config
curl http://localhost:8080/config  # Should default to JSON
```

---

## Implementation Plan

### Phase 1: TOML Serialization Core

**Files to modify:**
- `crates/superposition_core/src/toml_parser.rs`
- `crates/superposition_core/src/lib.rs`

**Tasks:**
1. Rename `TomlParseError` to `TomlError`
2. Add `SerializationError` and `InvalidContextCondition` variants
3. Implement `serialize_to_toml()` function
4. Implement helper functions:
   - `value_to_toml()`
   - `schema_to_toml()`
   - `condition_to_string()`
   - `get_schema_for_key()`
5. Add unit tests for serialization
6. Add round-trip tests

**Validation:** All unit tests pass, no compilation errors

### Phase 2: API Content Negotiation

**Files to modify:**
- `crates/context_aware_config/src/api/config/handlers.rs`

**Tasks:**
1. Add `ResponseFormat` enum
2. Implement `determine_response_format()` function
3. Modify `get_config()` handler to use content negotiation
4. Add error handling for serialization failures
5. Update imports to include `serialize_to_toml`

**Validation:** Code compiles, existing JSON tests still pass

### Phase 3: Integration Testing

**Files to create/modify:**
- `crates/context_aware_config/tests/config_api_tests.rs`

**Tasks:**
1. Add Accept header negotiation tests
2. Add backwards compatibility tests
3. Add custom headers verification tests
4. Test POST requests with TOML Accept header
5. Manual testing with curl/Postman

**Validation:** All integration tests pass

### Phase 4: Documentation

**Files to update:**
- API documentation (if exists)
- README or usage guide
- This design document (mark as Implemented)

**Tasks:**
1. Document Accept header usage in API docs
2. Add example requests/responses
3. Update CHANGELOG
4. Add migration notes for API consumers

**Validation:** Documentation reviewed and approved

---

## File Changes Summary

### New Files
None (all additions to existing files)

### Modified Files

**`crates/superposition_core/src/toml_parser.rs`** (~300 lines added)
- Rename `TomlParseError` → `TomlError`
- Add serialization error variants
- Implement `serialize_to_toml()` function
- Implement helper functions
- Add comprehensive tests

**`crates/superposition_core/src/lib.rs`** (~5 lines modified)
- Export `serialize_to_toml` function
- Update error type export

**`crates/context_aware_config/src/api/config/handlers.rs`** (~50 lines modified)
- Add `ResponseFormat` enum
- Add `determine_response_format()` function
- Modify `get_config()` handler
- Add error handling for TOML serialization

**`crates/context_aware_config/tests/config_api_tests.rs`** (~150 lines added)
- Add Accept header tests
- Add backwards compatibility tests
- Add error scenario tests

---

## API Usage Examples

### Request TOML Response

```bash
curl -H "Accept: application/toml" \
     http://localhost:8080/config?city=Bangalore
```

**Response:**
```toml
HTTP/1.1 200 OK
Content-Type: application/toml
x-config-version: 123
x-audit-id: 550e8400-e29b-41d4-a716-446655440000
Last-Modified: Thu, 02 Jan 2026 10:30:00 GMT

[default-config]
per_km_rate = { value = 20.0, schema = { "type" = "number" } }
surge_factor = { value = 0.0, schema = { "type" = "number" } }

[dimensions]
city = { position = 1, schema = { "type" = "string", "enum" = ["Bangalore", "Delhi"] } }
vehicle_type = { position = 2, schema = { "type" = "string", "enum" = ["auto", "cab", "bike"] } }

[context."city=Bangalore"]
per_km_rate = 22.0
```

### Request JSON Response (Default)

```bash
curl http://localhost:8080/config?city=Bangalore
# OR
curl -H "Accept: application/json" \
     http://localhost:8080/config?city=Bangalore
```

**Response:**
```json
HTTP/1.1 200 OK
Content-Type: application/json
x-config-version: 123
x-audit-id: 550e8400-e29b-41d4-a716-446655440000
Last-Modified: Thu, 02 Jan 2026 10:30:00 GMT

{
  "contexts": [...],
  "overrides": {...},
  "default_configs": {...},
  "dimensions": {...}
}
```

### POST Request with TOML Response

```bash
curl -X POST \
     -H "Accept: application/toml" \
     -H "Content-Type: application/json" \
     -d '{"context": {"city": "Bangalore"}}' \
     http://localhost:8080/config
```

---

## Backwards Compatibility

### Guaranteed Behaviors

1. **No Accept Header** → JSON response (existing behavior)
2. **Accept: `*/*`** → JSON response (backwards compatible)
3. **Accept: application/json** → JSON response (explicit)
4. **Existing client code** → No changes required
5. **Response structure** → Unchanged for JSON
6. **Custom headers** → Present in both formats
7. **HTTP status codes** → Unchanged

### Migration Path

**For API consumers who want TOML:**
1. Add `Accept: application/toml` header to requests
2. Parse response as TOML instead of JSON
3. Handle potential 500 errors (if serialization fails)

**For existing clients:**
- No changes required
- Will continue to receive JSON responses

---

## Security Considerations

1. **Input Validation**
   - TOML serialization uses same validated Config structure as JSON
   - No new attack surface

2. **Error Information Leakage**
   - Serialization errors logged server-side
   - Client receives generic 500 error
   - No sensitive data in TOML output (same as JSON)

3. **Resource Consumption**
   - TOML serialization comparable to JSON
   - No recursive structures (Config is flat)
   - Same size limits as JSON responses

4. **Content Type Confusion**
   - Content-Type header correctly set
   - Browsers/tools will handle appropriately

---

## Performance Considerations

### Serialization Performance

**Expected:** TOML serialization slightly slower than JSON
- JSON: native serde support, highly optimized
- TOML: custom string building logic

**Mitigation:**
- Most configs are small (< 100KB)
- Serialization is not in critical path (network I/O dominates)
- Can add caching if needed in future

### Benchmarking Plan

```rust
#[bench]
fn bench_json_serialization(b: &mut Bencher) {
    let config = /* large config */;
    b.iter(|| {
        serde_json::to_string(&config).unwrap()
    });
}

#[bench]
fn bench_toml_serialization(b: &mut Bencher) {
    let config = /* large config */;
    b.iter(|| {
        serialize_to_toml(&config).unwrap()
    });
}
```

**Acceptance Criteria:** TOML serialization < 5x slower than JSON

---

## Future Enhancements

Potential improvements (not in current scope):

1. **Quality-based Content Negotiation**
   - Parse quality values: `Accept: application/toml;q=0.9, application/json;q=0.8`
   - Return highest-quality format available

2. **Additional Formats**
   - YAML: `Accept: application/yaml`
   - XML: `Accept: application/xml`
   - Uses same content negotiation infrastructure

3. **Compression Support**
   - `Accept-Encoding: gzip`
   - Compress TOML/JSON responses

4. **TOML Serialization Caching**
   - Cache serialized TOML strings
   - Invalidate on config changes
   - Reduce CPU usage for repeated requests

5. **Streaming Serialization**
   - For very large configs (> 10MB)
   - Stream TOML output instead of building full string

6. **Schema-only Responses**
   - `Accept: application/toml+schema`
   - Return only schema information in TOML

---

## Success Criteria

The implementation is complete when:

1. ✅ `serialize_to_toml()` function implemented and tested
2. ✅ Content negotiation working in `get_config` handler
3. ✅ All unit tests pass (serialization round-trip)
4. ✅ All integration tests pass (Accept header handling)
5. ✅ Backwards compatibility verified (existing clients work)
6. ✅ Manual testing completed (curl, Postman)
7. ✅ Documentation updated (API docs, examples)
8. ✅ No performance regression for JSON responses
9. ✅ Code review completed
10. ✅ Design document updated (mark as Implemented)

---

## References

- **TOML Specification:** https://toml.io/
- **HTTP Content Negotiation:** https://developer.mozilla.org/en-US/docs/Web/HTTP/Content_negotiation
- **Actix-Web Documentation:** https://actix.rs/docs/
- **Existing TOML Parser Design:** `design-docs/2025-12-21-toml-parsing-ffi-design.md`

---

**End of Design Document**
