use serde_json::Value;
use superposition_core::{parse_json_config, parse_toml_config, Config};

/// Test that TOML and JSON produce equivalent Configs for the same logical configuration
#[test]
fn test_toml_json_equivalence() {
    let toml_input = r#"
[default-configs]
timeout = { value = 30, schema = { type = "integer" } }
enabled = { value = true, schema = { type = "boolean" } }

[dimensions]
os = { position = 1, schema = { type = "string" } }
region = { position = 2, schema = { type = "string" } }

[[overrides]]
_context_ = { os = "linux" }
timeout = 60

[[overrides]]
_context_ = { os = "linux", region = "us-east" }
timeout = 90
enabled = false
"#;

    let json_input = r#"{
  "default-configs": {
    "timeout": { "value": 30, "schema": { "type": "integer" } },
    "enabled": { "value": true, "schema": { "type": "boolean" } }
  },
  "dimensions": {
    "os": { "position": 1, "schema": { "type": "string" } },
    "region": { "position": 2, "schema": { "type": "string" } }
  },
  "overrides": [
    {
      "_context_": { "os": "linux" },
      "timeout": 60
    },
    {
      "_context_": { "os": "linux", "region": "us-east" },
      "timeout": 90,
      "enabled": false
    }
  ]
}"#;

    let toml_config = parse_toml_config(toml_input).expect("TOML should parse");
    let json_config = parse_json_config(json_input).expect("JSON should parse");

    // Both should produce equivalent configs
    assert_eq!(
        toml_config.default_configs, json_config.default_configs,
        "default_configs should be equal"
    );
    assert_eq!(
        toml_config.dimensions.len(),
        json_config.dimensions.len(),
        "dimension count should be equal"
    );
    assert_eq!(
        toml_config.contexts.len(),
        json_config.contexts.len(),
        "context count should be equal"
    );
    assert_eq!(
        toml_config.overrides.len(),
        json_config.overrides.len(),
        "override count should be equal"
    );
}

/// Test that validation works the same for both formats
#[test]
fn test_validation_equivalence_undeclared_dimension() {
    let toml = r#"
[default-configs]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os = { position = 1, schema = { type = "string" } }

[[overrides]]
_context_ = { unknown_dim = "value" }
timeout = 60
"#;

    let json = r#"{
  "default-configs": {
    "timeout": { "value": 30, "schema": { "type": "integer" } }
  },
  "dimensions": {
    "os": { "position": 1, "schema": { "type": "string" } }
  },
  "overrides": [
    {
      "_context_": { "unknown_dim": "value" },
      "timeout": 60
    }
  ]
}"#;

    let toml_result = parse_toml_config(toml);
    let json_result = parse_json_config(json);

    assert!(toml_result.is_err(), "TOML should fail validation");
    assert!(json_result.is_err(), "JSON should fail validation");

    // Both should have error messages about the unknown dimension
    let toml_err = toml_result.unwrap_err().to_string();
    let json_err = json_result.unwrap_err().to_string();

    assert!(
        toml_err.to_lowercase().contains("undeclared")
            || toml_err.to_lowercase().contains("not found"),
        "TOML error should mention unknown dimension: {}",
        toml_err
    );
    assert!(
        json_err.to_lowercase().contains("undeclared")
            || json_err.to_lowercase().contains("not found"),
        "JSON error should mention unknown dimension: {}",
        json_err
    );
}

/// Test complex configuration with cohort dimensions
#[test]
fn test_complex_config_with_cohorts() {
    let json = r#"{
  "default-configs": {
    "config": { 
      "value": { "host": "localhost", "port": 8080 }, 
      "schema": { "type": "object" } 
    },
    "max_count": { 
      "value": 10, 
      "schema": { "type": "number", "minimum": 0, "maximum": 100 } 
    }
  },
  "dimensions": {
    "os": { 
      "position": 2, 
      "schema": { "type": "string", "enum": ["linux", "windows", "macos"] } 
    },
    "os_cohort": { 
      "position": 1, 
      "schema": { 
        "type": "string", 
        "enum": ["unix", "otherwise"],
        "definitions": { 
          "unix": { "in": [{ "var": "os" }, ["linux", "macos"]] } 
        } 
      },
      "type": "LOCAL_COHORT:os"
    }
  },
  "overrides": [
    {
      "_context_": { "os": "linux" },
      "config": { "host": "prod.example.com", "port": 443 }
    },
    {
      "_context_": { "os_cohort": "unix" },
      "config": { "host": "prod.unix.com", "port": 8443 },
      "max_count": 95
    }
  ]
}"#;

    let config = parse_json_config(json).expect("Complex JSON should parse");

    assert_eq!(config.default_configs.len(), 2);
    assert_eq!(config.dimensions.len(), 2);
    assert_eq!(config.contexts.len(), 2);

    // Verify cohort dimension was parsed correctly
    let os_cohort = config
        .dimensions
        .get("os_cohort")
        .expect("os_cohort should exist");
    assert_eq!(os_cohort.position, 1);
}

/// Test priority calculation is consistent
#[test]
fn test_priority_calculation_json() {
    let json = r#"{
  "default-configs": {
    "timeout": { "value": 30, "schema": { "type": "integer" } }
  },
  "dimensions": {
    "os": { "position": 1, "schema": { "type": "string" } },
    "region": { "position": 2, "schema": { "type": "string" } }
  },
  "overrides": [
    {
      "_context_": { "os": "linux" },
      "timeout": 60
    },
    {
      "_context_": { "os": "linux", "region": "us-east" },
      "timeout": 90
    }
  ]
}"#;

    let config = parse_json_config(json).unwrap();

    // Print actual priorities for debugging
    println!(
        "Context priorities: {:?}",
        config
            .contexts
            .iter()
            .map(|c| c.priority)
            .collect::<Vec<_>>()
    );

    // Contexts are sorted by priority, but JSON doesn't sort them by default
    // The order should be preserved from the input
    assert_eq!(config.contexts.len(), 2, "Should have 2 contexts");
    // First context has lower priority (fewer dimensions), second has higher
    assert!(
        config.contexts[0].priority < config.contexts[1].priority,
        "Second context should have higher priority"
    );
}

/// Test that JSON round-trip preserves configuration
#[test]
fn test_json_round_trip_preserve_config() {
    use std::collections::BTreeMap;
    use superposition_core::serialize_to_json;
    use superposition_types::{DefaultConfigInfo, DefaultConfigsWithSchema};

    let json = r#"{
  "default-configs": {
    "feature_flag": { 
      "value": true, 
      "schema": { "type": "boolean" } 
    },
    "api_timeout": { 
      "value": 5000, 
      "schema": { "type": "integer", "minimum": 1000, "maximum": 10000 } 
    }
  },
  "dimensions": {
    "environment": { 
      "position": 1, 
      "schema": { "type": "string", "enum": ["dev", "staging", "prod"] } 
    }
  },
  "overrides": [
    {
      "_context_": { "environment": "prod" },
      "api_timeout": 3000
    }
  ]
}"#;

    // Parse the JSON
    let config = parse_json_config(json).expect("Should parse JSON");

    // Helper to create DetailedConfig from Config
    let default_configs: BTreeMap<String, DefaultConfigInfo> = config
        .default_configs
        .iter()
        .map(|(key, value)| {
            let schema = match value {
                Value::String(_) => serde_json::json!({ "type": "string" }),
                Value::Number(n) => {
                    if n.is_i64() {
                        serde_json::json!({ "type": "integer" })
                    } else {
                        serde_json::json!({ "type": "number" })
                    }
                }
                Value::Bool(_) => serde_json::json!({ "type": "boolean" }),
                Value::Array(_) => serde_json::json!({ "type": "array" }),
                Value::Object(_) => serde_json::json!({ "type": "object" }),
                Value::Null => serde_json::json!({ "type": "null" }),
            };
            (
                key.clone(),
                DefaultConfigInfo {
                    value: value.clone(),
                    schema,
                },
            )
        })
        .collect();

    let detailed_config = superposition_types::DetailedConfig {
        contexts: config.contexts.clone(),
        overrides: config.overrides.clone(),
        default_configs: DefaultConfigsWithSchema::from(default_configs),
        dimensions: config.dimensions.clone(),
    };

    // Serialize back to JSON
    let serialized = serialize_to_json(detailed_config).expect("Should serialize");

    // Parse again
    let reparsed = parse_json_config(&serialized).expect("Should parse serialized");

    // Verify equivalence
    assert_eq!(config.default_configs, reparsed.default_configs);
    assert_eq!(config.dimensions.len(), reparsed.dimensions.len());
    assert_eq!(config.contexts.len(), reparsed.contexts.len());
}
