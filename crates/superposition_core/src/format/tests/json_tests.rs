//! Tests for JSON format implementation

use std::collections::BTreeMap;
use superposition_types::{DefaultConfigInfo, DefaultConfigsWithSchema, DetailedConfig};

use crate::format::json_impl::{parse_json_config, serialize_to_json};
use crate::format::FormatError;
use superposition_types::Config;

/// Helper to create a DetailedConfig from Config for testing
fn config_to_detailed(config: &Config) -> DetailedConfig {
    let default_configs: BTreeMap<String, DefaultConfigInfo> = config
        .default_configs
        .iter()
        .map(|(key, value)| {
            let schema = match value {
                serde_json::Value::String(_) => {
                    serde_json::json!({ "type": "string" })
                }
                serde_json::Value::Number(n) => {
                    if n.is_i64() {
                        serde_json::json!({ "type": "integer" })
                    } else {
                        serde_json::json!({ "type": "number" })
                    }
                }
                serde_json::Value::Bool(_) => {
                    serde_json::json!({ "type": "boolean" })
                }
                serde_json::Value::Array(_) => serde_json::json!({ "type": "array" }),
                serde_json::Value::Object(_) => {
                    serde_json::json!({ "type": "object" })
                }
                serde_json::Value::Null => serde_json::json!({ "type": "null" }),
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

    DetailedConfig {
        contexts: config.contexts.clone(),
        overrides: config.overrides.clone(),
        default_configs: DefaultConfigsWithSchema::from(default_configs),
        dimensions: config.dimensions.clone(),
    }
}

const EXAMPLE_JSON: &str = r#"{
  "default-configs": {
    "timeout": { "value": 30, "schema": { "type": "integer" } },
    "enabled": { "value": true, "schema": { "type": "boolean" } }
  },
  "dimensions": {
    "os": { "position": 1, "schema": { "type": "string" } }
  },
  "overrides": [
    {
      "_context_": { "os": "linux" },
      "timeout": 60
    }
  ]
}"#;

#[test]
fn test_json_round_trip() {
    // Parse JSON -> Config
    let config: Config = parse_json_config(EXAMPLE_JSON).unwrap();

    // Verify parsed correctly
    assert_eq!(config.default_configs.len(), 2);
    assert_eq!(config.dimensions.len(), 1);
    assert_eq!(config.contexts.len(), 1);
    assert_eq!(config.overrides.len(), 1);

    // Serialize back to JSON
    let serialized = serialize_to_json(config_to_detailed(&config)).unwrap();

    // Parse again
    let reparsed: Config = parse_json_config(&serialized).unwrap();

    // Should be functionally equivalent
    assert_eq!(config.default_configs, reparsed.default_configs);
    assert_eq!(config.dimensions.len(), reparsed.dimensions.len());
    assert_eq!(config.contexts.len(), reparsed.contexts.len());
}

#[test]
fn test_json_invalid_syntax() {
    let invalid_json = r#"{ invalid json }"#;
    let result: Result<Config, FormatError> = parse_json_config(invalid_json);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("JSON"));
}

#[test]
fn test_json_undeclared_dimension() {
    let json = r#"{
            "default-configs": {
                "timeout": { "value": 30, "schema": { "type": "integer" } }
            },
            "dimensions": {
                "os": { "position": 1, "schema": { "type": "string" } }
            },
            "overrides": [
                {
                    "_context_": { "region": "us-east" },
                    "timeout": 60
                }
            ]
        }"#;

    let result: Result<Config, FormatError> = parse_json_config(json);
    assert!(
        result.is_err(),
        "Expected error for undeclared dimension, got: {:?}",
        result
    );
}

#[test]
fn test_json_invalid_override_key() {
    let json = r#"{
            "default-configs": {
                "timeout": { "value": 30, "schema": { "type": "integer" } }
            },
            "dimensions": {
                "os": { "position": 1, "schema": { "type": "string" } }
            },
            "overrides": [
                {
                    "_context_": { "os": "linux" },
                    "invalid_key": 60
                }
            ]
        }"#;

    let result: Result<Config, FormatError> = parse_json_config(json);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("not found"));
}
