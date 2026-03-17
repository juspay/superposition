//! Tests for JSON format implementation

use serde_json::{json, Map, Value};
use std::collections::BTreeMap;
use superposition_types::{
    Config, DefaultConfigInfo, DefaultConfigsWithSchema, DetailedConfig,
};

use crate::format::FormatError;
use crate::{ConfigFormat, JsonFormat};

/// Helper to create a DetailedConfig from Config for testing
fn config_to_detailed(config: &Config) -> DetailedConfig {
    let default_configs: BTreeMap<String, DefaultConfigInfo> = config
        .default_configs
        .iter()
        .map(|(key, value)| {
            let schema = match value {
                Value::String(_) => json!({ "type": "string" }),
                Value::Number(n) => {
                    if n.is_i64() {
                        json!({ "type": "integer" })
                    } else {
                        json!({ "type": "number" })
                    }
                }
                Value::Bool(_) => json!({ "type": "boolean" }),
                Value::Array(_) => json!({ "type": "array" }),
                Value::Object(_) => json!({ "type": "object" }),
                Value::Null => json!({ "type": "null" }),
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
    let config = JsonFormat::parse_config(EXAMPLE_JSON).unwrap();

    // Verify parsed correctly
    assert_eq!(config.default_configs.len(), 2);
    assert_eq!(config.dimensions.len(), 1);
    assert_eq!(config.contexts.len(), 1);
    assert_eq!(config.overrides.len(), 1);

    // Serialize back to JSON
    let serialized = JsonFormat::serialize(config_to_detailed(&config)).unwrap();

    // Parse again
    let reparsed = JsonFormat::parse_config(&serialized).unwrap();

    // Should be functionally equivalent
    assert_eq!(config.default_configs, reparsed.default_configs);
    assert_eq!(config.dimensions.len(), reparsed.dimensions.len());
    assert_eq!(config.contexts.len(), reparsed.contexts.len());
}

#[test]
fn test_json_round_trip_simple() {
    let json_str = r#"{
        "default-configs": {
            "time.out": { "value": 30, "schema": { "type": "integer" } }
        },
        "dimensions": {
            "os": { "position": 1, "schema": { "type": "string" } }
        },
        "overrides": [
            {
                "_context_": { "os": "linux" },
                "time.out": 60
            }
        ]
    }"#;

    let config = JsonFormat::parse_config(json_str).unwrap();
    let serialized = JsonFormat::serialize(config_to_detailed(&config)).unwrap();
    let reparsed = JsonFormat::parse_config(&serialized).unwrap();

    assert_eq!(config.default_configs, reparsed.default_configs);
    assert_eq!(config.dimensions.len(), reparsed.dimensions.len());
    assert_eq!(config.contexts.len(), reparsed.contexts.len());
}

#[test]
fn test_json_invalid_syntax() {
    let invalid_json = r#"{ invalid json }"#;
    let result = JsonFormat::parse_config(invalid_json);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("JSON"));
}

#[test]
fn test_json_undeclared_dimension() {
    let json_str = r#"{
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

    let result = JsonFormat::parse_config(json_str);
    assert!(
        result.is_err(),
        "Expected error for undeclared dimension, got: {:?}",
        result
    );
}

#[test]
fn test_json_invalid_override_key() {
    let json_str = r#"{
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

    let result = JsonFormat::parse_config(json_str);
    assert!(result.is_err());
    assert!(matches!(
        result,
        Err(FormatError::InvalidOverrideKey { .. })
    ));
}

#[test]
fn test_json_dimension_type_local_cohort() {
    let json_str = r#"{
        "default-configs": {
            "timeout": { "value": 30, "schema": { "type": "integer" } }
        },
        "dimensions": {
            "os": { "position": 2, "schema": { "type": "string" } },
            "os_cohort": { "position": 1, "type": "LOCAL_COHORT:os", "schema": { "type": "string", "enum": ["linux", "windows", "otherwise"], "definitions": { "linux": "rule_for_linux", "windows": "rule_for_windows" } } }
        },
        "overrides": [
            {
                "_context_": { "os": "linux" },
                "timeout": 60
            }
        ]
    }"#;

    let config = JsonFormat::parse_config(json_str).unwrap();
    let serialized = JsonFormat::serialize(config_to_detailed(&config)).unwrap();

    assert!(serialized.contains("LOCAL_COHORT:os"));
}

#[test]
fn test_json_priority_calculation() {
    let json_str = r#"{
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

    let parsed = JsonFormat::parse_config(json_str).unwrap();
    assert_eq!(parsed.contexts[0].priority, 0);
    assert_eq!(parsed.contexts[1].priority, 1);
}

#[test]
fn test_json_resolution_with_local_cohorts() {
    let json_str = r#"{
        "default-configs": {
            "config": { "value": { "host": "localhost", "port": 8080 }, "schema": { "type": "object" } },
            "max_count": { "value": 10, "schema": { "type": "number", "minimum": 0, "maximum": 100 } }
        },
        "dimensions": {
            "os": { "position": 2, "schema": { "type": "string", "enum": ["linux", "windows", "macos"] } },
            "os_cohort": { "position": 1, "schema": { "enum": ["unix", "otherwise"], "type": "string", "definitions": { "unix": { "in": [{ "var": "os" }, ["linux", "macos"]] } } }, "type": "LOCAL_COHORT:os" }
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

    let config = JsonFormat::parse_config(json_str).unwrap();
    let mut dims = Map::new();
    dims.insert("os".to_string(), Value::String("linux".to_string()));

    let result = crate::eval_config(
        (*config.default_configs).clone(),
        &config.contexts,
        &config.overrides,
        &config.dimensions,
        &dims,
        crate::MergeStrategy::MERGE,
        None,
    )
    .unwrap();

    assert_eq!(
        result.get("max_count"),
        Some(&Value::Number(serde_json::Number::from(95)))
    );
}

#[test]
fn test_json_round_trip_empty_config() {
    let json_str = r#"{
        "default-configs": {
            "timeout": { "value": 30, "schema": { "type": "integer" } }
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

    let config = JsonFormat::parse_config(json_str).unwrap();
    assert_eq!(config.default_configs.len(), 1);
    assert_eq!(config.contexts.len(), 1);
    assert_eq!(config.overrides.len(), 1);
}

#[test]
fn test_json_dimension_type_regular() {
    let json_str = r#"{
        "default-configs": {
            "timeout": { "value": 30, "schema": { "type": "integer" } }
        },
        "dimensions": {
            "os": { "position": 1, "schema": { "type": "string" }, "type": "REGULAR" }
        },
        "overrides": [
            {
                "_context_": { "os": "linux" },
                "timeout": 60
            }
        ]
    }"#;

    let config = JsonFormat::parse_config(json_str).unwrap();
    let serialized = JsonFormat::serialize(config_to_detailed(&config)).unwrap();
    let reparsed = JsonFormat::parse_config(&serialized).unwrap();

    assert!(serialized.contains("REGULAR"));
    assert_eq!(config.dimensions.len(), reparsed.dimensions.len());
}

#[test]
fn test_json_dimension_type_local_cohort_invalid_reference() {
    let json_str = r#"{
        "default-configs": {
            "timeout": { "value": 30, "schema": { "type": "integer" } }
        },
        "dimensions": {
            "os_cohort": { "position": 1, "schema": { "type": "string" }, "type": "LOCAL_COHORT:nonexistent" }
        },
        "overrides": [
            {
                "_context_": { "os": "linux" },
                "timeout": 60
            }
        ]
    }"#;

    let result = JsonFormat::parse_config(json_str);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("does not exist") || err.contains("not found"),
        "Expected error about nonexistent dimension, got: {err}"
    );
}

#[test]
fn test_json_dimension_type_local_cohort_empty_name() {
    let json_str = r#"{
        "default-configs": {
            "timeout": { "value": 30, "schema": { "type": "integer" } }
        },
        "dimensions": {
            "os": { "position": 1, "schema": { "type": "string" } },
            "os_cohort": { "position": 2, "schema": { "type": "string" }, "type": "LOCAL_COHORT:" }
        },
        "overrides": [
            {
                "_context_": { "os": "linux" },
                "timeout": 60
            }
        ]
    }"#;

    let result = JsonFormat::parse_config(json_str);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("does not exist") || err.contains("not found"),
        "Expected error about empty cohort name, got: {err}"
    );
}

#[test]
fn test_json_dimension_type_remote_cohort() {
    let json_str = r#"{
        "default-configs": {
            "timeout": { "value": 30, "schema": { "type": "integer" } }
        },
        "dimensions": {
            "os": { "position": 2, "schema": { "type": "string" } },
            "os_cohort": { "position": 1, "type": "REMOTE_COHORT:os", "schema": { "type": "string", "enum": ["linux", "windows", "macos"] } }
        },
        "overrides": [
            {
                "_context_": { "os": "linux" },
                "timeout": 60
            }
        ]
    }"#;

    let config = JsonFormat::parse_config(json_str).unwrap();
    let serialized = JsonFormat::serialize(config_to_detailed(&config)).unwrap();
    let reparsed = JsonFormat::parse_config(&serialized).unwrap();

    assert!(serialized.contains("REMOTE_COHORT:os"));
    assert_eq!(config.dimensions.len(), reparsed.dimensions.len());
}

#[test]
fn test_json_dimension_type_remote_cohort_invalid_reference() {
    let json_str = r#"{
        "default-configs": {
            "timeout": { "value": 30, "schema": { "type": "integer" } }
        },
        "dimensions": {
            "os_cohort": { "position": 1, "schema": { "type": "string" }, "type": "REMOTE_COHORT:nonexistent" }
        },
        "overrides": [
            {
                "_context_": { "os": "linux" },
                "timeout": 60
            }
        ]
    }"#;

    let result = JsonFormat::parse_config(json_str);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("does not exist") || err.contains("not found"),
        "Expected error about nonexistent dimension, got: {err}"
    );
}

#[test]
fn test_json_dimension_type_remote_cohort_empty_name() {
    let json_str = r#"{
        "default-configs": {
            "timeout": { "value": 30, "schema": { "type": "integer" } }
        },
        "dimensions": {
            "os": { "position": 1, "schema": { "type": "string" } },
            "os_cohort": { "position": 2, "schema": { "type": "string" }, "type": "REMOTE_COHORT:" }
        },
        "overrides": [
            {
                "_context_": { "os": "linux" },
                "timeout": 60
            }
        ]
    }"#;

    let result = JsonFormat::parse_config(json_str);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("does not exist") || err.contains("not found"),
        "Expected error about empty cohort name, got: {err}"
    );
}

#[test]
fn test_json_dimension_type_remote_cohort_invalid_schema() {
    let json_str = r#"{
        "default-configs": {
            "timeout": { "value": 30, "schema": { "type": "integer" } }
        },
        "dimensions": {
            "os": { "position": 1, "schema": { "type": "string" } },
            "os_cohort": { "position": 2, "type": "REMOTE_COHORT:os", "schema": { "type": "invalid_type" } }
        },
        "overrides": [
            {
                "_context_": { "os": "linux" },
                "timeout": 60
            }
        ]
    }"#;

    let result = JsonFormat::parse_config(json_str);
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("Schema validation failed"));
}

#[test]
fn test_json_dimension_type_default_regular() {
    let json_str = r#"{
        "default-configs": {
            "timeout": { "value": 30, "schema": { "type": "integer" } }
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

    let config = JsonFormat::parse_config(json_str).unwrap();
    let serialized = JsonFormat::serialize(config_to_detailed(&config)).unwrap();
    let reparsed = JsonFormat::parse_config(&serialized).unwrap();

    assert!(serialized.contains("REGULAR"));
    assert_eq!(config.dimensions.len(), reparsed.dimensions.len());
}

#[test]
fn test_json_dimension_type_invalid_format() {
    let json_str = r#"{
        "default-configs": {
            "timeout": { "value": 30, "schema": { "type": "integer" } }
        },
        "dimensions": {
            "os": { "position": 1, "schema": { "type": "string" }, "type": "local_cohort" }
        },
        "overrides": [
            {
                "_context_": { "os": "linux" },
                "timeout": 60
            }
        ]
    }"#;

    let result = JsonFormat::parse_config(json_str);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("local_cohort"));
}

#[test]
fn test_json_valid_parsing() {
    let json_str = r#"{
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

    let result = JsonFormat::parse_config(json_str);
    assert!(result.is_ok());
    let parsed = result.unwrap();
    assert_eq!(parsed.default_configs.len(), 2);
    assert_eq!(parsed.dimensions.len(), 1);
    assert_eq!(parsed.contexts.len(), 1);
    assert_eq!(parsed.overrides.len(), 1);
}

#[test]
fn test_json_missing_section_error() {
    let json_str = r#"{
        "default-configs": {
            "timeout": { "value": 30, "schema": { "type": "integer" } }
        }
    }"#;

    let result = JsonFormat::parse_config(json_str);
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("missing field `dimensions`"));
}

#[test]
fn test_json_missing_value_field() {
    let json_str = r#"{
        "default-configs": {
            "timeout": { "schema": { "type": "integer" } }
        },
        "dimensions": {
            "os": { "position": 1, "schema": { "type": "string" } }
        },
        "overrides": []
    }"#;

    let result = JsonFormat::parse_config(json_str);
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("missing field `value`"));
}

#[test]
fn test_json_duplicate_position_error() {
    let json_str = r#"{
        "default-configs": {
            "timeout": { "value": 30, "schema": { "type": "integer" } }
        },
        "dimensions": {
            "os": { "position": 1, "schema": { "type": "string" } },
            "region": { "position": 1, "schema": { "type": "string" } }
        },
        "overrides": [
            {
                "_context_": { "os": "linux" },
                "timeout": 60
            }
        ]
    }"#;

    let result = JsonFormat::parse_config(json_str);
    assert!(result.is_err());
    assert!(matches!(
        result,
        Err(FormatError::DuplicatePosition {
            position,
            dimensions
        }) if position == 1 && dimensions.len() == 2
    ));
}

#[test]
fn test_json_validation_valid_default_config() {
    let json_str = r#"{
        "default-configs": {
            "timeout": { "value": 30, "schema": { "type": "integer" } },
            "enabled": { "value": true, "schema": { "type": "boolean" } },
            "name": { "value": "test", "schema": { "type": "string" } }
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

    let result = JsonFormat::parse_config(json_str);
    assert!(result.is_ok());
}

#[test]
fn test_json_validation_invalid_default_config_type_mismatch() {
    let json_str = r#"{
        "default-configs": {
            "timeout": { "value": "not_an_integer", "schema": { "type": "integer" } }
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

    let result = JsonFormat::parse_config(json_str);
    assert!(result.is_err());
    assert!(matches!(result, Err(FormatError::ValidationError { .. })));
    let err = result.unwrap_err();
    assert!(err.to_string().contains("timeout"));
}

#[test]
fn test_json_validation_valid_context_override() {
    let json_str = r#"{
        "default-configs": {
            "timeout": { "value": 30, "schema": { "type": "integer" } }
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

    let result = JsonFormat::parse_config(json_str);
    assert!(result.is_ok());
}

#[test]
fn test_json_validation_invalid_context_override_type_mismatch() {
    let json_str = r#"{
        "default-configs": {
            "timeout": { "value": 30, "schema": { "type": "integer" } }
        },
        "dimensions": {
            "os": { "position": 1, "schema": { "type": "string" } }
        },
        "overrides": [
            {
                "_context_": { "os": "linux" },
                "timeout": "not_an_integer"
            }
        ]
    }"#;

    let result = JsonFormat::parse_config(json_str);
    assert!(result.is_err());
    assert!(matches!(result, Err(FormatError::ValidationError { .. })));
    let err = result.unwrap_err();
    assert!(err.to_string().contains("context[0].timeout"));
}

#[test]
fn test_json_validation_valid_dimension_value_in_context() {
    let json_str = r#"{
        "default-configs": {
            "config": { "value": { "host": "localhost", "port": 8080 }, "schema": { "type": "object" } },
            "max_count": { "value": 10, "schema": { "type": "number", "minimum": 0, "maximum": 100 } }
        },
        "dimensions": {
            "os": { "position": 2, "schema": { "type": "string", "enum": ["linux", "windows", "macos"] } },
            "os_cohort": { "position": 1, "schema": { "enum": ["unix", "otherwise"], "type": "string", "definitions": { "unix": { "in": [{ "var": "os" }, ["linux", "macos"]] } } }, "type": "LOCAL_COHORT:os" }
        },
        "overrides": [
            {
                "_context_": { "os": "linux" },
                "config": { "host": "prod.example.com", "port": 443 }
            },
            {
                "_context_": { "os_cohort": "unix" },
                "config": { "host": "prod.unix.com", "port": 8443 }
            }
        ]
    }"#;

    let config = JsonFormat::parse_config(json_str).unwrap();

    // Verify default config object was parsed correctly
    let default_config_value = config.default_configs.get("config").unwrap();
    assert_eq!(
        default_config_value.get("host"),
        Some(&Value::String("localhost".to_string()))
    );
    assert_eq!(
        default_config_value.get("port"),
        Some(&Value::Number(serde_json::Number::from(8080)))
    );

    // Serialize Config -> JSON
    let serialized = JsonFormat::serialize(config_to_detailed(&config)).unwrap();

    // Parse again
    let reparsed = JsonFormat::parse_config(&serialized).unwrap();

    // Configs should be functionally equivalent
    assert_eq!(config.default_configs, reparsed.default_configs);
    assert_eq!(config.contexts.len(), reparsed.contexts.len());

    // Collect override hosts from both contexts (order may vary)
    let mut override_hosts: Vec<String> = config
        .contexts
        .iter()
        .map(|ctx| {
            let override_key = ctx.override_with_keys.get_key();
            let overrides = config.overrides.get(override_key).unwrap();
            let override_config = overrides.get("config").unwrap();
            override_config
                .get("host")
                .unwrap()
                .as_str()
                .unwrap()
                .to_string()
        })
        .collect();
    override_hosts.sort();
    assert_eq!(
        override_hosts,
        vec!["prod.example.com".to_string(), "prod.unix.com".to_string()]
    );
}
