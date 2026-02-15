//! Tests for TOML format implementation

use serde_json::{Map, Value};
use std::collections::BTreeMap;
use superposition_types::{
    Config, DefaultConfigInfo, DefaultConfigsWithSchema, DetailedConfig,
};

use crate::format::toml_impl::{parse_toml_config, serialize_to_toml};

/// Helper function to convert Config to DetailedConfig by inferring schema from value.
fn config_to_detailed(config: &Config) -> DetailedConfig {
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

    DetailedConfig {
        contexts: config.contexts.clone(),
        overrides: config.overrides.clone(),
        default_configs: DefaultConfigsWithSchema::from(default_configs),
        dimensions: config.dimensions.clone(),
    }
}

#[test]
fn test_toml_round_trip_simple() {
    let original_toml = r#"
[default-configs]
"time.out" = { value = 30, schema = { type = "integer" } }

[dimensions]
os = { position = 1, schema = { "type" = "string" } }

[[overrides]]
_context_ = { os = "linux" }
"time.out" = 60
"#;

    let config = parse_toml_config(original_toml).unwrap();
    let serialized = serialize_to_toml(config_to_detailed(&config)).unwrap();
    let reparsed = parse_toml_config(&serialized).unwrap();

    assert_eq!(config.default_configs, reparsed.default_configs);
    assert_eq!(config.dimensions.len(), reparsed.dimensions.len());
    assert_eq!(config.contexts.len(), reparsed.contexts.len());
}

#[test]
fn test_dimension_type_local_cohort() {
    let toml = r#"
[default-configs]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os = { position = 2, schema = { type = "string" } }
os_cohort = { position = 1, type = "LOCAL_COHORT:os", schema = { type = "string", enum = ["linux", "windows", "otherwise"], definitions = { linux = "rule_for_linux", windows = "rule_for_windows" } } }

[[overrides]]
_context_ = { os = "linux" }
timeout = 60
"#;

    let config = parse_toml_config(toml).unwrap();
    let serialized = serialize_to_toml(config_to_detailed(&config)).unwrap();

    assert!(serialized.contains(r#"type = "LOCAL_COHORT:os""#));
}

#[test]
fn test_undeclared_dimension() {
    let toml = r#"
[default-configs]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os = { position = 1, schema = { type = "string" } }

[[overrides]]
_context_ = { region = "us-east" }
timeout = 60
"#;

    let result = parse_toml_config(toml);
    assert!(result.is_err());
}

#[test]
fn test_priority_calculation() {
    let toml = r#"
[default-configs]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os = { position = 1, schema = { type = "string" } }
region = { position = 2, schema = { type = "string" } }

[[overrides]]
_context_ = { os = "linux" }
timeout = 60

[[overrides]]
_context_ = { os = "linux", region = "us-east" }
timeout = 90
"#;

    let parsed = parse_toml_config(toml).unwrap();
    assert_eq!(parsed.contexts[0].priority, 0);
    assert_eq!(parsed.contexts[1].priority, 1);
}

#[test]
fn test_resolution_with_local_cohorts() {
    let toml = r#"
[default-configs]
config = { value = { host = "localhost", port = 8080 }, schema = { type = "object" } }
max_count = { value = 10, schema = { type = "number", minimum = 0, maximum = 100 } }

[dimensions]
os = { position = 2, schema = { type = "string", enum = ["linux", "windows", "macos"] } }
os_cohort = { position = 1, schema = { enum = ["unix", "otherwise"], type = "string", definitions = { unix = { in = [{ var = "os" }, ["linux", "macos"]] } } }, type = "LOCAL_COHORT:os" }

[[overrides]]
_context_ = { os = "linux" }
config = { host = "prod.example.com", port = 443 }

[[overrides]]
_context_ = { os_cohort = "unix" }
config = { host = "prod.unix.com", port = 8443 }
max_count = 95
"#;

    let config = parse_toml_config(toml).unwrap();
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
