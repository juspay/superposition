use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use superposition_types::{
    Config, DefaultConfigsWithSchema, DetailedConfig, DimensionInfo, Overrides,
};

use crate::format::{ConfigFormat, FormatError};
use crate::helpers::build_context;

/// JSON format representation that matches TOML structure
/// Uses "overrides" array like TOML, with each item having _context_ and override values
#[derive(Serialize, Deserialize)]
struct JsonConfig {
    #[serde(rename = "default-configs")]
    default_configs: DefaultConfigsWithSchema,
    dimensions: HashMap<String, DimensionInfo>,
    #[serde(rename = "overrides")]
    contexts: Vec<JsonContext>,
}

#[derive(Serialize, Deserialize)]
struct JsonContext {
    #[serde(rename = "_context_")]
    context: serde_json::Map<String, serde_json::Value>,
    #[serde(flatten)]
    overrides: serde_json::Map<String, serde_json::Value>,
}

/// JSON format implementation
pub struct JsonFormat;

impl ConfigFormat for JsonFormat {
    fn parse(input: &str) -> Result<DetailedConfig, FormatError> {
        // Parse JSON to intermediate struct (similar to TOML approach)
        let json_config: JsonConfig = serde_json::from_str(input)
            .map_err(|e| crate::format::syntax_error("JSON", e.to_string()))?;

        // Convert to DetailedConfig
        let mut overrides = HashMap::new();
        let mut contexts = Vec::new();

        for ctx in json_config.contexts {
            // Convert condition
            use superposition_types::Cac;
            let condition = Cac::<superposition_types::Condition>::try_from(ctx.context)
                .map(|cac| cac.into_inner())
                .map_err(|e| {
                    crate::format::conversion_error(
                        "JSON",
                        format!("Invalid condition: {}", e),
                    )
                })?;

            // Convert overrides
            let override_vals = Cac::<Overrides>::try_from(ctx.overrides)
                .map(|cac| cac.into_inner())
                .map_err(|e| {
                    crate::format::conversion_error(
                        "JSON",
                        format!("Invalid overrides: {}", e),
                    )
                })?;

            // Build context
            let (context, override_hash, override_vals) =
                build_context(condition, override_vals, &json_config.dimensions)
                    .map_err(|e| crate::format::conversion_error("JSON", e))?;

            overrides.insert(override_hash, override_vals);
            contexts.push(context);
        }

        // Sort contexts by priority (weight) - higher weight means higher priority
        contexts.sort_by(|a, b| b.priority.cmp(&a.priority));

        // Set correct values for weight and priority after sorting
        contexts.iter_mut().enumerate().for_each(|(index, ctx)| {
            ctx.weight = index as i32;
            ctx.priority = index as i32;
        });

        Ok(DetailedConfig {
            default_configs: json_config.default_configs,
            dimensions: json_config.dimensions,
            contexts,
            overrides,
        })
    }

    fn serialize(detailed_config: DetailedConfig) -> Result<String, FormatError> {
        // For serialization, we need to reconstruct the overrides array
        let contexts: Vec<JsonContext> = detailed_config
            .contexts
            .iter()
            .map(|ctx| {
                let override_key = ctx.override_with_keys.get_key();
                let overrides = detailed_config
                    .overrides
                    .get(override_key)
                    .cloned()
                    .unwrap_or_default();

                // Serialize condition to JSON value, then convert to Map
                let condition_value =
                    serde_json::to_value(&ctx.condition).unwrap_or_default();
                let context_map = match condition_value {
                    serde_json::Value::Object(map) => map,
                    _ => serde_json::Map::new(),
                };

                JsonContext {
                    context: context_map,
                    overrides: overrides.into(),
                }
            })
            .collect();

        let json_config = JsonConfig {
            default_configs: detailed_config.default_configs,
            dimensions: detailed_config.dimensions,
            contexts,
        };

        serde_json::to_string_pretty(&json_config)
            .map_err(|e| crate::format::serialization_error("JSON", e.to_string()))
    }

    fn format_name() -> &'static str {
        "JSON"
    }
}

/// Parse JSON configuration string into Config
///
/// # Arguments
/// * `json_str` - JSON string containing configuration
///
/// # Returns
/// * `Ok(Config)` - Successfully parsed configuration
/// * `Err(FormatError)` - Parsing or validation error
///
/// # Example JSON Format
/// ```json
/// {
///   "default-configs": {
///     "timeout": { "value": 30, "schema": { "type": "integer" } }
///   },
///   "dimensions": {
///     "os": { "position": 1, "schema": { "type": "string" } }
///   },
///   "overrides": [
///     {
///       "_context_": { "os": "linux" },
///       "timeout": 60
///     }
///   ]
/// }
/// ```
pub fn parse_json_config(json_str: &str) -> Result<Config, FormatError> {
    let detailed_config = JsonFormat::parse(json_str)?;
    let mut detailed = detailed_config;
    crate::format::validate_detailed_config(&mut detailed)?;
    Ok(Config::from(detailed))
}

/// Serialize DetailedConfig to JSON format
///
/// # Arguments
/// * `detailed_config` - The configuration to serialize
///
/// # Returns
/// * `Ok(String)` - Pretty-printed JSON string
/// * `Err(FormatError)` - Serialization error
pub fn serialize_to_json(detailed_config: DetailedConfig) -> Result<String, FormatError> {
    JsonFormat::serialize(detailed_config)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;
    use superposition_types::{DefaultConfigInfo, DefaultConfigsWithSchema};

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
        let config = parse_json_config(EXAMPLE_JSON).unwrap();

        // Verify parsed correctly
        assert_eq!(config.default_configs.len(), 2);
        assert_eq!(config.dimensions.len(), 1);
        assert_eq!(config.contexts.len(), 1);
        assert_eq!(config.overrides.len(), 1);

        // Serialize back to JSON
        let serialized = serialize_to_json(config_to_detailed(&config)).unwrap();

        // Parse again
        let reparsed = parse_json_config(&serialized).unwrap();

        // Should be functionally equivalent
        assert_eq!(config.default_configs, reparsed.default_configs);
        assert_eq!(config.dimensions.len(), reparsed.dimensions.len());
        assert_eq!(config.contexts.len(), reparsed.contexts.len());
    }

    #[test]
    fn test_json_invalid_syntax() {
        let invalid_json = r#"{ invalid json }"#;
        let result = parse_json_config(invalid_json);
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

        let result = parse_json_config(json);
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

        let result = parse_json_config(json);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("not found"));
    }
}
