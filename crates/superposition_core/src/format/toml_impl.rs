use std::collections::{BTreeMap, HashMap};
use std::ops::Deref;
use std::str::FromStr;

use serde::{Deserialize, Serialize};
use serde_json::Value;
use superposition_types::database::models::cac::{DependencyGraph, DimensionType};
use superposition_types::{
    Config, Context, DefaultConfigsWithSchema, DetailedConfig, DimensionInfo,
    ExtendedMap, Overrides,
};
use toml::Value as TomlValue;

use crate::format::{
    conversion_error, serialization_error, syntax_error, ConfigFormat, FormatError,
};
use crate::helpers::build_context;
use crate::toml::helpers::{
    format_key, format_toml_value, toml_to_json, try_condition_from_toml,
    try_overrides_from_toml,
};

/// TOML format implementation
pub struct TomlFormat;

/// TOML-specific structures (kept for backward compatibility)
#[derive(Serialize, Deserialize)]
pub struct DimensionInfoToml {
    pub position: i32,
    pub schema: toml::Table,
    #[serde(rename = "type", default = "dim_type_default")]
    pub dimension_type: String,
}

fn dim_type_default() -> String {
    DimensionType::default().to_string()
}

impl TryFrom<DimensionInfo> for DimensionInfoToml {
    type Error = FormatError;
    fn try_from(d: DimensionInfo) -> Result<Self, Self::Error> {
        let schema = toml::Table::try_from(d.schema.into_inner()).map_err(|e| {
            conversion_error(
                "TOML",
                format!("Schema contains incompatible values: {}", e),
            )
        })?;
        Ok(Self {
            position: d.position,
            schema,
            dimension_type: d.dimension_type.to_string(),
        })
    }
}

impl TryFrom<DimensionInfoToml> for DimensionInfo {
    type Error = FormatError;
    fn try_from(d: DimensionInfoToml) -> Result<Self, Self::Error> {
        let schema_json = toml_to_json(TomlValue::Table(d.schema));
        let schema_map = match schema_json {
            Value::Object(map) => map,
            _ => {
                return Err(conversion_error("TOML", "Schema must be an object"));
            }
        };
        Ok(Self {
            position: d.position,
            schema: ExtendedMap::from(schema_map),
            dimension_type: DimensionType::from_str(&d.dimension_type)
                .map_err(|e| conversion_error("TOML", e))?,
            dependency_graph: DependencyGraph(HashMap::new()),
            value_compute_function_name: None,
        })
    }
}

#[derive(Serialize, Deserialize)]
pub struct ContextToml {
    #[serde(rename = "_context_")]
    context: toml::Table,
    #[serde(flatten)]
    overrides: toml::Table,
}

impl TryFrom<(Context, &HashMap<String, Overrides>)> for ContextToml {
    type Error = FormatError;
    fn try_from(
        (context, overrides): (Context, &HashMap<String, Overrides>),
    ) -> Result<Self, Self::Error> {
        let context_toml: toml::Table =
            toml::Table::try_from(context.condition.deref().clone())
                .map_err(|e| conversion_error("TOML", e.to_string()))?;
        let overrides_toml: toml::Table =
            toml::Table::try_from(overrides.get(context.override_with_keys.get_key()))
                .map_err(|e| conversion_error("TOML", e.to_string()))?;

        Ok(Self {
            context: context_toml,
            overrides: overrides_toml,
        })
    }
}

#[derive(Serialize, Deserialize)]
pub struct DetailedConfigToml {
    #[serde(rename = "default-configs")]
    pub default_configs: DefaultConfigsWithSchema,
    pub dimensions: BTreeMap<String, DimensionInfoToml>,
    pub overrides: Vec<ContextToml>,
}

impl DetailedConfigToml {
    fn emit_default_configs(
        default_configs: DefaultConfigsWithSchema,
    ) -> Result<String, FormatError> {
        let mut out = String::new();
        out.push_str("[default-configs]\n");

        for (k, v) in default_configs.into_inner() {
            let v_toml = TomlValue::try_from(v).map_err(|e| {
                serialization_error("TOML", format!("Failed to serialize '{}': {}", k, e))
            })?;

            let v_str = format_toml_value(&v_toml);
            out.push_str(&format!("{} = {}\n", format_key(&k), v_str));
        }

        out.push('\n');
        Ok(out)
    }

    fn emit_dimensions(
        dimensions: BTreeMap<String, DimensionInfoToml>,
    ) -> Result<String, FormatError> {
        let mut out = String::new();
        out.push_str("[dimensions]\n");

        for (k, v) in dimensions {
            let v_toml = TomlValue::try_from(v).map_err(|e| {
                serialization_error(
                    "TOML",
                    format!("Failed to serialize dimension '{}': {}", k, e),
                )
            })?;
            let v_str = format_toml_value(&v_toml);
            out.push_str(&format!("{} = {}\n", format_key(&k), v_str));
        }

        out.push('\n');
        Ok(out)
    }

    fn emit_overrides(ctx: ContextToml) -> Result<String, FormatError> {
        let mut out = String::new();
        out.push_str("[[overrides]]\n");

        let context_str = format_toml_value(&TomlValue::Table(ctx.context));
        out.push_str(&format!("_context_ = {}\n", context_str));

        for (k, v) in ctx.overrides {
            let v_str = format_toml_value(&v);
            out.push_str(&format!("{} = {}\n", format_key(&k), v_str));
        }

        out.push('\n');
        Ok(out)
    }

    pub fn serialize_to_toml(self) -> Result<String, FormatError> {
        let mut out = String::new();

        out.push_str(&Self::emit_default_configs(self.default_configs)?);
        out.push('\n');

        out.push_str(&Self::emit_dimensions(self.dimensions)?);
        out.push('\n');

        for ctx in self.overrides {
            out.push_str(&Self::emit_overrides(ctx)?);
        }

        out.push('\n');
        Ok(out)
    }
}

impl TryFrom<DetailedConfig> for DetailedConfigToml {
    type Error = FormatError;
    fn try_from(d: DetailedConfig) -> Result<Self, Self::Error> {
        Ok(Self {
            default_configs: d.default_configs,
            dimensions: d
                .dimensions
                .into_iter()
                .map(|(k, v)| DimensionInfoToml::try_from(v).map(|dim| (k, dim)))
                .collect::<Result<BTreeMap<_, _>, _>>()?,
            overrides: d
                .contexts
                .into_iter()
                .map(|c| ContextToml::try_from((c, &d.overrides)))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
}

impl TryFrom<DetailedConfigToml> for DetailedConfig {
    type Error = FormatError;
    fn try_from(d: DetailedConfigToml) -> Result<Self, Self::Error> {
        let default_configs = d.default_configs;
        let mut overrides = HashMap::new();
        let mut contexts = Vec::new();
        let dimensions = d
            .dimensions
            .into_iter()
            .map(|(k, v)| v.try_into().map(|dim_info| (k, dim_info)))
            .collect::<Result<HashMap<_, DimensionInfo>, FormatError>>()?;

        // Build contexts and overrides from TOML overrides
        for ctx in d.overrides.into_iter() {
            let condition = try_condition_from_toml(ctx.context)?;
            let override_vals = try_overrides_from_toml(ctx.overrides)?;

            let (context, override_hash, override_vals) =
                build_context(condition, override_vals, &dimensions)
                    .map_err(|e| conversion_error("TOML", e))?;

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

        let mut detailed = Self {
            default_configs,
            dimensions,
            contexts,
            overrides,
        };

        // Validate and enrich the configuration (builds dependency graphs)
        crate::validations::validate_and_enrich_config(&mut detailed)?;

        Ok(detailed)
    }
}

impl ConfigFormat for TomlFormat {
    fn parse(input: &str) -> Result<DetailedConfig, FormatError> {
        let detailed_toml = toml::from_str::<DetailedConfigToml>(input)
            .map_err(|e| syntax_error("TOML", e.to_string()))?;
        DetailedConfig::try_from(detailed_toml)
    }

    fn serialize(detailed_config: DetailedConfig) -> Result<String, FormatError> {
        let toml_config = DetailedConfigToml::try_from(detailed_config)?;
        toml_config.serialize_to_toml()
    }

    fn format_name() -> &'static str {
        "TOML"
    }
}

/// Parse TOML configuration string into Config
pub fn parse_toml_config(toml_str: &str) -> Result<Config, FormatError> {
    // TomlFormat::parse now includes validation and enrichment internally via TryFrom
    let detailed = TomlFormat::parse(toml_str)?;
    Ok(Config::from(detailed))
}

/// Serialize DetailedConfig to TOML format
pub fn serialize_to_toml(detailed_config: DetailedConfig) -> Result<String, FormatError> {
    TomlFormat::serialize(detailed_config)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::Map;
    use superposition_types::{Config, DefaultConfigInfo, DefaultConfigsWithSchema};

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
}
