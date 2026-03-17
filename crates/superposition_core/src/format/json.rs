use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use superposition_types::{
    database::models::cac::{DependencyGraph, DimensionType},
    Cac, Condition, DefaultConfigsWithSchema, DetailedConfig, DimensionInfo, ExtendedMap,
    Overrides,
};

use crate::format::{ConfigFormat, FormatError};

fn dim_type_default() -> String {
    DimensionType::default().to_string()
}

/// JSON-specific dimension info that maps the user-facing `type` field
/// to the internal `dimension_type`, with a sensible default.
#[derive(Serialize, Deserialize)]
struct DimensionInfoJson {
    position: i32,
    schema: Value,
    #[serde(rename = "type", default = "dim_type_default")]
    dimension_type: String,
}

impl TryFrom<DimensionInfo> for DimensionInfoJson {
    type Error = FormatError;
    fn try_from(d: DimensionInfo) -> Result<Self, Self::Error> {
        Ok(Self {
            position: d.position,
            schema: Value::from(&d.schema),
            dimension_type: d.dimension_type.to_string(),
        })
    }
}

impl TryFrom<DimensionInfoJson> for DimensionInfo {
    type Error = FormatError;
    fn try_from(d: DimensionInfoJson) -> Result<Self, Self::Error> {
        let schema_map = match d.schema {
            Value::Object(map) => map,
            _ => {
                return Err(JsonFormat::conversion_error("Schema must be an object"));
            }
        };
        Ok(Self {
            position: d.position,
            schema: ExtendedMap::from(schema_map),
            dimension_type: DimensionType::from_str(&d.dimension_type)
                .map_err(JsonFormat::conversion_error)?,
            dependency_graph: DependencyGraph(HashMap::new()),
            value_compute_function_name: None,
        })
    }
}

use std::str::FromStr;

/// JSON format representation that matches TOML structure
/// Uses "overrides" array like TOML, with each item having _context_ and override values
#[derive(Serialize, Deserialize)]
struct JsonConfig {
    #[serde(rename = "default-configs")]
    default_configs: DefaultConfigsWithSchema,
    dimensions: HashMap<String, DimensionInfoJson>,
    overrides: Vec<JsonContext>,
}

#[derive(Serialize, Deserialize)]
struct JsonContext {
    #[serde(rename = "_context_")]
    context: Map<String, Value>,
    #[serde(flatten)]
    overrides: Map<String, Value>,
}

/// JSON format implementation
pub struct JsonFormat;

impl TryFrom<JsonConfig> for DetailedConfig {
    type Error = FormatError;

    fn try_from(json_config: JsonConfig) -> Result<Self, Self::Error> {
        let dimensions: HashMap<String, DimensionInfo> = json_config
            .dimensions
            .into_iter()
            .map(|(k, v)| Ok((k, DimensionInfo::try_from(v)?)))
            .collect::<Result<_, FormatError>>()?;

        JsonFormat::try_into_detailed(
            json_config.default_configs,
            dimensions,
            json_config.overrides,
            |ctx| {
                let condition = Cac::<Condition>::try_from(ctx.context)
                    .map(|cac| cac.into_inner())
                    .map_err(|e| {
                        JsonFormat::conversion_error(format!("Invalid condition: {}", e))
                    })?;

                let override_vals = Cac::<Overrides>::try_from(ctx.overrides)
                    .map(|cac| cac.into_inner())
                    .map_err(|e| {
                        JsonFormat::conversion_error(format!("Invalid overrides: {}", e))
                    })?;

                Ok((condition, override_vals))
            },
        )
    }
}

impl TryFrom<DetailedConfig> for JsonConfig {
    type Error = FormatError;

    fn try_from(detailed_config: DetailedConfig) -> Result<Self, Self::Error> {
        let dimensions: HashMap<String, DimensionInfoJson> = detailed_config
            .dimensions
            .iter()
            .map(|(k, v)| Ok((k.clone(), DimensionInfoJson::try_from(v.clone())?)))
            .collect::<Result<_, FormatError>>()?;

        let overrides = detailed_config
            .contexts
            .into_iter()
            .map(|ctx| {
                let override_key = ctx.override_with_keys.get_key();
                let overrides = detailed_config
                    .overrides
                    .get(override_key)
                    .cloned()
                    .unwrap_or_default();

                let condition_value =
                    serde_json::to_value(&ctx.condition).unwrap_or_default();
                let context_map = match condition_value {
                    Value::Object(map) => map,
                    _ => Map::new(),
                };

                JsonContext {
                    context: context_map,
                    overrides: overrides.into(),
                }
            })
            .collect();

        Ok(Self {
            default_configs: detailed_config.default_configs,
            dimensions,
            overrides,
        })
    }
}

impl ConfigFormat for JsonFormat {
    fn parse_into_detailed(input: &str) -> Result<DetailedConfig, FormatError> {
        let json_config: JsonConfig =
            serde_json::from_str(input).map_err(|e| Self::syntax_error(e.to_string()))?;
        DetailedConfig::try_from(json_config)
    }

    fn serialize(detailed_config: DetailedConfig) -> Result<String, FormatError> {
        let json_config = JsonConfig::try_from(detailed_config)?;
        serde_json::to_string_pretty(&json_config)
            .map_err(|e| Self::serialization_error(e.to_string()))
    }

    fn format_name() -> &'static str {
        "JSON"
    }
}
