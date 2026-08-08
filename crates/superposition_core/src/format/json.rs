use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use superposition_types::{
    Cac, Condition, DefaultConfigsWithSchema, DetailedConfig, DimensionInfo, ExtendedMap,
    Overrides,
    database::models::cac::{DependencyGraph, DimensionType},
};

use crate::format::{
    ConfigFormat, FormatError, MarkupFormat, validate_context_description,
};

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
    /// Optional on parse (the dimension name is used as a fallback when
    /// missing) and always present on export.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    description: Option<String>,
}

impl TryFrom<DimensionInfo> for DimensionInfoJson {
    type Error = FormatError;
    fn try_from(d: DimensionInfo) -> Result<Self, Self::Error> {
        Ok(Self {
            position: d.position,
            schema: Value::from(&d.schema),
            dimension_type: d.dimension_type.to_string(),
            description: Some(d.description),
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
            description: d.description.unwrap_or_default(),
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
    #[serde(default)]
    overrides: Vec<JsonContext>,
}

#[derive(Serialize, Deserialize)]
struct JsonContext {
    #[serde(rename = "_context_")]
    context: Map<String, Value>,
    #[serde(
        rename = "_description_",
        default,
        skip_serializing_if = "Option::is_none"
    )]
    description: Option<String>,
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
            .map(|(k, v)| {
                let mut dim_info = DimensionInfo::try_from(v)?;
                // Fall back to the dimension name when the description is
                // absent in the imported file.
                if dim_info.description.trim().is_empty() {
                    dim_info.description = k.clone();
                }
                Ok((k, dim_info))
            })
            .collect::<Result<_, FormatError>>()?;

        let mut default_configs = json_config.default_configs;
        for (k, info) in default_configs.iter_mut() {
            // Fall back to the key name when the description is absent.
            if info.description.trim().is_empty() {
                info.description = k.clone();
            }
        }

        JsonFormat::try_into_detailed(
            default_configs,
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

                let description = ctx
                    .description
                    .map(validate_context_description::<JsonFormat>)
                    .transpose()?;

                Ok((condition, override_vals, description))
            },
        )
    }
}

impl TryFrom<DetailedConfig> for JsonConfig {
    type Error = FormatError;

    fn try_from(detailed_config: DetailedConfig) -> Result<Self, Self::Error> {
        let DetailedConfig {
            contexts,
            context_descriptions,
            overrides,
            mut default_configs,
            dimensions,
        } = detailed_config;

        let dimensions: HashMap<String, DimensionInfoJson> = dimensions
            .iter()
            .map(|(k, v)| {
                let mut dim = DimensionInfoJson::try_from(v.clone())?;
                // Description is mandatory in the exported file; fall back to
                // the dimension name when it is missing.
                if dim.description.as_ref().is_none_or(|d| d.trim().is_empty()) {
                    dim.description = Some(k.clone());
                }
                Ok((k.clone(), dim))
            })
            .collect::<Result<_, FormatError>>()?;

        let overrides = contexts
            .into_iter()
            .map(|ctx| -> Result<_, FormatError> {
                let override_key = ctx.override_with_keys.get_key();
                let overrides =
                    overrides.get(override_key).cloned().ok_or_else(|| {
                        JsonFormat::serialization_error(format!(
                            "Missing override '{}' for context '{}'",
                            override_key, ctx.id
                        ))
                    })?;

                Ok(JsonContext {
                    context: ctx.condition.into_inner(),
                    description: context_descriptions.get(&ctx.id).cloned(),
                    overrides: overrides.into(),
                })
            })
            .collect::<Result<_, _>>()?;

        for (k, info) in default_configs.iter_mut() {
            // Description is mandatory in the exported file; fall back to the
            // key name when it is missing.
            if info.description.trim().is_empty() {
                info.description = k.clone();
            }
        }

        Ok(Self {
            default_configs,
            dimensions,
            overrides,
        })
    }
}

impl ConfigFormat for JsonFormat {
    fn parse_into_detailed(input: &str) -> Result<DetailedConfig, FormatError> {
        let json_config: JsonConfig = serde_json::from_str(input)
            .map_err(|e| Self::syntax_error(e.to_string(), None))?;
        DetailedConfig::try_from(json_config)
    }

    fn serialize(detailed_config: DetailedConfig) -> Result<String, FormatError> {
        let json_config = JsonConfig::try_from(detailed_config)?;
        serde_json::to_string_pretty(&json_config)
            .map_err(|e| Self::serialization_error(e.to_string()))
    }

    fn format_name() -> MarkupFormat {
        MarkupFormat::Json
    }
}
