use std::collections::HashMap;

use aws_smithy_types::Document;
use log::debug;
use serde_json::{json, Map, Value};
use superposition_core::experiment::{ExperimentGroups, FfiExperimentGroup};
use superposition_core::{Experiments, FfiExperiment};
use superposition_sdk::operation::list_experiment_groups::ListExperimentGroupsOutput;
use superposition_sdk::types::GroupType as SdkGroupType;
use superposition_types::database::models::cac::{DependencyGraph, DimensionType};
use superposition_types::database::models::experimentation::{
    Bucket, Buckets, GroupType as DbGroupType, Variant, VariantType, Variants,
};
use superposition_types::{
    Cac, Condition, Config, Context, DimensionInfo, Exp, ExtendedMap, OverrideWithKeys,
    Overrides,
};

use crate::types::*;

pub struct ConversionUtils;

impl ConversionUtils {
    pub fn convert_get_config_response(
        response: &superposition_sdk::operation::get_config::GetConfigOutput,
    ) -> Result<Config> {
        debug!("Converting get_config response to superposition_types::Config");

        // Convert default configs - these are already Value types
        let default_configs =
            Self::convert_condition_document(response.default_configs())?;

        // Convert overrides - HashMap<String, HashMap<String, Document>>
        let overrides = {
            let mut result_map = HashMap::new();
            for (override_key, inner_map) in response.overrides() {
                let override_values = Self::convert_condition_document(inner_map)?;

                // Create Overrides directly from Map<String, Value>
                let overrides_obj = Cac::<Overrides>::try_from(override_values)
                    .map_err(|e| SuperpositionError::SerializationError(e.to_string()))?;

                result_map.insert(override_key.clone(), overrides_obj.into_inner());
            }
            result_map
        };

        // Convert contexts - Vec<ContextPartial>
        let contexts = response
            .contexts()
            .iter()
            .map(|context_partial| {
                // Convert condition Document to Map<String, Value>
                let condition_map =
                    Self::convert_condition_document(context_partial.condition())?;

                // Create Condition directly from Map<String, Value>
                let condition =
                    Cac::<Condition>::try_from(condition_map).map_err(|e| {
                        SuperpositionError::SerializationError(format!(
                            "Invalid condition: {}",
                            e
                        ))
                    })?;

                let override_with_keys = OverrideWithKeys::try_from(
                    context_partial.override_with_keys().to_vec(),
                )
                .map_err(|e| {
                    SuperpositionError::SerializationError(format!(
                        "Invalid override_with_keys: {e}",
                    ))
                })?;

                Ok(Context {
                    id: context_partial.id().to_string(),
                    condition: condition.into_inner(),
                    priority: context_partial.priority(),
                    weight: context_partial.weight(),
                    override_with_keys,
                })
            })
            .collect::<Result<Vec<Context>>>()?;

        let dimensions = response
            .dimensions()
            .iter()
            .map(|(key, dimension_info)| {
                let schema = dimension_info
                    .schema()
                    .iter()
                    .map(|(k, v)| Self::document_to_value(v).map(|val| (k.clone(), val)))
                    .collect::<Result<Map<String, Value>>>()?;
                let dim_info = DimensionInfo {
                    schema: ExtendedMap::from(schema),
                    position: dimension_info.position(),
                    dimension_type: Self::try_dimension_type(
                        dimension_info.dimension_type(),
                    )?,
                    dependency_graph: DependencyGraph(
                        dimension_info.dependency_graph().clone(),
                    ),
                    autocomplete_function_name: dimension_info
                        .autocomplete_function_name()
                        .map(String::from),
                };
                Ok((key.clone(), dim_info))
            })
            .collect::<Result<HashMap<String, DimensionInfo>>>()?;

        let config = Config {
            contexts,
            overrides,
            default_configs,
            dimensions,
        };

        debug!("Successfully converted config with {} contexts, {} overrides, {} default configs", 
               config.contexts.len(), config.overrides.len(), config.default_configs.len());

        Ok(config)
    }

    fn try_dimension_type(
        dim_type: &superposition_sdk::types::DimensionType,
    ) -> Result<DimensionType> {
        match dim_type {
            superposition_sdk::types::DimensionType::RemoteCohort(cohort_based_on) => {
                Ok(DimensionType::RemoteCohort(cohort_based_on.clone()))
            }
            superposition_sdk::types::DimensionType::LocalCohort(cohort_based_on) => {
                Ok(DimensionType::LocalCohort(cohort_based_on.clone()))
            }
            superposition_sdk::types::DimensionType::Regular => {
                Ok(DimensionType::Regular {})
            }
            _ => Err(SuperpositionError::SerializationError(
                "Unknown dimension type".to_string(),
            )),
        }
    }

    pub fn convert_value_to_config(map: &Map<String, Value>) -> Result<Config> {
        // Extract contexts array
        let contexts =
            map.get("contexts")
                .and_then(|v| v.as_array())
                .ok_or_else(|| {
                    SuperpositionError::ConfigError(
                        "Missing or invalid 'contexts' field".to_string(),
                    )
                })?;

        let parsed_contexts: Result<Vec<Context>> = contexts
            .iter()
            .map(|context_val| {
                let context_obj = context_val.as_object().ok_or_else(|| {
                    SuperpositionError::ConfigError(
                        "Context must be an object".to_string(),
                    )
                })?;

                // Extract required fields
                let id = context_obj
                    .get("id")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| {
                        SuperpositionError::ConfigError(
                            "Missing or invalid 'id' field in context".to_string(),
                        )
                    })?
                    .to_string();

                let priority = context_obj
                    .get("priority")
                    .and_then(|v| v.as_i64())
                    .ok_or_else(|| {
                        SuperpositionError::ConfigError(
                            "Missing or invalid 'priority' field in context".to_string(),
                        )
                    })? as i32;

                let weight = context_obj
                    .get("weight")
                    .and_then(|v| v.as_i64())
                    .ok_or_else(|| {
                        SuperpositionError::ConfigError(
                            "Missing or invalid 'weight' field in context".to_string(),
                        )
                    })? as i32;

                let override_with_keys: Vec<String> = context_obj
                    .get("override_with_keys")
                    .and_then(|v| v.as_array())
                    .ok_or_else(|| {
                        SuperpositionError::ConfigError(
                            "Missing or invalid 'override_with_keys' field in context"
                                .to_string(),
                        )
                    })?
                    .iter()
                    .filter_map(|v| v.as_str().map(String::from))
                    .collect();
                let override_with_keys = OverrideWithKeys::try_from(override_with_keys)
                    .map_err(|e| {
                    SuperpositionError::ConfigError(format!(
                        "Invalid override_with_keys: {e}",
                    ))
                })?;

                // Extract condition
                let condition_map = context_obj
                    .get("condition")
                    .and_then(|v| v.as_object())
                    .ok_or_else(|| {
                        SuperpositionError::ConfigError(
                            "Missing or invalid 'condition' field in context".to_string(),
                        )
                    })?
                    .clone();

                let condition = Cac::<Condition>::try_from(condition_map)
                    .map_err(|e| {
                        SuperpositionError::SerializationError(format!(
                            "Invalid condition: {}",
                            e
                        ))
                    })?
                    .into_inner();

                Ok(Context {
                    id,
                    condition,
                    priority,
                    weight,
                    override_with_keys,
                })
            })
            .collect();

        let contexts = parsed_contexts?;

        // Extract overrides object
        let overrides_obj = map
            .get("overrides")
            .and_then(|v| v.as_object())
            .ok_or_else(|| {
                SuperpositionError::ConfigError(
                    "Missing or invalid 'overrides' field".to_string(),
                )
            })?;

        let mut overrides: HashMap<String, Overrides> = HashMap::new();
        for (key, value) in overrides_obj {
            let override_map = value
                .as_object()
                .ok_or_else(|| {
                    SuperpositionError::ConfigError(format!(
                        "Override '{}' must be an object",
                        key
                    ))
                })?
                .clone();

            let override_obj = Cac::<Overrides>::try_from(override_map)
                .map_err(|e| {
                    SuperpositionError::SerializationError(format!(
                        "Invalid override '{}': {}",
                        key, e
                    ))
                })?
                .into_inner();

            overrides.insert(key.clone(), override_obj);
        }

        // Extract default_configs object
        let default_configs = map
            .get("default_configs")
            .and_then(|v| v.as_object())
            .ok_or_else(|| {
                SuperpositionError::ConfigError(
                    "Missing or invalid 'default_configs' field".to_string(),
                )
            })?
            .clone();

        let dimensions = map
            .get("dimensions")
            .and_then(|v| v.as_object())
            .map(|dim| {
                dim.iter()
                    .map(|(key, value)| {
                        let dim_info: DimensionInfo =
                            serde_json::from_value(value.clone()).map_err(|e| {
                                SuperpositionError::SerializationError(format!(
                                    "Invalid dimension info for '{}': {}",
                                    key, e
                                ))
                            })?;
                        Ok((key.clone(), dim_info))
                    })
                    .collect::<Result<HashMap<String, DimensionInfo>>>()
            })
            .unwrap_or_else(|| Ok(HashMap::new()))?;

        Ok(Config {
            contexts,
            overrides,
            default_configs,
            dimensions,
        })
    }

    fn convert_condition_document(
        context: &HashMap<String, Document>,
    ) -> Result<Map<String, Value>> {
        let mut condition_map = Map::new();
        for (key, doc) in context {
            let value = Self::document_to_value(doc)?;
            condition_map.insert(key.clone(), value);
        }
        Ok(condition_map)
    }

    /// Convert list_experiment SDK response to structured experiment data
    pub fn convert_experiments_response(
        response: &superposition_sdk::operation::list_experiment::ListExperimentOutput,
    ) -> Result<Experiments> {
        debug!("Converting experiments response");

        let exp_list = response.data();
        let mut trimmed_exp_list: Experiments = Vec::new();

        for exp in exp_list {
            // Convert experiment context (condition)
            let condition_map = Self::convert_condition_document(exp.context())?;

            // Convert variants
            let mut variants: Variants = Variants::new(vec![]);
            for variant in exp.variants() {
                let variant_type = match variant.variant_type() {
                    superposition_sdk::types::VariantType::Control => {
                        VariantType::CONTROL
                    }
                    superposition_sdk::types::VariantType::Experimental => {
                        VariantType::EXPERIMENTAL
                    }
                    _ => {
                        return Err(SuperpositionError::SerializationError(
                            "Unknown variant type".to_string(),
                        ))
                    }
                };

                // Convert variant overrides - check if overrides exist
                let overrides_map = Self::hashmap_to_map(variant.overrides())?;

                let override_ = Exp::<Overrides>::try_from(overrides_map)
                    .map_err(|e| SuperpositionError::SerializationError(e.to_string()))?;

                let variant_value = Variant {
                    id: variant.id.clone(),
                    variant_type,
                    context_id: variant.context_id.clone(),
                    override_id: variant.override_id.clone(),
                    overrides: override_,
                };
                variants.push(variant_value);
            }
            let context = Exp::<Condition>::try_from(condition_map)
                .map_err(|e| {
                    SuperpositionError::SerializationError(format!(
                        "Invalid condition: {}",
                        e
                    ))
                })?
                .into_inner();
            let experiment = FfiExperiment {
                id: exp.id.clone(),
                context,
                variants,
                traffic_percentage: exp.traffic_percentage as u8,
            };

            trimmed_exp_list.push(experiment);
        }

        Ok(trimmed_exp_list)
    }

    pub fn convert_experiment_groups_response(
        response: &ListExperimentGroupsOutput,
    ) -> Result<ExperimentGroups> {
        debug!("Converting experiment groups response");

        let group_list = response.data();
        let mut trimmed_group_list: ExperimentGroups = Vec::new();

        for exp_group in group_list {
            // Convert experiment context (condition)
            let condition_map = Self::convert_condition_document(exp_group.context())?;

            let context = Exp::<Condition>::try_from(condition_map)
                .map_err(|e| {
                    SuperpositionError::SerializationError(format!(
                        "Invalid condition: {}",
                        e
                    ))
                })?
                .into_inner();
            let group_type = match exp_group.group_type {
                SdkGroupType::SystemGenerated => DbGroupType::SystemGenerated,
                SdkGroupType::UserCreated => DbGroupType::UserCreated,
                _ => {
                    return Err(SuperpositionError::SerializationError(
                        "Unknown group type".to_string(),
                    ))
                }
            };

            let experiment_group = FfiExperimentGroup {
                id: exp_group.id.clone(),
                context,
                traffic_percentage: exp_group.traffic_percentage as u8,
                member_experiment_ids: exp_group.member_experiment_ids().to_vec(),
                group_type,
                buckets: Buckets::try_from(
                    exp_group
                        .buckets
                        .iter()
                        .map(|b| {
                            b.as_ref().map(|bucket| Bucket {
                                variant_id: bucket.variant_id.clone(),
                                experiment_id: bucket.experiment_id.clone(),
                            })
                        })
                        .collect::<Vec<_>>(),
                )
                .map_err(SuperpositionError::SerializationError)?,
            };

            trimmed_group_list.push(experiment_group);
        }

        Ok(trimmed_group_list)
    }

    /// Convert AWS Smithy Document to serde_json::Value
    pub fn document_to_value(doc: &aws_smithy_types::Document) -> Result<Value> {
        Self::document_to_value_recursive(doc)
    }

    pub fn hashmap_to_map(
        hashmap: &HashMap<String, aws_smithy_types::Document>,
    ) -> Result<Map<String, Value>> {
        hashmap
            .iter()
            .map(|(k, v)| {
                let value = Self::document_to_value(v)?;
                Ok((k.clone(), value))
            })
            .collect()
    }

    /// Recursively convert AWS Smithy Document to serde_json::Value by properly matching variants
    fn document_to_value_recursive(doc: &aws_smithy_types::Document) -> Result<Value> {
        use aws_smithy_types::Document;

        match doc {
            Document::Object(obj) => {
                let mut map = Map::new();
                for (key, value) in obj {
                    let converted_value = Self::document_to_value_recursive(value)?;
                    map.insert(key.clone(), converted_value);
                }
                Ok(Value::Object(map))
            }
            Document::Array(arr) => {
                let mut vec = Vec::new();
                for item in arr {
                    let converted_item = Self::document_to_value_recursive(item)?;
                    vec.push(converted_item);
                }
                Ok(Value::Array(vec))
            }
            Document::Number(num) => {
                use aws_smithy_types::Number;
                match num {
                    Number::PosInt(val) => {
                        Ok(Value::Number(serde_json::Number::from(*val)))
                    }
                    Number::NegInt(val) => {
                        Ok(Value::Number(serde_json::Number::from(*val)))
                    }
                    Number::Float(val) => Ok(Value::Number(
                        serde_json::Number::from_f64(*val).ok_or_else(|| {
                            SuperpositionError::SerializationError(
                                "Invalid float value".into(),
                            )
                        })?,
                    )),
                }
            }
            Document::String(s) => Ok(Value::String(s.clone())),
            Document::Bool(b) => Ok(Value::Bool(*b)),
            Document::Null => Ok(Value::Null),
        }
    }

    pub fn convert_evaluation_context_value_to_serde_value(
        value: &open_feature::EvaluationContextFieldValue,
    ) -> Value {
        match value {
            open_feature::EvaluationContextFieldValue::Bool(b) => Value::Bool(*b),
            open_feature::EvaluationContextFieldValue::Int(i) => {
                Value::Number(serde_json::Number::from(*i))
            }
            open_feature::EvaluationContextFieldValue::Float(f) => json!(f),
            open_feature::EvaluationContextFieldValue::String(s) => {
                Value::String(s.clone())
            }
            open_feature::EvaluationContextFieldValue::DateTime(dt) => {
                Value::String(dt.to_string())
            }
            open_feature::EvaluationContextFieldValue::Struct(s) => {
                // Convert struct to serde_json::Value
                let struct_map: Map<String, Value> = s
                    .as_ref()
                    .downcast_ref::<Map<String, Value>>()
                    .cloned()
                    .unwrap_or_default();
                Value::Object(struct_map)
            }
        }
    }
    /// Convert evaluation context to dimension data format expected by superposition_types
    pub fn context_to_dimension_data(
        context: &open_feature::EvaluationContext,
    ) -> Map<String, Value> {
        let mut dimension_data = Map::new();

        // Add targeting key if present
        if let Some(targeting_key) = &context.targeting_key {
            dimension_data.insert(
                "targeting_key".to_string(),
                Value::String(targeting_key.to_string()),
            );
        }

        // Add all other fields from the context
        for (key, value) in &context.custom_fields {
            let serde_value =
                Self::convert_evaluation_context_value_to_serde_value(value);
            dimension_data.insert(key.clone(), serde_value);
        }

        debug!(
            "Converted evaluation context to dimension data with {} keys",
            dimension_data.len()
        );
        dimension_data
    }

    /// Convert Config back to the legacy format for compatibility with existing provider logic
    pub fn config_to_legacy_format(config: &Config) -> HashMap<String, Value> {
        let mut result = HashMap::new();

        // Convert default_configs
        result.insert(
            "default_configs".to_string(),
            Value::Object(config.default_configs.clone()),
        );

        // Convert overrides to the expected format
        let mut overrides_map = Map::new();
        for (key, overrides) in &config.overrides {
            let override_value: Map<String, Value> = overrides.clone().into();
            overrides_map.insert(key.clone(), Value::Object(override_value));
        }
        result.insert("overrides".to_string(), Value::Object(overrides_map));

        // Convert contexts
        let contexts_array: Vec<Value> = config
            .contexts
            .iter()
            .map(|context| {
                let condition_map: Map<String, Value> = context.condition.clone().into();
                serde_json::json!({
                    "id": context.id,
                    "priority": context.priority,
                    "weight": context.weight,
                    "override_with_keys": context.override_with_keys,
                    "condition": condition_map
                })
            })
            .collect();
        result.insert("contexts".to_string(), Value::Array(contexts_array));

        debug!(
            "Converted Config to legacy format with {} sections",
            result.len()
        );
        result
    }

    /// Evaluate config using superposition_types logic and return resolved values
    pub fn evaluate_config(
        config: &Config,
        dimension_data: &Map<String, Value>,
        prefix_filter: Option<&[String]>,
    ) -> Result<HashMap<String, Value>> {
        debug!(
            "Evaluating config with dimension data: {:?}",
            dimension_data.keys().collect::<Vec<_>>()
        );

        // Filter by dimensions first
        let filtered_config = config.filter_by_dimensions(dimension_data);
        debug!(
            "Filtered config has {} contexts after dimension filtering",
            filtered_config.contexts.len()
        );

        // Apply prefix filtering if specified
        let final_config = if let Some(prefixes) = prefix_filter {
            let prefix_set: std::collections::HashSet<String> =
                prefixes.iter().cloned().collect();
            filtered_config.filter_by_prefix(&prefix_set)
        } else {
            filtered_config
        };

        debug!(
            "Final config has {} contexts after prefix filtering",
            final_config.contexts.len()
        );

        // Start with default configs
        let mut result = final_config.default_configs.clone();

        // Apply overrides based on context priority (higher priority wins)
        let mut sorted_contexts = final_config.contexts.clone();
        sorted_contexts.sort_by_key(|c| std::cmp::Reverse(c.priority)); // Sort by priority descending

        for context in sorted_contexts {
            if let Some(override_key) = context.override_with_keys.first() {
                if let Some(overrides) = final_config.overrides.get(override_key) {
                    let override_map: Map<String, Value> = overrides.clone().into();
                    for (override_key, value) in override_map {
                        result.insert(override_key, value);
                        debug!("Applied override for key");
                    }
                }
            }
        }

        debug!(
            "Config evaluation completed with {} final keys",
            result.len()
        );

        // Convert Map<String, Value> to HashMap<String, Value>
        let final_result: HashMap<String, Value> = result.into_iter().collect();
        Ok(final_result)
    }

    /// Convert serde_json Value to boolean for OpenFeature provider
    pub fn serde_value_to_bool(value: &Value) -> Result<bool> {
        match value {
            Value::Bool(b) => Ok(*b),
            Value::String(s) => s.parse::<bool>().map_err(|_| {
                SuperpositionError::ConfigError(format!(
                    "Cannot convert string '{}' to boolean",
                    s
                ))
            }),
            _ => Err(SuperpositionError::ConfigError(format!(
                "Cannot convert {:?} to boolean",
                value
            ))),
        }
    }

    /// Convert serde_json Value to string for OpenFeature provider
    pub fn serde_value_to_string(value: &Value) -> Result<String> {
        match value {
            Value::String(s) => Ok(s.clone()),
            Value::Number(n) => Ok(n.to_string()),
            Value::Bool(b) => Ok(b.to_string()),
            _ => Err(SuperpositionError::ConfigError(format!(
                "Cannot convert {:?} to string",
                value
            ))),
        }
    }

    /// Convert serde_json Value to integer for OpenFeature provider
    pub fn serde_value_to_int(value: &Value) -> Result<i64> {
        match value {
            Value::Number(n) => n.as_i64().ok_or_else(|| {
                SuperpositionError::ConfigError(format!(
                    "Cannot convert number {} to i64",
                    n
                ))
            }),
            Value::String(s) => s.parse::<i64>().map_err(|_| {
                SuperpositionError::ConfigError(format!(
                    "Cannot convert string '{}' to i64",
                    s
                ))
            }),
            _ => Err(SuperpositionError::ConfigError(format!(
                "Cannot convert {:?} to i64",
                value
            ))),
        }
    }

    /// Convert serde_json Value to float for OpenFeature provider
    pub fn serde_value_to_float(value: &Value) -> Result<f64> {
        match value {
            Value::Number(n) => n.as_f64().ok_or_else(|| {
                SuperpositionError::ConfigError(format!(
                    "Cannot convert number {} to f64",
                    n
                ))
            }),
            Value::String(s) => s.parse::<f64>().map_err(|_| {
                SuperpositionError::ConfigError(format!(
                    "Cannot convert string '{}' to f64",
                    s
                ))
            }),
            _ => Err(SuperpositionError::ConfigError(format!(
                "Cannot convert {:?} to f64",
                value
            ))),
        }
    }

    /// Convert serde_json Value to OpenFeature StructValue
    pub fn serde_value_to_struct_value(
        value: &Value,
    ) -> Result<open_feature::StructValue> {
        match value {
            Value::Object(map) => {
                let mut fields = HashMap::new();
                for (k, v) in map {
                    let open_feature_value = Self::serde_value_to_openfeature_value(v)?;
                    fields.insert(k.clone(), open_feature_value);
                }
                // StructValue is just a struct with a fields HashMap, not a complex conversion
                Ok(open_feature::StructValue { fields })
            }
            Value::Array(list) => {
                let mut fields = HashMap::new();
                for (index, item) in list.iter().enumerate() {
                    let open_feature_value =
                        Self::serde_value_to_openfeature_value(item)?;
                    fields.insert(index.to_string(), open_feature_value);
                }
                Ok(open_feature::StructValue { fields })
            }
            _ => Err(SuperpositionError::ConfigError(format!(
                "Cannot convert {:?} to StructValue - flag must be an object/array",
                value
            ))),
        }
    }

    /// Convert serde_json Value to OpenFeature Value
    pub fn serde_value_to_openfeature_value(
        value: &Value,
    ) -> Result<open_feature::Value> {
        match value {
            Value::Bool(b) => Ok(open_feature::Value::Bool(*b)),
            Value::String(s) => Ok(open_feature::Value::String(s.clone())),
            Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    Ok(open_feature::Value::Int(i))
                } else if let Some(f) = n.as_f64() {
                    Ok(open_feature::Value::Float(f))
                } else {
                    Err(SuperpositionError::ConfigError(format!(
                        "Cannot convert number {} to OpenFeature value",
                        n
                    )))
                }
            }
            Value::Array(arr) => {
                let mut list = Vec::new();
                for item in arr {
                    list.push(Self::serde_value_to_openfeature_value(item)?);
                }
                // OpenFeature uses Array, not List
                Ok(open_feature::Value::Array(list))
            }
            Value::Object(map) => {
                let mut fields = HashMap::new();
                for (k, v) in map {
                    let open_feature_value = Self::serde_value_to_openfeature_value(v)?;
                    fields.insert(k.clone(), open_feature_value);
                }
                // Create StructValue directly with fields HashMap
                let struct_value = open_feature::StructValue { fields };
                Ok(open_feature::Value::Struct(struct_value))
            }
            Value::Null => Err(SuperpositionError::ConfigError(
                "Cannot convert null to OpenFeature value".to_string(),
            )),
        }
    }
}
