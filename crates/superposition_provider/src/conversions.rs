use std::collections::HashMap;

use aws_smithy_types::Document;
use open_feature::{EvaluationContext, EvaluationContextFieldValue};
use serde_json::{Map, Value};

use crate::{types::Result, SuperpositionError};

pub fn value_to_document(value: Value) -> Document {
    match value {
        Value::Null => Document::Null,
        Value::Bool(b) => Document::Bool(b),
        Value::Number(n) => {
            if let Some(u) = n.as_u64() {
                Document::Number(aws_smithy_types::Number::PosInt(u))
            } else if let Some(i) = n.as_i64() {
                Document::Number(aws_smithy_types::Number::NegInt(i))
            } else if let Some(f) = n.as_f64() {
                Document::Number(aws_smithy_types::Number::Float(f))
            } else {
                Document::Null
            }
        }
        Value::String(s) => Document::String(s),
        Value::Array(arr) => {
            Document::Array(arr.into_iter().map(value_to_document).collect())
        }
        Value::Object(obj) => {
            let map = obj
                .into_iter()
                .map(|(k, v)| (k, value_to_document(v)))
                .collect();
            Document::Object(map)
        }
    }
}

pub fn map_to_hashmap(map: Map<String, Value>) -> HashMap<String, Document> {
    map.into_iter()
        .map(|(k, v)| (k, value_to_document(v)))
        .collect()
}

pub fn hashmap_to_map(hashmap: HashMap<String, Document>) -> Map<String, Value> {
    hashmap
        .into_iter()
        .map(|(k, v)| (k, document_to_value(v)))
        .collect()
}

/// Recursively convert AWS Smithy Document to serde_json::Value by properly matching variants
pub fn document_to_value(doc: Document) -> Value {
    match doc {
        Document::Object(obj) => {
            let hashmap = obj
                .into_iter()
                .map(|(k, v)| (k, document_to_value(v)))
                .collect();
            Value::Object(hashmap)
        }
        Document::Array(arr) => {
            Value::Array(arr.into_iter().map(document_to_value).collect())
        }
        Document::Number(num) => {
            use aws_smithy_types::Number;
            match num {
                Number::PosInt(val) => Value::Number(serde_json::Number::from(val)),
                Number::NegInt(val) => Value::Number(serde_json::Number::from(val)),
                Number::Float(val) => Value::Number(
                    serde_json::Number::from_f64(val).unwrap_or_else(|| {
                        log::warn!(
                            "Failed to convert float {} to JSON number, using 0 instead",
                            val
                        );
                        serde_json::Number::from(0)
                    }),
                ),
            }
        }
        Document::String(s) => Value::String(s),
        Document::Bool(b) => Value::Bool(b),
        Document::Null => Value::Null,
    }
}

pub fn evaluation_context_to_value(value: EvaluationContextFieldValue) -> Value {
    match value {
        EvaluationContextFieldValue::Bool(b) => Value::Bool(b),
        EvaluationContextFieldValue::Int(i) => Value::Number(serde_json::Number::from(i)),
        EvaluationContextFieldValue::Float(f) => {
            Value::Number(serde_json::Number::from_f64(f).unwrap_or_else(|| {
                log::warn!(
                    "Failed to convert float {} to JSON number, using 0 instead",
                    f
                );
                serde_json::Number::from(0)
            }))
        }
        EvaluationContextFieldValue::String(s) => Value::String(s),
        EvaluationContextFieldValue::DateTime(dt) => Value::String(dt.to_string()),
        EvaluationContextFieldValue::Struct(s) => {
            // Convert struct to serde_json::Value
            let struct_map = s
                .downcast_ref::<HashMap<String, EvaluationContextFieldValue>>()
                .map(|m| {
                    m.iter()
                        .map(|(k, v)| (k.clone(), evaluation_context_to_value(v.clone())))
                        .collect()
                })
                .unwrap_or_else(|| {
                    log::warn!("Failed to downcast struct value to expected HashMap format, got {:?}. Returning empty object.", s.type_id());
                    Map::new()
                });
            Value::Object(struct_map)
        }
    }
}

/// Convert an EvaluationContext into a (Map<String, Value>, Option<String>) tuple
/// containing the custom fields as serde values and the targeting key.
/// This is used by both local and remote providers.
pub fn evaluation_context_to_query(
    ctx: EvaluationContext,
) -> (Map<String, Value>, Option<String>) {
    let context = ctx
        .custom_fields
        .into_iter()
        .map(|(k, v)| (k, evaluation_context_to_value(v)))
        .collect();

    (context, ctx.targeting_key)
}

/// Convert evaluation context to dimension data format expected by superposition_types
pub fn evaluation_context_to_map(context: EvaluationContext) -> Map<String, Value> {
    let mut dimension_data = Map::new();

    // Add targeting key if present
    if let Some(targeting_key) = context.targeting_key {
        dimension_data.insert("targeting_key".to_string(), Value::String(targeting_key));
    }

    // Add all other fields from the context
    for (key, value) in context.custom_fields {
        dimension_data.insert(key, evaluation_context_to_value(value));
    }

    log::debug!(
        "Converted evaluation context to dimension data with {} keys",
        dimension_data.len()
    );
    dimension_data
}

/// Convert serde_json Value to OpenFeature StructValue
pub fn value_to_struct(value: Value) -> Result<open_feature::StructValue> {
    match value {
        Value::Object(map) => {
            let mut fields = HashMap::new();
            for (k, v) in map {
                let open_feature_value = value_to_openfeature_value(v)?;
                fields.insert(k, open_feature_value);
            }
            // StructValue is just a struct with a fields HashMap, not a complex conversion
            Ok(open_feature::StructValue { fields })
        }
        Value::Array(list) => {
            let mut fields = HashMap::new();
            for (index, item) in list.into_iter().enumerate() {
                let open_feature_value = value_to_openfeature_value(item)?;
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
pub fn value_to_openfeature_value(value: Value) -> Result<open_feature::Value> {
    match value {
        Value::Bool(b) => Ok(open_feature::Value::Bool(b)),
        Value::String(s) => Ok(open_feature::Value::String(s)),
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
        Value::Array(arr) => Ok(open_feature::Value::Array(
            arr.into_iter()
                .map(value_to_openfeature_value)
                .collect::<Result<Vec<_>>>()?,
        )),
        Value::Object(map) => {
            let fields = map
                .into_iter()
                .map(|(k, v)| Ok((k, value_to_openfeature_value(v)?)))
                .collect::<Result<HashMap<String, open_feature::Value>>>()?;

            // Create StructValue directly with fields HashMap
            let struct_value = open_feature::StructValue { fields };
            Ok(open_feature::Value::Struct(struct_value))
        }
        Value::Null => Err(SuperpositionError::ConfigError(
            "Cannot convert null to OpenFeature value".to_string(),
        )),
    }
}
