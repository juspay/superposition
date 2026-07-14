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

pub fn evaluation_context_to_document(value: EvaluationContextFieldValue) -> Document {
    match value {
        EvaluationContextFieldValue::Bool(b) => Document::Bool(b),
        EvaluationContextFieldValue::Int(i) => {
            Document::Number(aws_smithy_types::Number::NegInt(i))
        }
        EvaluationContextFieldValue::Float(f) => {
            Document::Number(aws_smithy_types::Number::Float(f))
        }
        EvaluationContextFieldValue::String(s) => Document::String(s),
        EvaluationContextFieldValue::DateTime(dt) => Document::String(dt.to_string()),
        EvaluationContextFieldValue::Struct(s) => {
            let struct_map = s
                .downcast_ref::<HashMap<String, EvaluationContextFieldValue>>()
                .map(|m| {
                    m.iter()
                        .map(|(k, v)| (k.clone(), evaluation_context_to_document(v.clone())))
                        .collect()
                })
                .unwrap_or_else(|| {
                    log::warn!("Failed to downcast struct value to expected HashMap format, got {:?}. Returning empty object.", s.type_id());
                    HashMap::new()
                });
            Document::Object(struct_map)
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

pub fn evaluation_context_to_query_document(
    ctx: EvaluationContext,
) -> (HashMap<String, Document>, Option<String>) {
    let context = ctx
        .custom_fields
        .into_iter()
        .map(|(k, v)| (k, evaluation_context_to_document(v)))
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

/// Convert serde_json Value to OpenFeature StructValue.
///
/// Only JSON objects convert. A top-level array cannot: `resolve_struct` must return a
/// `StructValue`, which has no array form. Callers get a TYPE_MISMATCH rather than the
/// index-keyed map (`{"0": .., "1": ..}`) this used to fabricate, which was silently not
/// the value the flag held. The Java and Python clients *can* return a top-level array,
/// because their SDKs' object type admits one; this is an upstream limitation of
/// `open_feature`, not a deliberate difference.
pub fn value_to_struct(value: Value) -> Result<open_feature::StructValue> {
    match value {
        Value::Object(map) => Ok(open_feature::StructValue {
            fields: object_fields(map)?,
        }),
        _ => Err(SuperpositionError::ConfigError(format!(
            "Cannot convert {:?} to StructValue - flag must be an object",
            value
        ))),
    }
}

/// Convert a JSON object's entries to OpenFeature fields, dropping null-valued keys.
///
/// `open_feature::Value` has no null variant. Dropping the key leaves the rest of the
/// object readable; erroring would make one null field render the entire flag
/// unresolvable, which is what this used to do.
fn object_fields(map: Map<String, Value>) -> Result<HashMap<String, open_feature::Value>> {
    let mut fields = HashMap::new();
    for (k, v) in map {
        if v.is_null() {
            log::debug!(
                "Dropping null-valued field '{k}': OpenFeature has no null representation"
            );
            continue;
        }
        fields.insert(k, value_to_openfeature_value(v)?);
    }
    Ok(fields)
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
        Value::Object(map) => Ok(open_feature::Value::Struct(open_feature::StructValue {
            fields: object_fields(map)?,
        })),
        // Reachable only for a null *array element*: object fields are filtered out by
        // `object_fields` first. Dropping it here would shift every later index, so this
        // stays an error.
        Value::Null => Err(SuperpositionError::ConfigError(
            "Cannot convert null to OpenFeature value".to_string(),
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn an_object_flag_keeps_its_field_types() {
        let fields = value_to_struct(json!({"tier": "gold", "credits": 5, "ratio": 1.5}))
            .expect("an object converts")
            .fields;

        assert_eq!(fields["tier"], open_feature::Value::String("gold".into()));
        assert_eq!(fields["credits"], open_feature::Value::Int(5));
        assert_eq!(fields["ratio"], open_feature::Value::Float(1.5));
    }

    #[test]
    fn a_null_field_is_dropped_rather_than_failing_the_whole_flag() {
        let fields = value_to_struct(json!({"tier": "gold", "promo": null}))
            .expect("a null field does not fail the flag")
            .fields;

        assert_eq!(fields["tier"], open_feature::Value::String("gold".into()));
        assert!(!fields.contains_key("promo"));
    }

    #[test]
    fn a_top_level_array_is_a_type_mismatch_not_an_index_keyed_map() {
        // This used to succeed, fabricating StructValue { "0": .., "1": .. } — a value the
        // flag never held. Java and Python return the array itself; `StructValue` cannot.
        assert!(value_to_struct(json!([1, 2, 3])).is_err());
    }

    #[test]
    fn an_array_nested_in_an_object_needs_no_special_handling() {
        let fields = value_to_struct(json!({"tags": ["a", "b"]}))
            .expect("a nested array converts")
            .fields;

        assert_eq!(
            fields["tags"],
            open_feature::Value::Array(vec![
                open_feature::Value::String("a".into()),
                open_feature::Value::String("b".into()),
            ])
        );
    }

    #[test]
    fn a_primitive_is_not_an_object() {
        assert!(value_to_struct(json!("Rupee")).is_err());
        assert!(value_to_struct(json!(true)).is_err());
        assert!(value_to_struct(json!(10)).is_err());
    }
}
