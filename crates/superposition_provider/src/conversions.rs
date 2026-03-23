use std::collections::HashMap;

use aws_smithy_types::Document;
use serde_json::{Map, Value};

pub fn value_to_document(value: Value) -> Document {
    match value {
        Value::Null => Document::Null,
        Value::Bool(b) => Document::Bool(b),
        Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Document::Number(aws_smithy_types::Number::NegInt(i))
            } else if let Some(u) = n.as_u64() {
                Document::Number(aws_smithy_types::Number::PosInt(u))
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
                    serde_json::Number::from_f64(val)
                        .unwrap_or_else(|| serde_json::Number::from(0)),
                ),
            }
        }
        Document::String(s) => Value::String(s),
        Document::Bool(b) => Value::Bool(b),
        Document::Null => Value::Null,
    }
}
