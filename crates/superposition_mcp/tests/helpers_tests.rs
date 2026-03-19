use aws_smithy_types::{Document, Number};
use std::collections::HashMap;

// We test the helper functions directly through the public module
use superposition_mcp::helpers::{
    doc_map_to_json, doc_to_json, format_datetime, json_to_doc, json_to_doc_map,
};

#[test]
fn test_json_to_doc_null() {
    let doc = json_to_doc(serde_json::Value::Null);
    assert!(matches!(doc, Document::Null));
}

#[test]
fn test_json_to_doc_bool() {
    let doc = json_to_doc(serde_json::Value::Bool(true));
    assert!(matches!(doc, Document::Bool(true)));
}

#[test]
fn test_json_to_doc_string() {
    let doc = json_to_doc(serde_json::json!("hello"));
    assert!(matches!(doc, Document::String(s) if s == "hello"));
}

#[test]
fn test_json_to_doc_number_int() {
    let doc = json_to_doc(serde_json::json!(42));
    match doc {
        Document::Number(Number::NegInt(n)) => assert_eq!(n, 42),
        _ => panic!("Expected NegInt, got {:?}", doc),
    }
}

#[test]
fn test_json_to_doc_number_float() {
    let doc = json_to_doc(serde_json::json!(3.14));
    match doc {
        Document::Number(Number::Float(f)) => assert!((f - 3.14).abs() < f64::EPSILON),
        _ => panic!("Expected Float, got {:?}", doc),
    }
}

#[test]
fn test_json_to_doc_array() {
    let doc = json_to_doc(serde_json::json!([1, "two", true]));
    match doc {
        Document::Array(arr) => assert_eq!(arr.len(), 3),
        _ => panic!("Expected Array"),
    }
}

#[test]
fn test_json_to_doc_object() {
    let doc = json_to_doc(serde_json::json!({"key": "value"}));
    match doc {
        Document::Object(map) => {
            assert_eq!(map.len(), 1);
            assert!(matches!(map.get("key"), Some(Document::String(s)) if s == "value"));
        }
        _ => panic!("Expected Object"),
    }
}

#[test]
fn test_doc_to_json_roundtrip() {
    let original = serde_json::json!({
        "name": "test",
        "count": 42,
        "enabled": true,
        "tags": ["a", "b"],
        "nested": {"inner": "value"}
    });
    let doc = json_to_doc(original.clone());
    let result = doc_to_json(&doc);
    assert_eq!(original, result);
}

#[test]
fn test_doc_map_to_json() {
    let mut map = HashMap::new();
    map.insert("key".to_string(), Document::String("value".to_string()));
    map.insert("num".to_string(), Document::Number(Number::PosInt(10)));

    let json = doc_map_to_json(&map);
    assert!(json.is_object());
    let obj = json.as_object().unwrap();
    assert_eq!(obj.get("key").unwrap(), "value");
    assert_eq!(obj.get("num").unwrap(), 10);
}

#[test]
fn test_json_to_doc_map_valid() {
    let val = serde_json::json!({"a": 1, "b": "two"});
    let result = json_to_doc_map(val);
    assert!(result.is_ok());
    let map = result.unwrap();
    assert_eq!(map.len(), 2);
}

#[test]
fn test_json_to_doc_map_invalid() {
    let val = serde_json::json!([1, 2, 3]);
    let result = json_to_doc_map(val);
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), "Expected a JSON object");
}

#[test]
fn test_format_datetime() {
    let dt = aws_smithy_types::DateTime::from_secs(1700000000);
    let formatted = format_datetime(&dt);
    // Should produce an ISO 8601 date string
    assert!(formatted.contains("2023"));
    assert!(formatted.contains("T"));
}
