use std::str::FromStr;

use derive_more::{Deref, DerefMut};
use serde_json::{Map, Value, json};

pub trait HtmlDisplay: ToString {
    fn html_display(&self) -> String;
}

impl HtmlDisplay for Value {
    fn html_display(&self) -> String {
        match self {
            Value::Bool(v) => v.to_string(),
            Value::Number(v) => v.to_string(),
            Value::String(v) => v.to_string(),
            Value::Null => String::from("null"),
            Value::Array(arr) => {
                let items: Vec<String> = arr.iter().map(|v| v.to_string()).collect();
                format!("[{}]", items.join(","))
            }
            Value::Object(obj) => {
                let items: Vec<String> = obj
                    .iter()
                    .map(|(k, v)| format!("\"{}\": {}", k, v))
                    .collect();
                format!("{{{}}}", items.join(", "))
            }
        }
    }
}

impl HtmlDisplay for String {
    fn html_display(&self) -> String {
        self.clone()
    }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    PartialOrd,
    strum_macros::Display,
    strum_macros::EnumString,
    Default,
)]
#[strum(serialize_all = "lowercase")]
pub enum JsonSchemaType {
    Boolean,
    Number,
    String,
    Integer,
    Array,
    Object,
    #[default]
    Null,
}

impl From<&Value> for JsonSchemaType {
    fn from(value: &Value) -> Self {
        match value {
            Value::String(_) => JsonSchemaType::String,
            Value::Number(_) => JsonSchemaType::Number,
            Value::Array(_) => JsonSchemaType::Array,
            Value::Object(_) => JsonSchemaType::Object,
            Value::Null => JsonSchemaType::Null,
            Value::Bool(_) => JsonSchemaType::Boolean,
        }
    }
}

impl JsonSchemaType {
    pub fn precedence(&self) -> u8 {
        match self {
            JsonSchemaType::Null | JsonSchemaType::Boolean | JsonSchemaType::Integer => 1,
            JsonSchemaType::Number | JsonSchemaType::Array | JsonSchemaType::Object => 2,
            JsonSchemaType::String => 3,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SchemaType {
    Multiple(Vec<JsonSchemaType>),
    Single(JsonSchemaType),
    /// Schema has no resolvable top-level `type` (e.g. `anyOf`/`oneOf`/`allOf`/`$ref`).
    /// The value is accepted as free-form JSON and validated against the full schema
    /// on the backend.
    Any,
}

impl Default for SchemaType {
    fn default() -> Self {
        Self::Single(JsonSchemaType::default())
    }
}

impl SchemaType {
    pub fn default_value(&self) -> Value {
        match self {
            SchemaType::Multiple(_) => Value::String(String::default()),
            SchemaType::Single(JsonSchemaType::String) => {
                Value::String(String::default())
            }
            SchemaType::Single(JsonSchemaType::Number) => json!(0),
            SchemaType::Single(JsonSchemaType::Integer) => json!(0),
            SchemaType::Single(JsonSchemaType::Boolean) => Value::Bool(bool::default()),
            SchemaType::Single(JsonSchemaType::Object) => Value::Object(Map::new()),
            SchemaType::Single(JsonSchemaType::Array) => Value::Array(Vec::new()),
            SchemaType::Single(JsonSchemaType::Null) => {
                Value::String(String::from("null"))
            }
            SchemaType::Any => Value::Null,
        }
    }
    fn parse_from_array(arr: &[Value]) -> Result<Self, String> {
        arr.iter()
            .map(|v| match v {
                Value::String(s) => JsonSchemaType::from_str(s),
                _ => Err(strum::ParseError::VariantNotFound),
            })
            .collect::<Result<Vec<JsonSchemaType>, _>>()
            .map_err(|_| "not a valid JsonSchema type".to_string())
            .map(SchemaType::Multiple)
    }

    fn parse_from_string(s: &str) -> Result<Self, String> {
        JsonSchemaType::from_str(s)
            .map_err(|_| "not a valid JsonSchema type".to_string())
            .map(SchemaType::Single)
    }
}

impl TryFrom<&Map<String, Value>> for SchemaType {
    type Error = String;
    fn try_from(schema: &Map<String, Value>) -> Result<Self, Self::Error> {
        // Schemas without a top-level `type` (e.g. `anyOf`/`oneOf`/`allOf`/`$ref`/bare
        // `const`) can't be mapped to a single input widget, so treat them as free-form
        // JSON. The value is still validated against the full schema on the backend.
        let Some(type_) = schema.get("type") else {
            return Ok(SchemaType::Any);
        };

        match type_ {
            Value::Array(arr) => SchemaType::parse_from_array(arr),
            Value::String(s) => SchemaType::parse_from_string(s),
            _ => Err("type should be either a string or an array of strings".to_string()),
        }
    }
}

impl TryFrom<Value> for SchemaType {
    type Error = String;
    fn try_from(schema: Value) -> Result<Self, Self::Error> {
        schema
            .as_object()
            .ok_or("schema is not an object".to_string())
            .and_then(SchemaType::try_from)
    }
}

#[derive(Debug, Clone, PartialEq, Deref, DerefMut)]
pub struct EnumVariants(pub Vec<Value>);

impl TryFrom<&Map<String, Value>> for EnumVariants {
    type Error = String;
    fn try_from(schema: &Map<String, Value>) -> Result<Self, Self::Error> {
        let type_ = schema.get("enum").cloned().unwrap_or(Value::Array(vec![]));

        match type_ {
            Value::Array(arr) => Ok(EnumVariants(arr)),
            _ => Err("enum should be an array of options".to_string()),
        }
    }
}

impl TryFrom<Value> for EnumVariants {
    type Error = String;
    fn try_from(schema: Value) -> Result<Self, Self::Error> {
        schema
            .as_object()
            .ok_or("schema is not an object".to_string())
            .and_then(EnumVariants::try_from)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn any_of_schema_without_type_resolves_to_any() {
        let schema = json!({
            "anyOf": [
                { "type": "string" },
                { "const": "default_str_ignore_this" }
            ]
        });
        assert_eq!(SchemaType::try_from(schema), Ok(SchemaType::Any));
    }

    #[test]
    fn other_type_less_schemas_resolve_to_any() {
        for schema in [
            json!({ "oneOf": [{ "type": "string" }] }),
            json!({ "allOf": [{ "type": "string" }] }),
            json!({ "$ref": "#/definitions/foo" }),
            json!({ "const": "foo" }),
        ] {
            assert_eq!(
                SchemaType::try_from(schema.clone()),
                Ok(SchemaType::Any),
                "schema {schema} should resolve to Any"
            );
        }
    }

    #[test]
    fn typed_schemas_still_resolve_to_concrete_types() {
        assert_eq!(
            SchemaType::try_from(json!({ "type": "string" })),
            Ok(SchemaType::Single(JsonSchemaType::String))
        );
        assert_eq!(
            SchemaType::try_from(json!({ "type": ["string", "number"] })),
            Ok(SchemaType::Multiple(vec![
                JsonSchemaType::String,
                JsonSchemaType::Number
            ]))
        );
    }

    #[test]
    fn invalid_type_still_errors() {
        assert!(SchemaType::try_from(json!({ "type": "foobar" })).is_err());
        assert!(SchemaType::try_from(json!({ "type": 42 })).is_err());
    }
}
