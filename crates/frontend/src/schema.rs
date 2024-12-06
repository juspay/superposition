use std::str::FromStr;

use derive_more::{Deref, DerefMut};
use serde_json::{json, Map, Value};

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
                    .map(|(k, v)| format!("\"{}\": {}", k, v.to_string()))
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
    Debug, Clone, PartialEq, PartialOrd, strum_macros::Display, strum_macros::EnumString,
)]
#[strum(serialize_all = "lowercase")]
pub enum JsonSchemaType {
    Boolean,
    Number,
    String,
    Integer,
    Array,
    Object,
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

    pub fn try_from_schema_json(schema: Map<String, Value>) -> Result<Self, String> {
        let type_ = schema
            .get("type")
            .ok_or("type not defined in schema".to_string())?;

        match type_ {
            Value::Array(arr) => SchemaType::parse_from_array(arr),
            Value::String(s) => SchemaType::parse_from_string(s),
            _ => Err("type should be either a string or an array of strings".to_string()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Deref, DerefMut)]
pub struct EnumVariants(pub Vec<Value>);

impl EnumVariants {
    pub fn try_from_schema_json(
        schema: Map<String, Value>,
    ) -> Option<Result<Self, String>> {
        schema.get("enum").map(|type_| match type_ {
            Value::Array(arr) => Ok(EnumVariants(arr.clone())),
            _ => Err("enum should be an array of options".to_string()),
        })
    }
}
