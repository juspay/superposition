use std::str::FromStr;

use crate::{
    types::{Context, VariantType},
    utils::get_variable_name_and_value_2,
};
use derive_more::{Deref, DerefMut};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

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
            SchemaType::Multiple(_) => json!(""),
            SchemaType::Single(JsonSchemaType::String) => json!(""),
            SchemaType::Single(JsonSchemaType::Number) => json!(0),
            SchemaType::Single(JsonSchemaType::Integer) => json!(0),
            SchemaType::Single(JsonSchemaType::Boolean) => json!(false),
            SchemaType::Single(JsonSchemaType::Object) => json!({}),
            SchemaType::Single(JsonSchemaType::Array) => json!([]),
            SchemaType::Single(JsonSchemaType::Null) => json!("null"),
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

impl TryFrom<Value> for SchemaType {
    type Error = String;
    fn try_from(schema: Value) -> Result<Self, Self::Error> {
        schema
            .as_object()
            .ok_or("schema is not an object".to_string())
            .and_then(|obj| {
                let type_ = obj
                    .get("type")
                    .ok_or("type not defined in schema".to_string())?;

                match type_ {
                    Value::Array(arr) => SchemaType::parse_from_array(arr),
                    Value::String(s) => SchemaType::parse_from_string(s),
                    _ => Err("type should be either a string or an array of strings"
                        .to_string()),
                }
            })
    }
}

#[derive(Debug, Clone, PartialEq, Deref, DerefMut)]
pub struct EnumVariants(pub Vec<Value>);

impl TryFrom<Value> for EnumVariants {
    type Error = String;
    fn try_from(schema: Value) -> Result<Self, Self::Error> {
        schema
            .as_object()
            .ok_or("schema is not an object".to_string())
            .and_then(|obj| {
                let type_ = obj.get("enum").cloned().unwrap_or(Value::Array(vec![]));

                match type_ {
                    Value::Array(arr) => Ok(EnumVariants(arr)),
                    _ => Err("enum should be an array of options".to_string()),
                }
            })
    }
}

#[derive(Debug, Clone, Deref, DerefMut, Deserialize, Serialize)]
pub struct ContextForm(pub Vec<(String, String, String)>);

#[derive(Debug, Clone, Deref, DerefMut, Deserialize, Serialize)]
pub struct OverrideForm(pub Vec<(String, serde_json::Value)>);

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct VariantForm {
    pub overrides: OverrideForm,
    pub variant_type: VariantType,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ExperimentForm {
    pub name: String,
    pub context: ContextForm,
    pub variants: Vec<VariantForm>,
}

impl TryFrom<Context> for ContextForm {
    type Error = String;

    fn try_from(value: Context) -> Result<Self, Self::Error> {
        let context = value.condition;

        let conditions = match context.get("and") {
            Some(conditions_json) => conditions_json
                .as_array()
                .ok_or("An error occurred while extracting dimensions: failed parsing conditions as an array".to_string())?
                .clone(),
            None => vec![context.clone()],
        };

        let mut condition_tuples = Vec::new();
        for condition in &conditions {
            let condition_obj = condition
                .as_object()
                .ok_or("failed to parse condition as an object".to_string())?;
            let operators = condition_obj.keys();

            for operator in operators {
                let operands = condition_obj[operator]
                    .as_array()
                    .ok_or("failed to parse operands as an arrays".to_string())?;

                let (vname, vvalue) = get_variable_name_and_value_2(operands)?;

                let value = match operator.as_str() {
                    "==" => vvalue
                        .get(0)
                        .map(|v| format!("{v}"))
                        .unwrap_or(String::new()),
                    "<=" => vvalue
                        .iter()
                        .map(|v| format!("{v}"))
                        .collect::<Vec<String>>()
                        .join(","),
                    "in" => vvalue
                        .get(0)
                        .map(|v| format!("{v}"))
                        .unwrap_or(String::new()),
                    _ => String::new(),
                };

                condition_tuples.push((String::from(vname), operator.to_owned(), value));
            }
        }
        Ok(ContextForm(condition_tuples))
    }
}
