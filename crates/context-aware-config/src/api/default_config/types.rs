use serde::{Deserialize, Deserializer};
use serde_json::{Map, Value};

#[derive(Debug, Deserialize)]
pub struct CreateReq {
    #[serde(default, deserialize_with = "deserialize_option")]
    pub value: Option<Value>,
    pub schema: Option<Map<String, Value>>,
    #[serde(default, deserialize_with = "deserialize_option")]
    pub function_name: Option<Value>,
}

fn deserialize_option<'de, D>(deserializer: D) -> Result<Option<Value>, D::Error>
where
    D: Deserializer<'de>,
{
    let value: Value = Deserialize::deserialize(deserializer)?;
    Ok(Some(value))
}
