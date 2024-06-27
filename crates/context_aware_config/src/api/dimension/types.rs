use serde::{Deserialize, Deserializer};
use serde_json::Value;

#[derive(Debug, Deserialize)]
pub struct CreateReq {
    pub dimension: String,
    pub priority: i32,
    pub schema: Value,
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
