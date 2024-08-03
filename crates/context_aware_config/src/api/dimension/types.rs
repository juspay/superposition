use chrono::{DateTime, NaiveDateTime, Utc};
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::Value;

use crate::db::models::Dimension;

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

#[derive(Debug, Serialize, Deserialize)]
pub struct DimensionWithMandatory {
    pub dimension: String,
    pub priority: i32,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
    pub schema: Value,
    pub function_name: Option<String>,
    pub last_modified_at: NaiveDateTime,
    pub last_modified_by: String,
    pub mandatory: bool,
}

impl DimensionWithMandatory {
    pub fn new(value: Dimension, mandatory: bool) -> Self {
        DimensionWithMandatory {
            dimension: value.dimension,
            priority: value.priority,
            created_at: value.created_at,
            created_by: value.created_by,
            schema: value.schema,
            function_name: value.function_name,
            last_modified_at: value.last_modified_at,
            last_modified_by: value.last_modified_by,
            mandatory,
        }
    }
}
