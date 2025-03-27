use chrono::{offset::Utc, DateTime};
use serde::{Deserialize, Serialize};
use serde_json::Value;

use super::models::cac::{Dimension, Position};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DimensionWithMandatory {
    pub dimension: String,
    pub position: Position,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
    pub schema: Value,
    pub function_name: Option<String>,
    pub last_modified_at: DateTime<Utc>,
    pub last_modified_by: String,
    pub mandatory: bool,
    pub description: String,
    pub change_reason: String,
}

impl DimensionWithMandatory {
    pub fn new(value: Dimension, mandatory: bool) -> Self {
        Self {
            dimension: value.dimension,
            position: value.position,
            created_at: value.created_at,
            created_by: value.created_by,
            schema: value.schema,
            function_name: value.function_name,
            last_modified_at: value.last_modified_at,
            last_modified_by: value.last_modified_by,
            mandatory,
            description: value.description,
            change_reason: value.change_reason,
        }
    }
}
