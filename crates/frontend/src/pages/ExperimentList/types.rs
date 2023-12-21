use chrono::{DateTime, Utc};
use derive_more::{Deref, DerefMut};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

#[derive(
    Debug, Clone, Copy, PartialEq, Deserialize, Serialize, strum_macros::Display,
)]
#[strum(serialize_all = "UPPERCASE")]
pub enum ExperimentStatusType {
    CREATED,
    CONCLUDED,
    INPROGRESS,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ExperimentResponse {
    pub id: String,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
    pub last_modified: DateTime<Utc>,

    pub name: String,
    pub override_keys: Vec<String>,
    pub status: ExperimentStatusType,
    pub traffic_percentage: i32,

    pub context: Value,
    pub variants: Value,
    pub chosen_variant: Option<String>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ExperimentsResponse {
    pub total_items: i64,
    pub total_pages: i64,
    pub data: Vec<ExperimentResponse>,
}

#[derive(Deserialize, Debug, Clone, Deref, DerefMut, PartialEq)]
pub struct StatusTypes(pub Vec<ExperimentStatusType>);

#[derive(Debug, Clone, PartialEq)]
pub struct ListFilters {
    pub status: Option<StatusTypes>,
    pub from_date: Option<DateTime<Utc>>,
    pub to_date: Option<DateTime<Utc>>,
    pub page: Option<i64>,
    pub count: Option<i64>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Dimension {
    pub dimension: String,
    pub priority: i32,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
    pub schema: Value,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct DefaultConfig {
    pub key: String,
    pub value: Value,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
    pub schema: Value,
}

#[derive(Deserialize, Serialize, Clone, PartialEq, Debug, strum_macros::Display)]
#[strum(serialize_all = "UPPERCASE")]
pub enum VariantType {
    CONTROL,
    EXPERIMENTAL,
}

#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct Variant {
    pub id: String,
    pub variant_type: VariantType,
    pub context_id: Option<String>,
    pub override_id: Option<String>,
    pub overrides: Map<String, Value>,
}
