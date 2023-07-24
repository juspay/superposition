use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use crate::db::models;

use crate::db::models::ExperimentStatusType;

#[derive(Deserialize, Serialize, Clone)]
pub enum VariantType {
    CONTROL,
    EXPERIMENTAL,
}

#[derive(Deserialize, Serialize, Clone)]
pub struct Variant {
    pub id: String,
    pub variant_type: VariantType,
    pub overrides: Value,
}

#[derive(Deserialize)]
pub struct ExperimentCreateReq {
    pub name: String,
    pub override_keys: Vec<String>,
    pub traffic_percentage: i32,

    pub context: Value,
    pub variants: Vec<Variant>,
}

#[derive(Serialize)]
pub struct ExperimentCreateRes {
    pub message: String,
    pub data: models::Experiment,
}

#[derive(Deserialize, Debug)]
pub struct ListFilters {
    pub status: Vec<ExperimentStatusType>,
    pub from_date: DateTime<Utc>,
    pub to_date: DateTime<Utc>,
    pub page: i64,
    pub count: i64,
}
