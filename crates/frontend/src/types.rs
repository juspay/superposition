use leptos::{ReadSignal, WriteSignal};
use serde::{Deserialize, Serialize};
use std::{str::FromStr, vec::Vec};

use chrono::{DateTime, Utc};
use derive_more::{Deref, DerefMut};
use serde_json::{Map, Value};

#[derive(Clone, Debug)]
pub struct AppRoute {
    pub key: String,
    pub path: String,
    pub icon: String,
    pub label: String,
}

pub type InputVector = Vec<(ReadSignal<String>, WriteSignal<String>)>;

#[derive(Copy, Clone, Debug, Serialize, Deserialize, strum_macros::Display)]
#[strum(serialize_all = "lowercase")]
pub enum AppEnv {
    PROD,
    SANDBOX,
    DEV,
}

impl FromStr for AppEnv {
    type Err = String;
    fn from_str(val: &str) -> Result<AppEnv, Self::Err> {
        match val {
            "PROD" => Ok(AppEnv::PROD),
            "SANDBOX" => Ok(AppEnv::SANDBOX),
            "DEV" => Ok(AppEnv::DEV),
            _ => Err("invalid app env!!".to_string()),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Envs {
    pub host: String,
    pub app_env: AppEnv,
    pub tenants: Vec<String>,
}

/*********************** Experimentation Types ****************************************/

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

#[derive(Serialize, Deserialize, Debug, Clone, Deref, DerefMut, PartialEq)]
pub struct StatusTypes(pub Vec<ExperimentStatusType>);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ListFilters {
    pub status: Option<StatusTypes>,
    pub from_date: Option<DateTime<Utc>>,
    pub to_date: Option<DateTime<Utc>>,
    pub page: Option<i64>,
    pub count: Option<i64>,
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

pub type Variants = Vec<Variant>;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Experiment {
    pub(crate) variants: Variants,
    pub(crate) name: String,
    pub(crate) id: String,
    pub(crate) traffic_percentage: u8,
    pub(crate) context: Value,
    pub(crate) status: ExperimentStatusType,
    pub(crate) override_keys: Value,
    pub(crate) created_by: String,
    pub(crate) created_at: DateTime<Utc>,
    pub(crate) last_modified: DateTime<Utc>,
    pub(crate) chosen_variant: Option<String>,
}

/*************************** Context-Override types ********************************/

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

#[derive(Deserialize, Serialize, Clone)]
pub struct Context {
    pub id: String,
    pub condition: Value,
    pub override_with_keys: [String; 1],
}

#[derive(Deserialize, Serialize, Clone)]
pub struct Config {
    pub contexts: Vec<Context>,
    pub overrides: Map<String, Value>,
    pub default_configs: Map<String, Value>,
}
