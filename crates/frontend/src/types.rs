use leptos::{ReadSignal, WriteSignal};
use serde::{Deserialize, Serialize};
use std::{str::FromStr, vec::Vec};

use chrono::{DateTime, NaiveDateTime, Utc};
use derive_more::{Deref, DerefMut};
use serde_json::{json, Map, Value};

use crate::components::dropdown::utils::DropdownOption;
use crate::logic::Conditions;

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
    TEST,
    DEV,
}

impl FromStr for AppEnv {
    type Err = String;
    fn from_str(val: &str) -> Result<AppEnv, Self::Err> {
        match val {
            "PROD" => Ok(AppEnv::PROD),
            "SANDBOX" => Ok(AppEnv::SANDBOX),
            "DEV" => Ok(AppEnv::DEV),
            "TEST" => Ok(AppEnv::TEST),
            _ => Err("invalid app env!!".to_string()),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Envs {
    pub host: String,
    pub tenants: Vec<String>,
    pub service_prefix: &'static str,
}

/*************************Function Type ***************************/

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct FunctionResponse {
    pub function_name: String,
    pub published_code: Option<String>,
    pub draft_code: String,
    pub function_description: String,
    pub published_runtime_version: Option<String>,
    pub draft_runtime_version: String,
    pub published_at: Option<NaiveDateTime>,
    pub draft_edited_at: NaiveDateTime,
    pub published_by: Option<String>,
    pub draft_edited_by: String,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct FunctionTestResponse {
    pub message: String,
    pub stdout: String,
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

impl FromIterator<VariantFormT> for Vec<Variant> {
    fn from_iter<T: IntoIterator<Item = VariantFormT>>(iter: T) -> Self {
        iter.into_iter().map(Variant::from).collect()
    }
}

impl From<VariantFormT> for Variant {
    fn from(value: VariantFormT) -> Self {
        Variant {
            id: value.id,
            variant_type: value.variant_type,
            overrides: Map::from_iter(value.overrides),
            context_id: None,
            override_id: None,
        }
    }
}

#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct VariantFormT {
    pub id: String,
    pub variant_type: VariantType,
    pub overrides: Vec<(String, Value)>,
}

impl From<Variant> for VariantFormT {
    fn from(value: Variant) -> Self {
        VariantFormT {
            id: value.id,
            variant_type: value.variant_type,
            overrides: value.overrides.into_iter().collect(),
        }
    }
}

impl FromIterator<Variant> for Vec<VariantFormT> {
    fn from_iter<T: IntoIterator<Item = Variant>>(iter: T) -> Self {
        iter.into_iter().map(VariantFormT::from).collect()
    }
}

pub type Variants = Vec<Variant>;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Experiment {
    pub(crate) variants: Variants,
    pub(crate) name: String,
    pub(crate) id: String,
    pub(crate) traffic_percentage: u8,
    pub(crate) context: Conditions,
    pub(crate) status: ExperimentStatusType,
    pub(crate) override_keys: Value,
    pub(crate) created_by: String,
    pub(crate) created_at: DateTime<Utc>,
    pub(crate) last_modified: DateTime<Utc>,
    pub(crate) chosen_variant: Option<String>,
}

impl From<ExperimentResponse> for Experiment {
    fn from(value: ExperimentResponse) -> Self {
        Experiment {
            name: value.name,
            id: value.id,
            traffic_percentage: value.traffic_percentage as u8,
            status: value.status,
            override_keys: json!(value.override_keys),
            created_by: value.created_by,
            created_at: value.created_at,
            last_modified: value.last_modified,
            chosen_variant: value.chosen_variant,
            variants: serde_json::from_value(value.variants).unwrap_or_default(),
            context: Conditions::from_context_json(value.context)
                .unwrap_or(Conditions(vec![])),
        }
    }
}

/*************************** Context-Override types ********************************/

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Dimension {
    pub dimension: String,
    pub priority: i32,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
    pub schema: Value,
    pub function_name: Option<String>,
    pub mandatory: bool,
}

impl DropdownOption for Dimension {
    fn key(&self) -> String {
        self.dimension.clone()
    }
    fn label(&self) -> String {
        self.dimension.clone()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct DefaultConfig {
    pub key: String,
    pub value: Value,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
    pub schema: Value,
    pub function_name: Option<String>,
}

impl DropdownOption for DefaultConfig {
    fn key(&self) -> String {
        self.key.clone()
    }
    fn label(&self) -> String {
        self.key.clone()
    }
}

#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct Context {
    pub id: String,
    pub condition: Value,
    pub override_with_keys: [String; 1],
}

#[derive(Deserialize, Serialize, Clone, Debug, Default)]
pub struct Config {
    pub contexts: Vec<Context>,
    pub overrides: Map<String, Value>,
    pub default_configs: Map<String, Value>,
}

pub type FunctionsName = String;
impl DropdownOption for FunctionsName {
    fn key(&self) -> String {
        self.clone()
    }
    fn label(&self) -> String {
        self.clone()
    }
}

#[derive(Debug, Clone)]
pub struct BreadCrums {
    pub key: String,
    pub value: Option<String>,
    pub is_link: bool,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ErrorResponse {
    pub message: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct TypeTemplate {
    pub type_name: String,
    pub type_schema: Value,
    pub created_by: String,
    pub created_at: NaiveDateTime,
    pub last_modified_at: NaiveDateTime,
    pub last_modified_by: String,
}

impl DropdownOption for TypeTemplate {
    fn key(&self) -> String {
        self.type_name.clone()
    }
    fn label(&self) -> String {
        self.type_name.clone()
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct FetchTypeTemplateResponse {
    pub total_items: i64,
    pub total_pages: i64,
    pub data: Vec<TypeTemplate>,
}
