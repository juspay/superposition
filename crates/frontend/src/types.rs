use core::fmt;
use leptos::{ReadSignal, WriteSignal};
use serde::{Deserialize, Serialize};
use std::{fmt::Display, str::FromStr, vec::Vec};
use superposition_types::SortBy;

use chrono::{DateTime, NaiveDateTime, Utc};
use derive_more::{Deref, DerefMut};
use serde_json::{json, Map, Value};

use crate::{
    components::{
        condition_pills::{types::Condition, utils::extract_conditions},
        dropdown::utils::DropdownOption,
    },
    pages::experiment_list::utils::ExperimentSortOn,
};

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

impl ExperimentStatusType {
    pub fn badge_class(&self) -> &'static str {
        match self {
            ExperimentStatusType::CREATED => "badge-info",
            ExperimentStatusType::INPROGRESS => "badge-warning",
            ExperimentStatusType::CONCLUDED => "badge-success",
        }
    }
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
pub struct ExperimentListFilters {
    pub status: Option<StatusTypes>,
    pub from_date: Option<DateTime<Utc>>,
    pub to_date: Option<DateTime<Utc>>,
    pub experiment_name: Option<String>,
    pub experiment_ids: Option<String>,
    pub created_by: Option<String>,
    pub context: Option<String>,
    pub sort_on: Option<ExperimentSortOn>,
    pub sort_by: Option<SortBy>,
}

impl Display for ExperimentListFilters {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut query_params = vec![];
        if let Some(status) = &self.status {
            let status: Vec<String> = status.iter().map(|val| val.to_string()).collect();
            query_params.push(format!("status={}", status.join(",")));
        }
        if let Some(from_date) = self.from_date {
            query_params.push(format!("from_date={}", from_date));
        }
        if let Some(to_date) = self.to_date {
            query_params.push(format!("to_date={}", to_date));
        }
        if let Some(experiment_name) = &self.experiment_name {
            query_params.push(format!("experiment_name={}", experiment_name));
        }
        if let Some(experiment_ids) = &self.experiment_ids {
            query_params.push(format!("experiment_ids={}", experiment_ids));
        }
        if let Some(created_by) = &self.created_by {
            query_params.push(format!("created_by={}", created_by));
        }
        if let Some(context) = &self.context {
            query_params.push(format!("context={}", context));
        }
        if let Some(sort_on) = self.sort_on {
            query_params.push(format!("sort_on={}", sort_on));
        }
        if let Some(sort_by) = &self.sort_by {
            query_params.push(format!("sort_by={}", sort_by));
        }
        write!(f, "{}", query_params.join("&"))
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DefaultConfigFilters {
    pub name: Option<String>,
}

impl Default for DefaultConfigFilters {
    fn default() -> Self {
        Self { name: None }
    }
}

impl Display for DefaultConfigFilters {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut query_params = vec![];
        if let Some(key_name) = &self.name {
            query_params.push(format!("name={}", key_name));
        }
        write!(f, "{}", query_params.join("&"))
    }
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
    pub(crate) context: Vec<Condition>,
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
            context: extract_conditions(&value.context),
        }
    }
}

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

#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct ConfigVersionListResponse {
    pub total_pages: u64,
    pub total_items: i64,
    pub data: Vec<ConfigVersion>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ConfigVersion {
    pub config: Value,
    pub config_hash: String,
    pub created_at: String,
    pub id: u64,
    pub tags: Option<Vec<String>>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]

pub struct PaginationFilters {
    pub page: Option<i64>,
    pub count: Option<i64>,
    pub all: Option<bool>,
}

impl Default for PaginationFilters {
    fn default() -> Self {
        Self {
            page: Some(1),
            count: Some(10),
            all: None,
        }
    }
}

impl Display for PaginationFilters {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut parts = vec![];

        if let Some(page) = self.page {
            parts.push(format!("page={}", page));
        }

        if let Some(count) = self.count {
            parts.push(format!("count={}", count));
        }

        if let Some(all) = self.all {
            parts.push(format!("all={}", all));
        }

        write!(f, "{}", parts.join("&"))
    }
}

#[derive(Serialize, Debug, Clone, Deserialize)]
pub struct PaginatedResponse<T> {
    pub total_pages: i64,
    pub total_items: i64,
    pub data: Vec<T>,
}

impl<T> Default for PaginatedResponse<T> {
    fn default() -> Self {
        PaginatedResponse {
            total_pages: 0,
            total_items: 0,
            data: Vec::new(),
        }
    }
}
