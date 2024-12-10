use std::{str::FromStr, vec::Vec};

use chrono::{DateTime, Utc};
use derive_more::{Deref, DerefMut};
use leptos::{ReadSignal, WriteSignal};
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use superposition_types::{
    cac::models::{DefaultConfig, DimensionWithMandatory, TypeTemplates},
    experimentation::models::{ExperimentStatusType, Variant, VariantType},
    Exp, Overrides,
};

use crate::components::{
    condition_pills::{types::Condition, utils::extract_conditions},
    dropdown::utils::DropdownOption,
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
    fn from_str(val: &str) -> Result<Self, Self::Err> {
        match val {
            "PROD" => Ok(Self::PROD),
            "SANDBOX" => Ok(Self::SANDBOX),
            "DEV" => Ok(Self::DEV),
            "TEST" => Ok(Self::TEST),
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
pub struct FunctionTestResponse {
    pub message: String,
    pub stdout: String,
}

/*********************** Experimentation Types ****************************************/

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

#[derive(Serialize, Deserialize, Debug, Clone, Deref, DerefMut, PartialEq)]
pub struct StatusTypes(pub Vec<ExperimentStatusType>);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExpListFilters {
    pub status: Option<StatusTypes>,
    pub from_date: Option<DateTime<Utc>>,
    pub to_date: Option<DateTime<Utc>>,
}

impl FromIterator<VariantFormT> for Vec<Variant> {
    fn from_iter<T: IntoIterator<Item = VariantFormT>>(iter: T) -> Self {
        iter.into_iter()
            .filter_map(|v| Variant::try_from(v).ok())
            .collect()
    }
}

impl TryFrom<VariantFormT> for Variant {
    type Error = String;
    fn try_from(value: VariantFormT) -> Result<Self, Self::Error> {
        Ok(Self {
            id: value.id,
            variant_type: value.variant_type,
            overrides: Exp::<Overrides>::try_from(Map::from_iter(value.overrides))?,
            context_id: None,
            override_id: None,
        })
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
        Self {
            id: value.id,
            variant_type: value.variant_type,
            overrides: value.overrides.into_inner().into_iter().collect(),
        }
    }
}

#[derive(Deref)]
pub struct VariantFormTs(Vec<VariantFormT>);

impl Default for VariantFormTs {
    fn default() -> Self {
        Self(Vec::new())
    }
}

impl FromIterator<Variant> for VariantFormTs {
    fn from_iter<T: IntoIterator<Item = Variant>>(iter: T) -> Self {
        Self(iter.into_iter().map(VariantFormT::from).collect())
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
        Self {
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

/*************************** Context-Override types ********************************/

impl DropdownOption for DimensionWithMandatory {
    fn key(&self) -> String {
        self.dimension.clone()
    }
    fn label(&self) -> String {
        self.dimension.clone()
    }
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

impl DropdownOption for TypeTemplates {
    fn key(&self) -> String {
        self.type_name.clone()
    }
    fn label(&self) -> String {
        self.type_name.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ListFilters {
    pub page: Option<i64>,
    pub count: Option<i64>,
    pub all: Option<bool>,
}
