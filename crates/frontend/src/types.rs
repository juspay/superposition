use std::{fmt::Display, str::FromStr, vec::Vec};

use chrono::{DateTime, Utc};
use core::fmt;
use derive_more::{Deref, DerefMut};
use leptos::{ReadSignal, WriteSignal};
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use strum::IntoEnumIterator;
use superposition_types::{
    database::{
        models::cac::{DefaultConfig, TypeTemplate},
        models::experimentation::{ExperimentStatusType, Variant, VariantType},
        types::DimensionWithMandatory,
    },
    Exp, Overrides, SortBy,
};

use crate::logic::Conditions;
use crate::{
    components::dropdown::utils::DropdownOption,
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

impl Default for StatusTypes {
    fn default() -> Self {
        Self(ExperimentStatusType::iter().collect())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExperimentListFilters {
    pub status: Option<StatusTypes>,
    pub from_date: Option<DateTime<Utc>>,
    pub to_date: Option<DateTime<Utc>>,
    pub experiment_name: Option<String>,
    pub experiment_ids: Option<String>,
    pub created_by: Option<String>,
    pub context: Option<Conditions>,
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
            let mut contexts = vec![];
            for c in context.iter() {
                contexts.push(c.to_condition_json().to_string())
            }
            query_params.push(format!("context={}", contexts.join(",")));
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

impl Default for ExperimentListFilters {
    fn default() -> Self {
        let now = Utc::now();
        Self {
            status: None,
            from_date: Some(now - chrono::Duration::days(30)),
            to_date: Some(now),
            experiment_name: None,
            experiment_ids: None,
            created_by: None,
            context: None,
            sort_on: None,
            sort_by: None,
        }
    }
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
pub struct VariantFormTs(pub Vec<VariantFormT>);

impl FromIterator<Variant> for VariantFormTs {
    fn from_iter<T: IntoIterator<Item = Variant>>(iter: T) -> Self {
        Self(iter.into_iter().map(VariantFormT::from).collect())
    }
}

impl VariantFormTs {
    pub fn default_with_overrides(overrides: Vec<(String, Value)>) -> Self {
        Self(vec![
            VariantFormT {
                id: "control".to_string(),
                variant_type: VariantType::CONTROL,
                overrides: overrides.clone(),
            },
            VariantFormT {
                id: "experimental".to_string(),
                variant_type: VariantType::EXPERIMENTAL,
                overrides: overrides.clone(),
            },
        ])
    }
}

impl Default for VariantFormTs {
    fn default() -> Self {
        Self::default_with_overrides(Vec::new())
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
            context: Conditions::from_context_json(
                value.context.as_object().unwrap_or(&Map::new()),
            )
            .unwrap_or(Conditions(vec![])),
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

impl DropdownOption for TypeTemplate {
    fn key(&self) -> String {
        self.type_name.clone()
    }
    fn label(&self) -> String {
        self.type_name.clone()
    }
}

#[derive(Deref, DerefMut, Clone, Debug)]
pub struct Tenant(pub String);

#[derive(Deref, DerefMut, Clone, Debug)]
pub struct OrganisationId(pub String);
