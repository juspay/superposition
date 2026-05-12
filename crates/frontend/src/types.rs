use std::str::FromStr;

use derive_more::{Deref, DerefMut};
use leptos::{ReadSignal, WriteSignal};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use superposition_derives::{IsEmpty, QueryParam};
use superposition_types::{
    Exp, IsEmpty, Overrides,
    api::dimension::DimensionResponse,
    custom_query::QueryParam,
    database::models::{
        cac::{DefaultConfig, DimensionType, TypeTemplate},
        experimentation::{Variant, VariantType},
        others::{HttpMethod, PayloadVersion, WebhookEvent},
    },
};

use crate::components::dropdown::utils::DropdownOption;

#[derive(Clone, Debug)]
pub struct SsrSharedHttpRequestHeaders {
    pub cookie: Option<String>,
}

#[derive(Clone, Debug)]
pub struct AppRoute {
    pub key: String,
    pub path: String,
    pub icon: String,
    pub label: String,
}

pub type InputVector = Vec<(ReadSignal<String>, WriteSignal<String>)>;

#[derive(
    Copy,
    Clone,
    Debug,
    Serialize,
    Deserialize,
    strum_macros::Display,
    strum_macros::EnumString,
)]
#[strum(serialize_all = "lowercase")]
pub enum AppEnv {
    PROD,
    SANDBOX,
    TEST,
    DEV,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Envs {
    pub host: String,
    pub service_prefix: &'static str,
    #[serde(default)]
    pub auth_z: bool,
}

#[derive(PartialEq, Clone, IsEmpty, QueryParam, Default, Deserialize)]
pub struct AdminPageParams {
    pub admin: Option<bool>,
}

/*********************** Experimentation Types ****************************************/

impl FromIterator<VariantFormT> for Result<Vec<Variant>, String> {
    fn from_iter<T: IntoIterator<Item = VariantFormT>>(iter: T) -> Self {
        iter.into_iter().map(Variant::try_from).collect()
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

/*************************** Context-Override types ********************************/

impl DropdownOption for DimensionResponse {
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

pub type FunctionName = String;
impl DropdownOption for FunctionName {
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

/// Breadcrumb segment for global navigation breadcrumbs.
/// Represents a single item in the breadcrumb trail.
#[derive(Debug, Clone)]
pub struct BreadcrumbSegment {
    /// Human-readable label for the breadcrumb
    pub label: String,
    /// URL to navigate to when clicked (empty for current page)
    pub href: String,
    /// Whether this is the current/active page (non-clickable)
    pub is_current: bool,
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
pub struct Workspace(pub String);

#[derive(Deref, DerefMut, Clone, Debug)]
pub struct OrganisationId(pub String);

impl DropdownOption for WebhookEvent {
    fn key(&self) -> String {
        self.to_string()
    }
    fn label(&self) -> String {
        self.to_string()
    }
}

impl DropdownOption for HttpMethod {
    fn key(&self) -> String {
        self.to_string()
    }
    fn label(&self) -> String {
        self.to_string()
    }
}

impl DropdownOption for PayloadVersion {
    fn key(&self) -> String {
        self.to_string()
    }
    fn label(&self) -> String {
        self.to_string()
    }
}

pub type ValueComputeCallback = leptos::Callback<(String, WriteSignal<Vec<String>>), ()>;

pub type ValueComputeCallbacks = std::collections::HashMap<String, ValueComputeCallback>;

impl DropdownOption for Option<String> {
    fn key(&self) -> String {
        match self {
            Some(id) => id.clone(),
            None => "None".to_string(),
        }
    }
    fn label(&self) -> String {
        match self {
            Some(id) => id.clone(),
            None => "None".to_string(),
        }
    }
}

#[derive(
    Serialize,
    Deserialize,
    Clone,
    Default,
    Debug,
    PartialEq,
    Eq,
    strum_macros::Display,
    strum_macros::EnumString,
    strum_macros::EnumIter,
)]
pub enum DimensionTypeOptions {
    #[default]
    Regular,
    #[strum(serialize = "Local Cohort")]
    LocalCohort,
    #[strum(serialize = "Remote Cohort")]
    RemoteCohort,
}

impl DimensionTypeOptions {
    pub fn to_dimension_type(&self, cohort: String) -> DimensionType {
        match self {
            DimensionTypeOptions::Regular => DimensionType::Regular {},
            DimensionTypeOptions::LocalCohort => DimensionType::LocalCohort(cohort),
            DimensionTypeOptions::RemoteCohort => DimensionType::RemoteCohort(cohort),
        }
    }
}

impl From<&DimensionType> for DimensionTypeOptions {
    fn from(dim_type: &DimensionType) -> Self {
        match dim_type {
            DimensionType::Regular {} => Self::Regular,
            DimensionType::LocalCohort(_) => Self::LocalCohort,
            DimensionType::RemoteCohort(_) => Self::RemoteCohort,
        }
    }
}

impl DropdownOption for DimensionTypeOptions {
    fn key(&self) -> String {
        self.to_string()
    }

    fn label(&self) -> String {
        self.to_string()
    }
}

#[derive(
    Copy,
    Clone,
    Debug,
    PartialEq,
    Eq,
    Hash,
    strum_macros::Display,
    strum_macros::EnumString,
    strum_macros::EnumIter,
)]
pub enum RouteSegment {
    #[strum(serialize = "types")]
    Types,
    #[strum(serialize = "audit-log")]
    AuditLog,
    #[strum(serialize = "default-config")]
    DefaultConfig,
    #[strum(serialize = "experiment-groups")]
    ExperimentGroups,
    #[strum(serialize = "compare")]
    Compare,
    #[strum(serialize = "config")]
    Config,
    #[strum(serialize = "dimensions")]
    Dimensions,
    #[strum(serialize = "experiments")]
    Experiments,
    #[strum(serialize = "function")]
    Function,
    #[strum(serialize = "resolve")]
    Resolve,
    #[strum(serialize = "secrets")]
    Secrets,
    #[strum(serialize = "variables")]
    Variables,
    #[strum(serialize = "versions")]
    Versions,
    #[strum(serialize = "webhooks")]
    Webhooks,
    #[strum(serialize = "workspaces")]
    Workspaces,
    #[strum(serialize = "overrides")]
    Overrides,
    #[strum(serialize = "create")]
    Create,
    #[strum(serialize = "edit")]
    Edit,
    #[strum(serialize = "admin")]
    Admin,
    #[strum(serialize = "settings")]
    Settings,
    #[strum(serialize = "authz")]
    Authz,
    #[strum(serialize = "organisations")]
    Organisations,
    #[strum(serialize = "action")]
    Action,
    #[strum(serialize = "org-authz")]
    OrgAuthz,
}

impl RouteSegment {
    pub fn try_from_str(value: &str) -> Result<Self, strum::ParseError> {
        Self::from_str(value)
    }
}

/// A route part can be either a static segment or a dynamic `:param`.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum RoutePart {
    Static(RouteSegment),
    Dynamic(&'static str),
}

impl RoutePart {
    pub fn to_path_part(&self) -> String {
        match self {
            Self::Static(seg) => seg.to_string(),
            Self::Dynamic(name) => {
                let trimmed = name.trim_start_matches(':');
                format!(":{trimmed}")
            }
        }
    }
}

impl From<RouteSegment> for RoutePart {
    fn from(value: RouteSegment) -> Self {
        Self::Static(value)
    }
}

impl From<&'static str> for RoutePart {
    fn from(value: &'static str) -> Self {
        if let Ok(seg) = RouteSegment::from_str(value) {
            Self::Static(seg)
        } else {
            Self::Dynamic(value.trim_start_matches(':'))
        }
    }
}

/// Join static and dynamic route parts with `/`.
pub fn join_route_parts<I, T>(parts: I) -> String
where
    I: IntoIterator<Item = T>,
    T: Into<RoutePart>,
{
    parts
        .into_iter()
        .map(|p| p.into().to_path_part())
        .collect::<Vec<_>>()
        .join("/")
}
