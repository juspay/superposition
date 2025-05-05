use derive_more::{Deref, DerefMut};
use leptos::{ReadSignal, WriteSignal};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use superposition_types::{
    database::{
        models::{
            cac::{DefaultConfig, TypeTemplate},
            experimentation::{Variant, VariantType},
            others::{HttpMethod, PayloadVersion, WebhookEvent},
        },
        types::DimensionWithMandatory,
    },
    Exp, Overrides,
};

use crate::components::dropdown::utils::DropdownOption;

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
