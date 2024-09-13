use leptos::{ReadSignal, WriteSignal};
use serde::{Deserialize, Serialize};
use std::{str::FromStr, vec::Vec};

use chrono::{DateTime, NaiveDateTime, Utc};
use derive_more::{Deref, DerefMut};
use serde_json::{json, Map, Value};

use crate::components::{
    condition_pills::{types::Condition, utils::extract_conditions},
    dropdown::utils::DropdownOption,
};

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
