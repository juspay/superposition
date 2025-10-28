use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use superposition_derives::{IsEmpty, QueryParam};

use crate::{custom_query::QueryParam, IsEmpty};

#[derive(Deserialize)]
pub struct ContextPayload {
    pub context: Map<String, Value>,
}

#[derive(Deserialize, Serialize, IsEmpty, QueryParam, Default)]
pub struct ConfigQuery {
    pub version: Option<String>,
    pub prefix: Option<Vec<String>>,
}

#[derive(Deserialize, Serialize, IsEmpty, QueryParam, Default)]
pub struct ResolveConfigQuery {
    pub version: Option<String>,
    pub show_reasoning: Option<bool>,
    pub context_id: Option<String>,
    pub prefix: Option<Vec<String>>,
}
