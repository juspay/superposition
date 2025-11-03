use serde::Deserialize;
use serde_json::{Map, Value};
use superposition_derives::{IsEmpty, QueryParam};

use crate::{
    custom_query::{CommaSeparatedStringQParams, QueryParam},
    IsEmpty,
};

#[derive(Deserialize)]
pub struct ContextPayload {
    pub context: Map<String, Value>,
}

#[derive(Deserialize, IsEmpty, QueryParam, Default)]
pub struct ConfigQuery {
    #[query_param(skip_if_empty)]
    pub version: Option<String>,
    #[query_param(skip_if_empty, iterable)]
    pub prefix: Option<CommaSeparatedStringQParams>,
}

#[derive(Deserialize, IsEmpty, QueryParam, Default)]
pub struct ResolveConfigQuery {
    #[query_param(skip_if_empty)]
    pub version: Option<String>,
    pub show_reasoning: Option<bool>,
    #[query_param(skip_if_empty)]
    pub context_id: Option<String>,
    #[query_param(skip_if_empty, iterable)]
    pub prefix: Option<CommaSeparatedStringQParams>,
}
