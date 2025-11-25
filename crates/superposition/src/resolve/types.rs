use serde::Deserialize;
use superposition_derives::{IsEmpty, QueryParam};

use superposition_types::{
    custom_query::{CommaSeparatedStringQParams, QueryParam},
    IsEmpty,
};

#[derive(Deserialize, IsEmpty, QueryParam, Default)]
pub struct ExtendedResolveConfigQuery {
    #[query_param(skip_if_empty)]
    pub version: Option<String>,
    pub show_reasoning: Option<bool>,
    pub resolve_remote: Option<bool>,
    #[query_param(skip_if_empty)]
    pub context_id: Option<String>,
    #[query_param(skip_if_empty, iterable)]
    pub prefix: Option<CommaSeparatedStringQParams>,
    #[query_param(skip_if_empty)]
    pub targetting_key: Option<String>,
}
