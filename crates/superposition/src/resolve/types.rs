use serde::Deserialize;
use superposition_derives::{IsEmpty, QueryParam};
use superposition_types::{IsEmpty, custom_query::QueryParam};

/// Query param for targeting key or the identifier to be used in experiments.
/// Also known as toss
#[derive(Deserialize, IsEmpty, QueryParam, Default)]
pub struct IdentifierQuery {
    #[query_param(skip_if_empty)]
    pub identifier: Option<String>,
}
