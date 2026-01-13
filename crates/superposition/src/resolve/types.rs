use serde::Deserialize;
use superposition_derives::{IsEmpty, QueryParam};
use superposition_types::{IsEmpty, custom_query::QueryParam};

#[derive(Deserialize, IsEmpty, QueryParam, Default)]
pub struct IdentifierQuery {
    #[query_param(skip_if_empty)]
    pub identifier: Option<String>,
}
