use serde::Deserialize;

use crate::custom_query::QueryMap;

#[derive(Deserialize)]
pub struct ContextPayload {
    pub context: QueryMap,
}
