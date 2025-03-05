mod handlers;
pub use handlers::endpoints;
use serde::Deserialize;
use superposition_types::custom_query::QueryMap;
mod helpers;

#[derive(Deserialize)]
pub struct ContextPayload {
    pub context: QueryMap,
}
