use serde::Deserialize;
use superposition_derives::{IsEmpty, QueryString};
use superposition_types::{api::functions::Stage, custom_query::QueryString, IsEmpty};

#[derive(Deserialize, PartialEq, Clone, IsEmpty, QueryString)]
pub(super) struct PageParams {
    pub(super) tab: Stage,
}

impl Default for PageParams {
    fn default() -> Self {
        Self {
            tab: Stage::Published,
        }
    }
}
