use serde::Deserialize;
use superposition_derives::{IsEmpty, QueryParam};
use superposition_types::{api::functions::Stage, custom_query::QueryParam, IsEmpty};

#[derive(Deserialize, PartialEq, Clone, IsEmpty, QueryParam)]
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
