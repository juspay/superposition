use std::fmt::{self, Display};

use serde::Deserialize;
use superposition_derives::IsEmpty;
use superposition_types::{api::functions::Stage, IsEmpty};

#[derive(Deserialize, PartialEq, Clone, IsEmpty)]
pub(super) struct PageParams {
    pub(super) tab: Stage,
}

impl Display for PageParams {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut parts = vec![];

        parts.push(format!("tab={}", self.tab));

        write!(f, "{}", parts.join("&"))
    }
}

impl Default for PageParams {
    fn default() -> Self {
        Self {
            tab: Stage::Published,
        }
    }
}
