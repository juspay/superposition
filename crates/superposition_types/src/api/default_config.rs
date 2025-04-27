use serde::{Deserialize, Serialize};
use superposition_derives::DisplayQuery;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default, DisplayQuery)]
pub struct DefaultConfigFilters {
    pub name: Option<String>,
}
