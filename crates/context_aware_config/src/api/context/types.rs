use chrono::{DateTime, Utc};
use diesel::prelude::AsChangeset;
#[cfg(feature = "jsonlogic")]
use serde::Deserialize;
use serde::Serialize;
use superposition_types::{
    database::{
        models::{cac::FunctionCode, ChangeReason, Description},
        schema::contexts,
    },
    Overrides,
};

#[derive(Serialize, AsChangeset)]
#[diesel(table_name = contexts)]
pub(crate) struct UpdateContextOverridesChangeset {
    pub override_id: String,
    #[serde(rename = "override")]
    pub override_: Overrides,
    pub last_modified_at: DateTime<Utc>,
    pub last_modified_by: String,
    pub description: Option<Description>,
    pub change_reason: ChangeReason,
}

#[cfg(feature = "jsonlogic")]
#[derive(Deserialize, Clone)]
pub struct DimensionCondition {
    pub var: String,
}

#[derive(Clone)]
pub struct FunctionsInfo {
    pub name: String,
    pub code: Option<FunctionCode>,
}
