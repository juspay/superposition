use chrono::NaiveDateTime;
#[cfg(feature = "diesel_derives")]
use diesel::{AsChangeset, Insertable, QueryId, Queryable, Selectable};

use serde::{Deserialize, Serialize};

pub mod cac;
#[cfg(feature = "experimentation")]
pub mod experimentation;

#[cfg(feature = "diesel_derives")]
use super::superposition_schema::superposition::*;

#[derive(
    Debug, Clone, Copy, PartialEq, Deserialize, Serialize, strum_macros::Display,
)]
#[serde(rename_all = "UPPERCASE")]
#[strum(serialize_all = "UPPERCASE")]
#[cfg_attr(
    feature = "diesel_derives",
    derive(diesel_derive_enum::DbEnum, QueryId)
)]
#[cfg_attr(feature = "diesel_derives", DbValueStyle = "UPPERCASE")]
#[cfg_attr(
    feature = "diesel_derives",
    ExistingTypePath = "crate::database::superposition_schema::superposition::sql_types::WorkspaceStatus"
)]
pub enum WorkspaceStatus {
    ENABLED,
    DISABLED,
}

#[derive(Clone, Serialize, Deserialize, Debug)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(Queryable, Selectable, Insertable, AsChangeset)
)]
#[cfg_attr(feature = "diesel_derives", diesel(check_for_backend(diesel::pg::Pg)))]
#[cfg_attr(
    feature = "diesel_derives",
    diesel(primary_key(organization_id, workspace_name))
)]
pub struct Workspace {
    pub organization_id: String,
    pub organization_name: String,
    pub workspace_name: String,
    pub workspace_schema_name: String,
    pub workspace_status: WorkspaceStatus,
    pub workspace_admin_email: String,
    pub created_by: String,
    pub last_modified_by: String,
    pub last_modified_at: NaiveDateTime,
    pub created_at: NaiveDateTime,
    pub mandatory_dimensions: Option<Vec<String>>,
}
