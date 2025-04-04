use chrono::NaiveDateTime;
use derive_more::{AsRef, Deref, Into};
#[cfg(feature = "diesel_derives")]
use diesel::{
    deserialize::FromSqlRow, expression::AsExpression, sql_types::Json, AsChangeset,
    Insertable, QueryId, Queryable, Selectable,
};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use std::str::FromStr;
#[cfg(feature = "diesel_derives")]
use superposition_derives::{JsonFromSql, JsonToSql};

pub mod cac;
#[cfg(feature = "experimentation")]
pub mod experimentation;

use self::cac::Dimension;

#[cfg(feature = "diesel_derives")]
use super::superposition_schema::superposition::*;

#[derive(
    Debug, Clone, Copy, PartialEq, Deserialize, Serialize, strum_macros::Display,
)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
#[cfg_attr(
    feature = "diesel_derives",
    derive(diesel_derive_enum::DbEnum, QueryId)
)]
#[cfg_attr(feature = "diesel_derives", DbValueStyle = "SCREAMING_SNAKE_CASE")]
#[cfg_attr(
    feature = "diesel_derives",
    ExistingTypePath = "crate::database::superposition_schema::superposition::sql_types::OrgStatus"
)]
pub enum OrgStatus {
    Active,
    Inactive,
    PendingKyb,
}

#[derive(Clone, Serialize, Deserialize, Debug)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(Queryable, Selectable, Insertable, AsChangeset)
)]
#[cfg_attr(feature = "diesel_derives", diesel(check_for_backend(diesel::pg::Pg)))]
#[cfg_attr(feature = "diesel_derives", diesel(primary_key(id)))]
#[cfg_attr(feature = "diesel_derives", diesel(treat_none_as_null = true))]
pub struct Organisation {
    pub id: String,
    pub name: String,
    pub country_code: Option<String>,
    pub contact_email: Option<String>,
    pub contact_phone: Option<String>,
    pub created_by: String,
    pub admin_email: String,
    pub status: OrgStatus,
    pub sector: Option<String>,
    pub created_at: NaiveDateTime,
    pub updated_at: NaiveDateTime,
    pub updated_by: String,
}

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

impl FromStr for WorkspaceStatus {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "ENABLED" => Ok(WorkspaceStatus::ENABLED),
            "DISABLED" => Ok(WorkspaceStatus::DISABLED),
            _ => Err(format!("Invalid enum string: {}", s)),
        }
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(Queryable, Selectable, Insertable, AsChangeset)
)]
#[cfg_attr(feature = "diesel_derives", diesel(check_for_backend(diesel::pg::Pg)))]
#[cfg_attr(
    feature = "diesel_derives",
    diesel(primary_key(organisation_id, workspace_name))
)]
pub struct Workspace {
    pub organisation_id: String,
    pub organisation_name: String,
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

#[derive(Deserialize, Serialize, Clone, Deref, Debug, PartialEq, Into, AsRef)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(AsExpression, FromSqlRow, JsonFromSql, JsonToSql)
)]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = Json))]
pub struct DependencyGraph(Map<String, Value>);

impl DependencyGraph {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn insert<K, V>(&mut self, key: K, value: V) -> Option<Value>
    where
        K: Into<String>,
        V: Into<Value>,
    {
        self.0.insert(key.into(), value.into())
    }

    pub fn insert_dependents(&mut self, dependent_dimension: &Dimension) {
        if dependent_dimension.dependency_graph.is_empty() {
            self.0.insert(
                dependent_dimension.dimension.to_string(),
                Value::Array(vec![]),
            );
        } else {
            dependent_dimension
                .dependency_graph
                .iter()
                .for_each(|(key, value)| {
                    self.0.insert(key.to_string(), value.clone());
                });
        }
    }
}

impl Default for DependencyGraph {
    fn default() -> Self {
        DependencyGraph(Map::new())
    }
}
