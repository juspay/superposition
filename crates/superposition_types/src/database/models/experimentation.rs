use chrono::{DateTime, NaiveDateTime, Utc};
#[cfg(feature = "diesel_derives")]
use diesel::{
    deserialize::FromSqlRow, expression::AsExpression, sql_types::Json, Insertable,
    QueryId, Queryable, QueryableByName, Selectable,
};
use serde::{Deserialize, Serialize};
use serde_json::Value;
#[cfg(feature = "diesel_derives")]
use superposition_derives::{JsonFromSql, JsonToSql};

use crate::{Condition, Exp, Overrides};

#[cfg(feature = "diesel_derives")]
use super::super::schema::*;

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
    ExistingTypePath = "crate::database::schema::sql_types::ExperimentStatusType"
)]
pub enum ExperimentStatusType {
    CREATED,
    CONCLUDED,
    INPROGRESS,
    DISCARDED,
}

impl ExperimentStatusType {
    pub fn active(&self) -> bool {
        match self {
            Self::CREATED | Self::INPROGRESS => true,
            Self::CONCLUDED | Self::DISCARDED => false,
        }
    }

    pub fn discardable(&self) -> bool {
        match self {
            Self::CREATED => true,
            Self::INPROGRESS | Self::CONCLUDED | Self::DISCARDED => false,
        }
    }
}

#[derive(Deserialize, Serialize, Clone, PartialEq, Debug, strum_macros::Display)]
#[strum(serialize_all = "UPPERCASE")]
pub enum VariantType {
    CONTROL,
    EXPERIMENTAL,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Variant {
    pub id: String,
    pub variant_type: VariantType,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub context_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub override_id: Option<String>,
    pub overrides: Exp<Overrides>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(AsExpression, FromSqlRow, JsonFromSql, JsonToSql,)
)]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = Json))]
pub struct Variants(Vec<Variant>);

impl Variants {
    pub fn new(data: Vec<Variant>) -> Self {
        Self(data)
    }

    pub fn into_inner(self) -> Vec<Variant> {
        self.0
    }
}

#[derive(Serialize, Clone, Debug)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(QueryableByName, Queryable, Selectable, Insertable)
)]
#[cfg_attr(feature = "diesel_derives", diesel(check_for_backend(diesel::pg::Pg)))]
#[cfg_attr(feature = "diesel_derives", diesel(primary_key(id)))]
pub struct Experiment {
    pub id: i64,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
    pub last_modified: DateTime<Utc>,

    pub name: String,
    pub override_keys: Vec<String>,
    pub status: ExperimentStatusType,
    pub traffic_percentage: i32,

    pub context: Condition,
    pub variants: Variants,
    pub last_modified_by: String,
    pub chosen_variant: Option<String>,
    pub description: String,
    pub change_reason: String,
}

pub type Experiments = Vec<Experiment>;

#[derive(Clone, Serialize, Debug)]
#[cfg_attr(feature = "diesel_derives", derive(Queryable, Selectable, Insertable))]
#[cfg_attr(feature = "diesel_derives", diesel(check_for_backend(diesel::pg::Pg)))]
#[cfg_attr(feature = "diesel_derives", diesel(table_name = event_log))]
#[cfg_attr(feature = "diesel_derives", diesel(primary_key(id)))]
pub struct EventLog {
    pub id: uuid::Uuid,
    pub table_name: String,
    pub user_name: String,
    pub timestamp: NaiveDateTime,
    pub action: String,
    pub original_data: Option<Value>,
    pub new_data: Option<Value>,
    pub query: String,
}
