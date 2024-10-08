use crate::db::schema::*;
use chrono::{DateTime, NaiveDateTime, Utc};

use diesel::{
    deserialize::FromSqlRow, expression::AsExpression, query_builder::QueryId,
    sql_types::Json, Insertable, Queryable, QueryableByName, Selectable,
};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use superposition_derives::{JsonFromSql, JsonToSql};
use superposition_types::{Exp, Overrides};

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Deserialize,
    Serialize,
    diesel_derive_enum::DbEnum,
    QueryId,
)]
#[DbValueStyle = "UPPERCASE"]
#[ExistingTypePath = "crate::db::schema::sql_types::ExperimentStatusType"]
pub enum ExperimentStatusType {
    CREATED,
    CONCLUDED,
    INPROGRESS,
}

#[derive(Deserialize, Serialize, Clone, PartialEq, Debug)]
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

#[derive(
    Debug, Clone, Serialize, Deserialize, AsExpression, FromSqlRow, JsonFromSql, JsonToSql,
)]
#[diesel(sql_type = Json)]
pub struct Variants(Vec<Variant>);

impl Variants {
    pub fn new(data: Vec<Variant>) -> Self {
        Self(data)
    }

    pub fn into_inner(self) -> Vec<Variant> {
        self.0
    }
}

#[derive(QueryableByName, Queryable, Selectable, Insertable, Serialize, Clone, Debug)]
#[diesel(check_for_backend(diesel::pg::Pg))]
#[diesel(primary_key(id))]
pub struct Experiment {
    pub id: i64,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
    pub last_modified: DateTime<Utc>,

    pub name: String,
    pub override_keys: Vec<String>,
    pub status: ExperimentStatusType,
    pub traffic_percentage: i32,

    pub context: Value,
    pub variants: Variants,
    pub last_modified_by: String,
    pub chosen_variant: Option<String>,
}

pub type Experiments = Vec<Experiment>;

#[derive(Queryable, Selectable, Insertable, Serialize, Clone, Debug)]
#[diesel(check_for_backend(diesel::pg::Pg))]
#[diesel(table_name = event_log)]
#[diesel(primary_key(id))]
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
