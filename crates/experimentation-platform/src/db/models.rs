use crate::db::schema::*;
use chrono::{DateTime, Utc};

use diesel::{Insertable, Queryable, QueryableByName, Selectable};
use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Debug, Clone, Copy, Deserialize, Serialize, diesel_derive_enum::DbEnum)]
#[DbValueStyle = "UPPERCASE"]
#[ExistingTypePath = "crate::db::schema::sql_types::ExperimentStatusType"]
pub enum ExperimentStatusType {
    CREATED,
    CONCLUDED,
    INPROGRESS,
}

#[derive(QueryableByName, Queryable, Selectable, Insertable, Serialize, Clone, Debug)]
#[diesel(check_for_backend(diesel::pg::Pg))]
#[diesel(primary_key(id))]
pub struct Experiment {
    pub id: i64,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
    pub last_modified: Option<DateTime<Utc>>,

    pub name: String,
    pub override_keys: Vec<String>,
    pub status: ExperimentStatusType,
    pub traffic_percentage: i32,

    pub context: Value,
    pub variants: Value,
}

pub type Experiments = Vec<Experiment>;
