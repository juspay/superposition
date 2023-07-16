use crate::db::schema::*;
use chrono::offset::Utc;
use chrono::DateTime;
use diesel::{Selectable, Queryable, Insertable, QueryableByName};
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

/***
impl ToSql<crate::db::schema::sql_types::ExperimentStatusType, Pg> for ExperimentStatusType {
    fn to_sql<'b>(&'b self, out: &mut Output<'b, '_, Pg>) -> serialize::Result {
        match *self {
            ExperimentStatusType::CREATED => out.write_all("CREATED"),
            ExperimentStatusType::CONCLUDED => out.write_all("CONCLUDED"),
            ExperimentStatusType::INPROGRESS => out.write_all("INPROGRESS"),
        }
        Ok(IsNull::No)
    }
}

impl FromSql<crate::db::schema::sql_types::ExperimentStatusType, Pg> for ExperimentStatusType {
    fn from_sql(bytes: PgValue<'_>)-> deserialize::Result<Self> {
        match bytes.as_bytes() { 
            b"CREATED" => Ok(ExperimentStatusType::CREATED),
            b"CONCLUDED" => Ok(ExperimentStatusType::CONCLUDED),
            b"INPROGRESS" => Ok(ExperimentStatusType::INPROGRESS),
            _ => Err("Unrecognized enum variant".into()),
        }
    }
}
**/

#[derive(QueryableByName, Queryable, Selectable, Insertable, Serialize, Clone)]
#[diesel(check_for_backend(diesel::pg::Pg))]
#[diesel(primary_key(id))]
pub struct Experiment {
  pub id: i64,
  pub created_by: String,
  pub created_at: DateTime<Utc>,

  pub name: String,
  pub override_keys: Vec<String>,
  pub traffic_percentage: i32,
  pub status: ExperimentStatusType,

  pub context: Value,
  pub variants: Value
}
