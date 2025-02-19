use chrono::{DateTime, NaiveDateTime, Utc};
use derive_more::{Deref, DerefMut};
#[cfg(feature = "diesel_derives")]
use diesel::{
    deserialize::{FromSql, FromSqlRow},
    expression::AsExpression,
    pg::Pg,
    serialize::ToSql,
    sql_types::{Integer, Json},
    Insertable, QueryId, Queryable, QueryableByName, Selectable,
};
use serde::{Deserialize, Serialize};
use serde_json::Value;
#[cfg(feature = "diesel_derives")]
use superposition_derives::{JsonFromSql, JsonToSql};

use crate::{Condition, Exp, Overridden, Overrides};

#[cfg(feature = "diesel_derives")]
use super::super::schema::*;
use super::{ChangeReason, Description};

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Deserialize,
    Serialize,
    strum_macros::Display,
    strum_macros::EnumString,
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

#[derive(Serialize, Deserialize, Debug, Copy, Clone, Default, Deref, DerefMut)]
#[serde(try_from = "i32")]
#[cfg_attr(feature = "diesel_derives", derive(AsExpression, FromSqlRow))]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = Integer))]
pub struct TrafficPercentage(u8);

#[cfg(feature = "diesel_derives")]
impl FromSql<Integer, Pg> for TrafficPercentage {
    fn from_sql(bytes: diesel::pg::PgValue<'_>) -> diesel::deserialize::Result<Self> {
        let num = <i32 as FromSql<Integer, Pg>>::from_sql(bytes)?;
        num.try_into()
            .map_err(|e: String| Box::<dyn std::error::Error + Send + Sync>::from(e))
    }
}

#[cfg(feature = "diesel_derives")]
impl ToSql<Integer, Pg> for TrafficPercentage {
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, Pg>,
    ) -> diesel::serialize::Result {
        let num = self.0 as i32;
        <i32 as ToSql<Integer, Pg>>::to_sql(&num, &mut out.reborrow())
    }
}

impl TryFrom<i32> for TrafficPercentage {
    type Error = String;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        if value < 0 || value > 100 {
            return Err(String::from(
                "Traffic percent cannot be lower than 0 and greater than 100",
            ));
        }
        Ok(Self(value as u8))
    }
}

impl TrafficPercentage {
    pub fn into_inner(self) -> u8 {
        self.0
    }

    pub fn check_max_allowed(&self, variants_count: u8) -> Result<(), String> {
        let max = 100 / variants_count;
        if self.0 > max {
            return Err(format!("The traffic_percentage cannot exceed {max}. Provide a traffic percentage less than {max}"));
        }
        Ok(())
    }

    pub fn compare_old(&self, old: &Self) -> Result<(), String> {
        if self.0 != 0 && self.0 == old.0 {
            return Err("The traffic percentage is same as provided")?;
        }
        Ok(())
    }
}

#[derive(Deserialize, Serialize, Clone, PartialEq, Debug, strum_macros::Display)]
#[strum(serialize_all = "UPPERCASE")]
pub enum VariantType {
    CONTROL,
    EXPERIMENTAL,
}

#[repr(C)]
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

impl Overridden<Exp<Overrides>> for Variant {
    fn get_overrides(&self) -> Overrides {
        self.overrides.clone().into_inner()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Deref, DerefMut)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(AsExpression, FromSqlRow, JsonFromSql, JsonToSql)
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

#[derive(Serialize, Deserialize, Clone)]
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
    pub traffic_percentage: TrafficPercentage,

    pub context: Condition,
    pub variants: Variants,
    pub last_modified_by: String,
    pub chosen_variant: Option<String>,
    pub description: Description,
    pub change_reason: ChangeReason,
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
