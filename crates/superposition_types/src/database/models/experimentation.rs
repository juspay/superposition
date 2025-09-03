use chrono::{DateTime, Utc};
use derive_more::{Deref, DerefMut};
#[cfg(feature = "diesel_derives")]
use diesel::{
    deserialize::{self, FromSql, FromSqlRow},
    expression::AsExpression,
    pg::{Pg, PgValue},
    serialize::{self, Output, ToSql},
    sql_types::{Array, Integer, Json, Nullable},
    Insertable, QueryId, Queryable, QueryableByName, Selectable,
};
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::Value;
#[cfg(feature = "diesel_derives")]
use superposition_derives::{JsonFromSql, JsonToSql};

use crate::{Condition, Contextual, Exp, Overridden, Overrides};

#[cfg(feature = "diesel_derives")]
use super::super::schema::*;
use super::{i64_formatter, ChangeReason, Description, Metrics};

#[derive(
    Debug,
    Clone,
    Copy,
    Eq,
    Hash,
    PartialEq,
    Deserialize,
    Serialize,
    strum_macros::Display,
    strum_macros::EnumIter,
    strum_macros::EnumString,
    uniffi::Enum,
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
    INPROGRESS,
    PAUSED,
    CONCLUDED,
    DISCARDED,
}

impl ExperimentStatusType {
    pub fn active(&self) -> bool {
        match self {
            Self::CREATED | Self::INPROGRESS => true,
            Self::CONCLUDED | Self::DISCARDED | Self::PAUSED => false,
        }
    }

    pub fn active_list() -> Vec<Self> {
        vec![Self::CREATED, Self::INPROGRESS]
    }

    pub fn concludable(&self) -> bool {
        match self {
            Self::INPROGRESS => true,
            Self::CREATED | Self::CONCLUDED | Self::DISCARDED | Self::PAUSED => false,
        }
    }

    pub fn discardable(&self) -> bool {
        match self {
            Self::CREATED | Self::PAUSED | Self::INPROGRESS => true,
            Self::CONCLUDED | Self::DISCARDED => false,
        }
    }

    pub fn pausable(&self) -> bool {
        match self {
            Self::INPROGRESS => true,
            Self::CREATED | Self::CONCLUDED | Self::DISCARDED | Self::PAUSED => false,
        }
    }

    pub fn resumable(&self) -> bool {
        match self {
            Self::PAUSED => true,
            Self::CREATED | Self::INPROGRESS | Self::CONCLUDED | Self::DISCARDED => false,
        }
    }

    pub fn badge_color(&self) -> &'static str {
        match self {
            Self::CREATED => "badge-info",
            Self::INPROGRESS => "badge-warning",
            Self::CONCLUDED => "badge-success",
            Self::DISCARDED => "badge-neutral",
            Self::PAUSED => "badge-error",
        }
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    Eq,
    Hash,
    PartialEq,
    Deserialize,
    Serialize,
    Default,
    strum_macros::Display,
    strum_macros::EnumIter,
    strum_macros::EnumString,
    uniffi::Enum,
)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
#[strum(serialize_all = "SCREAMING_SNAKE_CASE")]
#[cfg_attr(
    feature = "diesel_derives",
    derive(diesel_derive_enum::DbEnum, QueryId)
)]
#[cfg_attr(feature = "diesel_derives", DbValueStyle = "SCREAMING_SNAKE_CASE")]
#[cfg_attr(
    feature = "diesel_derives",
    ExistingTypePath = "crate::database::schema::sql_types::ExperimentType"
)]
#[allow(non_camel_case_types)]
pub enum ExperimentType {
    #[default]
    Default,
    DeleteOverrides,
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
        Self::validate(value)?;
        Ok(Self(value as u8))
    }
}

impl TryFrom<u8> for TrafficPercentage {
    type Error = String;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Self::validate(value)?;
        Ok(Self(value))
    }
}

impl TryFrom<String> for TrafficPercentage {
    type Error = String;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        match value.parse::<i32>() {
            Ok(percent) => Self::try_from(percent),
            Err(err) => Err(format!(
                "Traffic percent could not be parsed. reason: {err}"
            )),
        }
    }
}

impl TrafficPercentage {
    pub fn check_max_allowed(&self, variants_count: u8) -> Result<(), String> {
        let max = if variants_count < 2 {
            100
        } else {
            100 / variants_count
        };
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

    fn validate<T: TryInto<u8>>(val: T) -> Result<(), String> {
        let value: u8 = val.try_into().map_err(|_| {
            "Traffic percentage must be a number between 0 and 100 (both inclusive)"
                .to_string()
        })?;
        if value > 100 {
            return Err(
                "Traffic percentage must be a number between 0 and 100 (both inclusive)"
                    .to_string(),
            );
        }
        Ok(())
    }
}

#[derive(
    Deserialize, Serialize, Clone, PartialEq, Debug, strum_macros::Display, uniffi::Enum,
)]
#[strum(serialize_all = "UPPERCASE")]
pub enum VariantType {
    CONTROL,
    EXPERIMENTAL,
}

#[repr(C)]
#[derive(Debug, Clone, Serialize, Deserialize, uniffi::Record)]
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
uniffi::custom_newtype!(Variants, Vec<Variant>);
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
    pub experiment_type: ExperimentType,
    pub override_keys: Vec<String>,
    pub status: ExperimentStatusType,
    pub traffic_percentage: TrafficPercentage,
    pub started_at: Option<DateTime<Utc>>,
    pub started_by: Option<String>,

    pub context: Condition,
    pub variants: Variants,
    pub last_modified_by: String,
    pub chosen_variant: Option<String>,
    pub description: Description,
    pub change_reason: ChangeReason,
    pub metrics: Metrics,
    pub experiment_group_id: Option<i64>,
}

impl Contextual for Experiment {
    fn get_condition(&self) -> Condition {
        self.context.clone()
    }
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
    pub timestamp: DateTime<Utc>,
    pub action: String,
    pub original_data: Option<Value>,
    pub new_data: Option<Value>,
    pub query: String,
}

#[derive(
    Debug,
    Clone,
    Copy,
    Eq,
    Hash,
    PartialEq,
    Deserialize,
    Serialize,
    strum_macros::Display,
    strum_macros::EnumIter,
    strum_macros::EnumString,
    uniffi::Enum,
)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
#[strum(serialize_all = "SCREAMING_SNAKE_CASE")]
#[cfg_attr(
    feature = "diesel_derives",
    derive(diesel_derive_enum::DbEnum, QueryId)
)]
#[cfg_attr(feature = "diesel_derives", DbValueStyle = "SCREAMING_SNAKE_CASE")]
#[cfg_attr(
    feature = "diesel_derives",
    ExistingTypePath = "crate::database::schema::sql_types::GroupType"
)]
pub enum GroupType {
    UserCreated,
    SystemGenerated,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(QueryableByName, Queryable, Selectable, Insertable)
)]
#[cfg_attr(feature = "diesel_derives", diesel(check_for_backend(diesel::pg::Pg)))]
#[cfg_attr(feature = "diesel_derives", diesel(primary_key(experiment_group_id)))]
pub struct ExperimentGroup {
    #[serde(with = "i64_formatter")]
    pub id: i64,
    pub context_hash: String,
    pub name: String,
    pub description: Description,
    pub change_reason: ChangeReason,
    pub context: Condition,
    pub traffic_percentage: TrafficPercentage,
    #[serde(with = "i64_vec_formatter")]
    pub member_experiment_ids: Vec<i64>,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
    pub last_modified_at: DateTime<Utc>,
    pub last_modified_by: String,
    pub buckets: Buckets,
    pub group_type: GroupType,
}

pub type ExperimentGroups = Vec<ExperimentGroup>;

pub mod i64_vec_formatter {
    use serde::{self, Deserialize, Deserializer, Serialize, Serializer};

    // Serialize Vec<i64> to Vec<String>
    pub fn serialize<S>(value: &[i64], serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let string_values: Vec<String> = value.iter().map(|v| v.to_string()).collect();
        string_values.serialize(serializer)
    }

    // Deserialize Vec<String> to Vec<i64>
    pub fn deserialize<'de, D>(deserializer: D) -> Result<Vec<i64>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let string_values = Vec::<String>::deserialize(deserializer)?;
        string_values
            .iter()
            .map(|s| {
                s.parse::<i64>().map_err(|e| {
                    serde::de::Error::custom(format!("Failed to parse i64: {}", e))
                })
            })
            .collect()
    }
}

pub fn i64_vec_deserialize<'de, D>(deserializer: D) -> Result<Option<Vec<i64>>, D::Error>
where
    D: Deserializer<'de>,
{
    let string_values: Option<Vec<String>> = Option::deserialize(deserializer)?;
    let Some(string_values) = string_values else {
        return Ok(None);
    };
    let numbers: Vec<i64> = string_values
        .iter()
        .map(|s| {
            s.parse::<i64>().map_err(|e| {
                serde::de::Error::custom(format!(
                    "the vector field needs to contain strings of numbers : {}",
                    e
                ))
            })
        })
        .collect::<Result<Vec<i64>, D::Error>>()?;
    Ok(Some(numbers))
}

#[derive(Deserialize, Serialize, Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(AsExpression, FromSqlRow, JsonFromSql, JsonToSql)
)]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = Json))]
pub struct Bucket {
    pub variant_id: String,
    pub experiment_id: String,
}

#[derive(Deserialize, Debug, Clone, PartialEq, Deref, DerefMut)]
#[serde(try_from = "Vec<Option<Bucket>>")]
#[cfg_attr(feature = "diesel_derives", derive(AsExpression, FromSqlRow))]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = Array<Nullable<Json>>))]
pub struct Buckets([Option<Bucket>; 100]);

impl Default for Buckets {
    fn default() -> Self {
        Self(std::array::from_fn(|_| None))
    }
}

impl From<[Option<Bucket>; 100]> for Buckets {
    fn from(value: [Option<Bucket>; 100]) -> Self {
        Self(value)
    }
}

impl Serialize for Buckets {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.to_vec().serialize(serializer)
    }
}

impl TryFrom<Vec<Option<Bucket>>> for Buckets {
    type Error = String;

    fn try_from(value: Vec<Option<Bucket>>) -> Result<Self, Self::Error> {
        let size = value.len();
        value
            .try_into()
            .map(Self)
            .map_err(|_| format!("Buckets must contain exactly 100 elements, got {size}"))
    }
}

#[cfg(feature = "diesel_derives")]
impl FromSql<Array<Nullable<Json>>, Pg> for Buckets {
    fn from_sql(bytes: PgValue<'_>) -> deserialize::Result<Self> {
        let string_array: Vec<Option<Bucket>> =
            FromSql::<Array<Nullable<Json>>, Pg>::from_sql(bytes)?;
        Self::try_from(string_array)
            .map_err(|e: String| Box::<dyn std::error::Error + Send + Sync>::from(e))
    }
}

#[cfg(feature = "diesel_derives")]
impl ToSql<Array<Nullable<Json>>, Pg> for Buckets {
    fn to_sql<'b>(&'b self, out: &mut Output<'b, '_, Pg>) -> serialize::Result {
        <Vec<Option<Bucket>> as ToSql<Array<Nullable<Json>>, Pg>>::to_sql(
            &self.0.to_vec(),
            &mut out.reborrow(),
        )
    }
}
