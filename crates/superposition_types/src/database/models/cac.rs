#[cfg(feature = "diesel_derives")]
use std::str::{self, FromStr};
use std::{collections::HashMap, fmt::Display};

#[cfg(feature = "diesel_derives")]
use base64::prelude::*;
use bigdecimal::BigDecimal;
use chrono::{offset::Utc, DateTime};
use derive_more::{AsRef, Deref, DerefMut, Into};
#[cfg(feature = "diesel_derives")]
use diesel::{
    deserialize::{FromSql, FromSqlRow, Result as DResult},
    expression::AsExpression,
    pg::{Pg, PgValue},
    serialize::{Output, Result as SResult, ToSql},
    sql_types::{Integer, Json},
    AsChangeset, Insertable, QueryId, Queryable, Selectable,
};
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::Value;
#[cfg(feature = "diesel_derives")]
use superposition_derives::{JsonFromSql, JsonToSql, TextFromSql, TextToSql};

use crate::{Cac, Condition, Contextual, Overridden, Overrides};

#[cfg(feature = "diesel_derives")]
use super::super::schema::{
    config_versions, contexts, default_configs, dimensions, event_log, functions,
    type_templates,
};
use super::{i64_formatter, ChangeReason, Description};

#[derive(Clone, Serialize, Deserialize, Debug)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(Queryable, Selectable, Insertable, AsChangeset)
)]
#[cfg_attr(feature = "diesel_derives", diesel(check_for_backend(diesel::pg::Pg)))]
#[cfg_attr(feature = "diesel_derives", diesel(primary_key(id)))]
pub struct Context {
    pub id: String,
    pub value: Condition,
    pub override_id: String,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
    #[serde(rename = "override")]
    pub override_: Overrides,
    pub last_modified_at: DateTime<Utc>,
    pub last_modified_by: String,
    pub weight: BigDecimal,
    pub description: Description,
    pub change_reason: ChangeReason,
}

impl Contextual for Context {
    fn get_condition(&self) -> Condition {
        self.value.clone()
    }
}

impl Overridden<Cac<Overrides>> for Context {
    fn get_overrides(&self) -> Overrides {
        self.override_.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
#[cfg_attr(
    feature = "diesel_derives",
    derive(Eq, AsExpression, FromSqlRow, TextFromSql, TextToSql)
)]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = diesel::sql_types::Text))]
pub enum DimensionType {
    Regular {},
    LocalCohort(String),
    RemoteCohort(String),
}

impl Default for DimensionType {
    fn default() -> Self {
        DimensionType::Regular{}
    }
}

impl Display for DimensionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DimensionType::Regular{} => write!(f, "REGULAR"),
            DimensionType::LocalCohort(cohort_based_on) => {
                write!(f, "LOCAL_COHORT:{}", cohort_based_on)
            }
            DimensionType::RemoteCohort(cohort_based_on) => {
                write!(f, "REMOTE_COHORT:{}", cohort_based_on)
            }
        }
    }
}

#[cfg(feature = "diesel_derives")]
impl FromStr for DimensionType {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split(':').collect();
        match parts[0] {
            "REGULAR" => Ok(DimensionType::Regular{}),
            "LOCAL_COHORT" => Ok(DimensionType::LocalCohort(parts[1].to_string())),
            "REMOTE_COHORT" => Ok(DimensionType::RemoteCohort(parts[1].to_string())),
            _ => Err(format!("Invalid dimension type: {}", s)),
        }
    }
}

#[cfg(feature = "diesel_derives")]
impl TryFrom<String> for DimensionType {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        DimensionType::from_str(&value)
    }
}

#[cfg(feature = "diesel_derives")]
impl From<&DimensionType> for String {
    fn from(value: &DimensionType) -> String {
        value.to_string()
    }
}

#[derive(Clone, Serialize, Deserialize)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(Queryable, Selectable, Insertable, AsChangeset)
)]
#[cfg_attr(feature = "diesel_derives", diesel(check_for_backend(diesel::pg::Pg)))]
#[cfg_attr(feature = "diesel_derives", diesel(primary_key(dimension)))]
#[cfg_attr(feature = "diesel_derives", diesel(treat_none_as_null = true))]
pub struct Dimension {
    pub dimension: String,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
    pub schema: Value,
    pub function_name: Option<String>,
    pub last_modified_at: DateTime<Utc>,
    pub last_modified_by: String,
    pub position: Position,
    pub description: Description,
    pub change_reason: ChangeReason,
    pub dependency_graph: DependencyGraph,
    pub autocomplete_function_name: Option<String>,
    pub dimension_type: DimensionType,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(Queryable, Selectable, Insertable, AsChangeset)
)]
#[cfg_attr(feature = "diesel_derives", diesel(check_for_backend(diesel::pg::Pg)))]
#[cfg_attr(feature = "diesel_derives", diesel(primary_key(key)))]
#[cfg_attr(feature = "diesel_derives", diesel(treat_none_as_null = true))]
pub struct DefaultConfig {
    pub key: String,
    pub value: Value,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
    pub schema: Value,
    pub function_name: Option<String>,
    pub last_modified_at: DateTime<Utc>,
    pub last_modified_by: String,
    pub description: Description,
    pub change_reason: ChangeReason,
    pub autocomplete_function_name: Option<String>,
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Deserialize,
    Serialize,
    Default,
    strum_macros::Display,
    strum_macros::EnumIter,
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
    ExistingTypePath = "crate::database::schema::sql_types::FunctionTypes"
)]
pub enum FunctionType {
    #[default]
    Validation,
    Autocomplete,
}

impl FunctionType {
    pub fn get_fn_signature(&self) -> String {
        match self {
            FunctionType::Validation => "validate({key}, {value})".to_string(),
            FunctionType::Autocomplete => {
                "autocomplete({name}, {prefix}, {environment})".to_string()
            }
        }
    }

    pub fn get_js_fn_name(&self) -> String {
        match self {
            FunctionType::Validation => "validate".to_string(),
            FunctionType::Autocomplete => "autocomplete".to_string(),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(Queryable, Selectable, Insertable, AsChangeset)
)]
#[cfg_attr(feature = "diesel_derives", diesel(check_for_backend(diesel::pg::Pg)))]
#[cfg_attr(feature = "diesel_derives", diesel(primary_key(name)))]
pub struct Function {
    pub function_name: String,
    pub published_code: Option<FunctionCode>,
    pub draft_code: FunctionCode,
    pub description: Description,
    pub published_runtime_version: Option<String>,
    pub draft_runtime_version: String,
    pub published_at: Option<DateTime<Utc>>,
    pub draft_edited_at: DateTime<Utc>,
    pub published_by: Option<String>,
    pub draft_edited_by: String,
    pub last_modified_at: DateTime<Utc>,
    pub last_modified_by: String,
    pub change_reason: ChangeReason,
    pub function_type: FunctionType,
    pub created_by: String,
    pub created_at: DateTime<Utc>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
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

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "diesel_derives", derive(Queryable, Selectable, Insertable))]
#[cfg_attr(feature = "diesel_derives", diesel(check_for_backend(diesel::pg::Pg)))]
#[cfg_attr(feature = "diesel_derives", diesel(primary_key(id)))]
pub struct ConfigVersion {
    #[serde(with = "i64_formatter")]
    pub id: i64,
    pub config: Value,
    pub config_hash: String,
    pub tags: Option<Vec<String>>,
    pub created_at: DateTime<Utc>,
    pub description: Description,
}

#[derive(Deserialize, Serialize, Clone)]
#[cfg_attr(feature = "diesel_derives", derive(Queryable, Selectable))]
#[cfg_attr(feature = "diesel_derives", diesel(check_for_backend(diesel::pg::Pg)))]
#[cfg_attr(feature = "diesel_derives", diesel(table_name = config_versions))]
pub struct ConfigVersionListItem {
    #[serde(with = "i64_formatter")]
    pub id: i64,
    pub config_hash: String,
    pub tags: Option<Vec<String>>,
    pub created_at: DateTime<Utc>,
    pub description: Description,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(Queryable, Selectable, Insertable, AsChangeset)
)]
#[cfg_attr(feature = "diesel_derives", diesel(check_for_backend(diesel::pg::Pg)))]
#[cfg_attr(feature = "diesel_derives", diesel(primary_key(type_name)))]
pub struct TypeTemplate {
    pub type_name: String,
    pub type_schema: Value,
    pub created_by: String,
    pub created_at: DateTime<Utc>,
    pub last_modified_at: DateTime<Utc>,
    pub last_modified_by: String,
    pub description: Description,
    pub change_reason: ChangeReason,
}

#[derive(
    Deserialize,
    Serialize,
    Default,
    Clone,
    Deref,
    DerefMut,
    Debug,
    PartialEq,
    Eq,
    Ord,
    PartialOrd,
    Into,
    Copy,
)]
#[cfg_attr(feature = "diesel_derives", derive(AsExpression, FromSqlRow))]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = Integer))]
#[serde(try_from = "i32")]
pub struct Position(i32);

impl TryFrom<i32> for Position {
    type Error = String;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        if value < 0 {
            Err("Position should be greater than equal to 0".to_string())
        } else {
            Ok(Self(value))
        }
    }
}

impl From<u32> for Position {
    fn from(value: u32) -> Self {
        Self(value as i32)
    }
}

#[cfg(feature = "diesel_derives")]
impl FromSql<Integer, Pg> for Position {
    fn from_sql(bytes: PgValue<'_>) -> DResult<Self> {
        let value = <i32 as FromSql<Integer, Pg>>::from_sql(bytes)?;
        Ok(Position(value))
    }
}

#[cfg(feature = "diesel_derives")]
impl ToSql<Integer, Pg> for Position {
    fn to_sql<'b>(&'b self, out: &mut Output<'b, '_, Pg>) -> SResult {
        let value = self.to_owned().into();
        <i32 as ToSql<Integer, Pg>>::to_sql(&value, &mut out.reborrow())
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, Deref, Into, AsRef)]
#[cfg_attr(feature = "diesel_derives", derive(AsExpression, FromSqlRow))]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = diesel::sql_types::Text))]
pub struct FunctionCode(pub String);

impl Display for FunctionCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(feature = "diesel_derives")]
impl FromSql<diesel::sql_types::Text, Pg> for FunctionCode {
    fn from_sql(bytes: PgValue<'_>) -> DResult<Self> {
        let value = <String as FromSql<diesel::sql_types::Text, Pg>>::from_sql(bytes)?;
        let decode_val = decode_base64_to_string(&value)?;
        Ok(Self(decode_val))
    }
}

#[cfg(feature = "diesel_derives")]
impl ToSql<diesel::sql_types::Text, Pg> for FunctionCode {
    fn to_sql<'b>(&'b self, out: &mut Output<'b, '_, Pg>) -> SResult {
        let value: String = BASE64_STANDARD.encode(self.0.clone());
        <String as ToSql<diesel::sql_types::Text, Pg>>::to_sql(
            &value,
            &mut out.reborrow(),
        )
    }
}

#[cfg(feature = "diesel_derives")]
fn decode_base64_to_string(code: &String) -> Result<String, String> {
    BASE64_STANDARD
        .decode(code)
        .map_err(|e: base64::DecodeError| {
            log::info!("Error while decoding function: {}", e);
            e.to_string()
        })
        .and_then(|decoded_code| {
            str::from_utf8(&decoded_code)
                .map(|res| res.to_string())
                .map_err(|err| {
                    log::info!("Error while decoding function: {}", err);
                    err.to_string()
                })
        })
}

pub fn deserialize_function_name<'de, D>(
    deserializer: D,
) -> Result<Option<Option<String>>, D::Error>
where
    D: Deserializer<'de>,
{
    let opt: Result<Value, _> = Deserialize::deserialize(deserializer);
    match opt {
        Ok(Value::String(func_name)) => Ok(Some(Some(func_name))),
        Ok(Value::Null) => Ok(Some(None)),
        Err(_) => Ok(None), // If the field is missing, return None instead of throwing an errors
        _ => {
            log::error!("Expected a string or null literal as the function name.");
            Err(serde::de::Error::custom(
                "Expected a string or null literal as the function name.",
            ))
        }
    }
}

#[derive(
    Deserialize, Serialize, Clone, Deref, DerefMut, Debug, PartialEq, Into, AsRef, Default,
)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(AsExpression, FromSqlRow, JsonFromSql, JsonToSql)
)]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = Json))]
pub struct DependencyGraph(pub HashMap<String, Vec<String>>);

impl DependencyGraph {
    pub fn new() -> Self {
        Self::default()
    }
}
