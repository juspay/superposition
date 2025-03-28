#[cfg(feature = "diesel_derives")]
use std::str;

#[cfg(feature = "diesel_derives")]
use base64::prelude::*;
use bigdecimal::BigDecimal;
use chrono::{offset::Utc, DateTime, NaiveDateTime};
use derive_more::{AsRef, Deref, DerefMut, Into};
#[cfg(feature = "diesel_derives")]
use diesel::{
    deserialize::{FromSql, FromSqlRow, Result as DResult},
    expression::AsExpression,
    pg::{Pg, PgValue},
    serialize::{Output, Result as SResult, ToSql},
    sql_types::Integer,
    AsChangeset, Insertable, Queryable, Selectable,
};
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::Value;

use crate::{Cac, Condition, Contextual, Overridden, Overrides};

#[cfg(feature = "diesel_derives")]
use super::super::schema::{
    config_versions, contexts, default_configs, dimensions, event_log, functions,
    type_templates,
};

#[derive(Clone, Serialize, Debug)]
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
    #[serde(rename(serialize = "override"))]
    pub override_: Overrides,
    pub last_modified_at: NaiveDateTime,
    pub last_modified_by: String,
    pub weight: BigDecimal,
    pub description: String,
    pub change_reason: String,
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
    pub last_modified_at: NaiveDateTime,
    pub last_modified_by: String,
    pub position: Position,
    pub description: String,
    pub change_reason: String,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
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
    pub last_modified_at: NaiveDateTime,
    pub last_modified_by: String,
    pub description: String,
    pub change_reason: String,
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
    pub description: String,
    pub published_runtime_version: Option<String>,
    pub draft_runtime_version: String,
    pub published_at: Option<NaiveDateTime>,
    pub draft_edited_at: NaiveDateTime,
    pub published_by: Option<String>,
    pub draft_edited_by: String,
    pub last_modified_at: NaiveDateTime,
    pub last_modified_by: String,
    pub change_reason: String,
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
    pub timestamp: NaiveDateTime,
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
    pub id: i64,
    pub config: Value,
    pub config_hash: String,
    pub tags: Option<Vec<String>>,
    pub created_at: NaiveDateTime,
    pub description: String,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
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
    pub created_at: NaiveDateTime,
    pub last_modified_at: NaiveDateTime,
    pub last_modified_by: String,
    pub description: String,
    pub change_reason: String,
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
