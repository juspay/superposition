use chrono::{DateTime, Utc};
use derive_more::{AsRef, Deref, DerefMut, Into};
#[cfg(feature = "diesel_derives")]
use diesel::{
    deserialize::FromSqlRow,
    expression::AsExpression,
    query_builder::QueryId,
    sql_types::{Json, Text},
    AsChangeset, Insertable, Queryable, Selectable,
};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use strum_macros::{Display, EnumIter, EnumString};
#[cfg(feature = "diesel_derives")]
use superposition_derives::{JsonFromSql, JsonToSql};
#[cfg(feature = "diesel_derives")]
use superposition_derives::{TextFromSql, TextToSql};

use crate::RegexEnum;

#[cfg(feature = "diesel_derives")]
use super::super::schema::{variables, webhooks};
use super::{ChangeReason, Description, NonEmptyString};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Default)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(Queryable, Selectable, Insertable, AsChangeset)
)]
#[cfg_attr(feature = "diesel_derives", diesel(check_for_backend(diesel::pg::Pg)))]
#[cfg_attr(feature = "diesel_derives", diesel(table_name = webhooks))]
#[cfg_attr(feature = "diesel_derives", diesel(primary_key(name)))]
pub struct Webhook {
    pub name: String,
    pub description: Description,
    pub enabled: bool,
    pub url: NonEmptyString,
    pub method: HttpMethod,
    pub payload_version: PayloadVersion,
    pub custom_headers: CustomHeaders,
    pub events: Vec<WebhookEvent>,
    pub max_retries: i32,
    pub last_triggered_at: Option<DateTime<Utc>>,
    pub change_reason: ChangeReason,
    pub created_by: String,
    pub created_at: DateTime<Utc>,
    pub last_modified_by: String,
    pub last_modified_at: DateTime<Utc>,
}

#[derive(
    Debug,
    Serialize,
    Deserialize,
    Clone,
    Copy,
    PartialEq,
    Default,
    strum_macros::Display,
    strum_macros::EnumString,
    strum_macros::EnumIter,
)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(Eq, AsExpression, FromSqlRow, TextFromSql, TextToSql)
)]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = Text))]
pub enum PayloadVersion {
    #[default]
    V1,
}

impl TryFrom<String> for PayloadVersion {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value
            .as_str()
            .try_into()
            .map_err(|e| format!("Invalid WebhookVersion: {e}"))
    }
}

impl From<&PayloadVersion> for String {
    fn from(value: &PayloadVersion) -> Self {
        value.to_string()
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Deserialize,
    Serialize,
    strum_macros::Display,
    strum_macros::EnumIter,
    Default,
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
    ExistingTypePath = "crate::database::schema::sql_types::HttpMethod"
)]
pub enum HttpMethod {
    Get,
    Put,
    #[default]
    Post,
    Delete,
    Patch,
    Head,
}

#[derive(
    Serialize, Deserialize, Debug, Clone, Copy, Display, PartialEq, EnumString, EnumIter,
)]
#[cfg_attr(feature = "diesel_derives", derive(TextFromSql, TextToSql))]
pub enum WebhookEvent {
    ExperimentCreated,
    ExperimentStarted,
    ExperimentInprogress,
    ExperimentUpdated,
    ExperimentConcluded,
    ExperimentDiscarded,
    ExperimentPaused,
}

impl TryFrom<String> for WebhookEvent {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value
            .as_str()
            .try_into()
            .map_err(|e| format!("Invalid WebhookEvent: {e}"))
    }
}

impl From<&WebhookEvent> for String {
    fn from(value: &WebhookEvent) -> Self {
        value.to_string()
    }
}

#[derive(
    Deserialize, Serialize, Clone, Deref, Debug, PartialEq, Into, AsRef, Default,
)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(AsExpression, FromSqlRow, JsonFromSql, JsonToSql)
)]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = Json))]
pub struct CustomHeaders(Map<String, Value>);

impl From<Map<String, Value>> for CustomHeaders {
    fn from(value: Map<String, Value>) -> Self {
        Self(value)
    }
}

#[derive(
    Debug, Serialize, Deserialize, AsRef, Deref, DerefMut, Clone, PartialEq, Into,
)]
#[serde(try_from = "String")]
#[cfg_attr(
    feature = "diesel_derives",
    derive(AsExpression, FromSqlRow, TextFromSql, TextToSql)
)]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = Text))]
pub struct VariableName(pub String);

impl VariableName {
    pub fn validate(name: String) -> Result<Self, String> {
        let name = name.trim();

        RegexEnum::VariableName
            .match_regex(name)
            .map(|_| Self(name.to_string()))
    }
}

impl TryFrom<String> for VariableName {
    type Error = String;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::validate(value)
    }
}

impl From<&VariableName> for String {
    fn from(value: &VariableName) -> Self {
        value.0.clone()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(Queryable, Selectable, Insertable, AsChangeset)
)]
#[cfg_attr(feature = "diesel_derives", diesel(check_for_backend(diesel::pg::Pg)))]
#[cfg_attr(feature = "diesel_derives", diesel(primary_key(name)))]
pub struct Variable {
    pub name: VariableName,
    pub value: String,
    pub description: Description,
    pub change_reason: ChangeReason,
    pub created_at: DateTime<Utc>,
    pub last_modified_at: DateTime<Utc>,
    pub created_by: String,
    pub last_modified_by: String,
}
