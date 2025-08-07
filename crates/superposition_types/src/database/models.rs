pub mod cac;
#[cfg(feature = "experimentation")]
pub mod experimentation;
pub mod others;

use std::str::FromStr;

use chrono::{DateTime, Utc};
use derive_more::{Deref, DerefMut};
#[cfg(feature = "diesel_derives")]
use diesel::{
    sql_types::{Json, Text},
    AsChangeset, AsExpression, FromSqlRow, Insertable, QueryId, Queryable, Selectable,
};
use serde::{Deserialize, Deserializer, Serialize};
#[cfg(all(
    feature = "diesel_derives",
    not(feature = "disable_db_data_validation")
))]
use superposition_derives::TextFromSql;
#[cfg(all(feature = "diesel_derives", feature = "disable_db_data_validation"))]
use superposition_derives::TextFromSqlNoValidation;
#[cfg(feature = "diesel_derives")]
use superposition_derives::{JsonFromSql, JsonToSql, TextToSql};

#[cfg(feature = "diesel_derives")]
use super::superposition_schema::superposition::*;
#[cfg(feature = "disable_db_data_validation")]
use super::DisableDBValidation;

#[derive(Deserialize, Serialize, Debug, Clone, PartialEq, Deref, DerefMut)]
#[serde(try_from = "String")]
#[cfg_attr(
    feature = "diesel_derives",
    derive(AsExpression, FromSqlRow, TextToSql)
)]
#[cfg_attr(
    all(
        feature = "diesel_derives",
        not(feature = "disable_db_data_validation")
    ),
    derive(TextFromSql)
)]
#[cfg_attr(
    all(feature = "diesel_derives", feature = "disable_db_data_validation"),
    derive(TextFromSqlNoValidation)
)]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = Text))]
pub struct ChangeReason(String);
const CHANGE_REASON_CHAR_LIMIT: usize = 255;

impl Default for ChangeReason {
    fn default() -> Self {
        Self(String::from("Change Reason not provided"))
    }
}

#[cfg(feature = "disable_db_data_validation")]
impl DisableDBValidation for ChangeReason {
    type Source = String;
    fn from_db_unvalidated(data: Self::Source) -> Self {
        // Defaulting, to convert "" entries to Self::default
        Self::try_from(data).unwrap_or_default()
    }
}

impl From<&ChangeReason> for String {
    fn from(value: &ChangeReason) -> String {
        value.0.clone()
    }
}

impl TryFrom<String> for ChangeReason {
    type Error = String;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        if value.is_empty() {
            return Err(String::from("Empty reason not allowed"));
        }
        let len = value.len();
        if len > CHANGE_REASON_CHAR_LIMIT {
            return Err(format!(
                "Reason longer than {CHANGE_REASON_CHAR_LIMIT} characters not allowed, current length: {len}",
            ));
        }
        Ok(Self(value))
    }
}

#[derive(Deserialize, Serialize, Debug, Clone, PartialEq, Deref, DerefMut)]
#[serde(try_from = "String")]
#[cfg_attr(
    feature = "diesel_derives",
    derive(AsExpression, FromSqlRow, TextToSql)
)]
#[cfg_attr(
    all(
        feature = "diesel_derives",
        not(feature = "disable_db_data_validation")
    ),
    derive(TextFromSql)
)]
#[cfg_attr(
    all(feature = "diesel_derives", feature = "disable_db_data_validation"),
    derive(TextFromSqlNoValidation)
)]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = Text))]
pub struct Description(String);
const DESCRIPTION_CHAR_LIMIT: usize = 1024;

impl Description {
    pub fn try_from_change_reasons(
        change_reasons: Vec<ChangeReason>,
    ) -> Result<Self, String> {
        let description = change_reasons
            .into_iter()
            .map(|reason| reason.0)
            .collect::<Vec<_>>()
            .join(", ");

        if description.is_empty() {
            return Err(String::from("Empty description not allowed"));
        }

        match Self::try_from(description.clone()) {
            Ok(desc) => Ok(desc),
            Err(_) => {
                let truncated_desc = description
                    .chars()
                    .take(DESCRIPTION_CHAR_LIMIT - 3)
                    .collect::<String>()
                    + "...";
                Self::try_from(truncated_desc)
            }
        }
    }
}

impl Default for Description {
    fn default() -> Self {
        Self(String::from("Description not provided"))
    }
}

#[cfg(feature = "disable_db_data_validation")]
impl DisableDBValidation for Description {
    type Source = String;
    fn from_db_unvalidated(data: Self::Source) -> Self {
        // Defaulting, to convert "" entries to Self::default
        Self::try_from(data).unwrap_or_default()
    }
}

impl From<&Description> for String {
    fn from(value: &Description) -> String {
        value.0.clone()
    }
}

impl TryFrom<String> for Description {
    type Error = String;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        if value.is_empty() {
            return Err(String::from("Empty description not allowed"));
        }
        let len = value.len();
        if len > DESCRIPTION_CHAR_LIMIT {
            return Err(format!(
                "Description longer than {DESCRIPTION_CHAR_LIMIT} characters not allowed, current length: {len}",
            ));
        }
        Ok(Self(value))
    }
}

impl From<ChangeReason> for Description {
    fn from(value: ChangeReason) -> Self {
        Self(value.0)
    }
}

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
    pub updated_at: DateTime<Utc>,
    pub created_at: DateTime<Utc>,
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
    pub config_version: Option<i64>,
    pub created_by: String,
    pub last_modified_by: String,
    pub last_modified_at: DateTime<Utc>,
    pub created_at: DateTime<Utc>,
    pub mandatory_dimensions: Option<Vec<String>>,
    pub strict_mode: bool,
    pub metrics: Metrics,
    pub allow_experiment_self_approval: bool,
    pub auto_populate_control: bool,
}

#[derive(Clone, Serialize, Deserialize, Debug)]
#[serde(rename_all = "lowercase")]
pub enum MetricSource {
    Grafana {
        base_url: String,
        dashboard_uid: String,
        dashboard_slug: String,
        variant_id_alias: Option<String>,
    },
}

impl Default for MetricSource {
    fn default() -> Self {
        Self::Grafana {
            base_url: String::new(),
            dashboard_uid: String::new(),
            dashboard_slug: String::new(),
            variant_id_alias: None,
        }
    }
}

#[derive(Clone, Debug, Serialize, Default)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(AsExpression, FromSqlRow, JsonFromSql, JsonToSql)
)]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = Json))]
pub struct Metrics {
    pub enabled: bool,
    pub source: Option<MetricSource>,
}

// TODO: Add validation for the source - that the given URL is valid and the
// dashboard UID and slug are present in the URL - possible once API_KEY is added

impl Metrics {
    pub fn source(&self) -> Option<MetricSource> {
        self.enabled.then(|| self.source.clone()).flatten()
    }
}

impl<'de> Deserialize<'de> for Metrics {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct MetricsHelper {
            enabled: bool,
            source: Option<MetricSource>,
        }
        let helper = MetricsHelper::deserialize(deserializer)?;
        if helper.enabled && helper.source.is_none() {
            return Err(serde::de::Error::custom(
                "`source` must be provided when enabled is true",
            ));
        }
        Ok(Metrics {
            enabled: helper.enabled,
            source: helper.source.clone(),
        })
    }
}

#[derive(Deserialize, Serialize, Debug, Clone, PartialEq, Deref, DerefMut)]
#[serde(try_from = "String")]
#[cfg_attr(
    feature = "diesel_derives",
    derive(AsExpression, FromSqlRow, TextToSql)
)]
#[cfg_attr(
    all(
        feature = "diesel_derives",
        not(feature = "disable_db_data_validation")
    ),
    derive(TextFromSql)
)]
#[cfg_attr(
    all(feature = "diesel_derives", feature = "disable_db_data_validation"),
    derive(TextFromSqlNoValidation)
)]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = Text))]
pub struct NonEmptyString(String);

impl Default for NonEmptyString {
    fn default() -> Self {
        Self(String::from("String not provided"))
    }
}

#[cfg(feature = "disable_db_data_validation")]
impl DisableDBValidation for NonEmptyString {
    type Source = String;
    fn from_db_unvalidated(data: Self::Source) -> Self {
        // Defaulting, to convert "" entries to Self::default
        Self::try_from(data).unwrap_or_default()
    }
}

impl From<&NonEmptyString> for String {
    fn from(value: &NonEmptyString) -> String {
        value.0.clone()
    }
}

impl TryFrom<String> for NonEmptyString {
    type Error = String;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        if value.is_empty() {
            return Err(String::from("Empty value not allowed"));
        }
        Ok(Self(value))
    }
}

pub mod i64_formatter {
    use serde::{self, Deserialize, Deserializer, Serializer};

    // Serialize i64 to String
    pub fn serialize<S>(value: &i64, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&value.to_string())
    }

    // Deserialize String to i64
    pub fn deserialize<'de, D>(deserializer: D) -> Result<i64, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        s.parse::<i64>()
            .map_err(|e| serde::de::Error::custom(format!("Failed to parse i64: {}", e)))
    }
}
