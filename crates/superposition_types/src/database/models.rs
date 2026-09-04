pub mod cac;
#[cfg(feature = "experimentation")]
pub mod experimentation;
pub mod others;

use std::{collections::HashSet, str::FromStr};

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
    pub name: NonEmptyString,
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
    pub organisation_name: NonEmptyString,
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
    pub metrics: Metrics,
    pub allow_experiment_self_approval: bool,
    pub auto_populate_control: bool,
    pub enable_context_validation: bool,
    pub enable_change_reason_validation: bool,
    pub encryption_key: String,
    pub key_rotated_at: Option<DateTime<Utc>>,
    pub workspace_lock_id: Option<uuid::Uuid>,
    pub workspace_lock_operation: Option<String>,
    pub workspace_locked_by: Option<String>,
    pub workspace_lock_acquired_at: Option<DateTime<Utc>>,
    pub workspace_lock_expires_at: Option<DateTime<Utc>>,
}

#[derive(Clone, PartialEq, Serialize, Debug)]
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

impl<'de> Deserialize<'de> for MetricSource {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(rename_all = "lowercase")]
        enum MetricSourceHelper {
            Grafana {
                base_url: String,
                dashboard_uid: String,
                dashboard_slug: String,
                variant_id_alias: Option<String>,
            },
        }

        let MetricSourceHelper::Grafana {
            base_url,
            dashboard_uid,
            dashboard_slug,
            variant_id_alias,
        } = MetricSourceHelper::deserialize(deserializer)?;

        let base_url = base_url.trim();
        let dashboard_uid = dashboard_uid.trim();
        let dashboard_slug = dashboard_slug.trim();
        // an empty optional alias is absent, not a reason to reject the source
        let variant_id_alias = variant_id_alias
            .map(|alias| alias.trim().to_string())
            .filter(|alias| !alias.is_empty());

        if base_url.is_empty() || dashboard_uid.is_empty() || dashboard_slug.is_empty() {
            return Err(serde::de::Error::custom(
                "Invalid Grafana source: base_url, dashboard_uid, and \
                 dashboard_slug must all be non-empty",
            ));
        }

        Ok(Self::Grafana {
            base_url: base_url.to_string(),
            dashboard_uid: dashboard_uid.to_string(),
            dashboard_slug: dashboard_slug.to_string(),
            variant_id_alias,
        })
    }
}

#[derive(
    Clone,
    Copy,
    Debug,
    Default,
    PartialEq,
    Eq,
    Serialize,
    Deserialize,
    strum_macros::Display,
    strum_macros::EnumIter,
)]
#[serde(rename_all = "snake_case")]
#[strum(serialize_all = "snake_case")]
pub enum MetricDirection {
    #[default]
    Maximize,
    Minimize,
}

#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct MetricDefinition {
    pub name: NonEmptyString,
    pub direction: MetricDirection,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, Default)]
pub struct MetricSelection {
    pub primary: MetricDefinition,
    pub secondary: Option<MetricDefinition>,
    pub guardrail: NonEmptyString,
}

impl MetricSelection {
    pub fn validate(&self, definitions: &[MetricDefinition]) -> Result<(), String> {
        let selected = [
            ("primary", Some(&self.primary.name)),
            ("secondary", self.secondary.as_ref().map(|def| &def.name)),
            ("guardrail", Some(&self.guardrail)),
        ];
        for (category, metric_name) in selected {
            let Some(metric_name) = metric_name else {
                continue;
            };
            if !definitions
                .iter()
                .any(|defined| defined.name == *metric_name)
            {
                let message = format!(
                "The {category} metric '{}' and its direction are not defined in the workspace",
                **metric_name
            );
                return Err(message);
            }
        }

        Ok(())
    }
}

// TODO: Add validation for the source - that the given URL is valid and the
// dashboard UID and slug are present in the URL - possible once API_KEY is added
#[derive(Clone, Debug, Serialize, Deserialize, Default)]
#[serde(try_from = "MetricsHelper")]
#[cfg_attr(
    feature = "diesel_derives",
    derive(AsExpression, FromSqlRow, JsonFromSql, JsonToSql)
)]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = Json))]
pub struct Metrics {
    pub enabled: bool,
    pub source: Option<MetricSource>,
    pub definitions: Option<Vec<MetricDefinition>>,
}

#[derive(Deserialize)]
struct MetricsHelper {
    enabled: bool,
    source: Option<MetricSource>,
    definitions: Option<Vec<MetricDefinition>>,
}

impl TryFrom<MetricsHelper> for Metrics {
    type Error = String;

    fn try_from(helper: MetricsHelper) -> Result<Self, Self::Error> {
        let definitions = helper.definitions.filter(|list| !list.is_empty());

        if let Some(definitions) = definitions.as_ref() {
            let mut seen = HashSet::with_capacity(definitions.len());
            for metric in definitions {
                if !seen.insert(&metric.name) {
                    return Err(format!(
                        "duplicate metric name in `definitions`: '{}'",
                        *metric.name
                    ));
                }
            }
        }

        if helper.enabled && helper.source.is_none() && definitions.is_none() {
            return Err(
                "at least one of `source` or a non-empty `definitions` must \
                 be provided when metrics is enabled"
                    .to_string(),
            );
        }

        Ok(Self {
            enabled: helper.enabled,
            source: helper.source,
            definitions,
        })
    }
}

#[derive(Deserialize, Serialize, Debug, Clone, PartialEq, Eq, Deref, Hash)]
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

impl NonEmptyString {
    pub fn inner(&self) -> &str {
        &self.0
    }

    pub fn into_inner(self) -> String {
        self.0
    }

    /// Trims before storing, so `"x"` and `" x "` are one value, not two.
    fn new(value: &str) -> Result<Self, String> {
        let trimmed = value.trim();
        if trimmed.is_empty() {
            return Err(String::from("Empty value not allowed"));
        }
        Ok(Self(trimmed.to_string()))
    }
}

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
        Self::new(&value)
    }
}

impl TryFrom<&str> for NonEmptyString {
    type Error = String;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl FromStr for NonEmptyString {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::new(s)
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

#[cfg(test)]
mod metrics_tests {
    use serde_json::json;

    use super::{MetricSelection, MetricSource, Metrics};

    fn enabled_metrics(definitions: serde_json::Value) -> serde_json::Value {
        json!({
            "enabled": true,
            "source": {
                "grafana": {
                    "base_url": "https://grafana.example.com",
                    "dashboard_uid": "experiment-metrics",
                    "dashboard_slug": "experiments"
                }
            },
            "definitions": definitions
        })
    }

    #[test]
    fn enabled_metrics_require_source_or_list() {
        // Neither source nor list: error
        assert!(serde_json::from_value::<Metrics>(json!({
            "enabled": true
        }))
        .is_err());
        // Source only: ok
        assert!(serde_json::from_value::<Metrics>(json!({
            "enabled": true,
            "source": {
                "grafana": {
                    "base_url": "https://grafana.example.com",
                    "dashboard_uid": "experiment-metrics",
                    "dashboard_slug": "experiments"
                }
            }
        }))
        .is_ok());
        // List only: ok
        assert!(serde_json::from_value::<Metrics>(json!({
            "enabled": true,
            "definitions": [{"name": "conversion", "direction": "maximize"}]
        }))
        .is_ok());
        // Both: ok
        assert!(serde_json::from_value::<Metrics>(enabled_metrics(json!([
            {"name": "conversion", "direction": "maximize"}
        ])))
        .is_ok());
    }

    #[test]
    fn enabled_metrics_reject_blank_or_duplicate_names() {
        assert!(serde_json::from_value::<Metrics>(enabled_metrics(json!([
            {"name": "conversion", "direction": "maximize"},
            {"name": " ", "direction": "minimize"}
        ])))
        .is_err());
        assert!(serde_json::from_value::<Metrics>(enabled_metrics(json!([
            {"name": "conversion", "direction": "maximize"},
            {"name": "conversion", "direction": "minimize"}
        ])))
        .is_err());
    }

    #[test]
    fn blank_grafana_source_is_rejected() {
        // A half-filled source is an error whether or not definitions are set.
        assert!(serde_json::from_value::<Metrics>(json!({
            "enabled": true,
            "source": {"grafana": {"base_url": "", "dashboard_uid": "", "dashboard_slug": ""}}
        }))
        .is_err());
        assert!(serde_json::from_value::<Metrics>(json!({
            "enabled": true,
            "source": {"grafana": {"base_url": "  ", "dashboard_uid": "", "dashboard_slug": ""}},
            "definitions": [{"name": "conversion", "direction": "maximize"}]
        }))
        .is_err());
        // `dashboard_slug` counts too.
        assert!(serde_json::from_value::<Metrics>(json!({
            "enabled": true,
            "source": {"grafana": {
                "base_url": "https://grafana.example.com",
                "dashboard_uid": "experiment-metrics",
                "dashboard_slug": " "
            }}
        }))
        .is_err());
    }

    #[test]
    fn grafana_source_is_trimmed_and_a_blank_alias_is_dropped() {
        let metrics = serde_json::from_value::<Metrics>(json!({
            "enabled": true,
            "source": {"grafana": {
                "base_url": "  https://grafana.example.com  ",
                "dashboard_uid": "experiment-metrics",
                "dashboard_slug": "experiments",
                "variant_id_alias": "  "
            }}
        }))
        .expect("blank alias is absent, not invalid");

        let Some(MetricSource::Grafana {
            base_url,
            variant_id_alias,
            ..
        }) = metrics.source
        else {
            panic!("expected a grafana source");
        };
        assert_eq!(base_url, "https://grafana.example.com");
        assert_eq!(variant_id_alias, None);
    }

    #[test]
    fn empty_definitions_normalise_to_none() {
        let metrics = serde_json::from_value::<Metrics>(json!({
            "enabled": false,
            "definitions": []
        }))
        .expect("empty definitions");
        assert!(metrics.definitions.is_none());
    }

    #[test]
    fn definition_names_are_validated_even_when_disabled() {
        assert!(serde_json::from_value::<Metrics>(json!({
            "enabled": false,
            "definitions": [
                {"name": "conversion", "direction": "maximize"},
                {"name": "conversion", "direction": "minimize"}
            ]
        }))
        .is_err());
    }

    #[test]
    fn names_differing_only_by_surrounding_space_are_duplicates() {
        assert!(serde_json::from_value::<Metrics>(json!({
            "enabled": true,
            "definitions": [
                {"name": "conversion", "direction": "maximize"},
                {"name": " conversion ", "direction": "minimize"}
            ]
        }))
        .is_err());
    }

    #[test]
    fn metric_selection_requires_primary_and_guardrail() {
        assert!(serde_json::from_value::<MetricSelection>(json!({
            "primary": {"name": "conversion", "direction": "maximize"}
        }))
        .is_err());
        assert!(serde_json::from_value::<MetricSelection>(json!({
            "guardrail": {"name": "latency", "direction": "minimize"}
        }))
        .is_err());
        assert!(serde_json::from_value::<MetricSelection>(json!({
            "primary": {"name": "conversion", "direction": "maximize"},
            "guardrail": {"name": "latency", "direction": "minimize"}
        }))
        .is_ok());
    }
}
