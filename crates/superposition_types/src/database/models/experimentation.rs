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
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_json::Value;
#[cfg(feature = "diesel_derives")]
use superposition_derives::{JsonFromSql, JsonToSql};
use uniffi::deps::anyhow;

use crate::{
    experimental::{Experimental, ExperimentalVariants},
    Condition, Contextual, Exp, Overridden, Overrides,
};

#[cfg(feature = "diesel_derives")]
use super::super::schema::*;
use super::{i64_formatter, ChangeReason, Description, MetricSelection, MetricSource};

#[derive(Clone, Debug, Default, PartialEq)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(AsExpression, FromSqlRow, JsonFromSql, JsonToSql)
)]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = Json))]
pub struct ExperimentMetrics {
    selection: Option<MetricSelection>,
    source: Option<MetricSource>,
}

impl ExperimentMetrics {
    /// Create a new ExperimentMetrics with at least one of selection or source.
    /// Use `ExperimentMetrics::default()` for disabled metrics.
    pub fn new(
        selection: Option<MetricSelection>,
        source: Option<MetricSource>,
    ) -> Result<Self, String> {
        if selection.is_none() && source.is_none() {
            return Err("At least one of selection or source must be provided. \
                 Use ExperimentMetrics::default() for disabled metrics."
                .to_string());
        }
        Ok(Self { selection, source })
    }

    /// Check if metrics are enabled (at least one of selection or source is present)
    pub fn is_enabled(&self) -> bool {
        self.selection.is_some() || self.source.is_some()
    }

    /// Returns a reference to the metric source, if present.
    pub fn source(&self) -> Option<&MetricSource> {
        self.source.as_ref()
    }

    /// Build directly from optional parts. `(None, None)` is the disabled
    /// sentinel (equivalent to `default()`); any other combination represents
    /// enabled metrics. Prefer `new()` when validation of "at least one
    /// present" is desired.
    pub fn from_parts(
        selection: Option<MetricSelection>,
        source: Option<MetricSource>,
    ) -> Self {
        Self { selection, source }
    }

    /// Returns a reference to the metric selection, if present.
    pub fn selection(&self) -> Option<&MetricSelection> {
        self.selection.as_ref()
    }

    /// Consumes self and returns both the selection and source.
    pub fn into_parts(self) -> (Option<MetricSelection>, Option<MetricSource>) {
        (self.selection, self.source)
    }
}

impl Serialize for ExperimentMetrics {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match (&self.selection, &self.source) {
            // Default: Disabled
            (None, None) => serde_json::json!({ "enabled": false }).serialize(serializer),
            // Both present: merge into single object
            (Some(selection), Some(source)) => {
                let mut value =
                    serde_json::to_value(selection).map_err(serde::ser::Error::custom)?;
                value
                    .as_object_mut()
                    .expect("selection serializes to object")
                    .insert(
                        "source".to_string(),
                        serde_json::to_value(source)
                            .map_err(serde::ser::Error::custom)?,
                    );
                value.serialize(serializer)
            }
            // Only selection
            (Some(selection), None) => selection.serialize(serializer),
            // Only source
            (None, Some(source)) => {
                let mut map = serde_json::Map::new();
                map.insert(
                    "source".to_string(),
                    serde_json::to_value(source).map_err(serde::ser::Error::custom)?,
                );
                Value::Object(map).serialize(serializer)
            }
        }
    }
}

impl<'de> Deserialize<'de> for ExperimentMetrics {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value = Value::deserialize(deserializer)?;

        // Handle disabled/null case (default)
        if value.is_null() || value.get("enabled") == Some(&Value::Bool(false)) {
            return Ok(Self::default());
        }

        // Try to extract source
        let mut value = value;
        let source = value
            .as_object_mut()
            .and_then(|map| map.remove("source"))
            .map(serde_json::from_value)
            .transpose()
            .map_err(serde::de::Error::custom)?;

        // Try to extract selection (only if it has metric fields)
        let has_metric_fields =
            value.get("primary").is_some() || value.get("guardrail").is_some();

        let selection = if has_metric_fields {
            Some(serde_json::from_value(value).map_err(serde::de::Error::custom)?)
        } else {
            None
        };

        Ok(Self { selection, source })
    }
}

#[cfg(test)]
mod experiment_metrics_tests {
    use serde_json::json;

    use super::ExperimentMetrics;

    #[test]
    fn disabled_metrics_deserialize_to_default() {
        let metrics = serde_json::from_value::<ExperimentMetrics>(json!({
            "enabled": false
        }))
        .expect("disabled metrics sentinel");

        assert!(metrics.selection().is_none());
        assert!(metrics.source().is_none());
        assert!(!metrics.is_enabled());
    }

    #[test]
    fn default_serializes_to_disabled() {
        assert_eq!(
            serde_json::to_value(ExperimentMetrics::default()).unwrap(),
            json!({ "enabled": false })
        );
    }

    #[test]
    fn source_only_round_trips() {
        let value = json!({
            "source": {
                "grafana": {
                    "base_url": "https://grafana.example.com",
                    "dashboard_uid": "experiment-metrics",
                    "dashboard_slug": "experiments",
                    "variant_id_alias": null
                }
            }
        });
        let metrics = serde_json::from_value::<ExperimentMetrics>(value.clone())
            .expect("source only");

        assert!(metrics.selection().is_none());
        assert!(metrics.source().is_some());
        assert!(metrics.is_enabled());
        assert_eq!(serde_json::to_value(metrics).unwrap(), value);
    }

    #[test]
    fn selection_only_round_trips() {
        let value = json!({
            "primary": {"name": "conversion", "direction": "maximize"},
            "secondary": null,
            "guardrail": {"name": "latency", "direction": "minimize"}
        });
        let metrics = serde_json::from_value::<ExperimentMetrics>(value.clone())
            .expect("selection only");

        assert!(metrics.selection().is_some());
        assert!(metrics.source().is_none());
        assert!(metrics.is_enabled());
        assert_eq!(serde_json::to_value(metrics).unwrap(), value);
    }

    #[test]
    fn both_selection_and_source_round_trips() {
        let value = json!({
            "primary": {"name": "conversion", "direction": "maximize"},
            "secondary": null,
            "guardrail": {"name": "latency", "direction": "minimize"},
            "source": {
                "grafana": {
                    "base_url": "https://grafana.example.com",
                    "dashboard_uid": "experiment-metrics",
                    "dashboard_slug": "experiments",
                    "variant_id_alias": null
                }
            }
        });
        let metrics = serde_json::from_value::<ExperimentMetrics>(value.clone())
            .expect("both selection and source");

        assert!(metrics.selection().is_some());
        assert!(metrics.source().is_some());
        assert!(metrics.is_enabled());
        assert_eq!(serde_json::to_value(metrics).unwrap(), value);
    }

    #[test]
    fn new_requires_at_least_one() {
        use crate::database::models::{MetricDefinition, MetricDirection, MetricSource};

        let selection = crate::database::models::MetricSelection {
            primary: MetricDefinition {
                name: "conversion".to_string(),
                direction: MetricDirection::Maximize,
            },
            secondary: None,
            guardrail: MetricDefinition {
                name: "latency".to_string(),
                direction: MetricDirection::Minimize,
            },
        };

        assert!(ExperimentMetrics::new(None, None).is_err());
        assert!(ExperimentMetrics::new(Some(selection.clone()), None).is_ok());
        assert!(ExperimentMetrics::new(None, Some(MetricSource::default())).is_ok());
        assert!(
            ExperimentMetrics::new(Some(selection), Some(MetricSource::default()))
                .is_ok()
        );
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

    pub fn compare_old(&self, old: &Self) -> bool {
        self.0 != 0 && self.0 == old.0
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

impl Overridden for Variant {
    fn get_overrides_mut(&mut self) -> &mut Overrides {
        &mut self.overrides
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

impl IntoIterator for Variants {
    type Item = Variant;
    type IntoIter = std::vec::IntoIter<Variant>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
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
    pub metrics: ExperimentMetrics,
    pub experiment_group_id: Option<i64>,
    pub idempotency_key: Option<String>,
}

impl Contextual for Experiment {
    fn get_condition(&self) -> &Condition {
        &self.context
    }
}

impl Experimental for Experiment {
    fn get_condition(&self) -> &Condition {
        &self.context
    }
}

impl ExperimentalVariants for Experiment {
    fn get_variants_mut(&mut self) -> &mut Vec<Variant> {
        &mut self.variants
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

impl Contextual for ExperimentGroup {
    fn get_condition(&self) -> &Condition {
        &self.context
    }
}

impl Experimental for ExperimentGroup {
    fn get_condition(&self) -> &Condition {
        &self.context
    }
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
#[derive(uniffi::Record)]
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

uniffi::custom_type!(Buckets, Vec<Option<Bucket>>, {
    try_lift: |val: Vec<Option<Bucket>>| {
        Buckets::try_from(val).map_err(|e| anyhow::anyhow!(e))
    },
    lower: |obj: Buckets| obj.0.to_vec(),
});

#[cfg(test)]
mod tests {
    /// These cases pin down the intended behaviour of `TrafficPercentage`, not
    /// the current shape of its implementation. Do NOT edit or delete them to
    /// make a change compile or pass — if a change breaks one of these tests,
    /// the change is wrong unless the intended behaviour itself was explicitly
    /// agreed to be different. Add new cases freely; adjust existing ones only
    /// alongside a deliberate, documented behaviour change.
    mod traffic_percentage {
        use crate::database::models::experimentation::TrafficPercentage;

        fn traffic(value: u8) -> TrafficPercentage {
            TrafficPercentage::try_from(value).unwrap()
        }

        #[test]
        fn test_compare_old_both_zero() {
            // A ramp of 0 is never treated as a no-op, even when unchanged.
            assert!(!traffic(0).compare_old(&traffic(0)));
        }

        #[test]
        fn test_compare_old_zero_to_non_zero() {
            assert!(!traffic(10).compare_old(&traffic(0)));
        }

        #[test]
        fn test_compare_old_non_zero_to_zero() {
            assert!(!traffic(0).compare_old(&traffic(10)));
        }

        #[test]
        fn test_compare_old_same_non_zero() {
            assert!(traffic(10).compare_old(&traffic(10)));
        }
    }
}
