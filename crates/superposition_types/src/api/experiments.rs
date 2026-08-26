use chrono::{DateTime, Utc};
#[cfg(feature = "diesel_derives")]
use diesel::AsChangeset;
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::{Map, Value};
use strum_macros::Display;
use superposition_derives::{IsEmpty, QueryParam};

#[cfg(feature = "diesel_derives")]
use crate::database::schema::experiments;
use crate::{
    api::{deserialize_option_i64, i64_option_formatter, DimensionMatchStrategy},
    custom_query::{CommaSeparatedQParams, CommaSeparatedStringQParams, QueryParam},
    database::models::{
        experimentation::{
            Experiment, ExperimentMetrics, ExperimentStatusType, ExperimentType,
            TrafficPercentage, Variant, Variants,
        },
        ChangeReason, Description, MetricSelection, MetricSource,
    },
    experimental::{Experimental, ExperimentalVariants},
    Condition, Exp, IsEmpty, Overrides, SortBy,
};

use super::I64Update;

/// Update payload for an experiment's metrics.
///
/// Serializes to a flat object with optional selection keys (primary,
/// secondary, guardrail) and an optional `source` key:
/// - selection keys omitted: keep the experiment's existing selection
/// - `source` omitted: keep the experiment's existing source
/// - `source: null`: clear the experiment's source
/// - `source: {...}`: set a new source
///
/// At least one of selection keys or `source` must be present.
#[derive(Debug, Clone, PartialEq)]
pub enum MetricSelectionUpdate {
    Set {
        selection: Option<MetricSelection>,
        source: Option<Option<MetricSource>>,
    },
    Remove,
}

impl Serialize for MetricSelectionUpdate {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Self::Set { selection, source } => {
                let mut value = match selection {
                    Some(selection) => serde_json::to_value(selection)
                        .map_err(serde::ser::Error::custom)?,
                    None => serde_json::json!({}),
                };
                if let Some(source) = source {
                    value
                        .as_object_mut()
                        .expect("selection serializes to object")
                        .insert(
                            "source".to_string(),
                            serde_json::to_value(source)
                                .map_err(serde::ser::Error::custom)?,
                        );
                }
                value.serialize(serializer)
            }
            Self::Remove => serializer.serialize_none(),
        }
    }
}

fn deserialize_metric_selection_update<'de, D>(
    deserializer: D,
) -> Result<Option<MetricSelectionUpdate>, D::Error>
where
    D: Deserializer<'de>,
{
    let mut value = Value::deserialize(deserializer)?;
    if value.is_null() {
        return Ok(Some(MetricSelectionUpdate::Remove));
    }

    // Extract the `source` key before deserializing the selection, since the
    // selection itself has no `source` field. `null` means "clear the source",
    // a missing key means "leave the source unchanged".
    let source_value = value.as_object_mut().and_then(|map| map.remove("source"));
    let source = source_value
        .map(serde_json::from_value::<Option<MetricSource>>)
        .transpose()
        .map_err(serde::de::Error::custom)?;

    // Selection keys are optional: a payload with only `source` updates just
    // the source, leaving the existing selection untouched.
    let has_selection_keys =
        value.get("primary").is_some() || value.get("guardrail").is_some();
    let selection = if has_selection_keys {
        Some(serde_json::from_value(value).map_err(serde::de::Error::custom)?)
    } else {
        None
    };

    if selection.is_none() && source.is_none() {
        return Err(serde::de::Error::custom(
            "metrics update must contain at least one of selection keys or `source`",
        ));
    }

    Ok(Some(MetricSelectionUpdate::Set { selection, source }))
}

/********** Experiment Response Type **************/
// Same as models::Experiments but `id` field is String
// JS have limitation of 53-bit integers, so on
// deserializing from JSON to JS Object will lead incorrect `id` values
#[repr(C)]
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct ExperimentResponse {
    pub id: String,
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
    pub metrics_url: Option<String>,
    pub experiment_group_id: Option<String>,
}

impl From<Experiment> for ExperimentResponse {
    fn from(experiment: Experiment) -> Self {
        let metrics_url = experiment.started_at.and_then(|started_at| {
            experiment.metrics.source().map(|source| match source {
                MetricSource::Grafana {
                    base_url,
                    dashboard_uid,
                    dashboard_slug,
                    variant_id_alias,
                } => {
                    let to = if experiment.status.active() {
                        "now".to_string()
                    } else {
                        experiment.last_modified.to_string()
                    };
                    let from = started_at.timestamp_millis();
                    let variant_var = format!(
                        "var-{}",
                        variant_id_alias.as_deref().unwrap_or("variantIds")
                    );
                    let query = experiment
                        .variants
                        .iter()
                        .map(|variant| format!("{}={}", variant_var, variant.id))
                        .collect::<Vec<_>>()
                        .join("&");

                    format!(
                        "{base_url}/d/{dashboard_uid}/{dashboard_slug}?{query}&from={from}&to={to}&kiosk&theme=light"
                    )
                }
            })
        });

        Self {
            id: experiment.id.to_string(),
            created_at: experiment.created_at,
            created_by: experiment.created_by,
            last_modified: experiment.last_modified,

            name: experiment.name,
            experiment_type: experiment.experiment_type,
            override_keys: experiment.override_keys,
            status: experiment.status,
            traffic_percentage: experiment.traffic_percentage,
            started_at: experiment.started_at,
            started_by: experiment.started_by,

            context: experiment.context,
            variants: experiment.variants,
            last_modified_by: experiment.last_modified_by,
            chosen_variant: experiment.chosen_variant,
            description: experiment.description,
            change_reason: experiment.change_reason,
            metrics: experiment.metrics,
            metrics_url,
            experiment_group_id: experiment.experiment_group_id.map(|id| id.to_string()),
        }
    }
}

impl Experimental for ExperimentResponse {
    fn get_condition(&self) -> &Condition {
        &self.context
    }
}

impl ExperimentalVariants for ExperimentResponse {
    fn get_variants_mut(&mut self) -> &mut Vec<Variant> {
        &mut self.variants
    }
}

#[derive(Deserialize, Serialize)]
pub struct ExperimentCreateRequest {
    pub name: String,
    pub context: Exp<Condition>,
    pub variants: Vec<Variant>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metrics: Option<ExperimentMetrics>,
    #[serde(default)]
    pub experiment_type: ExperimentType,
    #[serde(default = "Description::default")]
    pub description: Description,
    #[serde(default = "ChangeReason::default")]
    pub change_reason: ChangeReason,
    #[serde(default, with = "i64_option_formatter")]
    pub experiment_group_id: Option<i64>,
}

/********** Experiment Ramp Req Types **********/

#[derive(Deserialize, Serialize, Debug)]
pub struct RampRequest {
    pub traffic_percentage: TrafficPercentage,
    #[serde(default = "ChangeReason::default")]
    pub change_reason: ChangeReason,
}

/********** Experiment Conclude Req Types **********/

#[derive(Deserialize, Serialize, Debug)]
pub struct ConcludeExperimentRequest {
    pub chosen_variant: String,
    pub description: Option<Description>,
    #[serde(default = "ChangeReason::default")]
    pub change_reason: ChangeReason,
}

/********** Experiment Discard Req Types **********/

#[derive(Deserialize, Serialize, Debug)]
#[cfg_attr(feature = "diesel_derives", derive(AsChangeset))]
#[cfg_attr(feature = "diesel_derives", diesel(table_name = experiments))]
pub struct ExperimentStateChangeRequest {
    #[serde(default = "ChangeReason::default")]
    pub change_reason: ChangeReason,
}

/********** Applicable Variants API Type *************/
#[derive(Debug, Deserialize, IsEmpty, QueryParam)]
pub struct ApplicableVariantsQuery {
    #[serde(alias = "toss")]
    pub identifier: Option<String>,
    #[query_param(skip_if_empty, iterable)]
    pub prefix: Option<CommaSeparatedStringQParams>,
    #[query_param(skip_if_empty, iterable)]
    pub exclude_prefix: Option<CommaSeparatedStringQParams>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ApplicableVariantsRequest {
    pub context: Map<String, Value>,
    // TODO: remove this once services start using newer sdk
    #[serde(alias = "toss", deserialize_with = "deserialize_identifier")]
    pub identifier: String,
}

fn deserialize_identifier<'de, D>(deserializer: D) -> Result<String, D::Error>
where
    D: Deserializer<'de>,
{
    match Value::deserialize(deserializer)? {
        Value::Number(toss) => Ok(toss.to_string()),
        Value::String(identifier) => Ok(identifier),
        _ => Err(serde::de::Error::custom("identifier must be a string")),
    }
}

/********** List API Filter Type *************/

#[derive(Copy, Display, Deserialize, Serialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "snake_case")]
#[strum(serialize_all = "snake_case")]
pub enum ExperimentSortOn {
    LastModifiedAt,
    CreatedAt,
}

impl Default for ExperimentSortOn {
    fn default() -> Self {
        Self::LastModifiedAt
    }
}

#[derive(Deserialize)]
pub struct ExperimentListRequest {
    pub context: Option<Map<String, Value>>,
}

#[derive(Deserialize, Clone, PartialEq, IsEmpty, QueryParam)]
pub struct ExperimentListFilters {
    #[query_param(skip_if_empty, iterable)]
    pub status: Option<CommaSeparatedQParams<ExperimentStatusType>>,
    pub from_date: Option<DateTime<Utc>>,
    pub to_date: Option<DateTime<Utc>>,
    #[query_param(skip_if_empty)]
    pub experiment_name: Option<String>,
    #[query_param(skip_if_empty, iterable)]
    pub experiment_ids: Option<CommaSeparatedStringQParams>,
    #[query_param(skip_if_empty, iterable)]
    pub experiment_group_ids: Option<CommaSeparatedStringQParams>,
    #[query_param(skip_if_empty, iterable)]
    pub created_by: Option<CommaSeparatedStringQParams>,
    pub sort_on: Option<ExperimentSortOn>,
    pub sort_by: Option<SortBy>,
    pub global_experiments_only: Option<bool>,
    #[query_param(skip_if_empty, iterable)]
    pub prefix: Option<CommaSeparatedStringQParams>,
    #[query_param(skip_if_empty, iterable)]
    pub exclude_prefix: Option<CommaSeparatedStringQParams>,
    pub dimension_match_strategy: Option<DimensionMatchStrategy>,
}

impl Default for ExperimentListFilters {
    fn default() -> Self {
        Self {
            status: Some(CommaSeparatedQParams(
                CommaSeparatedQParams::<ExperimentStatusType>::default()
                    .iter()
                    .filter(|&s| {
                        *s != ExperimentStatusType::DISCARDED
                            && *s != ExperimentStatusType::CONCLUDED
                    })
                    .copied()
                    .collect::<Vec<_>>(),
            )),
            from_date: None,
            to_date: None,
            experiment_name: None,
            experiment_ids: None,
            experiment_group_ids: None,
            created_by: None,
            sort_on: None,
            sort_by: Some(SortBy::Desc),
            global_experiments_only: None,
            prefix: None,
            exclude_prefix: None,
            dimension_match_strategy: None,
        }
    }
}

/********** Update API type ********/

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct VariantUpdateRequest {
    pub id: String,
    pub overrides: Exp<Overrides>,
}

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct OverrideKeysUpdateRequest {
    #[serde(alias = "variant_list")]
    pub variants: Vec<VariantUpdateRequest>,
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        deserialize_with = "deserialize_metric_selection_update"
    )]
    pub metrics: Option<MetricSelectionUpdate>,
    pub description: Option<Description>,
    #[serde(default = "ChangeReason::default")]
    pub change_reason: ChangeReason,
    #[serde(default, deserialize_with = "deserialize_option_i64")]
    pub experiment_group_id: Option<I64Update>,
}

#[cfg(test)]
mod metric_selection_update_tests {
    use serde_json::json;

    use super::{MetricSelectionUpdate, OverrideKeysUpdateRequest};
    use crate::database::models::ChangeReason;

    #[test]
    fn omitted_metrics_leave_the_selection_unchanged() {
        let request = serde_json::from_value::<OverrideKeysUpdateRequest>(json!({
            "variants": []
        }))
        .expect("update request without metrics");

        assert!(request.metrics.is_none());
    }

    #[test]
    fn null_metrics_clear_the_selection() {
        let request = serde_json::from_value::<OverrideKeysUpdateRequest>(json!({
            "variants": [],
            "metrics": null
        }))
        .expect("update request clearing metrics");

        assert!(matches!(
            request.metrics,
            Some(MetricSelectionUpdate::Remove)
        ));
    }

    #[test]
    fn metric_object_replaces_the_selection() {
        let request = serde_json::from_value::<OverrideKeysUpdateRequest>(json!({
            "variants": [],
            "metrics": {
                "primary": {
                    "name": "conversion",
                    "direction": "maximize"
                },
                "guardrail": {
                    "name": "latency",
                    "direction": "minimize"
                }
            }
        }))
        .expect("update request setting metrics");

        assert!(matches!(
            request.metrics,
            Some(MetricSelectionUpdate::Set { source: None, .. })
        ));
    }

    #[test]
    fn metrics_with_source_sets_the_source() {
        let request = serde_json::from_value::<OverrideKeysUpdateRequest>(json!({
            "variants": [],
            "metrics": {
                "primary": {
                    "name": "conversion",
                    "direction": "maximize"
                },
                "guardrail": {
                    "name": "latency",
                    "direction": "minimize"
                },
                "source": {
                    "grafana": {
                        "base_url": "https://grafana.example.com",
                        "dashboard_uid": "uid",
                        "dashboard_slug": "slug",
                        "variant_id_alias": null
                    }
                }
            }
        }))
        .expect("update request setting metrics with source");

        match request.metrics {
            Some(MetricSelectionUpdate::Set {
                source: Some(Some(_)),
                ..
            }) => {}
            other => panic!("expected Set with Some(Some(source)), got {other:?}"),
        }
    }

    #[test]
    fn null_source_clears_the_source() {
        let request = serde_json::from_value::<OverrideKeysUpdateRequest>(json!({
            "variants": [],
            "metrics": {
                "primary": {
                    "name": "conversion",
                    "direction": "maximize"
                },
                "guardrail": {
                    "name": "latency",
                    "direction": "minimize"
                },
                "source": null
            }
        }))
        .expect("update request clearing the source");

        match request.metrics {
            Some(MetricSelectionUpdate::Set {
                source: Some(None), ..
            }) => {}
            other => panic!("expected Set with Some(None), got {other:?}"),
        }
    }

    #[test]
    fn remove_serializes_as_explicit_null() {
        let request = OverrideKeysUpdateRequest {
            variants: vec![],
            metrics: Some(MetricSelectionUpdate::Remove),
            description: None,
            change_reason: ChangeReason::default(),
            experiment_group_id: None,
        };

        assert!(serde_json::to_value(request).unwrap()["metrics"].is_null());
    }

    #[test]
    fn set_with_source_round_trips() {
        use crate::database::models::{
            MetricDefinition, MetricDirection, MetricSelection, MetricSource,
        };

        let request = OverrideKeysUpdateRequest {
            variants: vec![],
            metrics: Some(MetricSelectionUpdate::Set {
                selection: Some(MetricSelection {
                    primary: MetricDefinition {
                        name: "conversion".to_string(),
                        direction: MetricDirection::Maximize,
                    },
                    secondary: None,
                    guardrail: MetricDefinition {
                        name: "latency".to_string(),
                        direction: MetricDirection::Minimize,
                    },
                }),
                source: Some(Some(MetricSource::Grafana {
                    base_url: "https://grafana.example.com".to_string(),
                    dashboard_uid: "uid".to_string(),
                    dashboard_slug: "slug".to_string(),
                    variant_id_alias: None,
                })),
            }),
            description: None,
            change_reason: ChangeReason::default(),
            experiment_group_id: None,
        };

        let value = serde_json::to_value(&request).unwrap();
        let round_tripped =
            serde_json::from_value::<OverrideKeysUpdateRequest>(value).unwrap();
        assert_eq!(round_tripped.metrics, request.metrics);
    }
}
