use std::collections::HashMap;

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
    api::{deserialize_option_i64, i64_option_formatter},
    custom_query::{CommaSeparatedQParams, CommaSeparatedStringQParams, QueryParam},
    database::models::{
        experimentation::{
            Experiment, ExperimentStatusType, ExperimentType, TrafficPercentage, Variant,
            Variants,
        },
        ChangeReason, Description, MetricSource, Metrics,
    },
    Condition, Exp, IsEmpty, Overrides, SortBy,
};

use super::I64Update;

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
    pub metrics: Metrics,
    pub metrics_url: Option<String>,
    pub experiment_group_id: Option<String>,
}

impl From<Experiment> for ExperimentResponse {
    fn from(experiment: Experiment) -> Self {
        let metrics_url =
            experiment.started_at.and_then(|started_at| {
                experiment.metrics.source().map(|source| {
                    match source {
                        MetricSource::Grafana { base_url, dashboard_uid, dashboard_slug, variant_id_alias } => {
                            let to = if experiment.status.active() {
                                "now".to_string()
                            } else {
                                experiment.last_modified.to_string()
                            };
                            let from = started_at.timestamp_millis();

                            let variant_var = format!("var-{}", variant_id_alias.unwrap_or_else(|| "variantIds".to_string()));
                            let query = experiment.variants.iter().map(|v| {
                                format!("{}={}", variant_var, v.id)
                            }).collect::<Vec<String>>().join("&");

                            format!("{base_url}/d/{dashboard_uid}/{dashboard_slug}?{query}&from={from}&to={to}&kiosk&theme=light")
                        }
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

#[derive(Deserialize, Serialize)]
pub struct ExperimentCreateRequest {
    pub name: String,
    pub context: Exp<Condition>,
    pub variants: Vec<Variant>,
    pub metrics: Option<Metrics>,
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
#[derive(Debug, Deserialize)]
#[serde(try_from = "HashMap<String,String>")]
pub struct ApplicableVariantsQuery {
    pub context: Map<String, Value>,
    pub identifier: String,
}

impl TryFrom<HashMap<String, String>> for ApplicableVariantsQuery {
    type Error = String;
    fn try_from(mut value: HashMap<String, String>) -> Result<Self, Self::Error> {
        let identifier = match value.remove("identifier") {
            Some(v) => v,
            None => value
                .get("toss")
                .ok_or_else(|| "'identifier' must be provided".to_string())?
                .to_string(),
        };
        let value = value
            .into_iter()
            .map(|(key, value)| (key, value.parse().unwrap_or(Value::String(value))))
            .collect::<Map<_, _>>();

        Ok(Self {
            identifier,
            context: value,
        })
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ApplicableVariantsRequest {
    pub context: Map<String, Value>,
    #[serde(alias = "toss", deserialize_with = "deserialize_identifier")]
    pub identifier: String,
}

impl From<ApplicableVariantsRequest> for ApplicableVariantsQuery {
    fn from(value: ApplicableVariantsRequest) -> Self {
        Self {
            context: value.context,
            identifier: value.identifier,
        }
    }
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

#[derive(Deserialize, Serialize, Debug, Clone, PartialEq, IsEmpty, QueryParam)]
pub struct ExperimentListFilters {
    #[query_param(skip_if_empty)]
    pub status: Option<CommaSeparatedQParams<ExperimentStatusType>>,
    pub from_date: Option<DateTime<Utc>>,
    pub to_date: Option<DateTime<Utc>>,
    pub experiment_name: Option<String>,
    #[query_param(skip_if_empty)]
    pub experiment_ids: Option<CommaSeparatedStringQParams>,
    #[query_param(skip_if_empty)]
    pub experiment_group_ids: Option<CommaSeparatedStringQParams>,
    #[query_param(skip_if_empty)]
    pub created_by: Option<CommaSeparatedStringQParams>,
    pub sort_on: Option<ExperimentSortOn>,
    pub sort_by: Option<SortBy>,
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
    pub metrics: Option<Metrics>,
    pub description: Option<Description>,
    #[serde(default = "ChangeReason::default")]
    pub change_reason: ChangeReason,
    #[serde(default, deserialize_with = "deserialize_option_i64")]
    pub experiment_group_id: Option<I64Update>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct AuditQueryFilters {
    pub from_date: Option<DateTime<Utc>>,
    pub to_date: Option<DateTime<Utc>>,
    pub table: Option<CommaSeparatedStringQParams>,
    pub action: Option<CommaSeparatedStringQParams>,
    pub username: Option<String>,
    pub count: Option<i64>,
    pub page: Option<i64>,
}
