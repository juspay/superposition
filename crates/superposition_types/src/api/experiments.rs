use std::{collections::HashMap, fmt::Display};

use chrono::{DateTime, Utc};
use core::fmt;
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::{Map, Value};
use strum_macros::Display;
use superposition_derives::IsEmpty;

use crate::{
    custom_query::{CommaSeparatedQParams, CommaSeparatedStringQParams},
    database::models::{
        experimentation::{
            Experiment, ExperimentStatusType, TrafficPercentage, Variant, Variants,
        },
        ChangeReason, Description, MetricSource, Metrics,
    },
    Condition, Exp, IsEmpty, Overrides, SortBy,
};

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
}

impl From<Experiment> for ExperimentResponse {
    fn from(experiment: Experiment) -> Self {
        let metrics_url =
            experiment.started_at.and_then(|started_at| {
                experiment.metrics.source().map(|source| {
                    match source {
                        MetricSource::Grafana { base_url, dashboard_uid, dashboard_slug, variant_id_alias } => {
                            let to = if experiment.status == ExperimentStatusType::CONCLUDED {
                                experiment.last_modified.to_string()
                            } else {
                                "now".to_string()
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
        }
    }
}

#[derive(Deserialize, Serialize)]
pub struct ExperimentCreateRequest {
    pub name: String,
    pub context: Exp<Condition>,
    pub variants: Vec<Variant>,
    pub metrics: Option<Metrics>,
    #[serde(default = "Description::default")]
    pub description: Description,
    #[serde(default = "ChangeReason::default")]
    pub change_reason: ChangeReason,
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
pub struct DiscardExperimentRequest {
    #[serde(default = "ChangeReason::default")]
    pub change_reason: ChangeReason,
}

/********** Applicable Variants API Type *************/
#[derive(Debug, Deserialize)]
#[serde(try_from = "HashMap<String,String>")]
pub struct ApplicableVariantsQuery {
    pub context: Map<String, Value>,
    pub toss: i8,
}

impl TryFrom<HashMap<String, String>> for ApplicableVariantsQuery {
    type Error = String;
    fn try_from(value: HashMap<String, String>) -> Result<Self, Self::Error> {
        let mut value = value
            .into_iter()
            .map(|(key, value)| (key, value.parse().unwrap_or(Value::String(value))))
            .collect::<Map<_, _>>();

        let toss = value
            .remove("toss")
            .and_then(|toss| toss.as_i64())
            .and_then(|toss| {
                if -1 <= toss && toss <= 100 {
                    Some(toss as i8)
                } else {
                    None
                }
            })
            .ok_or_else(|| {
                log::error!("toss should be a an interger between -1 and 100 (included)");
                String::from("toss should be a an interger between -1 and 100 (included)")
            })?;

        Ok(Self {
            toss,
            context: value,
        })
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ApplicableVariantsRequest {
    pub context: Map<String, Value>,
    #[serde(deserialize_with = "deserialize_toss")]
    pub toss: i8,
}

impl From<ApplicableVariantsRequest> for ApplicableVariantsQuery {
    fn from(value: ApplicableVariantsRequest) -> Self {
        value.into()
    }
}

fn deserialize_toss<'de, D>(deserializer: D) -> Result<i8, D::Error>
where
    D: Deserializer<'de>,
{
    let toss: i8 = Deserialize::deserialize(deserializer)?;
    if -1 <= toss && toss <= 100 {
        Ok(toss)
    } else {
        Err(serde::de::Error::custom(
            "toss should be a an interger between -1 and 100 (included)",
        ))
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

#[derive(Deserialize, Serialize, Debug, Clone, PartialEq, IsEmpty)]
pub struct ExperimentListFilters {
    pub status: Option<CommaSeparatedQParams<ExperimentStatusType>>,
    pub from_date: Option<DateTime<Utc>>,
    pub to_date: Option<DateTime<Utc>>,
    pub experiment_name: Option<String>,
    pub experiment_ids: Option<CommaSeparatedStringQParams>,
    pub created_by: Option<CommaSeparatedStringQParams>,
    pub context: Option<CommaSeparatedStringQParams>,
    pub sort_on: Option<ExperimentSortOn>,
    pub sort_by: Option<SortBy>,
}

impl Display for ExperimentListFilters {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut query_params = vec![];
        if let Some(status) = &self.status {
            let status: Vec<String> =
                status.0.iter().map(|val| val.to_string()).collect();
            query_params.push(format!("status={}", status.join(",")));
        }
        if let Some(from_date) = self.from_date {
            query_params.push(format!("from_date={}", from_date));
        }
        if let Some(to_date) = self.to_date {
            query_params.push(format!("to_date={}", to_date));
        }
        if let Some(experiment_name) = &self.experiment_name {
            query_params.push(format!("experiment_name={}", experiment_name));
        }
        if let Some(experiment_ids) = &self.experiment_ids {
            query_params.push(format!("experiment_ids={}", experiment_ids));
        }
        if let Some(created_by) = &self.created_by {
            query_params.push(format!("created_by={}", created_by));
        }
        if let Some(context) = &self.context {
            query_params.push(format!("context={}", context));
        }
        if let Some(sort_on) = self.sort_on {
            query_params.push(format!("sort_on={}", sort_on));
        }
        if let Some(sort_by) = &self.sort_by {
            query_params.push(format!("sort_by={}", sort_by));
        }
        write!(f, "{}", query_params.join("&"))
    }
}

impl Default for ExperimentListFilters {
    fn default() -> Self {
        Self {
            status: None,
            from_date: None,
            to_date: None,
            experiment_name: None,
            experiment_ids: None,
            created_by: None,
            context: None,
            sort_on: None,
            sort_by: Some(SortBy::Desc),
        }
    }
}

/********** Update API type ********/

#[derive(Deserialize, Serialize, Debug)]
pub struct VariantUpdateRequest {
    pub id: String,
    pub overrides: Exp<Overrides>,
}

#[derive(Deserialize, Serialize, Debug)]
pub struct OverrideKeysUpdateRequest {
    #[serde(alias = "variant_list")]
    pub variants: Vec<VariantUpdateRequest>,
    pub metrics: Option<Metrics>,
    pub description: Option<Description>,
    #[serde(default = "ChangeReason::default")]
    pub change_reason: ChangeReason,
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
