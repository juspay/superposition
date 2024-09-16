use std::collections::HashMap;

use chrono::{DateTime, NaiveDateTime, Utc};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use service_utils::helpers::deserialize_stringified_list;
use superposition_types::{Condition, Exp, Overrides};

use crate::db::models::{self, ExperimentStatusType, Variant};

/********** Experiment Create Req Types ************/

#[derive(Deserialize)]
pub struct ExperimentCreateRequest {
    pub name: String,
    pub context: Exp<Condition>,
    pub variants: Vec<Variant>,
}

#[derive(Serialize)]
pub struct ExperimentCreateResponse {
    pub experiment_id: String,
}

impl From<models::Experiment> for ExperimentCreateResponse {
    fn from(experiment: models::Experiment) -> Self {
        ExperimentCreateResponse {
            experiment_id: experiment.id.to_string(),
        }
    }
}

/********** Experiment Response Type **************/
// Same as models::Experiments but `id` field is String
// JS have limitation of 53-bit integers, so on
// deserializing from JSON to JS Object will lead incorrect `id` values
#[derive(Serialize, Deserialize)]
pub struct ExperimentResponse {
    pub id: String,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
    pub last_modified: DateTime<Utc>,

    pub name: String,
    pub override_keys: Vec<String>,
    pub status: models::ExperimentStatusType,
    pub traffic_percentage: i32,

    pub context: Value,
    pub variants: Vec<Variant>,
    pub last_modified_by: String,
    pub chosen_variant: Option<String>,
}

impl From<models::Experiment> for ExperimentResponse {
    fn from(experiment: models::Experiment) -> Self {
        ExperimentResponse {
            id: experiment.id.to_string(),
            created_at: experiment.created_at,
            created_by: experiment.created_by,
            last_modified: experiment.last_modified,

            name: experiment.name,
            override_keys: experiment.override_keys,
            status: experiment.status,
            traffic_percentage: experiment.traffic_percentage,

            context: experiment.context,
            variants: experiment.variants.into_inner(),
            last_modified_by: experiment.last_modified_by,
            chosen_variant: experiment.chosen_variant,
        }
    }
}

#[derive(Serialize)]
pub struct ExperimentsResponse {
    pub total_items: i64,
    pub total_pages: i64,
    pub data: Vec<ExperimentResponse>,
}

/********** Experiment Conclude Req Types **********/

#[derive(Deserialize, Debug)]
pub struct ConcludeExperimentRequest {
    pub chosen_variant: String,
}

/********** Context Bulk API Type *************/

#[derive(Deserialize, Serialize, Clone)]
pub struct ContextPutReq {
    pub context: Map<String, Value>,
    pub r#override: Value,
}

#[derive(Deserialize, Serialize, Clone)]
pub enum ContextAction {
    PUT(ContextPutReq),
    DELETE(String),
    MOVE((String, ContextMoveReq)),
}

#[derive(Deserialize, Serialize, Debug)]
pub struct ContextPutResp {
    pub context_id: String,
    pub override_id: String,
    pub priority: i32,
}

#[derive(Serialize, Deserialize, Debug)]
pub enum ContextBulkResponse {
    PUT(ContextPutResp),
    DELETE(String),
    MOVE(ContextPutResp),
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
            .map(|(key, value)| {
                (key, value.parse().unwrap_or_else(|_| Value::String(value)))
            })
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

/********** List API Filter Type *************/

#[derive(Deserialize, Debug, Clone)]
pub struct StatusTypes(
    #[serde(deserialize_with = "deserialize_stringified_list")]
    pub  Vec<ExperimentStatusType>,
);

#[derive(Deserialize, Debug)]
pub struct ListFilters {
    pub status: Option<StatusTypes>,
    pub from_date: Option<DateTime<Utc>>,
    pub to_date: Option<DateTime<Utc>>,
    pub page: Option<i64>,
    pub count: Option<i64>,
}

/********** Ramp API type **********/
#[derive(Deserialize, Debug)]
pub struct RampRequest {
    pub traffic_percentage: u64,
}

/********** Update API type ********/

#[derive(Deserialize, Debug)]
pub struct VariantUpdateRequest {
    pub id: String,
    pub overrides: Exp<Overrides>,
}

#[derive(Deserialize, Debug)]
pub struct OverrideKeysUpdateRequest {
    pub variants: Vec<VariantUpdateRequest>,
}

#[derive(Deserialize, Serialize, Clone)]
pub struct ContextMoveReq {
    pub context: Map<String, Value>,
}

/*********** List Audit API Filter Type **************/

#[derive(Deserialize, Debug, Clone)]
pub struct StringArgs(
    #[serde(deserialize_with = "deserialize_stringified_list")] pub Vec<String>,
);

#[derive(Debug, Clone, Deserialize)]
pub struct AuditQueryFilters {
    pub from_date: Option<NaiveDateTime>,
    pub to_date: Option<NaiveDateTime>,
    pub table: Option<StringArgs>,
    pub action: Option<StringArgs>,
    pub username: Option<String>,
    pub count: Option<i64>,
    pub page: Option<i64>,
}
