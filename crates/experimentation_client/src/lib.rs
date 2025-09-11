#![deny(unused_crate_dependencies)]
mod interface;
mod types;
mod utils;
use std::{
    collections::{HashMap, HashSet},
    hash::{DefaultHasher, Hash, Hasher},
    sync::Arc,
};

use chrono::{DateTime, TimeZone, Utc};
use derive_more::{Deref, DerefMut};
use reqwest::StatusCode;
use serde_json::Value;
use superposition_types::{
    api::experiments::ExperimentListFilters,
    custom_query::{CommaSeparatedQParams, PaginationParams, QueryParam},
    database::models::experimentation::{
        Bucket, ExperimentGroup, ExperimentStatusType, GroupType, Variant,
    },
    Overridden, PaginatedResponse,
};
pub use superposition_types::{
    api::experiments::ExperimentResponse, database::models::experimentation::Variants,
};
use tokio::{
    sync::RwLock,
    time::{self, Duration},
};
pub use types::{Config, Experiments};
use types::{ExperimentGroupStore, ExperimentStore};
use utils::MapError;

#[derive(Clone, Debug)]
pub struct Client {
    pub client_config: Arc<Config>,
    pub(crate) experiments: Arc<RwLock<ExperimentStore>>,
    pub(crate) experiment_groups: Arc<RwLock<ExperimentGroupStore>>,
    pub(crate) http_client: reqwest::Client,
    last_polled: Arc<RwLock<DateTime<Utc>>>,
}

//TODO: replace all unwraps with proper error handling
// DO NOT let panics show up in library

impl Client {
    pub fn new(config: Config) -> Self {
        Self {
            client_config: Arc::new(config),
            experiments: Arc::new(RwLock::new(HashMap::new())),
            experiment_groups: Arc::new(RwLock::new(HashMap::new())),
            http_client: reqwest::Client::new(),
            last_polled: Arc::new(RwLock::new(
                Utc.with_ymd_and_hms(2023, 1, 1, 0, 0, 0).unwrap(),
            )),
        }
    }

    pub async fn run_polling_updates(self: Arc<Self>) {
        let poll_interval = self.client_config.poll_frequency;
        let hostname = &self.client_config.hostname;
        let mut interval = time::interval(Duration::from_secs(poll_interval));
        let mut start_date = self.last_polled.write().await;
        loop {
            // NOTE: this additional block scopes the write lock
            // at the end of this block, the write lock on exp store is released
            // allowing other threads to read updated data
            {
                let experiments_result = get_experiments(
                    hostname.clone(),
                    self.http_client.clone(),
                    *start_date,
                    self.client_config.tenant.to_string(),
                )
                .await;

                let experiment_groups_result = get_experiment_groups(
                    hostname.clone(),
                    self.http_client.clone(),
                    *start_date,
                    self.client_config.tenant.to_string(),
                )
                .await;

                match (experiments_result, experiment_groups_result) {
                    (Ok(experiments), Ok(experiment_groups)) => {
                        let mut exp_store = self.experiments.write().await;
                        *exp_store = experiments;
                        let mut exp_group_store = self.experiment_groups.write().await;
                        *exp_group_store = experiment_groups;
                        *start_date = Utc::now();
                    }
                    (Err(e), Ok(_)) => {
                        log::error!(
                            "Failed to fetch experiments from the server with error: {}",
                            e
                        );
                    }
                    (Ok(_), Err(e)) => {
                        log::error!("Failed to fetch experiment groups from the server with error: {}", e);
                    }
                    (Err(exp_err), Err(group_err)) => {
                        log::error!("Failed to fetch experiments: {}", exp_err);
                        log::error!("Failed to fetch experiment groups: {}", group_err);
                    }
                }
            } // write lock on exp store releases here
            interval.tick().await;
        }
    }

    pub async fn get_applicable_variant(
        &self,
        context: &Value,
        identifier: &str,
    ) -> Result<Vec<String>, String> {
        let experiment_groups = self
            .experiment_groups
            .read()
            .await
            .iter()
            .map(|(_, exp_group)| exp_group.clone())
            .collect::<Vec<_>>();

        let buckets =
            get_applicable_buckets_from_group(&experiment_groups, context, identifier);

        let experiments = self.experiments.read().await;

        let applicable_variants =
            get_applicable_variants_from_group_response(&experiments, context, &buckets);

        Ok(applicable_variants)
    }

    pub async fn get_satisfied_experiments(
        &self,
        context: &Value,
        prefix: Option<Vec<String>>,
    ) -> Result<Experiments, String> {
        let running_experiments = self
            .experiments
            .read()
            .await
            .iter()
            .filter(|(_, exp)| {
                cfg_if::cfg_if! {
                    if #[cfg(feature = "jsonlogic")] {
                        exp.context.is_empty()
                            || jsonlogic::apply(
                                &Value::Object(exp.context.clone().into()),
                                context,
                            ) == Ok(Value::Bool(true))
                    } else {
                        superposition_types::apply(
                            &exp.context,
                            &context.as_object().cloned().unwrap_or_default(),
                        )
                    }
                }
            })
            .map(|(_, exp)| exp.clone())
            .collect::<Experiments>();

        if let Some(prefix_list) = prefix {
            return Ok(Self::filter_experiments_by_prefix(
                running_experiments,
                prefix_list,
            ));
        }

        Ok(running_experiments)
    }

    pub async fn get_filtered_satisfied_experiments(
        &self,
        context: &Value,
        prefix: Option<Vec<String>>,
    ) -> Result<Experiments, String> {
        let experiments = self.experiments.read().await;

        let filtered_running_experiments = experiments
            .iter()
            .filter_map(|(_, exp)| {
                if exp.context.is_empty() {
                    Some(exp.clone())
                } else {
                    cfg_if::cfg_if! {
                        if #[cfg(feature = "jsonlogic")] {
                            match jsonlogic::partial_apply(
                                &Value::Object(exp.context.clone().into()),
                                context,
                            ) {
                                Ok(jsonlogic::PartialApplyOutcome::Resolved(Value::Bool(
                                    true,
                                )))
                                | Ok(jsonlogic::PartialApplyOutcome::Ambiguous) => {
                                    Some(exp.clone())
                                }
                                _ => None,
                            }
                        } else {
                            superposition_types::partial_apply(
                                &exp.context,
                                &context.as_object().cloned().unwrap_or_default(),
                            )
                            .then(|| exp.clone())
                        }
                    }
                }
            })
            .collect::<Vec<_>>();

        if let Some(prefix_list) = prefix {
            return Ok(Self::filter_experiments_by_prefix(
                filtered_running_experiments,
                prefix_list,
            ));
        }

        Ok(filtered_running_experiments)
    }

    pub async fn get_running_experiments(&self) -> Result<Experiments, String> {
        let running_experiments = self.experiments.read().await;
        let experiments: Experiments = running_experiments.values().cloned().collect();
        Ok(experiments)
    }

    fn filter_experiments_by_prefix(
        experiments: Vec<ExperimentResponse>,
        prefix_list: Vec<String>,
    ) -> Vec<ExperimentResponse> {
        let prefix_list: HashSet<String> = HashSet::from_iter(prefix_list);
        experiments
            .into_iter()
            .filter_map(|experiment| {
                let variants: Vec<_> = experiment
                    .variants
                    .into_inner()
                    .into_iter()
                    .filter_map(|mut variant| {
                        Variant::filter_keys_by_prefix(&variant, &prefix_list)
                            .map(|filtered_overrides_map| {
                                variant.overrides = filtered_overrides_map;
                                variant
                            })
                            .ok()
                    })
                    .collect();

                if !variants.is_empty() {
                    Some(ExperimentResponse {
                        variants: Variants::new(variants),
                        ..experiment
                    })
                } else {
                    None // Skip this experiment
                }
            })
            .collect()
    }
}

pub fn get_applicable_buckets_from_group(
    experiment_groups: &[ExperimentGroup],
    context: &Value,
    identifier: &str,
) -> Vec<(usize, Bucket)> {
    experiment_groups
        .iter()
        .filter_map(|exp_group| {
            let hashed_percentage = calculate_bucket_index(identifier, &exp_group.id);
            log::info!(
                "Identifier: {}, Experiment Group ID: {}, Hashed Percentage: {}",
                identifier,
                exp_group.id,
                hashed_percentage
            );
            let exp_context = &exp_group.context;

            cfg_if::cfg_if! {
                if #[cfg(feature = "jsonlogic")] {
                    let valid_context = exp_context.is_empty()
                        || jsonlogic::apply(&Value::Object(exp_context.clone().into()), context)
                            == Ok(Value::Bool(true));
                } else {
                    let valid_context = superposition_types::apply(
                        exp_context,
                        &context.as_object().cloned().unwrap_or_default(),
                    );
                }
            }

            let res =
                valid_context && *exp_group.traffic_percentage >= hashed_percentage as u8;

            res.then_some(
                exp_group
                    .buckets
                    .get(hashed_percentage)
                    .and_then(Clone::clone),
            )
            .flatten()
            .and_then(|b| {
                if exp_group.group_type == GroupType::SystemGenerated {
                    Some((hashed_percentage, b))
                } else {
                    (*exp_group.traffic_percentage > 0).then_some((
                        (hashed_percentage * 100) / *exp_group.traffic_percentage as usize,
                        b,
                    ))
                }
            })
        })
        .collect()
}

pub fn get_applicable_variants_from_group_response(
    experiments: &HashMap<String, ExperimentResponse>,
    context: &Value,
    bucket_response: &[(usize, Bucket)],
) -> Vec<String> {
    bucket_response
        .iter()
        .filter_map(|(toss, bucket)| {
            experiments.get(&bucket.experiment_id).and_then(|exp| {
                cfg_if::cfg_if! {
                    if #[cfg(feature = "jsonlogic")] {
                        let valid_context = exp.context.is_empty()
                            || jsonlogic::apply(
                                &Value::Object(exp.context.clone().into()),
                                context,
                            ) == Ok(Value::Bool(true));
                    } else {
                        let valid_context = superposition_types::apply(
                            &exp.context,
                            &context.as_object().cloned().unwrap_or_default(),
                        );
                    }
                }

                let res = valid_context
                    && (*exp.traffic_percentage as usize * exp.variants.len()) >= *toss;

                res.then_some(bucket.variant_id.clone())
            })
        })
        .collect()
}

#[inline]
pub fn calculate_bucket_index(identifier: &str, group_id: &i64) -> usize {
    let mut hasher = DefaultHasher::new();
    (identifier, group_id).hash(&mut hasher);
    (hasher.finish() % 100) as usize
}

async fn get_experiments(
    hostname: String,
    http_client: reqwest::Client,
    start_date: DateTime<Utc>,
    tenant: String,
) -> Result<ExperimentStore, String> {
    let list_filters = ExperimentListFilters {
        status: Some(CommaSeparatedQParams(ExperimentStatusType::active_list())),
        from_date: None,
        to_date: None,
        experiment_name: None,
        experiment_ids: None,
        experiment_group_ids: None,
        created_by: None,
        sort_on: None,
        sort_by: None,
        global_experiments_only: None,
        dimension_match_strategy: None,
    };
    let pagination_params = PaginationParams::all_entries();
    let endpoint = format!(
        "{hostname}/experiments?{}&{}",
        pagination_params.to_query_param(),
        list_filters.to_query_param()
    );
    let experiment_response = http_client
        .get(endpoint)
        .header("x-tenant", tenant.to_string())
        .header("If-Modified-Since", start_date.to_rfc2822())
        .send()
        .await
        .map_err_to_string()?;

    match experiment_response.status() {
        StatusCode::NOT_MODIFIED => {
            return Err(format!(
                "{} EXP: skipping update, remote not modified",
                tenant
            ));
        }
        StatusCode::OK => log::info!(
            "{}",
            format!("{} EXP: new config received, updating", tenant)
        ),
        x => return Err(format!("{} CAC: fetch failed, status: {}", tenant, x)),
    };
    let list_experiments_response = experiment_response
        .json::<PaginatedResponse<ExperimentResponse>>()
        .await
        .map_err_to_string()?;

    let experiments = list_experiments_response.data;
    Ok(experiments
        .into_iter()
        .map(|exp| (exp.id.clone(), exp))
        .collect())
}

async fn get_experiment_groups(
    hostname: String,
    http_client: reqwest::Client,
    start_date: DateTime<Utc>,
    tenant: String,
) -> Result<ExperimentGroupStore, String> {
    let pagination_params = PaginationParams::all_entries();
    let endpoint = format!(
        "{hostname}/experiment-groups?{}",
        pagination_params.to_query_param()
    );

    let experiment_group_response = http_client
        .get(endpoint)
        .header("x-tenant", tenant.to_string())
        .header("If-Modified-Since", start_date.to_rfc2822())
        .send()
        .await
        .map_err_to_string()?;

    match experiment_group_response.status() {
        StatusCode::NOT_MODIFIED => {
            return Err(format!(
                "{} EXP: skipping update, remote not modified",
                tenant
            ));
        }
        StatusCode::OK => {
            log::info!("{} EXP: new experiment groups received, updating", tenant)
        }
        x => return Err(format!("{} CAC: fetch failed, status: {}", tenant, x)),
    };
    let list_experiment_groups_response = experiment_group_response
        .json::<PaginatedResponse<ExperimentGroup>>()
        .await
        .map_err_to_string()?;

    let experiment_groups = list_experiment_groups_response.data;
    Ok(experiment_groups
        .into_iter()
        .map(|experiment_group| (experiment_group.id.to_string(), experiment_group))
        .collect())
}

#[derive(Deref, DerefMut)]
pub struct ClientFactory(RwLock<HashMap<String, Arc<Client>>>);
impl ClientFactory {
    pub async fn create_client(
        &self,
        tenant: String,
        poll_frequency: u64,
        hostname: String,
    ) -> Result<Arc<Client>, String> {
        let mut factory = self.write().await;

        if let Some(client) = factory.get(&tenant) {
            return Ok(client.clone());
        }

        let client = Arc::new(Client::new(Config {
            tenant: tenant.to_string(),
            hostname,
            poll_frequency,
        }));

        factory.insert(tenant.to_string(), client.clone());
        Ok(client.clone())
    }

    pub async fn get_client(&self, tenant: String) -> Result<Arc<Client>, String> {
        let factory = self.read().await;
        match factory.get(&tenant) {
            Some(client) => Ok(client.clone()),
            None => Err("No such tenant found".to_string()),
        }
    }
}

use once_cell::sync::Lazy;
pub static CLIENT_FACTORY: Lazy<ClientFactory> =
    Lazy::new(|| ClientFactory(RwLock::new(HashMap::new())));
