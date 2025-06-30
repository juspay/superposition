#![deny(unused_crate_dependencies)]
mod interface;
mod types;
mod utils;
use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use chrono::{DateTime, TimeZone, Utc};
use derive_more::{Deref, DerefMut};
use reqwest::StatusCode;
use serde_json::Value;
use superposition_types::{
    api::experiments::ExperimentListFilters,
    custom_query::{CommaSeparatedQParams, PaginationParams},
    database::models::experimentation::{ExperimentStatusType, Variant, VariantType},
    Overridden, PaginatedResponse,
};
pub use superposition_types::{
    api::experiments::ExperimentResponse, database::models::experimentation::Variants,
};
use tokio::{
    sync::RwLock,
    time::{self, Duration},
};
use types::ExperimentStore;
pub use types::{Config, Experiments};
use utils::MapError;

#[derive(Clone, Debug)]
pub struct Client {
    pub client_config: Arc<Config>,
    pub(crate) experiments: Arc<RwLock<ExperimentStore>>,
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
                let experiments = get_experiments(
                    hostname.clone(),
                    self.http_client.clone(),
                    *start_date,
                    self.client_config.tenant.to_string(),
                )
                .await;
                match experiments {
                    Ok(experiments) => {
                        let mut exp_store = self.experiments.write().await;
                        *exp_store = experiments;
                        *start_date = Utc::now();
                    }
                    Err(e) => {
                        log::error!(
                            "Failed to fetch experiments from the server with error: {}",
                            e
                        );
                    }
                };
            } // write lock on exp store releases here
            interval.tick().await;
        }
    }

    pub async fn get_applicable_variant(
        &self,
        context: &Value,
        toss: i8,
    ) -> Result<Vec<String>, String> {
        let experiments: Experiments =
            self.get_satisfied_experiments(context, None).await?;
        let mut variants: Vec<String> = Vec::new();
        for exp in experiments {
            if let Some(v) =
                self.decide_variant(*exp.traffic_percentage, exp.variants, toss)?
            {
                variants.push(v.id)
            }
        }
        Ok(variants)
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
                exp.context.is_empty()
                    || jsonlogic::apply(
                        &Value::Object(exp.context.clone().into()),
                        context,
                    ) == Ok(Value::Bool(true))
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

    // decide which variant to return among all applicable experiments
    fn decide_variant(
        &self,
        traffic: u8,
        applicable_variants: Variants,
        toss: i8,
    ) -> Result<Option<Variant>, String> {
        if toss < 0 {
            for variant in applicable_variants.iter() {
                if variant.variant_type == VariantType::EXPERIMENTAL {
                    return Ok(Some(variant.clone()));
                }
            }
        }
        let variant_count = applicable_variants.len() as u8;
        let range = (traffic * variant_count) as i32;
        if (toss as i32) >= range {
            return Ok(None);
        }
        let buckets = (1..=variant_count)
            .map(|i| (traffic * i) as i8)
            .collect::<Vec<i8>>();
        let index = buckets
            .into_iter()
            .position(|x| toss < x)
            .ok_or_else(|| "Unable to fetch variant's index".to_string())
            .map_err_to_string()?;
        Ok(applicable_variants.get(index).cloned())
    }
}

async fn get_experiments(
    hostname: String,
    http_client: reqwest::Client,
    start_date: DateTime<Utc>,
    tenant: String,
) -> Result<ExperimentStore, String> {
    let list_filters = ExperimentListFilters {
        status: Some(CommaSeparatedQParams(ExperimentStatusType::active_list())),
        from_date: Some(start_date),
        to_date: Some(Utc::now()),
        experiment_name: None,
        experiment_ids: None,
        created_by: None,
        sort_on: None,
        sort_by: None,
    };
    let pagination_params = PaginationParams::all_entries();
    let endpoint = format!("{hostname}/experiments?{pagination_params}&{list_filters}");
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
