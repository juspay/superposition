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
use serde_json::Value;
use superposition_types::{
    database::models::experimentation::VariantType, Overridden, PaginatedResponse,
};
use tokio::{
    sync::RwLock,
    time::{self, Duration},
};
pub use types::{Config, Experiment, Experiments, Variants};
use types::{ExperimentStore, Variant};
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
                    start_date.to_string(),
                    self.client_config.tenant.to_string(),
                )
                .await
                .unwrap_or(HashMap::new());

                let mut exp_store = self.experiments.write().await;
                for (exp_id, experiment) in experiments.into_iter() {
                    if experiment.status.active() {
                        exp_store.insert(exp_id, experiment)
                    } else {
                        exp_store.remove(&exp_id)
                    };
                }
            } // write lock on exp store releases here
            *start_date = Utc::now();
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
                self.decide_variant(exp.traffic_percentage, exp.variants, toss)?
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
                let is_empty = exp
                    .context
                    .as_object()
                    .map_or(false, |context| context.is_empty());
                is_empty
                    || jsonlogic::apply(&exp.context, context) == Ok(Value::Bool(true))
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
                let is_empty = exp
                    .context
                    .as_object()
                    .map_or(false, |context| context.is_empty());
                if is_empty {
                    Some(exp.clone())
                } else {
                    match jsonlogic::partial_apply(&exp.context, context) {
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
            .collect::<Vec<Experiment>>();

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
        experiments: Vec<Experiment>,
        prefix_list: Vec<String>,
    ) -> Vec<Experiment> {
        let prefix_list: HashSet<String> = HashSet::from_iter(prefix_list);
        experiments
            .into_iter()
            .filter_map(|experiment| {
                let variants: Vec<Variant> = experiment
                    .variants
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
                    Some(Experiment {
                        variants,
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
    start_date: String,
    tenant: String,
) -> Result<ExperimentStore, String> {
    let mut curr_exp_store: ExperimentStore = HashMap::new();
    let requesting_count = 10;
    let mut page = 1;
    let now = Utc::now();
    loop {
        let endpoint = format!(
            "{hostname}/experiments?from_date={start_date}&to_date={now}&page={page}&count={requesting_count}"
        );
        let list_experiments_response = http_client
            .get(format!(
                "{endpoint}&status=CREATED,INPROGRESS,CONCLUDED,DISCARDED"
            ))
            .header("x-tenant", tenant.to_string())
            .send()
            .await
            .map_err_to_string()?
            .json::<PaginatedResponse<Experiment>>()
            .await
            .map_err_to_string()?;

        let experiments = list_experiments_response.data;

        for experiment in experiments.into_iter() {
            curr_exp_store.insert(experiment.id.to_string(), experiment);
        }
        if page < list_experiments_response.total_pages {
            page += 1;
        } else {
            break;
        }
    }

    Ok(curr_exp_store)
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
