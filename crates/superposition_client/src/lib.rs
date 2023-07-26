mod types;
use std::{collections::HashMap, sync::Arc};

use chrono::{DateTime, TimeZone, Utc};
use serde_json::Value;
use tokio::{
    sync::RwLock,
    time::{self, Duration},
};
pub use types::{Config, Variants};
use types::{ExperimentStore, Experiments, Variant};

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
        Client {
            client_config: Arc::new(config),
            experiments: Arc::new(RwLock::new(HashMap::new())),
            http_client: reqwest::Client::new(),
            last_polled: Arc::new(RwLock::new(
                Utc.with_ymd_and_hms(2023, 01, 01, 0, 0, 0).unwrap(),
            )),
        }
    }

    pub async fn run_polling_updates(self) {
        let poll_interval = self.client_config.poll_frequency;
        let hostname = &self.client_config.hostname;
        let mut interval = time::interval(Duration::from_secs(poll_interval));
        let mut start_date = self.last_polled.write().await;
        loop {
            {
                let experiments = get_experiments(
                    hostname.clone(),
                    self.http_client.clone(),
                    start_date.to_string(),
                )
                .await
                .unwrap();

                let mut exp_store = self.experiments.write().await;
                for (exp_id, experiment) in experiments.into_iter() {
                    match experiment.status {
                        types::ExperimentStatusType::CONCLUDED => {
                            exp_store.remove(&exp_id)
                        }
                        types::ExperimentStatusType::INPROGRESS => {
                            exp_store.insert(exp_id, experiment)
                        }
                    };
                }
            }
            *start_date = Utc::now();
            interval.tick().await;
        }
    }

    pub async fn get_applicable_variant(
        &self,
        contexts: &Value,
        toss: u8,
    ) -> Vec<String> {
        let running_experiments = self.experiments.read().await;
        // try and if json logic works
        let mut experiments: Experiments = Vec::new();
        for (_, exps) in running_experiments.iter() {
            if let Ok(Value::Bool(true)) = jsonlogic::apply(&exps.context, contexts) {
                experiments.push(exps.clone());
            }
        }

        let mut variants: Vec<String> = Vec::new();

        for exp in experiments {
            if let Some(v) =
                self.decide_variant(exp.traffic_percentage, exp.variants, toss)
            {
                variants.push(v.id)
            }
        }
        variants
    }

    // decide which variant to return among all applicable experiments
    fn decide_variant(
        &self,
        traffic: u8,
        applicable_exps: Variants,
        toss: u8,
    ) -> Option<Variant> {
        let variant_count = applicable_exps.len() as u8;
        let range = (traffic * variant_count) as u32;
        if (toss as u32) > range {
            return None
        }
        let buckets = (1..=variant_count)
            .map(|i| traffic * i)
            .collect::<Vec<u8>>();
        let index = buckets.into_iter().position(|x| toss < x);
        applicable_exps.get(index.unwrap()).map(|x| x.clone())
    }
}

async fn get_experiments(
    hostname: String,
    http_client: reqwest::Client,
    start_date: String,
) -> Result<ExperimentStore, String> {
    let mut curr_exp_store: ExperimentStore = HashMap::new();
    let now = Utc::now();
    let endpoint = format!(
        "{hostname}/experiments?from_date={start_date}&to_date={now}&page=1&count=100"
    );
    let experiments = http_client
        .get(format!("{endpoint}&status=INPROGRESS,CONCLUDED"))
        .send()
        .await
        .unwrap()
        .json::<Experiments>()
        .await
        .unwrap_or_default();

    // println!("got these running experiments: {:?}", running_experiments);

    for experiments in experiments.into_iter() {
        curr_exp_store.insert(experiments.id.to_string(), experiments);
    }

    Ok(curr_exp_store)
}
