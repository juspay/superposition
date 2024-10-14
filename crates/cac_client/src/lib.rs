#![deny(unused_crate_dependencies)]
mod eval;
mod interface;
mod utils;

use std::{
    collections::{HashMap, HashSet},
    convert::identity,
    sync::Arc,
    time::{Duration, UNIX_EPOCH},
};

use actix_web::{rt::time::interval, web::Data};
use chrono::{DateTime, Utc};
use derive_more::{Deref, DerefMut};
use reqwest::{RequestBuilder, Response, StatusCode};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use superposition_types::{Cac, Condition, Contextual, Overrides};
use tokio::sync::RwLock;
use utils::core::MapError;

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Context {
    pub condition: Condition,
    pub override_with_keys: [String; 1],
}

impl Contextual for Context {
    fn get_condition(&self) -> Condition {
        self.condition.clone()
    }
}

#[repr(C)]
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Config {
    pub contexts: Vec<Context>,
    pub overrides: HashMap<String, Overrides>,
    pub default_configs: Map<String, Value>,
}

#[derive(strum_macros::EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum MergeStrategy {
    MERGE,
    REPLACE,
}

impl Default for MergeStrategy {
    fn default() -> Self {
        Self::MERGE
    }
}

impl From<String> for MergeStrategy {
    fn from(value: String) -> Self {
        match value.to_lowercase().as_str() {
            "replace" => Self::REPLACE,
            "merge" => Self::MERGE,
            _ => Self::default(),
        }
    }
}

#[repr(C)]
#[derive(Clone)]
pub struct Client {
    tenant: String,
    reqw: Data<reqwest::RequestBuilder>,
    polling_interval: Duration,
    last_modified: Data<RwLock<DateTime<Utc>>>,
    config: Data<RwLock<Config>>,
}

fn clone_reqw(reqw: &RequestBuilder) -> Result<RequestBuilder, String> {
    reqw.try_clone()
        .ok_or_else(|| "Unable to clone reqw".to_string())
}

fn get_last_modified(resp: &Response) -> Option<DateTime<Utc>> {
    resp.headers().get("last-modified").and_then(|header_val| {
        let header_str = header_val.to_str().ok()?;
        DateTime::parse_from_rfc2822(header_str)
            .map(|datetime| datetime.with_timezone(&Utc))
            .map_err(|e| {
                log::error!("Failed to parse date: {e}");
            })
            .ok()
    })
}

impl Client {
    pub async fn new(
        tenant: String,
        polling_interval: Duration,
        hostname: String,
    ) -> Result<Self, String> {
        let reqw_client = reqwest::Client::builder().build().map_err_to_string()?;
        let cac_endpoint = format!("{hostname}/config");
        let reqw = reqw_client
            .get(cac_endpoint)
            .header("x-tenant", tenant.to_string());

        let reqwc = clone_reqw(&reqw)?;
        let resp = reqwc.send().await.map_err_to_string()?;
        let last_modified_at = get_last_modified(&resp);
        if resp.status().is_client_error() {
            return Err("Invalid tenant".to_string());
        }
        let config = resp.json::<Config>().await.map_err_to_string()?;

        let client = Client {
            tenant,
            reqw: Data::new(reqw),
            polling_interval,
            last_modified: Data::new(RwLock::new(
                last_modified_at.unwrap_or(DateTime::<Utc>::from(UNIX_EPOCH)),
            )),
            config: Data::new(RwLock::new(config)),
        };
        Ok(client)
    }

    async fn fetch(&self) -> Result<reqwest::Response, String> {
        let last_modified = self.last_modified.read().await;
        let reqw = clone_reqw(&self.reqw)?
            .header("If-Modified-Since", last_modified.to_rfc2822());
        let resp = reqw.send().await.map_err_to_string()?;
        match resp.status() {
            StatusCode::NOT_MODIFIED => {
                return Err(format!(
                    "{} CAC: skipping update, remote not modified",
                    self.tenant
                ));
            }
            StatusCode::OK => log::info!(
                "{}",
                format!("{} CAC: new config received, updating", self.tenant)
            ),
            x => return Err(format!("{} CAC: fetch failed, status: {}", self.tenant, x)),
        };
        Ok(resp)
    }

    async fn update_cac(&self) -> Result<String, String> {
        let fetched_config = self.fetch().await?;
        let mut config = self.config.write().await;
        let mut last_modified = self.last_modified.write().await;
        let last_modified_at = get_last_modified(&fetched_config);
        *config = fetched_config.json::<Config>().await.map_err_to_string()?;
        if let Some(val) = last_modified_at {
            *last_modified = val;
        }
        Ok(format!("{}: CAC updated successfully", self.tenant))
    }

    pub async fn run_polling_updates(self: Arc<Self>) {
        let mut interval = interval(self.polling_interval);
        loop {
            interval.tick().await;
            let result = self.update_cac().await.unwrap_or_else(identity);
            log::info!("{result}",);
        }
    }

    pub async fn get_full_config_state_with_filter(
        &self,
        query_data: Option<Map<String, Value>>,
        prefix: Option<Vec<String>>,
    ) -> Result<Config, String> {
        let cac = self.config.read().await;
        let mut config = cac.to_owned();
        if let Some(prefix_list) = prefix {
            config = filter_config_by_prefix(&config, &prefix_list)?;
        }

        let dimension_filtered_config = query_data
            .filter(|query_map| !query_map.is_empty())
            .map(|query_map| filter_config_by_dimensions(&config, &query_map));

        if let Some(filtered_config) = dimension_filtered_config {
            config = filtered_config;
        };

        Ok(config)
    }

    pub async fn get_last_modified(&self) -> DateTime<Utc> {
        self.last_modified.read().await.clone()
    }

    pub async fn eval(
        &self,
        query_data: Map<String, Value>,
        merge_strategy: MergeStrategy,
    ) -> Result<Map<String, Value>, String> {
        let cac = self.config.read().await;
        eval::eval_cac(
            cac.default_configs.to_owned(),
            &cac.contexts,
            &cac.overrides,
            &query_data,
            merge_strategy,
        )
    }

    pub async fn get_resolved_config(
        &self,
        query_data: Map<String, Value>,
        filter_keys: Option<Vec<String>>,
        merge_strategy: MergeStrategy,
    ) -> Result<Map<String, Value>, String> {
        let mut cac = self.eval(query_data, merge_strategy).await?;
        if let Some(keys) = filter_keys {
            cac = filter_keys_by_prefix(cac, &keys);
        }
        Ok(cac)
    }

    pub async fn get_default_config(
        &self,
        filter_keys: Option<Vec<String>>,
    ) -> Result<Map<String, Value>, String> {
        let configs = self.config.read().await;
        let mut default_configs = configs.default_configs.clone();
        if let Some(keys) = filter_keys {
            default_configs = filter_keys_by_prefix(default_configs, &keys);
        }
        Ok(default_configs)
    }
}

#[derive(Deref, DerefMut)]
pub struct ClientFactory(RwLock<HashMap<String, Arc<Client>>>);
impl ClientFactory {
    pub async fn create_client(
        &self,
        tenant: String,
        polling_interval: Duration,
        hostname: String,
    ) -> Result<Arc<Client>, String> {
        let mut factory = self.write().await;

        if let Some(client) = factory.get(&tenant) {
            return Ok(client.clone());
        }

        let client =
            Arc::new(Client::new(tenant.to_string(), polling_interval, hostname).await?);
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

pub use eval::eval_cac;
pub use eval::eval_cac_with_reasoning;
pub use eval::merge;

pub fn filter_keys_by_prefix(
    keys: Map<String, Value>,
    prefix_list: &Vec<String>,
) -> Map<String, Value> {
    let prefix_list: HashSet<String> = prefix_list.iter().cloned().collect();
    keys.into_iter()
        .filter(|(key, _)| {
            prefix_list
                .iter()
                .any(|prefix_str| key.starts_with(prefix_str))
        })
        .collect()
}

pub fn filter_config_by_prefix(
    config: &Config,
    prefix_list: &Vec<String>,
) -> Result<Config, String> {
    let mut filtered_overrides: HashMap<String, Overrides> = HashMap::new();

    let filtered_default_config: Map<String, Value> =
        filter_keys_by_prefix(config.default_configs.clone(), &prefix_list);

    for (key, overrides) in &config.overrides {
        let filtered_overrides_map: Map<String, Value> = overrides
            .clone()
            .into_iter()
            .filter(|(key, _)| prefix_list.contains(key))
            .collect();

        let _ = Cac::<Overrides>::try_from(filtered_overrides_map).map(
            |filtered_overrides_map| {
                filtered_overrides
                    .insert(key.clone(), filtered_overrides_map.into_inner())
            },
        );
    }

    let filtered_context: Vec<Context> = config
        .contexts
        .clone()
        .into_iter()
        .filter(|context| filtered_overrides.contains_key(&context.override_with_keys[0]))
        .collect();

    let filtered_config = Config {
        contexts: filtered_context,
        overrides: filtered_overrides,
        default_configs: filtered_default_config,
    };

    Ok(filtered_config)
}

pub fn filter_config_by_dimensions(
    config: &Config,
    dimension_data: &Map<String, Value>,
) -> Config {
    let filtered_context =
        Context::filter_by_eval(config.contexts.clone(), dimension_data);

    let filtered_overrides: HashMap<String, Overrides> = filtered_context
        .iter()
        .flat_map(|ele| {
            let override_with_key = &ele.override_with_keys[0];
            config
                .overrides
                .get(override_with_key)
                .map(|value| (override_with_key.to_string(), value.clone()))
        })
        .collect();

    let filtered_config = Config {
        contexts: filtered_context,
        overrides: filtered_overrides,
        default_configs: config.default_configs.clone(),
    };

    filtered_config
}
