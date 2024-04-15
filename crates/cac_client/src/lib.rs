mod eval;
mod interface;
mod utils;

use actix_web::{rt::time::interval, web::Data};
use chrono::{DateTime, Utc};
use derive_more::{Deref, DerefMut};
use reqwest::{RequestBuilder, Response, StatusCode};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use std::{
    collections::{HashMap, HashSet},
    convert::identity,
    sync::{Arc, RwLock},
    time::{Duration, UNIX_EPOCH},
};
use strum_macros;
use utils::core::MapError;

use service_utils::{helpers::extract_dimensions, result, unexpected_error};

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Context {
    pub condition: Value,
    pub override_with_keys: [String; 1],
}

#[repr(C)]
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Config {
    contexts: Vec<Context>,
    overrides: Map<String, Value>,
    default_configs: Map<String, Value>,
}

#[derive(strum_macros::EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum MergeStrategy {
    MERGE,
    REPLACE,
}

impl Default for MergeStrategy {
    fn default() -> Self {
        return Self::MERGE;
    }
}

impl From<String> for MergeStrategy {
    fn from(value: String) -> Self {
        match value.to_lowercase().as_str() {
            "replace" => MergeStrategy::REPLACE,
            "merge" => MergeStrategy::MERGE,
            _ => MergeStrategy::default(),
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
        let last_modified = self.last_modified.read().map_err_to_string()?.to_rfc2822();
        let reqw = clone_reqw(&self.reqw)?.header("If-Modified-Since", last_modified);
        let resp = reqw.send().await.map_err_to_string()?;
        match resp.status() {
            StatusCode::NOT_MODIFIED => {
                return Err(String::from(format!(
                    "{} CAC: skipping update, remote not modified",
                    self.tenant
                )));
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
        let mut config = self.config.write().map_err_to_string()?;
        let mut last_modified = self.last_modified.write().map_err_to_string()?;
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

    pub fn get_full_config_state_with_filter(
        &self,
        query_data: Option<Map<String, Value>>,
    ) -> Result<Config, String> {
        let mut config = self.config.read().map(|c| c.clone()).map_err_to_string()?;
        if let Some(mut query_map) = query_data {
            if let Some(prefix) = query_map.get("prefix") {
                let prefix_list: HashSet<&str> = prefix
                    .as_str()
                    .ok_or_else(|| {
                        log::error!("Prefix is not a valid string.");
                        format!("Prefix is not a valid string.")
                    })
                    .map_err_to_string()?
                    .split(",")
                    .collect();
                config =
                    filter_config_by_prefix(&config, &prefix_list).map_err_to_string()?;
            }

            query_map.remove("prefix");

            if !query_map.is_empty() {
                config = filter_config_by_dimensions(&config, &query_map)
                    .map_err_to_string()?;
            }
        }
        Ok(config)
    }

    pub fn get_last_modified(&self) -> Result<DateTime<Utc>, String> {
        self.last_modified.read().map(|t| *t).map_err_to_string()
    }

    pub fn eval(
        &self,
        query_data: Map<String, Value>,
        merge_strategy: MergeStrategy,
    ) -> Result<Map<String, Value>, String> {
        let cac = self.config.read().map_err_to_string()?;
        eval::eval_cac(
            cac.default_configs.to_owned(),
            &cac.contexts,
            &cac.overrides,
            &query_data,
            merge_strategy,
        )
    }

    pub fn get_resolved_config(
        &self,
        query_data: Map<String, Value>,
        filter_keys: Option<Vec<String>>,
        merge_strategy: MergeStrategy,
    ) -> Result<Map<String, Value>, String> {
        let mut cac = self.eval(query_data, merge_strategy)?;
        if let Some(keys) = filter_keys {
            cac = filter_keys_by_prefix(cac, &keys.iter().map(|s| s.as_str()).collect())
                .map_err_to_string()?;
        }
        return Ok(cac);
    }

    pub fn get_default_config(
        &self,
        filter_keys: Option<Vec<String>>,
    ) -> Result<Map<String, Value>, String> {
        let configs = self.config.read().map_err(|e| e.to_string())?;
        let mut default_configs = configs.default_configs.clone();
        if let Some(keys) = filter_keys {
            default_configs = filter_keys_by_prefix(
                default_configs,
                &keys.iter().map(|s| s.as_str()).collect(),
            )
            .map_err_to_string()?;
        }
        return Ok(default_configs);
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
        let mut factory = match self.write() {
            Ok(factory) => factory,
            Err(e) => {
                log::error!("CAC_CLIENT_FACTORY: failed to acquire write lock {}", e);
                return Err("CAC_CLIENT_FACTORY: Failed to create client".to_string());
            }
        };

        if let Some(client) = factory.get(&tenant) {
            return Ok(client.clone());
        }

        let client =
            Arc::new(Client::new(tenant.to_string(), polling_interval, hostname).await?);
        factory.insert(tenant.to_string(), client.clone());
        return Ok(client.clone());
    }

    pub fn get_client(&self, tenant: String) -> Result<Arc<Client>, String> {
        let factory = match self.read() {
            Ok(factory) => factory,
            Err(e) => {
                log::error!("CAC_CLIENT_FACTORY: failed to acquire read lock {}", e);
                return Err("CAC_CLIENT_FACTORY: Failed to acquire client.".to_string());
            }
        };

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
    prefix_list: &HashSet<&str>,
) -> result::Result<Map<String, Value>> {
    Ok(keys
        .into_iter()
        .filter(|(key, _)| {
            prefix_list
                .iter()
                .any(|prefix_str| key.starts_with(prefix_str))
        })
        .collect())
}

pub fn filter_config_by_prefix(
    config: &Config,
    prefix_list: &HashSet<&str>,
) -> result::Result<Config> {
    let mut filtered_overrides: Map<String, Value> = Map::new();

    let filtered_default_config: Map<String, Value> =
        filter_keys_by_prefix(config.default_configs.clone(), prefix_list)?;

    for (key, overrides) in &config.overrides {
        let overrides_map = overrides
            .as_object()
            .ok_or_else(|| {
                log::error!("failed to decode overrides.");
                unexpected_error!("failed to decode overrides.")
            })?
            .clone();

        let filtered_overrides_map: Map<String, Value> = overrides_map
            .into_iter()
            .filter(|(key, _)| filtered_default_config.contains_key(key))
            .collect();

        if !filtered_overrides_map.is_empty() {
            filtered_overrides.insert(key.clone(), Value::Object(filtered_overrides_map));
        }
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
    query_params_map: &Map<String, Value>,
) -> result::Result<Config> {
    let filter_context = |contexts: &Vec<Context>,
                          query_params_map: &Map<String, Value>|
     -> result::Result<Vec<Context>> {
        let mut filtered_context: Vec<Context> = Vec::new();
        for context in contexts.iter() {
            let dimension = extract_dimensions(&context.condition)?;
            let should_add_ctx = dimension.iter().all(|(key, value)| {
                query_params_map.get(key).map_or(true, |val| {
                    val == value || val.as_array().unwrap_or(&vec![]).contains(value)
                })
            });
            if should_add_ctx {
                filtered_context.push(context.clone());
            }
        }
        return Ok(filtered_context);
    };

    let filtered_context = filter_context(&config.contexts, &query_params_map)?;
    let filtered_overrides: Map<String, Value> = filtered_context
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

    Ok(filtered_config)
}
