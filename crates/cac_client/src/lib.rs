#![deny(unused_crate_dependencies)]
mod eval;
mod interface;
pub mod utils;

use std::{
    collections::{BTreeSet, HashMap, HashSet},
    convert::identity,
    sync::Arc,
    time::{Duration, UNIX_EPOCH},
};

use actix_web::{rt::time::interval, web::Data};
use chrono::{DateTime, Utc};
use derive_more::{Deref, DerefMut};
use itertools::Itertools;
use mini_moka::sync::Cache;
use reqwest::{RequestBuilder, Response, StatusCode};
use serde_json::{Map, Value};
pub use superposition_types::api::config::MergeStrategy;
use superposition_types::{Config, Context, ExtendedMap};
use tokio::sync::RwLock;
use utils::{core::MapError, json_to_sorted_string};

static CACHE_MAX_CAPACITY: u64 = 10 * 1024 * 1024; //in mb
static CACHE_TTL: u64 = 180 * 60; //in minutes
static CACHE_TTI: u64 = 30 * 60; //in minutes

#[repr(C)]
#[derive(Clone)]
pub struct Client {
    tenant: String,
    reqw: Data<reqwest::RequestBuilder>,
    polling_interval: Duration,
    last_modified: Data<RwLock<DateTime<Utc>>>,
    config: Data<RwLock<Config>>,
    config_cache: Cache<String, Map<String, Value>>,
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
    /** cache_max_capacity: Max size of cache in mb, default 10 mb
     *  cache_ttl: Time to live value in minutes, default 180 minutes
     *  cache_tti: Time to idle value in minutes, default 30 minutes
     */
    pub async fn new(
        tenant: String,
        polling_interval: Duration,
        hostname: String,
        cache_max_capacity: Option<u64>,
        cache_ttl: Option<u64>,
        cache_tti: Option<u64>,
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
        let config_cache = Cache::builder()
            .weigher(|_key, value: &Map<String, Value>| -> u32 {
                Value::Object(value.to_owned())
                    .to_string()
                    .len()
                    .try_into()
                    .unwrap_or(u32::MAX)
            })
            // max size of cache in mb
            .max_capacity(
                cache_max_capacity.map_or(CACHE_MAX_CAPACITY, |v| v * 1024 * 1024),
            )
            // Time to live (TTL): in minutes
            .time_to_live(Duration::from_secs(cache_ttl.map_or(CACHE_TTL, |v| v * 60)))
            // Time to idle (TTI):  in minutes
            .time_to_idle(Duration::from_secs(cache_tti.map_or(CACHE_TTI, |v| v * 60)))
            // Create the cache.
            .build();
        let client = Client {
            tenant,
            reqw: Data::new(reqw),
            polling_interval,
            last_modified: Data::new(RwLock::new(
                last_modified_at.unwrap_or(DateTime::<Utc>::from(UNIX_EPOCH)),
            )),
            config: Data::new(RwLock::new(config)),
            config_cache,
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
        self.config_cache.invalidate_all();
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
            config = config.filter_by_prefix(&HashSet::from_iter(prefix_list));
        }

        let dimension_filtered_config = query_data
            .filter(|query_map| !query_map.is_empty())
            .map(|query_map| config.filter_by_dimensions(&query_map));

        if let Some(filtered_config) = dimension_filtered_config {
            config = filtered_config;
        };

        Ok(config)
    }

    pub async fn get_last_modified(&self) -> DateTime<Utc> {
        *self.last_modified.read().await
    }

    pub async fn get_resolved_config(
        &self,
        query_data: Map<String, Value>,
        filter_keys: Option<Vec<String>>,
        merge_strategy: MergeStrategy,
    ) -> Result<Map<String, Value>, String> {
        let filter_keys_concat = if let Some(vec) = filter_keys.clone() {
            BTreeSet::from_iter(vec).iter().join(",")
        } else {
            "null".to_string()
        };
        let hash_key = json_to_sorted_string(&Value::Object(query_data.clone()))
            + "?"
            + &merge_strategy.clone().to_string()
            + "?"
            + &filter_keys_concat;
        if let Some(value) = self.config_cache.get(&hash_key) {
            Ok(value)
        } else {
            let cac = self.config.read().await;
            let mut config = cac.to_owned();
            if let Some(keys) = filter_keys {
                config = config.filter_by_prefix(&HashSet::from_iter(keys));
            }
            let evaled_cac = eval::eval_cac(&config, &query_data, merge_strategy)?;
            self.config_cache.insert(hash_key, evaled_cac.clone());
            Ok(evaled_cac)
        }
    }

    pub async fn get_default_config(
        &self,
        filter_keys: Option<Vec<String>>,
    ) -> Result<ExtendedMap, String> {
        let configs = self.config.read().await;
        let default_configs = match filter_keys {
            Some(keys) => configs.filter_default_by_prefix(&HashSet::from_iter(keys)),
            _ => configs.default_configs.clone(),
        };
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

        let client = Arc::new(
            Client::new(
                tenant.to_string(),
                polling_interval,
                hostname,
                None,
                None,
                None,
            )
            .await?,
        );
        factory.insert(tenant.to_string(), client.clone());
        Ok(client.clone())
    }

    pub async fn create_client_with_cache_properties(
        &self,
        tenant: String,
        polling_interval: Duration,
        hostname: String,
        cache_max_capacity: u64,
        cache_ttl: u64,
        cache_tti: u64,
    ) -> Result<Arc<Client>, String> {
        let mut factory = self.write().await;

        if let Some(client) = factory.get(&tenant) {
            return Ok(client.clone());
        }

        let client = Arc::new(
            Client::new(
                tenant.to_string(),
                polling_interval,
                hostname,
                Some(cache_max_capacity),
                Some(cache_ttl),
                Some(cache_tti),
            )
            .await?,
        );
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
