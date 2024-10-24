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
use serde_json::{Map, Value};
use superposition_types::{Config, Context};
use tokio::sync::RwLock;
use utils::core::MapError;

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
        self.last_modified.read().await.clone()
    }

    pub async fn get_resolved_config(
        &self,
        query_data: Map<String, Value>,
        filter_keys: Option<Vec<String>>,
        merge_strategy: MergeStrategy,
    ) -> Result<Map<String, Value>, String> {
        let cac = self.config.read().await;
        let mut config = cac.to_owned();
        if let Some(keys) = filter_keys {
            config = config.filter_by_prefix(&HashSet::from_iter(keys));
        }
        eval::eval_cac(
            config.default_configs.to_owned(),
            &config.contexts,
            &config.overrides,
            &query_data,
            merge_strategy,
        )
    }

    pub async fn get_default_config(
        &self,
        filter_keys: Option<Vec<String>>,
    ) -> Result<Map<String, Value>, String> {
        let configs = self.config.read().await;
        let mut default_configs = configs.default_configs.clone();
        if let Some(keys) = filter_keys {
            default_configs = configs.filter_default_by_prefix(&HashSet::from_iter(keys));
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
