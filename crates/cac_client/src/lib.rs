mod eval;
mod utils;

use actix_web::{
    rt::{self, time::interval},
    web::Data,
};
use chrono::{DateTime, Utc};
use reqwest::{RequestBuilder, StatusCode};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use std::{convert::identity, sync::RwLock, time::Duration};
use utils::core::MapError;

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Context {
    pub condition: Value,
    pub override_with_keys: [String; 1],
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Config {
    contexts: Vec<Context>,
    overrides: Map<String, Value>,
    default_configs: Map<String, Value>,
}

#[derive(Clone)]
pub struct Client {
    reqw: Data<reqwest::RequestBuilder>,
    polling_interval: Duration,
    last_modified: Data<RwLock<DateTime<Utc>>>,
    config: Data<RwLock<Config>>,
}

fn clone_reqw(reqw: &RequestBuilder) -> Result<RequestBuilder, String> {
    reqw.try_clone()
        .ok_or_else(|| "Unable to clone reqw".to_string())
}

impl Client {
    pub async fn new(
        update_config_periodically: bool,
        polling_interval: Duration,
        hostname: String,
    ) -> Result<Self, String> {
        let reqw_client = reqwest::Client::builder().build().map_err_to_string()?;
        let cac_endpoint = format!("{hostname}/config");
        let reqw = reqw_client.get(cac_endpoint);
        let reqwc = clone_reqw(&reqw)?;
        let resp = reqwc.send().await.map_err_to_string()?;
        let config = resp.json::<Config>().await.map_err_to_string()?;
        let timestamp = Utc::now();
        let client = Client {
            reqw: Data::new(reqw),
            polling_interval,
            last_modified: Data::new(RwLock::new(timestamp)),
            config: Data::new(RwLock::new(config)),
        };
        if update_config_periodically {
            client.clone().start_polling_update().await;
        }
        Ok(client)
    }

    async fn fetch(&self) -> Result<Config, String> {
        let last_modified = self.last_modified.read().map_err_to_string()?.to_rfc2822();
        let reqw = clone_reqw(&self.reqw)?.header("If-Modified-Since", last_modified);
        let resp = reqw.send().await.map_err_to_string()?;
        match resp.status() {
            StatusCode::NOT_MODIFIED => {
                return Err(String::from("CAC: skipping update, remote not modified"));
            }
            StatusCode::OK => log::info!("CAC: new config received, updating"),
            x => return Err(format!("CAC: fetch failed, status: {}", x,)),
        };
        resp.json::<Config>().await.map_err_to_string()
    }

    async fn update_cac(&self) -> Result<String, String> {
        let fetched_config = self.fetch().await?;
        let mut config = self.config.write().map_err_to_string()?;
        let mut last_modified = self.last_modified.write().map_err_to_string()?;
        *config = fetched_config;
        *last_modified = Utc::now();
        Ok("CAC updated successfully".to_string())
    }

    pub async fn start_polling_update(self) {
        rt::spawn(async move {
            let mut interval = interval(self.polling_interval);
            loop {
                interval.tick().await;
                let result = self.update_cac().await.unwrap_or_else(identity);
                log::info!("{result}",);
            }
        });
    }

    pub fn get_config(&self) -> Result<Config, String> {
        self.config.read().map(|c| c.clone()).map_err_to_string()
    }

    pub fn get_last_modified<E>(&'static self) -> Result<DateTime<Utc>, String> {
        self.last_modified.read().map(|t| *t).map_err_to_string()
    }

    pub fn eval(
        &self,
        query_data: Map<String, Value>,
    ) -> Result<Map<String, Value>, String> {
        let cac = self.config.read().map_err_to_string()?;
        eval::eval_cac(
            cac.default_configs.to_owned(),
            &cac.contexts,
            &cac.overrides,
            &query_data,
        )
    }
}

pub use eval::eval_cac;
