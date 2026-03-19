use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use log::error;
use notify::{Event, RecommendedWatcher, Watcher};
use serde_json::{Map, Value};
use superposition_core::{ConfigFormat, TomlFormat};
use tokio::sync::broadcast;

use crate::data_source::FetchResponse;
use crate::types::{Result, SuperpositionError, WatchStream};

use super::{
    ConfigData, ExperimentGroupResponse, ExperimentResponse, SuperpositionDataSource,
};

pub struct FileDataSource {
    file_path: PathBuf,
    watcher: Arc<Mutex<Option<RecommendedWatcher>>>,
    broadcast_tx: Arc<Mutex<Option<broadcast::Sender<()>>>>,
}

impl FileDataSource {
    pub fn new(file_path: PathBuf) -> Self {
        Self {
            file_path,
            watcher: Arc::new(Mutex::new(None)),
            broadcast_tx: Arc::new(Mutex::new(None)),
        }
    }
}

#[async_trait]
impl SuperpositionDataSource for FileDataSource {
    async fn fetch_config(
        &self,
        _: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ConfigData>> {
        let content = tokio::fs::read_to_string(&self.file_path)
            .await
            .map_err(|e| {
                SuperpositionError::ConfigError(format!(
                    "Failed to read config file {:?}: {}",
                    self.file_path, e
                ))
            })?;

        let config = TomlFormat::parse_config(&content).map_err(|e| {
            SuperpositionError::ConfigError(format!("Failed to parse TOML config: {}", e))
        })?;

        Ok(FetchResponse::Data(ConfigData::new(config)))
    }

    async fn fetch_filtered_config(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        last_fetched_at: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ConfigData>> {
        let resp = self
            .fetch_config(last_fetched_at)
            .await?
            .map_data(|mut data| {
                data.config = data.config.filter(
                    context.as_ref(),
                    prefix_filter.map(|p| p.into_iter().collect()).as_ref(),
                );
                data
            });

        Ok(resp)
    }

    async fn fetch_active_experiments(
        &self,
        _last_fetched_at: Option<DateTime<Utc>>,
    ) -> Result<(
        FetchResponse<ExperimentResponse>,
        FetchResponse<ExperimentGroupResponse>,
    )> {
        Err(SuperpositionError::ConfigError(
            "Experiments not supported by FileDataSource".into(),
        ))
    }

    async fn fetch_candidate_active_experiments(
        &self,
        _context: Option<Map<String, Value>>,
        _prefix_filter: Option<Vec<String>>,
        _last_fetched_at: Option<DateTime<Utc>>,
    ) -> Result<(
        FetchResponse<ExperimentResponse>,
        FetchResponse<ExperimentGroupResponse>,
    )> {
        Err(SuperpositionError::ConfigError(
            "Experiments not supported by FileDataSource".into(),
        ))
    }

    async fn fetch_matching_active_experiments(
        &self,
        _context: Option<Map<String, Value>>,
        _prefix_filter: Option<Vec<String>>,
        _last_fetched_at: Option<DateTime<Utc>>,
    ) -> Result<(
        FetchResponse<ExperimentResponse>,
        FetchResponse<ExperimentGroupResponse>,
    )> {
        Err(SuperpositionError::ConfigError(
            "Experiments not supported by FileDataSource".into(),
        ))
    }

    fn supports_experiments(&self) -> bool {
        false
    }

    fn watch(&self) -> Result<Option<WatchStream>> {
        // Check if already watching - return a new subscriber to existing broadcast
        let tx_guard = self.broadcast_tx.lock().map_err(|e| {
            SuperpositionError::ConfigError(format!(
                "Failed to lock broadcast mutex: {}",
                e
            ))
        })?;

        if let Some(tx) = tx_guard.as_ref() {
            // Already watching - return a new subscriber
            return Ok(Some(WatchStream {
                receiver: tx.subscribe(),
            }));
        }

        // Drop the lock before creating the watcher to avoid holding lock during setup
        drop(tx_guard);

        // Create broadcast channel for multiple consumers
        let (tx, _rx) = broadcast::channel(16);
        let tx_clone = tx.clone();

        let mut watcher = notify::recommended_watcher(
            move |res: std::result::Result<Event, notify::Error>| match res {
                Ok(_event) => {
                    let _ = tx_clone.send(());
                }
                Err(e) => {
                    error!("FileDataSource: watch error: {}", e);
                }
            },
        )
        .map_err(|e| {
            SuperpositionError::ConfigError(format!(
                "Failed to create file watcher: {}",
                e
            ))
        })?;

        watcher
            .watch(&self.file_path, notify::RecursiveMode::NonRecursive)
            .map_err(|e| {
                SuperpositionError::ConfigError(format!(
                    "Failed to watch file {:?}: {}",
                    self.file_path, e
                ))
            })?;

        let mut watcher_guard = self.watcher.lock().map_err(|e| {
            SuperpositionError::ConfigError(format!(
                "Failed to lock watcher mutex: {}",
                e
            ))
        })?;

        let mut tx_guard = self.broadcast_tx.lock().map_err(|e| {
            SuperpositionError::ConfigError(format!(
                "Failed to lock broadcast mutex: {}",
                e
            ))
        })?;

        // Double-check in case another thread raced us
        if let Some(tx) = tx_guard.as_ref() {
            return Ok(Some(WatchStream {
                receiver: tx.subscribe(),
            }));
        }

        *watcher_guard = Some(watcher);
        *tx_guard = Some(tx.clone());

        Ok(Some(WatchStream {
            receiver: tx.subscribe(),
        }))
    }

    async fn close(&self) -> Result<()> {
        let mut guard = self.watcher.lock().map_err(|e| {
            SuperpositionError::ConfigError(format!(
                "Failed to lock watcher mutex: {}",
                e
            ))
        })?;
        *guard = None;

        let mut tx_guard = self.broadcast_tx.lock().map_err(|e| {
            SuperpositionError::ConfigError(format!(
                "Failed to lock broadcast mutex: {}",
                e
            ))
        })?;
        *tx_guard = None;

        Ok(())
    }
}
