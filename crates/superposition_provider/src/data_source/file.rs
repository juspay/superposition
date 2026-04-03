use std::path::PathBuf;
use std::sync::Mutex;

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use notify::{Event, RecommendedWatcher, Watcher};
use serde_json::{Map, Value};
use superposition_core::{ConfigFormat, TomlFormat};
use tokio::sync::broadcast;

use crate::data_source::FetchResponse;
use crate::types::{Result, SuperpositionError, WatchStream};

use super::{ConfigData, ExperimentData, SuperpositionDataSource};

struct WatcherInner {
    _watcher: RecommendedWatcher,
    broadcast_tx: broadcast::Sender<()>,
}

pub struct FileDataSource {
    file_path: PathBuf,
    watcher: Mutex<Option<WatcherInner>>,
}

impl FileDataSource {
    pub fn new(file_path: PathBuf) -> Self {
        Self {
            file_path,
            watcher: Mutex::new(None),
        }
    }
}

#[async_trait]
impl SuperpositionDataSource for FileDataSource {
    async fn fetch_config(
        &self,
        if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ConfigData>> {
        if if_modified_since.is_some() {
            log::debug!("FileDataSource: ignoring if_modified_since, always reading fresh from file");
        }
        let now = Utc::now();
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

        Ok(FetchResponse::Data(ConfigData {
            data: config,
            fetched_at: now,
        }))
    }

    async fn fetch_filtered_config(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ConfigData>> {
        let resp = self
            .fetch_config(if_modified_since)
            .await?
            .map_data(|mut data| {
                data.data = data.data.filter(
                    context.as_ref(),
                    prefix_filter.map(|p| p.into_iter().collect()).as_ref(),
                );
                data
            });

        Ok(resp)
    }

    async fn fetch_active_experiments(
        &self,
        _if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>> {
        Err(SuperpositionError::ConfigError(
            "Experiments not supported by FileDataSource".into(),
        ))
    }

    async fn fetch_candidate_active_experiments(
        &self,
        _context: Option<Map<String, Value>>,
        _prefix_filter: Option<Vec<String>>,
        _if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>> {
        Err(SuperpositionError::ConfigError(
            "Experiments not supported by FileDataSource".into(),
        ))
    }

    async fn fetch_matching_active_experiments(
        &self,
        _context: Option<Map<String, Value>>,
        _prefix_filter: Option<Vec<String>>,
        _if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>> {
        Err(SuperpositionError::ConfigError(
            "Experiments not supported by FileDataSource".into(),
        ))
    }

    fn supports_experiments(&self) -> bool {
        false
    }

    fn watch(&self) -> Result<Option<WatchStream>> {
        // Acquire both locks upfront to prevent concurrent watcher creation
        let mut watcher_guard = self.watcher.lock().map_err(|e| {
            SuperpositionError::ConfigError(format!(
                "Failed to lock watcher mutex: {}",
                e
            ))
        })?;

        // If already watching, return a new subscriber to the existing broadcast
        if let Some(inner) = watcher_guard.as_ref() {
            return Ok(Some(WatchStream {
                receiver: inner.broadcast_tx.subscribe(),
            }));
        }

        // Both checks confirmed None — safe to create under the lock
        let (tx, _rx) = broadcast::channel(16);
        let tx_clone = tx.clone();

        let mut watcher = notify::recommended_watcher(
            move |res: std::result::Result<Event, notify::Error>| match res {
                Ok(_event) => {
                    let _ = tx_clone.send(());
                }
                Err(e) => {
                    log::error!("FileDataSource: watch error: {}", e);
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

        let subscriber = tx.subscribe();
        *watcher_guard = Some(WatcherInner {
            _watcher: watcher,
            broadcast_tx: tx,
        });

        Ok(Some(WatchStream {
            receiver: subscriber,
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

        Ok(())
    }
}
