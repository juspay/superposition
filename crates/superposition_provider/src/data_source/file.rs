use std::path::PathBuf;
use std::sync::Mutex;

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use notify::{Event, RecommendedWatcher, Watcher};
use serde_json::{Map, Value};
use superposition_core::{ConfigFormat, JsonFormat, TomlFormat};
use superposition_types::{ConfigFilter, PrefixList};
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
    file_format: &'static str,
    watcher: Mutex<Option<WatcherInner>>,
}

impl FileDataSource {
    pub fn new(file_path: PathBuf) -> Result<Self> {
        let file_format = match file_path
            .extension()
            .and_then(|ext| ext.to_str())
            .map(|s| s.to_lowercase())
        {
            Some(ref ext) if ext == "json" => "json",
            Some(ref ext) if ext == "toml" => "toml",
            Some(ext) => {
                return Err(SuperpositionError::DataSourceError(format!(
                    "Unsupported file extension '{}'. Supported formats are 'json' and 'toml'.",
                    ext
                )))
            }
            None => {
                return Err(SuperpositionError::DataSourceError(
                    "File path must have an extension to determine format.".into(),
                ));
            }
        };

        Ok(Self {
            file_path,
            file_format,
            watcher: Mutex::new(None),
        })
    }

    async fn last_modified_at(&self) -> Result<DateTime<Utc>> {
        tokio::fs::metadata(&self.file_path)
            .await
            .map_err(|e| {
                SuperpositionError::DataSourceError(format!(
                    "Failed to read metadata for config file {:?}: {}",
                    self.file_path, e
                ))
            })?
            .modified()
            .map(DateTime::<Utc>::from)
            .map_err(|e| {
                SuperpositionError::DataSourceError(format!(
                    "Failed to read modified time for config file {:?}: {}",
                    self.file_path, e
                ))
            })
    }

    async fn is_not_modified(&self, if_modified_since: DateTime<Utc>) -> Result<bool> {
        let last_modified_at = self.last_modified_at().await?;
        Ok(last_modified_at <= if_modified_since)
    }
}

#[async_trait]
impl SuperpositionDataSource for FileDataSource {
    async fn fetch_filtered_config(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        exclude_prefix_filter: Option<Vec<String>>,
        if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ConfigData>> {
        if let Some(if_modified_since) = if_modified_since {
            if self.is_not_modified(if_modified_since).await? {
                log::debug!(
                    "FileDataSource: config file not modified since {:?}",
                    if_modified_since
                );
                return Ok(FetchResponse::NotModified);
            }
        }

        let now = Utc::now();
        let content = tokio::fs::read_to_string(&self.file_path)
            .await
            .map_err(|e| {
                SuperpositionError::DataSourceError(format!(
                    "Failed to read config file {:?}: {}",
                    self.file_path, e
                ))
            })?;

        let parser = match self.file_format.to_lowercase().as_str() {
            "json" => JsonFormat::parse_config,
            _ => TomlFormat::parse_config,
        };
        let mut config = parser(&content).map_err(|e| {
            SuperpositionError::DataSourceError(format!(
                "Failed to parse {} config: {}",
                self.file_format.to_uppercase(),
                e
            ))
        })?;

        config = config.filter(
            context,
            prefix_filter.map(PrefixList::from_iter).as_ref(),
            exclude_prefix_filter.map(PrefixList::from_iter).as_ref(),
        );

        Ok(FetchResponse::Data(ConfigData {
            data: config,
            fetched_at: now,
        }))
    }

    async fn fetch_active_experiments(
        &self,
        _if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>> {
        Err(SuperpositionError::DataSourceError(
            "Experiments not supported by FileDataSource".into(),
        ))
    }

    async fn fetch_candidate_active_experiments(
        &self,
        _context: Option<Map<String, Value>>,
        _prefix_filter: Option<Vec<String>>,
        _exclude_prefix_filter: Option<Vec<String>>,
        _if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>> {
        Err(SuperpositionError::DataSourceError(
            "Experiments not supported by FileDataSource".into(),
        ))
    }

    async fn fetch_matching_active_experiments(
        &self,
        _context: Option<Map<String, Value>>,
        _prefix_filter: Option<Vec<String>>,
        _exclude_prefix_filter: Option<Vec<String>>,
        _if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>> {
        Err(SuperpositionError::DataSourceError(
            "Experiments not supported by FileDataSource".into(),
        ))
    }

    fn supports_experiments(&self) -> bool {
        false
    }

    fn watch(&self) -> Result<Option<WatchStream>> {
        // Acquire both locks upfront to prevent concurrent watcher creation
        let mut watcher_guard = self.watcher.lock().map_err(|e| {
            SuperpositionError::DataSourceError(format!(
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

        // Watch the parent directory, not the file node: editors save via atomic rename (write a
        // temp file, then rename it over the target), which replaces the inode. A node-level watch
        // would keep watching the stale inode and miss the new file, so watch the directory and
        // filter events down to the target file by name.
        let watch_dir = self
            .file_path
            .parent()
            .filter(|p| !p.as_os_str().is_empty())
            .map(std::path::Path::to_path_buf)
            .unwrap_or_else(|| PathBuf::from("."));
        let target_name = self.file_path.file_name().map(|n| n.to_os_string());

        let mut watcher = notify::recommended_watcher(
            move |res: std::result::Result<Event, notify::Error>| match res {
                Ok(event) => {
                    if event
                        .paths
                        .iter()
                        .any(|p| p.file_name() == target_name.as_deref())
                    {
                        let _ = tx_clone.send(());
                    }
                }
                Err(e) => {
                    log::error!("FileDataSource: watch error: {}", e);
                }
            },
        )
        .map_err(|e| {
            SuperpositionError::DataSourceError(format!(
                "Failed to create file watcher: {}",
                e
            ))
        })?;

        watcher
            .watch(&watch_dir, notify::RecursiveMode::NonRecursive)
            .map_err(|e| {
                SuperpositionError::DataSourceError(format!(
                    "Failed to watch directory {:?}: {}",
                    watch_dir, e
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
            SuperpositionError::DataSourceError(format!(
                "Failed to lock watcher mutex: {}",
                e
            ))
        })?;
        *guard = None;

        Ok(())
    }
}
