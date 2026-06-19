use std::path::PathBuf;
use std::sync::Mutex;

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use notify::{Event, RecommendedWatcher, Watcher};
use serde_json::{Map, Value};
use superposition_core::{ConfigFormat, JsonFormat, TomlFormat};
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
    pub fn new(file_path: PathBuf) -> std::result::Result<Self, String> {
        let file_format = match file_path
            .extension()
            .and_then(|ext| ext.to_str())
            .map(|s| s.to_lowercase())
        {
            Some(ref ext) if ext == "json" => "json",
            Some(ref ext) if ext == "toml" => "toml",
            Some(ext) => return Err(format!("Unsupported file extension '{}'.", ext)),
            None => {
                return Err(
                    "File path must have an extension to determine format.".into()
                );
            }
        };

        Ok(Self {
            file_path,
            file_format,
            watcher: Mutex::new(None),
        })
    }

    async fn last_modified_at(&self) -> Result<DateTime<Utc>> {
        let metadata = tokio::fs::metadata(&self.file_path).await.map_err(|e| {
            SuperpositionError::ConfigError(format!(
                "Failed to read metadata for config file {:?}: {}",
                self.file_path, e
            ))
        })?;

        metadata.modified().map(DateTime::<Utc>::from).map_err(|e| {
            SuperpositionError::ConfigError(format!(
                "Failed to read modified time for config file {:?}: {}",
                self.file_path, e
            ))
        })
    }

    fn is_not_modified(
        last_modified_at: DateTime<Utc>,
        if_modified_since: Option<DateTime<Utc>>,
    ) -> bool {
        if_modified_since.is_some_and(|modified_since| last_modified_at <= modified_since)
    }

    async fn read_config(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        fetched_at: DateTime<Utc>,
    ) -> Result<FetchResponse<ConfigData>> {
        let content = tokio::fs::read_to_string(&self.file_path)
            .await
            .map_err(|e| {
                SuperpositionError::ConfigError(format!(
                    "Failed to read config file {:?}: {}",
                    self.file_path, e
                ))
            })?;

        let parser = match self.file_format.to_lowercase().as_str() {
            "json" => JsonFormat::parse_config,
            _ => TomlFormat::parse_config,
        };
        let mut config = parser(&content).map_err(|e| {
            SuperpositionError::ConfigError(format!(
                "Failed to parse {} config: {}",
                self.file_format.to_uppercase(),
                e
            ))
        })?;

        config = config.filter(
            context.as_ref(),
            prefix_filter.map(|p| p.into_iter().collect()).as_ref(),
        );

        Ok(FetchResponse::Data(ConfigData {
            data: config,
            fetched_at,
        }))
    }
}

#[async_trait]
impl SuperpositionDataSource for FileDataSource {
    async fn fetch_filtered_config(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ConfigData>> {
        if if_modified_since.is_some() {
            log::debug!("FileDataSource: ignoring if_modified_since, always reading fresh from file");
        }

        self.read_config(context, prefix_filter, Utc::now()).await
    }

    async fn fetch_config_if_modified(
        &self,
        if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ConfigData>> {
        let last_modified_at = self.last_modified_at().await?;
        if Self::is_not_modified(last_modified_at, if_modified_since) {
            log::debug!("FileDataSource: config file not modified");
            return Ok(FetchResponse::NotModified);
        }

        self.read_config(None, None, last_modified_at).await
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

#[cfg(test)]
mod tests {
    use std::path::{Path, PathBuf};

    use tokio::time::{sleep, Duration};

    use super::*;

    fn config_content(timeout: i64) -> String {
        format!(
            r#"[default-configs]
timeout = {{ value = {timeout}, schema = {{ type = "integer" }} }}

[dimensions]
os = {{ position = 1, schema = {{ type = "string" }} }}

[[overrides]]
_context_ = {{ os = "linux" }}
timeout = 45
"#
        )
    }

    fn temp_config_path() -> PathBuf {
        std::env::temp_dir().join(format!(
            "superposition-provider-file-source-{}.toml",
            uuid::Uuid::new_v4()
        ))
    }

    async fn write_config(path: &Path, timeout: i64) {
        tokio::fs::write(path, config_content(timeout))
            .await
            .unwrap();
    }

    async fn rewrite_config_after(
        source: &FileDataSource,
        path: &Path,
        timeout: i64,
        previous_modified_at: DateTime<Utc>,
    ) {
        for _ in 0..100 {
            sleep(Duration::from_millis(20)).await;
            write_config(path, timeout).await;

            if source.last_modified_at().await.unwrap() > previous_modified_at {
                return;
            }
        }

        panic!("config file modified time did not advance");
    }

    #[tokio::test]
    async fn fetch_config_keeps_reading_fresh_when_if_modified_since_is_present() {
        let path = temp_config_path();
        write_config(&path, 30).await;

        let source = FileDataSource::new(path.clone()).unwrap();
        let first_fetch = source.fetch_config(None).await.unwrap();
        let fetched_at = match first_fetch {
            FetchResponse::Data(data) => {
                assert_eq!(data.data.default_configs.get("timeout"), Some(&30.into()));
                data.fetched_at
            }
            FetchResponse::NotModified => panic!("initial fetch should return data"),
        };

        let second_fetch = source.fetch_config(Some(fetched_at)).await.unwrap();
        match second_fetch {
            FetchResponse::Data(data) => {
                assert_eq!(data.data.default_configs.get("timeout"), Some(&30.into()));
            }
            FetchResponse::NotModified => {
                panic!("normal fetch should keep reading fresh")
            }
        }

        let _ = tokio::fs::remove_file(path).await;
    }

    #[tokio::test]
    async fn fetch_config_if_modified_returns_not_modified_when_file_has_not_changed() {
        let path = temp_config_path();
        write_config(&path, 30).await;

        let source = FileDataSource::new(path.clone()).unwrap();
        let first_fetch = source.fetch_config(None).await.unwrap();
        let fetched_at = match first_fetch {
            FetchResponse::Data(data) => data.fetched_at,
            FetchResponse::NotModified => panic!("initial fetch should return data"),
        };

        let second_fetch = source
            .fetch_config_if_modified(Some(fetched_at))
            .await
            .unwrap();
        assert!(second_fetch.is_not_modified());

        let _ = tokio::fs::remove_file(path).await;
    }

    #[tokio::test]
    async fn fetch_config_if_modified_returns_data_after_file_changes() {
        let path = temp_config_path();
        write_config(&path, 30).await;

        let source = FileDataSource::new(path.clone()).unwrap();
        let first_fetch = source.fetch_config(None).await.unwrap();
        let fetched_at = match first_fetch {
            FetchResponse::Data(data) => data.fetched_at,
            FetchResponse::NotModified => panic!("initial fetch should return data"),
        };

        rewrite_config_after(&source, &path, 60, fetched_at).await;

        let changed_fetch = source
            .fetch_config_if_modified(Some(fetched_at))
            .await
            .unwrap();
        match changed_fetch {
            FetchResponse::Data(data) => {
                assert_eq!(data.data.default_configs.get("timeout"), Some(&60.into()));
                assert!(data.fetched_at > fetched_at);
            }
            FetchResponse::NotModified => panic!("changed file should return data"),
        }

        let _ = tokio::fs::remove_file(path).await;
    }
}
