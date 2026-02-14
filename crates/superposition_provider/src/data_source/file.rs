use std::collections::HashSet;
use std::path::PathBuf;

use async_trait::async_trait;
use serde_json::{Map, Value};
use superposition_core::toml::parse_toml_config;

use super::{ConfigData, ExperimentData, SuperpositionDataSource};
use crate::types::{Result, SuperpositionError};

pub struct FileDataSource {
    file_path: PathBuf,
}

impl FileDataSource {
    pub fn new(file_path: PathBuf) -> Self {
        Self { file_path }
    }
}

#[async_trait]
impl SuperpositionDataSource for FileDataSource {
    async fn fetch_config(&self) -> Result<ConfigData> {
        let content =
            tokio::fs::read_to_string(&self.file_path)
                .await
                .map_err(|e| {
                    SuperpositionError::ConfigError(format!(
                        "Failed to read config file {:?}: {}",
                        self.file_path, e
                    ))
                })?;

        let config = parse_toml_config(&content).map_err(|e| {
            SuperpositionError::ConfigError(format!(
                "Failed to parse TOML config: {}",
                e
            ))
        })?;

        Ok(ConfigData::new(config))
    }

    async fn fetch_filtered_config(
        &self,
        context: Option<&Map<String, Value>>,
        prefix_filter: Option<&[String]>,
    ) -> Result<ConfigData> {
        let config_data = self.fetch_config().await?;
        let mut config = config_data.config;

        if let Some(ctx) = context {
            if !ctx.is_empty() {
                config = config.filter_by_dimensions(ctx);
            }
        }

        if let Some(prefixes) = prefix_filter {
            if !prefixes.is_empty() {
                let prefix_set: HashSet<String> =
                    HashSet::from_iter(prefixes.iter().cloned());
                config = config.filter_by_prefix(&prefix_set);
            }
        }

        Ok(ConfigData::new(config))
    }

    async fn fetch_active_experiments(&self) -> Result<Option<ExperimentData>> {
        Ok(None)
    }

    async fn fetch_candidate_active_experiments(
        &self,
        _context: Option<&Map<String, Value>>,
        _prefix_filter: Option<&[String]>,
    ) -> Result<Option<ExperimentData>> {
        Ok(None)
    }

    async fn fetch_matching_active_experiments(
        &self,
        _context: Option<&Map<String, Value>>,
        _prefix_filter: Option<&[String]>,
    ) -> Result<Option<ExperimentData>> {
        Ok(None)
    }

    fn supports_experiments(&self) -> bool {
        false
    }

    async fn close(&self) -> Result<()> {
        Ok(())
    }
}
