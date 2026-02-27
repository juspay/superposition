use std::collections::HashSet;

use async_trait::async_trait;
use log::info;
use serde_json::{Map, Value};
use superposition_sdk::types::ExperimentStatusType;
use superposition_sdk::{Client, Config as SdkConfig};
use superposition_types::logic::{apply, partial_apply};

use super::{ConfigData, ExperimentData, SuperpositionDataSource};
use crate::types::{Result, SuperpositionError, SuperpositionOptions};
use crate::utils::ConversionUtils;

pub struct HttpDataSource {
    options: SuperpositionOptions,
}

impl HttpDataSource {
    pub fn new(options: SuperpositionOptions) -> Self {
        Self { options }
    }

    fn create_client(&self) -> Client {
        let sdk_config = SdkConfig::builder()
            .endpoint_url(&self.options.endpoint)
            .bearer_token(self.options.token.clone().into())
            .behavior_version_latest()
            .build();

        Client::from_conf(sdk_config)
    }

    async fn fetch_experiments_and_groups(&self) -> Result<ExperimentData> {
        let client = self.create_client();

        let (experiments_result, groups_result) = tokio::join!(
            async {
                client
                    .list_experiment()
                    .workspace_id(&self.options.workspace_id)
                    .org_id(&self.options.org_id)
                    .all(true)
                    .status(ExperimentStatusType::Created)
                    .status(ExperimentStatusType::Inprogress)
                    .send()
                    .await
                    .map_err(|e| {
                        SuperpositionError::NetworkError(format!(
                            "Failed to list experiments: {}",
                            e
                        ))
                    })
            },
            async {
                client
                    .list_experiment_groups()
                    .workspace_id(&self.options.workspace_id)
                    .org_id(&self.options.org_id)
                    .all(true)
                    .send()
                    .await
                    .map_err(|e| {
                        SuperpositionError::NetworkError(format!(
                            "Failed to list experiment groups: {}",
                            e
                        ))
                    })
            }
        );

        let experiments_response = experiments_result?;
        let groups_response = groups_result?;

        let experiments =
            ConversionUtils::convert_experiments_response(&experiments_response)?;
        let experiment_groups =
            ConversionUtils::convert_experiment_groups_response(&groups_response)?;

        info!(
            "Successfully fetched {} experiments and {} experiment groups",
            experiments.len(),
            experiment_groups.len()
        );

        Ok(ExperimentData::new(experiments, experiment_groups))
    }

}

#[async_trait]
impl SuperpositionDataSource for HttpDataSource {
    async fn fetch_config(&self) -> Result<ConfigData> {
        let client = self.create_client();

        info!("Fetching config from Superposition service using SDK");

        let response = client
            .get_config()
            .workspace_id(&self.options.workspace_id)
            .org_id(&self.options.org_id)
            .send()
            .await
            .map_err(|e| {
                SuperpositionError::NetworkError(format!(
                    "Failed to get config: {}",
                    e
                ))
            })?;

        let config = ConversionUtils::convert_get_config_response(&response)?;

        info!(
            "Successfully fetched config with {} contexts, {} overrides, {} default configs",
            config.contexts.len(),
            config.overrides.len(),
            config.default_configs.len()
        );

        Ok(ConfigData::new(config))
    }

    async fn fetch_filtered_config(
        &self,
        context: Option<&Map<String, Value>>,
        prefix_filter: Option<&[String]>,
    ) -> Result<ConfigData> {
        let config_data = self.fetch_config().await?;
        let mut config = config_data.config;

        // Filter by dimensions if context is provided and non-empty
        if let Some(ctx) = context {
            if !ctx.is_empty() {
                config = config.filter_by_dimensions(ctx);
            }
        }

        // Filter by prefix if prefix_filter is provided and non-empty
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
        let data = self.fetch_experiments_and_groups().await?;
        Ok(Some(data))
    }

    async fn fetch_candidate_active_experiments(
        &self,
        context: Option<&Map<String, Value>>,
        prefix_filter: Option<&[String]>,
    ) -> Result<Option<ExperimentData>> {
        let data = self.fetch_experiments_and_groups().await?;
        let filtered = data.filter(context, prefix_filter, partial_apply);
        Ok(Some(filtered))
    }

    async fn fetch_matching_active_experiments(
        &self,
        context: Option<&Map<String, Value>>,
        prefix_filter: Option<&[String]>,
    ) -> Result<Option<ExperimentData>> {
        let data = self.fetch_experiments_and_groups().await?;
        let filtered = data.filter(context, prefix_filter, apply);
        Ok(Some(filtered))
    }

    fn supports_experiments(&self) -> bool {
        true
    }

    async fn close(&self) -> Result<()> {
        Ok(())
    }
}
