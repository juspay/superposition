use async_trait::async_trait;
use log::{debug, info};
use superposition_sdk::types::ExperimentStatusType;
use superposition_sdk::{Client, Config as SdkConfig};
use tokio::join;

use crate::data_source::{ConfigData, ExperimentData, SuperpositionDataSource};
use crate::types::{Result, SuperpositionError, SuperpositionOptions};
use crate::utils::ConversionUtils;

/// HTTP-based data source that fetches configuration from Superposition API
///
/// This data source uses the Superposition SDK to fetch configuration
/// and experiment data via HTTP requests to the Superposition service.
#[derive(Debug, Clone)]
pub struct HttpDataSource {
    options: SuperpositionOptions,
    client: Client,
}

impl HttpDataSource {
    /// Create a new HTTP data source
    pub fn new(options: SuperpositionOptions) -> Self {
        debug!("Creating HttpDataSource with endpoint: {}", options.endpoint);

        // Create SDK config
        let sdk_config = SdkConfig::builder()
            .endpoint_url(&options.endpoint)
            .bearer_token(options.token.clone().into())
            .behavior_version_latest()
            .build();

        // Create Superposition client
        let client = Client::from_conf(sdk_config);

        Self { options, client }
    }
}

#[async_trait]
impl SuperpositionDataSource for HttpDataSource {
    async fn fetch_config(&self) -> Result<ConfigData> {
        info!("Fetching config from HTTP endpoint: {}", self.options.endpoint);

        let response = self
            .client
            .get_config()
            .workspace_id(&self.options.workspace_id)
            .org_id(&self.options.org_id)
            .send()
            .await
            .map_err(|e| {
                SuperpositionError::NetworkError(format!("Failed to get config: {}", e))
            })?;

        let config = ConversionUtils::convert_get_config_response(&response)?;

        debug!(
            "Fetched config with {} contexts, {} overrides, {} default configs",
            config.contexts.len(),
            config.overrides.len(),
            config.default_configs.len()
        );

        Ok(ConfigData::new(config))
    }

    async fn fetch_experiments(&self) -> Result<Option<ExperimentData>> {
        info!("Fetching experiments from HTTP endpoint: {}", self.options.endpoint);

        // Fetch experiments and experiment groups in parallel
        let (experiments_result, groups_result) = join!(
            self.client
                .list_experiment()
                .workspace_id(&self.options.workspace_id)
                .org_id(&self.options.org_id)
                .all(true)
                .status(ExperimentStatusType::Created)
                .status(ExperimentStatusType::Inprogress)
                .send(),
            self.client
                .list_experiment_groups()
                .workspace_id(&self.options.workspace_id)
                .org_id(&self.options.org_id)
                .all(true)
                .send()
        );

        // Handle experiments response
        let experiments_output = experiments_result.map_err(|e| {
            SuperpositionError::NetworkError(format!("Failed to list experiments: {}", e))
        })?;

        // Handle experiment groups response
        let groups_output = groups_result.map_err(|e| {
            SuperpositionError::NetworkError(format!("Failed to list experiment groups: {}", e))
        })?;

        // Convert to internal types
        let experiments = ConversionUtils::convert_experiments_response(&experiments_output)?;
        let experiment_groups =
            ConversionUtils::convert_experiment_groups_response(&groups_output)?;

        debug!(
            "Fetched {} experiments and {} experiment groups",
            experiments.len(),
            experiment_groups.len()
        );

        Ok(Some(ExperimentData::new(experiments, experiment_groups)))
    }

    fn source_name(&self) -> &str {
        "HTTP"
    }

    fn supports_experiments(&self) -> bool {
        true
    }

    async fn close(&self) -> Result<()> {
        debug!("Closing HttpDataSource");
        // No cleanup needed for HTTP client
        Ok(())
    }
}
