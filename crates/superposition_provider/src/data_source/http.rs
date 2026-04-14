use async_trait::async_trait;
use chrono::{DateTime, TimeZone, Utc};
use serde_json::{Map, Value};
use superposition_sdk::error::SdkError;
use superposition_sdk::types::DimensionMatchStrategy;
use superposition_sdk::{Client, Config as SdkConfig};

use crate::conversions;
use crate::types::{Result, SuperpositionError, SuperpositionOptions};
use crate::utils::ConversionUtils;

use super::{ConfigData, ExperimentData, FetchResponse, SuperpositionDataSource};

pub struct HttpDataSource {
    options: SuperpositionOptions,
    client: Client,
}

fn create_client(options: &SuperpositionOptions) -> Client {
    let sdk_config = SdkConfig::builder()
        .endpoint_url(&options.endpoint)
        .bearer_token(options.token.clone().into())
        .behavior_version_latest()
        .build();

    Client::from_conf(sdk_config)
}

impl HttpDataSource {
    pub fn new(options: SuperpositionOptions) -> Self {
        Self {
            client: create_client(&options),
            options,
        }
    }

    async fn fetch_experiments_with_filters(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        if_modified_since: Option<DateTime<Utc>>,
        filter: Option<DimensionMatchStrategy>,
    ) -> Result<FetchResponse<ExperimentData>> {
        let mut experiment_builder = self
            .client
            .get_experiment_config()
            .workspace_id(&self.options.workspace_id)
            .org_id(&self.options.org_id);

        if let Some(modified_since) = if_modified_since
            .and_then(|t| t.timestamp_nanos_opt())
            .and_then(|t| aws_smithy_types::DateTime::from_nanos(t.into()).ok())
        {
            experiment_builder = experiment_builder.if_modified_since(modified_since);
        }

        if let Some(context) = context {
            if !context.is_empty() {
                let context = conversions::map_to_hashmap(context);
                experiment_builder = experiment_builder.set_context(Some(context));
            }
        }

        if let Some(prefixes) = prefix_filter {
            if !prefixes.is_empty() {
                experiment_builder = experiment_builder.set_prefix(Some(prefixes));
            }
        }

        if let Some(filter) = filter {
            experiment_builder = experiment_builder.dimension_match_strategy(filter);
        }

        log::info!("Fetching experiments from Superposition service using SDK");
        let experiments_result = experiment_builder.send().await;

        let experiments_response = match experiments_result {
            Ok(res) => {
                let modified_at =
                    Utc.timestamp_nanos(res.last_modified.as_nanos() as i64);
                ConversionUtils::convert_experiment_config_response(res)
                    .map(|d| ExperimentData {
                        data: d,
                        fetched_at: modified_at,
                    })
                    .map(FetchResponse::Data)
            }
            Err(SdkError::ResponseError(r)) if r.raw().status().as_u16() == 304 => {
                Ok(FetchResponse::NotModified)
            }
            Err(e) => Err(SuperpositionError::NetworkError(format!(
                "Failed to list experiments: {}",
                e
            ))),
        }?;

        log::info!("Successfully fetched {}", experiments_response);

        Ok(experiments_response)
    }
}

#[async_trait]
impl SuperpositionDataSource for HttpDataSource {
    async fn fetch_filtered_config(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ConfigData>> {
        log::info!("Fetching config from Superposition service using SDK");
        let mut builder = self
            .client
            .get_config()
            .workspace_id(&self.options.workspace_id)
            .org_id(&self.options.org_id);

        if let Some(modified_since) = if_modified_since
            .and_then(|t| t.timestamp_nanos_opt())
            .and_then(|t| aws_smithy_types::DateTime::from_nanos(t.into()).ok())
        {
            builder = builder.if_modified_since(modified_since);
        }

        if let Some(context) = context {
            if !context.is_empty() {
                let context = conversions::map_to_hashmap(context);
                builder = builder.set_context(Some(context));
            }
        }

        if let Some(prefixes) = prefix_filter {
            if !prefixes.is_empty() {
                builder = builder.set_prefix(Some(prefixes));
            }
        }

        let config_result = builder.send().await;

        let resp = match config_result {
            Ok(res) => {
                let modified_at =
                    Utc.timestamp_nanos(res.last_modified.as_nanos() as i64);
                ConversionUtils::convert_get_config_response(res)
                    .map(|d| ConfigData {
                        data: d,
                        fetched_at: modified_at,
                    })
                    .map(FetchResponse::Data)
            }
            Err(SdkError::ResponseError(r)) if r.raw().status().as_u16() == 304 => {
                Ok(FetchResponse::NotModified)
            }
            Err(e) => Err(SuperpositionError::NetworkError(format!(
                "Failed to fetch config: {}",
                e
            ))),
        };

        resp
    }

    async fn fetch_active_experiments(
        &self,
        if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>> {
        self.fetch_experiments_with_filters(None, None, if_modified_since, None)
            .await
    }

    async fn fetch_candidate_active_experiments(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>> {
        self.fetch_experiments_with_filters(
            context,
            prefix_filter,
            if_modified_since,
            Some(DimensionMatchStrategy::Exact),
        )
        .await
    }

    async fn fetch_matching_active_experiments(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>> {
        self.fetch_experiments_with_filters(
            context,
            prefix_filter,
            if_modified_since,
            Some(DimensionMatchStrategy::Subset),
        )
        .await
    }

    fn supports_experiments(&self) -> bool {
        true
    }

    async fn close(&self) -> Result<()> {
        Ok(())
    }
}
