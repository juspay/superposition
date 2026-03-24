use async_trait::async_trait;
use chrono::{DateTime, Utc};
use log::info;
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

    async fn fetch_experiments_with_filters(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        last_fetched_at: Option<DateTime<Utc>>,
        filter: Option<bool>,
    ) -> Result<FetchResponse<ExperimentData>> {
        let client = self.create_client();

        let mut experiment_builder = client
            .get_experiment_config()
            .workspace_id(&self.options.workspace_id)
            .org_id(&self.options.org_id);

        if let Some(fetched_at) = last_fetched_at
            .and_then(|t| t.timestamp_nanos_opt())
            .and_then(|t| aws_smithy_types::DateTime::from_nanos(t.into()).ok())
        {
            experiment_builder = experiment_builder.if_modified_since(fetched_at);
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
            if filter {
                experiment_builder = experiment_builder
                    .dimension_match_strategy(DimensionMatchStrategy::Subset);
            } else {
                experiment_builder = experiment_builder
                    .dimension_match_strategy(DimensionMatchStrategy::Exact);
            }
        }

        info!("Fetching experiments from Superposition service using SDK");
        let experiments_result = experiment_builder.send().await;

        let experiments_response = match experiments_result {
            Ok(res) => ConversionUtils::convert_experiment_config_response(res)
                .map(ExperimentData::new)
                .map(FetchResponse::Data),
            Err(SdkError::ResponseError(r)) if r.raw().status().as_u16() == 304 => {
                Ok(FetchResponse::NotModified)
            }
            Err(e) => Err(SuperpositionError::NetworkError(format!(
                "Failed to list experiments: {}",
                e
            ))),
        }?;

        info!("Successfully fetched {}", experiments_response);

        Ok(experiments_response)
    }

    async fn fetch_config_with_filters(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        last_fetched_at: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ConfigData>> {
        let client = self.create_client();

        info!("Fetching config from Superposition service using SDK");
        let mut builder = client
            .get_config()
            .workspace_id(&self.options.workspace_id)
            .org_id(&self.options.org_id);

        if let Some(fetched_at) = last_fetched_at
            .and_then(|t| t.timestamp_nanos_opt())
            .and_then(|t| aws_smithy_types::DateTime::from_nanos(t.into()).ok())
        {
            builder = builder.if_modified_since(fetched_at);
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
            Ok(res) => ConversionUtils::convert_get_config_response(res)
                .map(ConfigData::new)
                .map(FetchResponse::Data),
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
}

#[async_trait]
impl SuperpositionDataSource for HttpDataSource {
    async fn fetch_config(
        &self,
        last_fetched_at: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ConfigData>> {
        self.fetch_config_with_filters(None, None, last_fetched_at)
            .await
    }

    async fn fetch_filtered_config(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        last_fetched_at: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ConfigData>> {
        self.fetch_config_with_filters(context, prefix_filter, last_fetched_at)
            .await
    }

    async fn fetch_active_experiments(
        &self,
        last_fetched_at: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>> {
        self.fetch_experiments_with_filters(None, None, last_fetched_at, None)
            .await
    }

    async fn fetch_candidate_active_experiments(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        last_fetched_at: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>> {
        self.fetch_experiments_with_filters(
            context,
            prefix_filter,
            last_fetched_at,
            Some(false),
        )
        .await
    }

    async fn fetch_matching_active_experiments(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        last_fetched_at: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>> {
        self.fetch_experiments_with_filters(
            context,
            prefix_filter,
            last_fetched_at,
            Some(true),
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
