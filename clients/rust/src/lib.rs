use std::{collections::HashMap, sync::Arc, time::Duration};

use open_feature::{
    async_trait,
    provider::{FeatureProvider, ProviderMetadata, ProviderStatus, ResolutionDetails},
    EvaluationContext, EvaluationResult, StructValue,
};
use serde_json::{Map, Value};
use superposition_rust_sdk::{
    self as superpositon_rs, operation::get_config::GetConfigOutput,
};
use superposition_types::{Condition, Config, Context, Overrides};
use tokio::{sync::RwLock, task::JoinHandle};

pub struct EvaluationCacheOptions {
    ttl: Option<i64>,
    size: Option<i64>,
}

pub struct PollingOptions {
    interval: u64,
    timeout: Option<i64>,
}

pub struct OnDemandOptions {
    ttl: u64,
    timeout: Option<i64>,
    use_stale_on_error: bool,
}

pub enum RefreshStrategy {
    Polling(PollingOptions),
    OnDemand(OnDemandOptions),
}

pub struct ExperimentationOptions {
    refresh_strategy: RefreshStrategy,
    evaluation_cache: Option<EvaluationCacheOptions>,
}

pub struct SuperpositionOptions {
    endpoint: String,
    token: String,
    org_id: String,
    workspace: String,

    fallback_config: serde_json::Map<String, serde_json::Value>,
    evaluation_cache_options: Option<EvaluationCacheOptions>,
    refresh_strategy: RefreshStrategy,

    experimentation_options: ExperimentationOptions,
}

pub struct SuperpositionClient {
    options: SuperpositionOptions,
    config: Arc<RwLock<Config>>,
    poll_thread: Option<JoinHandle<()>>,
}

#[derive(thiserror::Error, Debug)]
enum SuperpositionError {
    #[error("failed to initialize superposition client {0}")]
    InitFailed(String),
    #[error("failed to fetch config {0}")]
    FetchFailed(String),
}

type SuperpositionResult<T> = Result<T, SuperpositionError>;

impl SuperpositionClient {
    fn create_client(endpoint: &str) -> superpositon_rs::Client {
        let config = superposition_rust_sdk::Config::builder()
            .endpoint_url(endpoint)
            .build();
        superposition_rust_sdk::client::Client::from_conf(config)
    }
    async fn fetch_config(endpoint: &str, org_id: String, workspace: String) -> Config {
        let GetConfigOutput {
            overrides,
            contexts,
            default_configs,
            ..
        } = Self::create_client(endpoint)
            .get_config()
            .set_org_id(Some(org_id))
            .set_workspace_id(Some(workspace))
            .send()
            .await
            .unwrap();

        let sdc = default_configs
            .unwrap()
            .into_iter()
            .map(|(k, v)| serde_json::to_value(v).map(|v| (k, v)))
            .collect::<Result<Map<String, Value>, serde_json::Error>>()
            .unwrap();
        let so = overrides
            .unwrap()
            .into_iter()
            .map(|(k, v)| {
                serde_json::to_value(v)
                    .map(|v| v.as_object().cloned().unwrap())
                    .map(|v| (k, Overrides::validate_data(v).unwrap()))
            })
            .collect::<Result<HashMap<String, Overrides>, serde_json::Error>>()
            .unwrap();
        let sc = contexts
            .unwrap()
            .into_iter()
            .map(|c| {
                let so = c
                    .condition
                    .unwrap()
                    .into_iter()
                    .map(|(k, v)| serde_json::to_value(v).map(|v| (k, v)))
                    .collect::<Result<Map<String, Value>, serde_json::Error>>()
                    .unwrap();
                Context {
                    id: c.id.unwrap(),
                    condition: Condition::validate_data_for_cac(so).unwrap(),
                    priority: c.priority.unwrap(),
                    weight: c.weight.unwrap(),
                    override_with_keys: c.override_with_keys.unwrap(),
                }
            })
            .collect::<Vec<Context>>();
        Config {
            contexts: sc,
            default_configs: sdc,
            overrides: so,
        }
    }
    async fn new(options: SuperpositionOptions) -> SuperpositionClient {
        let fetch_result = Self::fetch_config(
            options.endpoint.as_str(),
            options.org_id.clone(),
            options.workspace.clone(),
        )
        .await;
        let config: Arc<RwLock<Config>> = Arc::new(RwLock::new(fetch_result));

        let poll_thread = if let RefreshStrategy::Polling(ref o) =
            options.refresh_strategy
        {
            let interval = o.interval;

            let endpoint = options.endpoint.clone();
            let org_id = options.org_id.clone();
            let workspace = options.workspace.clone();
            let config = config.clone();
            Some(tokio::spawn(async move {
                let mut interval = tokio::time::interval(Duration::from_secs(interval));
                loop {
                    interval.tick().await;
                    let result = Self::fetch_config(
                        endpoint.as_str(),
                        org_id.clone(),
                        workspace.clone(),
                    )
                    .await;
                    let mut writer = config.write().await;
                    writer.contexts = result.contexts;
                    writer.default_configs = result.default_configs;
                    writer.overrides = result.overrides;
                }
            }))
        } else {
            None
        };

        SuperpositionClient {
            options,
            config,
            poll_thread,
        }
    }
}

pub struct SuperpositionProvider {
    metadata: ProviderMetadata,
    status: ProviderStatus,
    client: SuperpositionClient,
}

impl SuperpositionProvider {
    pub async fn new(client_options: SuperpositionOptions) -> Self {
        let client = SuperpositionClient::new(client_options).await;
        SuperpositionProvider {
            metadata: ProviderMetadata {
                name: "superposition".to_string(),
            },
            client,
            status: ProviderStatus::Ready,
        }
    }
}

impl SuperpositionProvider {
    pub async fn resolve_value<T>(&self) {
        let config = self.client.config.read().await;
        let resolved_value = superposition_core::eval_config(
            config.default_configs,
            configs.contexts,
            configs.overrides,


    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
        );
    }
}

#[async_trait]
impl FeatureProvider for SuperpositionProvider {
    fn metadata(&self) -> &ProviderMetadata {
        &self.metadata
    }

    fn status(&self) -> ProviderStatus {
        match self.status {
            ProviderStatus::Ready => ProviderStatus::Ready,
            ProviderStatus::Error => ProviderStatus::Error,
            ProviderStatus::NotReady => ProviderStatus::NotReady,
            ProviderStatus::STALE => ProviderStatus::STALE,
        }
    }

    async fn resolve_bool_value(
        &self,
        _flag_key: &str,
        _evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<bool>> {
    }

    async fn resolve_int_value(
        &self,
        _flag_key: &str,
        _evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<i64>> {
        todo!("implement")
    }

    async fn resolve_float_value(
        &self,
        _flag_key: &str,
        _evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<f64>> {
        todo!("implement")
    }

    async fn resolve_string_value(
        &self,
        _flag_key: &str,
        _evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<String>> {
        todo!("implement")
    }

    async fn resolve_struct_value(
        &self,
        _flag_key: &str,
        _evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<StructValue>> {
        todo!("implement")
    }
}
