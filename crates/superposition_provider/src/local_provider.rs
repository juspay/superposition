use std::collections::HashMap;
use std::sync::Arc;

use async_trait::async_trait;
use log::{debug, error, info, warn};
use open_feature::{
    provider::FeatureProvider,
    provider::{ProviderMetadata, ProviderStatus, ResolutionDetails},
    EvaluationContext, EvaluationError, EvaluationErrorCode, EvaluationResult, StructValue,
};
use serde_json::{Map, Value};
use superposition_core::{eval_config, get_applicable_variants, MergeStrategy};
use superposition_types::DimensionInfo;
use tokio::sync::RwLock;
use tokio::task::JoinHandle;
use tokio::time::{sleep, Duration};

use crate::data_source::{ConfigData, ExperimentData, SuperpositionDataSource};
use crate::traits::{AllFeatureProvider, FeatureExperimentMeta};
use crate::types::*;
use crate::utils::ConversionUtils;

pub struct LocalResolutionProvider {
    primary: Arc<dyn SuperpositionDataSource>,
    fallback: Option<Arc<dyn SuperpositionDataSource>>,
    refresh_strategy: RefreshStrategy,
    cached_config: Arc<RwLock<Option<ConfigData>>>,
    cached_experiments: Arc<RwLock<Option<ExperimentData>>>,
    polling_task: RwLock<Option<JoinHandle<()>>>,
    metadata: ProviderMetadata,
    status: RwLock<ProviderStatus>,
}

impl LocalResolutionProvider {
    pub fn new(
        primary: Box<dyn SuperpositionDataSource>,
        fallback: Option<Box<dyn SuperpositionDataSource>>,
        refresh_strategy: RefreshStrategy,
    ) -> Self {
        Self {
            primary: Arc::from(primary),
            fallback: fallback.map(Arc::from),
            refresh_strategy,
            cached_config: Arc::new(RwLock::new(None)),
            cached_experiments: Arc::new(RwLock::new(None)),
            polling_task: RwLock::new(None),
            metadata: ProviderMetadata {
                name: "LocalResolutionProvider".to_string(),
            },
            status: RwLock::new(ProviderStatus::NotReady),
        }
    }

    pub async fn init(&self) -> Result<()> {
        // Fetch initial config from primary, fall back if needed
        let config_data = match self.primary.fetch_config().await {
            Ok(data) => {
                info!("LocalResolutionProvider: fetched config from primary source");
                data
            }
            Err(e) => {
                warn!(
                    "LocalResolutionProvider: primary config fetch failed: {}",
                    e
                );
                if let Some(fallback) = &self.fallback {
                    fallback.fetch_config().await.map_err(|fb_err| {
                        error!(
                            "LocalResolutionProvider: fallback config fetch also failed: {}",
                            fb_err
                        );
                        SuperpositionError::ConfigError(format!(
                            "Both primary and fallback config fetch failed. Primary: {}. Fallback: {}",
                            e, fb_err
                        ))
                    })?
                } else {
                    return Err(SuperpositionError::ConfigError(format!(
                        "Primary config fetch failed and no fallback configured: {}",
                        e
                    )));
                }
            }
        };

        {
            let mut cached = self.cached_config.write().await;
            *cached = Some(config_data);
        }

        // Fetch experiments best-effort: try primary, optionally fallback, but don't fail
        let exp_data = match self.primary.fetch_active_experiments().await {
            Ok(data) => data,
            Err(e) => {
                warn!(
                    "LocalResolutionProvider: primary experiment fetch failed (best-effort): {}",
                    e
                );
                if let Some(fallback) = &self.fallback {
                    match fallback.fetch_active_experiments().await {
                        Ok(data) => data,
                        Err(fb_err) => {
                            warn!(
                                "LocalResolutionProvider: fallback experiment fetch also failed (best-effort): {}",
                                fb_err
                            );
                            None
                        }
                    }
                } else {
                    None
                }
            }
        };

        if let Some(data) = exp_data {
            let mut cached = self.cached_experiments.write().await;
            *cached = Some(data);
            info!("LocalResolutionProvider: experiments cached");
        }

        // Start refresh strategy
        match &self.refresh_strategy {
            RefreshStrategy::Polling(polling_strategy) => {
                info!(
                    "LocalResolutionProvider: starting polling with interval={}s",
                    polling_strategy.interval
                );
                let task = self.start_polling(polling_strategy.interval).await;
                let mut polling_task = self.polling_task.write().await;
                *polling_task = Some(task);
            }
            RefreshStrategy::OnDemand(on_demand_strategy) => {
                info!(
                    "LocalResolutionProvider: using OnDemand strategy with ttl={}s",
                    on_demand_strategy.ttl
                );
            }
            RefreshStrategy::Watch(watch_strategy) => {
                let debounce_ms = watch_strategy.debounce_ms.unwrap_or(500);
                match self.primary.watch() {
                    Ok(Some(stream)) => {
                        info!(
                            "LocalResolutionProvider: starting watch with debounce={}ms",
                            debounce_ms
                        );
                        let task = self.start_watching(stream, debounce_ms).await;
                        let mut polling_task = self.polling_task.write().await;
                        *polling_task = Some(task);
                    }
                    Ok(None) => {
                        warn!("Watch strategy selected but data source does not support watching");
                    }
                    Err(e) => {
                        warn!("Failed to start watch: {}", e);
                    }
                }
            }
            RefreshStrategy::Manual => {
                info!("LocalResolutionProvider: using Manual refresh strategy");
            }
        }

        {
            let mut status = self.status.write().await;
            *status = ProviderStatus::Ready;
        }

        Ok(())
    }

    pub async fn refresh(&self) -> Result<()> {
        self.do_refresh().await
    }

    pub async fn close(&self) -> Result<()> {
        // Abort polling task
        {
            let mut polling_task = self.polling_task.write().await;
            if let Some(task) = polling_task.take() {
                task.abort();
            }
        }

        // Close data sources
        if let Err(e) = self.primary.close().await {
            warn!("LocalResolutionProvider: error closing primary source: {}", e);
        }
        if let Some(fallback) = &self.fallback {
            if let Err(e) = fallback.close().await {
                warn!(
                    "LocalResolutionProvider: error closing fallback source: {}",
                    e
                );
            }
        }

        // Clear caches
        {
            let mut cached = self.cached_config.write().await;
            *cached = None;
        }
        {
            let mut cached = self.cached_experiments.write().await;
            *cached = None;
        }

        // Set status to NotReady
        {
            let mut status = self.status.write().await;
            *status = ProviderStatus::NotReady;
        }

        Ok(())
    }

    async fn do_refresh(&self) -> Result<()> {
        // Fetch config from primary; keep last known good on failure
        let config_result = self.primary.fetch_config().await;
        match &config_result {
            Ok(data) => {
                let mut cached = self.cached_config.write().await;
                *cached = Some(data.clone());
                debug!("LocalResolutionProvider: config refreshed from primary");
            }
            Err(e) => {
                warn!(
                    "LocalResolutionProvider: config refresh failed, keeping last known good: {}",
                    e
                );
            }
        }

        // Experiments refresh is best-effort, don't propagate errors
        if self.primary.supports_experiments() {
            match self.primary.fetch_active_experiments().await {
                Ok(Some(data)) => {
                    let mut cached = self.cached_experiments.write().await;
                    *cached = Some(data);
                    debug!("LocalResolutionProvider: experiments refreshed from primary");
                }
                Ok(None) => {
                    debug!("LocalResolutionProvider: no experiments returned from primary");
                }
                Err(e) => {
                    warn!(
                        "LocalResolutionProvider: experiment refresh failed, keeping last known good: {}",
                        e
                    );
                }
            }
        }

        config_result.map(|_| ())
    }

    async fn start_polling(&self, interval: u64) -> JoinHandle<()> {
        let primary = self.primary.clone();
        let cached_config = self.cached_config.clone();
        let cached_experiments = self.cached_experiments.clone();

        tokio::spawn(async move {
            loop {
                sleep(Duration::from_secs(interval)).await;

                // Refresh config
                match primary.fetch_config().await {
                    Ok(data) => {
                        let mut cached = cached_config.write().await;
                        *cached = Some(data);
                        debug!("LocalResolutionProvider: config updated via polling");
                    }
                    Err(e) => {
                        error!("LocalResolutionProvider: polling config error: {}", e);
                    }
                }

                // Refresh experiments
                match primary.fetch_active_experiments().await {
                    Ok(Some(data)) => {
                        let mut cached = cached_experiments.write().await;
                        *cached = Some(data);
                        debug!("LocalResolutionProvider: experiments updated via polling");
                    }
                    Ok(None) => {}
                    Err(e) => {
                        error!(
                            "LocalResolutionProvider: polling experiments error: {}",
                            e
                        );
                    }
                }
            }
        })
    }

    async fn start_watching(
        &self,
        mut watch_stream: crate::types::WatchStream,
        debounce_ms: u64,
    ) -> JoinHandle<()> {
        let primary = self.primary.clone();
        let cached_config = self.cached_config.clone();
        let cached_experiments = self.cached_experiments.clone();

        tokio::spawn(async move {
            loop {
                match watch_stream.receiver.recv().await {
                    Some(()) => {
                        // Debounce: wait, then drain any queued events
                        sleep(Duration::from_millis(debounce_ms)).await;
                        while watch_stream.receiver.try_recv().is_ok() {}

                        // Refresh config
                        match primary.fetch_config().await {
                            Ok(data) => {
                                let mut cached = cached_config.write().await;
                                *cached = Some(data);
                                debug!("LocalResolutionProvider: config updated via watch");
                            }
                            Err(e) => {
                                error!(
                                    "LocalResolutionProvider: watch config refresh error: {}",
                                    e
                                );
                            }
                        }

                        // Refresh experiments
                        match primary.fetch_active_experiments().await {
                            Ok(Some(data)) => {
                                let mut cached = cached_experiments.write().await;
                                *cached = Some(data);
                                debug!(
                                    "LocalResolutionProvider: experiments updated via watch"
                                );
                            }
                            Ok(None) => {}
                            Err(e) => {
                                error!(
                                    "LocalResolutionProvider: watch experiments refresh error: {}",
                                    e
                                );
                            }
                        }
                    }
                    None => {
                        info!("LocalResolutionProvider: watch channel closed, stopping");
                        break;
                    }
                }
            }
        })
    }

    async fn ensure_fresh_data(&self) -> Result<()> {
        if let RefreshStrategy::OnDemand(on_demand) = &self.refresh_strategy {
            let ttl = on_demand.ttl;
            let use_stale_on_error = on_demand.use_stale_on_error.unwrap_or(false);

            let should_refresh = {
                let cached = self.cached_config.read().await;
                match cached.as_ref() {
                    Some(data) => {
                        let elapsed =
                            (chrono::Utc::now() - data.fetched_at).num_seconds();
                        elapsed > ttl as i64
                    }
                    None => true,
                }
            };

            if should_refresh {
                debug!("LocalResolutionProvider: TTL expired, refreshing on-demand");
                if let Err(e) = self.do_refresh().await {
                    if !use_stale_on_error {
                        return Err(e);
                    }
                    warn!(
                        "LocalResolutionProvider: on-demand refresh failed, using stale data: {}",
                        e
                    );
                }
            }
        }
        Ok(())
    }

    async fn get_dimensions_info(&self) -> HashMap<String, DimensionInfo> {
        let cached = self.cached_config.read().await;
        match cached.as_ref() {
            Some(data) => data.config.dimensions.clone(),
            None => HashMap::new(),
        }
    }

    async fn eval_with_context(
        &self,
        context: &EvaluationContext,
        prefix_filter: Option<&[String]>,
    ) -> Result<Map<String, Value>> {
        self.ensure_fresh_data().await?;

        let (mut query_data, targeting_key) =
            ConversionUtils::evaluation_context_to_query(context);

        let dimensions_info = self.get_dimensions_info().await;

        // If experiments are cached, get applicable variants and inject variantIds
        {
            let cached_exp = self.cached_experiments.read().await;
            if let Some(exp_data) = cached_exp.as_ref() {
                match get_applicable_variants(
                    &dimensions_info,
                    exp_data.experiments.clone(),
                    &exp_data.experiment_groups,
                    &query_data,
                    &targeting_key.clone().unwrap_or_default(),
                    None,
                ) {
                    Ok(variant_ids) => {
                        query_data.insert(
                            "variantIds".to_string(),
                            Value::Array(
                                variant_ids.into_iter().map(Value::String).collect(),
                            ),
                        );
                    }
                    Err(e) => {
                        warn!(
                            "LocalResolutionProvider: failed to get applicable variants: {}",
                            e
                        );
                    }
                }
            }
        }

        // Evaluate config using cached data
        let cached = self.cached_config.read().await;
        match cached.as_ref() {
            Some(config_data) => eval_config(
                (*config_data.config.default_configs).clone(),
                &config_data.config.contexts,
                &config_data.config.overrides,
                &config_data.config.dimensions,
                &query_data,
                MergeStrategy::MERGE,
                prefix_filter.map(|p| p.to_vec()),
            )
            .map_err(|e| {
                SuperpositionError::ConfigError(format!(
                    "Failed to evaluate config: {}",
                    e
                ))
            }),
            None => Err(SuperpositionError::ConfigError(
                "No cached config available".into(),
            )),
        }
    }
}

#[async_trait]
impl AllFeatureProvider for LocalResolutionProvider {
    async fn resolve_all_features(
        &self,
        context: &EvaluationContext,
    ) -> Result<Map<String, Value>> {
        self.eval_with_context(context, None).await
    }

    async fn resolve_all_features_with_filter(
        &self,
        context: &EvaluationContext,
        prefix_filter: Option<&[String]>,
    ) -> Result<Map<String, Value>> {
        self.eval_with_context(context, prefix_filter).await
    }
}

#[async_trait]
impl FeatureExperimentMeta for LocalResolutionProvider {
    async fn get_applicable_variants(
        &self,
        context: &EvaluationContext,
    ) -> Result<Vec<String>> {
        self.ensure_fresh_data().await?;

        let (query_data, targeting_key) =
            ConversionUtils::evaluation_context_to_query(context);
        let dimensions_info = self.get_dimensions_info().await;

        let cached_exp = self.cached_experiments.read().await;
        match cached_exp.as_ref() {
            Some(exp_data) => {
                get_applicable_variants(
                    &dimensions_info,
                    exp_data.experiments.clone(),
                    &exp_data.experiment_groups,
                    &query_data,
                    &targeting_key.unwrap_or_default(),
                    None,
                )
                .map_err(|e| {
                    SuperpositionError::ConfigError(format!(
                        "Failed to get applicable variants: {}",
                        e
                    ))
                })
            }
            None => Ok(vec![]),
        }
    }
}

#[async_trait]
impl FeatureProvider for LocalResolutionProvider {
    async fn initialize(&mut self, _context: &EvaluationContext) {
        info!("Initializing LocalResolutionProvider...");
        {
            let mut status = self.status.write().await;
            *status = ProviderStatus::NotReady;
        }
        if (self.init().await).is_err() {
            let mut status = self.status.write().await;
            *status = ProviderStatus::Error;
            return;
        }

        let mut status = self.status.write().await;
        *status = ProviderStatus::Ready;

        info!("LocalResolutionProvider initialized successfully");
    }

    async fn resolve_bool_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<bool>> {
        match self.resolve_all_features(evaluation_context).await {
            Ok(config) => match config.get(flag_key) {
                Some(value) => match value.as_bool() {
                    Some(bool_val) => Ok(ResolutionDetails::new(bool_val)),
                    None => Err(EvaluationError {
                        code: EvaluationErrorCode::TypeMismatch,
                        message: Some(format!("Flag '{}' is not a boolean", flag_key)),
                    }),
                },
                None => Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some(format!("Flag '{}' not found", flag_key)),
                }),
            },
            Err(e) => {
                error!("Error evaluating boolean flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::General(format!(
                        "Error evaluating flag '{}': {}",
                        flag_key, e
                    )),
                    message: Some(format!("Error evaluating flag '{}': {}", flag_key, e)),
                })
            }
        }
    }

    async fn resolve_string_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<String>> {
        match self.resolve_all_features(evaluation_context).await {
            Ok(config) => match config.get(flag_key) {
                Some(value) => match value.as_str() {
                    Some(str_val) => Ok(ResolutionDetails::new(str_val.to_owned())),
                    None => Err(EvaluationError {
                        code: EvaluationErrorCode::TypeMismatch,
                        message: Some(format!("Flag '{}' is not a string", flag_key)),
                    }),
                },
                None => Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some(format!("Flag '{}' not found", flag_key)),
                }),
            },
            Err(e) => {
                error!("Error evaluating String flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::General(format!(
                        "Error evaluating flag '{}': {}",
                        flag_key, e
                    )),
                    message: Some(format!("Error evaluating flag '{}': {}", flag_key, e)),
                })
            }
        }
    }

    async fn resolve_int_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<i64>> {
        match self.resolve_all_features(evaluation_context).await {
            Ok(config) => match config.get(flag_key) {
                Some(value) => match value.as_i64() {
                    Some(int_val) => Ok(ResolutionDetails::new(int_val)),
                    None => Err(EvaluationError {
                        code: EvaluationErrorCode::TypeMismatch,
                        message: Some(format!("Flag '{}' is not an integer", flag_key)),
                    }),
                },
                None => Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some(format!("Flag '{}' not found", flag_key)),
                }),
            },
            Err(e) => {
                error!("Error evaluating integer flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::General(format!(
                        "Error evaluating flag '{}': {}",
                        flag_key, e
                    )),
                    message: Some(format!("Error evaluating flag '{}': {}", flag_key, e)),
                })
            }
        }
    }

    async fn resolve_float_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<f64>> {
        match self.resolve_all_features(evaluation_context).await {
            Ok(config) => match config.get(flag_key) {
                Some(value) => match value.as_f64() {
                    Some(float_val) => Ok(ResolutionDetails::new(float_val)),
                    None => Err(EvaluationError {
                        code: EvaluationErrorCode::TypeMismatch,
                        message: Some(format!("Flag '{}' is not a float", flag_key)),
                    }),
                },
                None => Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some(format!("Flag '{}' not found", flag_key)),
                }),
            },
            Err(e) => {
                error!("Error evaluating float flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::General(format!(
                        "Error evaluating flag '{}': {}",
                        flag_key, e
                    )),
                    message: Some(format!("Error evaluating flag '{}': {}", flag_key, e)),
                })
            }
        }
    }

    async fn resolve_struct_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<StructValue>> {
        match self.resolve_all_features(evaluation_context).await {
            Ok(config) => match config.get(flag_key) {
                Some(value) => match ConversionUtils::serde_value_to_struct_value(value) {
                    Ok(struct_value) => Ok(ResolutionDetails::new(struct_value)),
                    Err(e) => {
                        error!("Error converting value to StructValue: {}", e);
                        Err(EvaluationError {
                            code: EvaluationErrorCode::TypeMismatch,
                            message: Some(format!(
                                "Flag '{}' is not a struct: {}",
                                flag_key, e
                            )),
                        })
                    }
                },
                None => Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some(format!("Flag '{}' not found", flag_key)),
                }),
            },
            Err(e) => {
                error!("Error evaluating Object flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::General(format!(
                        "Error evaluating flag '{}': {}",
                        flag_key, e
                    )),
                    message: Some(format!("Error evaluating flag '{}': {}", flag_key, e)),
                })
            }
        }
    }

    fn metadata(&self) -> &ProviderMetadata {
        &self.metadata
    }

    fn status(&self) -> ProviderStatus {
        match self.status.try_read() {
            Ok(status) => match *status {
                ProviderStatus::Ready => ProviderStatus::Ready,
                ProviderStatus::Error => ProviderStatus::Error,
                ProviderStatus::NotReady => ProviderStatus::NotReady,
                ProviderStatus::STALE => ProviderStatus::STALE,
            },
            Err(_) => ProviderStatus::NotReady,
        }
    }
}
