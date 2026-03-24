use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use derive_more::{Deref, DerefMut};
use open_feature::provider::{
    FeatureProvider, ProviderMetadata, ProviderStatus, ResolutionDetails,
};
use open_feature::{EvaluationContext, EvaluationResult, StructValue};
use serde_json::{Map, Value};
use superposition_core::experiment::{filter_experiments_by_context, FfiExperimentGroup};
use superposition_core::{
    eval_config, get_applicable_variants, get_satisfied_experiments, MergeStrategy,
};
use superposition_types::experimental::Experimental;
use superposition_types::DimensionInfo;
use tokio::sync::RwLock;
use tokio::task::JoinHandle;
use tokio::time::{sleep, Duration};

use crate::data_source::{
    ConfigData, ExperimentData, FetchResponse, SuperpositionDataSource,
};
use crate::traits::{AllFeatureProvider, FeatureExperimentMeta};
use crate::{conversions, types::*};

pub struct LocalResolutionProviderInner {
    primary: Arc<dyn SuperpositionDataSource>,
    fallback: Option<Arc<dyn SuperpositionDataSource>>,
    refresh_strategy: RefreshStrategy,
    cached_config: RwLock<Option<ConfigData>>,
    cached_experiments: RwLock<Option<ExperimentData>>,
    polling_task: RwLock<Option<JoinHandle<()>>>,
    metadata: ProviderMetadata,
    status: RwLock<ProviderStatus>,
    global_context: RwLock<EvaluationContext>,
}

#[derive(Deref, DerefMut, Clone)]
pub struct LocalResolutionProvider(Arc<LocalResolutionProviderInner>);

impl LocalResolutionProvider {
    pub fn new(
        primary: Box<dyn SuperpositionDataSource>,
        fallback: Option<Box<dyn SuperpositionDataSource>>,
        refresh_strategy: RefreshStrategy,
    ) -> Self {
        Self(Arc::new(LocalResolutionProviderInner {
            primary: Arc::from(primary),
            fallback: fallback.map(Arc::from),
            refresh_strategy,
            cached_config: RwLock::new(None),
            cached_experiments: RwLock::new(None),
            polling_task: RwLock::new(None),
            metadata: ProviderMetadata {
                name: "LocalResolutionProvider".to_string(),
            },
            status: RwLock::new(ProviderStatus::NotReady),
            global_context: RwLock::new(EvaluationContext::default()),
        }))
    }

    pub async fn init(&self, context: EvaluationContext) -> Result<()> {
        // Fetch initial config from primary, fall back if needed
        let config_data = match self.primary.fetch_config(None).await {
            Ok(data) => {
                log::info!("LocalResolutionProvider: fetched config from primary source");
                data
            }
            Err(e) => {
                log::warn!(
                    "LocalResolutionProvider: primary config fetch failed: {}",
                    e
                );
                if let Some(fallback) = &self.fallback {
                    fallback.fetch_config(None).await.map_err(|fb_err| {
                        log::error!(
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
            *cached = config_data.into_data();
        }

        // Fetch experiments best-effort: try primary, optionally fallback, but don't fail
        let exp_data = match self.primary.fetch_active_experiments(None).await {
            Ok(exp_resp) => exp_resp.into_data(),
            Err(e) => {
                log::warn!(
                    "LocalResolutionProvider: primary experiment fetch failed (best-effort): {}",
                    e
                );
                if let Some(fallback) = &self.fallback {
                    match fallback.fetch_active_experiments(None).await {
                        Ok(exp_resp) => exp_resp.into_data(),
                        Err(fb_err) => {
                            log::warn!(
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
        }

        // Start refresh strategy
        match &self.refresh_strategy {
            RefreshStrategy::Polling(polling_strategy) => {
                log::info!(
                    "LocalResolutionProvider: starting polling with interval={}s",
                    polling_strategy.interval
                );
                let task = self.start_polling(polling_strategy.interval).await;
                let mut polling_task = self.polling_task.write().await;
                *polling_task = Some(task);
            }
            RefreshStrategy::OnDemand(on_demand_strategy) => {
                log::info!(
                    "LocalResolutionProvider: using OnDemand strategy with ttl={}s",
                    on_demand_strategy.ttl
                );
            }
            RefreshStrategy::Watch(watch_strategy) => {
                let debounce_ms = watch_strategy.debounce_ms.unwrap_or(500);
                match self.primary.watch() {
                    Ok(Some(stream)) => {
                        log::info!(
                            "LocalResolutionProvider: starting watch with debounce={}ms",
                            debounce_ms
                        );
                        let task = self.start_watching(stream, debounce_ms).await;
                        let mut polling_task = self.polling_task.write().await;
                        *polling_task = Some(task);
                    }
                    Ok(None) => {
                        log::warn!("Watch strategy selected but data source does not support watching");
                    }
                    Err(e) => {
                        log::warn!("Failed to start watch: {}", e);
                    }
                }
            }
            RefreshStrategy::Manual => {
                log::info!("LocalResolutionProvider: using Manual refresh strategy");
            }
        }

        {
            let mut status = self.status.write().await;
            *status = ProviderStatus::Ready;
        }

        {
            let mut global_context = self.global_context.write().await;
            *global_context = context;
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
            log::warn!(
                "LocalResolutionProvider: error closing primary source: {}",
                e
            );
        }
        if let Some(fallback) = &self.fallback {
            if let Err(e) = fallback.close().await {
                log::warn!(
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
        let last_fetched_at = {
            self.cached_config
                .read()
                .await
                .as_ref()
                .map(|data| data.fetched_at)
        };

        let config_result = self.primary.fetch_config(last_fetched_at).await;
        let mut resp = match config_result {
            Ok(FetchResponse::Data(data)) => {
                let mut cached = self.cached_config.write().await;
                *cached = Some(data);
                log::debug!("LocalResolutionProvider: config refreshed from primary");
                Ok(())
            }
            Ok(FetchResponse::NotModified) => {
                log::debug!("LocalResolutionProvider: config not modified");
                Ok(())
            }
            Err(e) => {
                log::warn!(
                    "LocalResolutionProvider: config refresh failed, keeping last known good: {}",
                    e
                );
                Err(e)
            }
        };

        // Experiments refresh is best-effort, don't propagate errors
        if self.primary.supports_experiments() {
            let exp_last_fetched_at = {
                self.cached_experiments
                    .read()
                    .await
                    .as_ref()
                    .map(|d| d.fetched_at)
            };
            match self
                .primary
                .fetch_active_experiments(exp_last_fetched_at)
                .await
            {
                Ok(exp_resp) => {
                    let mut cached = self.cached_experiments.write().await;
                    if let Some(data) = exp_resp.into_data() {
                        *cached = Some(data);
                    }
                    log::debug!(
                        "LocalResolutionProvider: experiments refreshed from primary"
                    );
                }
                Err(e) => {
                    log::warn!(
                        "LocalResolutionProvider: experiment refresh failed, keeping last known good: {}",
                        e
                    );
                    if resp.is_ok() {
                        resp = Err(e);
                    }
                }
            }
        }

        resp
    }

    async fn start_polling(&self, interval: u64) -> JoinHandle<()> {
        let provider_clone = self.clone();
        tokio::spawn(async move {
            loop {
                sleep(Duration::from_secs(interval)).await;
                let _ = provider_clone.do_refresh().await;
            }
        })
    }

    async fn start_watching(
        &self,
        mut watch_stream: crate::types::WatchStream,
        debounce_ms: u64,
    ) -> JoinHandle<()> {
        let provider_clone = self.clone();

        tokio::spawn(async move {
            loop {
                match watch_stream.receiver.recv().await {
                    Ok(()) => {
                        // Debounce: wait, then drain any queued events
                        sleep(Duration::from_millis(debounce_ms)).await;
                        while watch_stream.receiver.try_recv().is_ok() {}

                        let last_fetched_at = {
                            provider_clone
                                .cached_config
                                .read()
                                .await
                                .as_ref()
                                .map(|data| data.fetched_at)
                        };

                        // Refresh config
                        match provider_clone.primary.fetch_config(last_fetched_at).await {
                            Ok(FetchResponse::Data(data)) => {
                                let mut cached =
                                    provider_clone.cached_config.write().await;
                                *cached = Some(data);
                                log::debug!(
                                    "LocalResolutionProvider: config updated via watch"
                                );
                            }
                            Ok(FetchResponse::NotModified) => {
                                log::debug!(
                                    "LocalResolutionProvider: config not modified on watch refresh"
                                );
                            }
                            Err(e) => {
                                log::error!(
                                    "LocalResolutionProvider: watch config refresh error: {}",
                                    e
                                );
                            }
                        }

                        let exp_last_fetched_at = {
                            provider_clone
                                .cached_experiments
                                .read()
                                .await
                                .as_ref()
                                .map(|d| d.fetched_at)
                        };
                        // Refresh experiments
                        match provider_clone
                            .primary
                            .fetch_active_experiments(exp_last_fetched_at)
                            .await
                        {
                            Ok(exp_resp) => {
                                let mut cached =
                                    provider_clone.cached_experiments.write().await;
                                if let Some(data) = exp_resp.into_data() {
                                    *cached = Some(data);
                                }
                                log::debug!(
                                    "LocalResolutionProvider: experiments refreshed from primary on watch update"
                                );
                            }
                            Err(e) => {
                                log::warn!(
                                    "LocalResolutionProvider: experiment refresh failed, keeping last known good on watch update: {}",
                                    e
                                );
                            }
                        }
                    }
                    Err(e) => {
                        log::error!(
                            "LocalResolutionProvider: watch channel error: {}",
                            e
                        );
                        break;
                    }
                }
            }
        })
    }

    async fn ensure_fresh_data(&self) -> Result<()> {
        if let RefreshStrategy::OnDemand(on_demand) = &self.refresh_strategy {
            let ttl = on_demand.ttl;
            let use_stale_on_error = on_demand.use_stale_on_error.unwrap_or_default();

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
                log::debug!("LocalResolutionProvider: TTL expired, refreshing on-demand");
                if let Err(e) = self.do_refresh().await {
                    if !use_stale_on_error {
                        return Err(e);
                    }
                    log::warn!(
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
        mut context: EvaluationContext,
        prefix_filter: Option<&[String]>,
    ) -> Result<Map<String, Value>> {
        self.ensure_fresh_data().await?;

        let global_context = self.global_context.read().await;
        context.merge_missing(&global_context);

        let (mut query_data, targeting_key) =
            conversions::evaluation_context_to_query(context);

        let dimensions_info = self.get_dimensions_info().await;

        // If experiments are cached, get applicable variants and inject variantIds
        {
            let cached_exp = self.cached_experiments.read().await;
            if let Some(exp_data) = cached_exp.as_ref() {
                let variant_ids = get_applicable_variants(
                    &dimensions_info,
                    exp_data.data.experiments.clone(),
                    &exp_data.data.experiment_groups,
                    &query_data,
                    &targeting_key.clone().unwrap_or_default(),
                    None,
                );

                query_data.insert(
                    "variantIds".to_string(),
                    Value::Array(variant_ids.into_iter().map(Value::String).collect()),
                );
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
        context: EvaluationContext,
    ) -> Result<Map<String, Value>> {
        self.eval_with_context(context, None).await
    }

    async fn resolve_all_features_with_filter(
        &self,
        context: EvaluationContext,
        prefix_filter: Option<&[String]>,
    ) -> Result<Map<String, Value>> {
        self.eval_with_context(context, prefix_filter).await
    }
}

#[async_trait]
impl FeatureExperimentMeta for LocalResolutionProvider {
    async fn get_applicable_variants(
        &self,
        mut context: EvaluationContext,
    ) -> Result<Vec<String>> {
        self.ensure_fresh_data().await?;

        let global_context = self.global_context.read().await;
        context.merge_missing(&global_context);

        let (query_data, targeting_key) =
            conversions::evaluation_context_to_query(context);
        let dimensions_info = self.get_dimensions_info().await;

        let cached_exp = self.cached_experiments.read().await;
        let resp = match cached_exp.as_ref() {
            Some(exp_data) => get_applicable_variants(
                &dimensions_info,
                exp_data.data.experiments.clone(),
                &exp_data.data.experiment_groups,
                &query_data,
                &targeting_key.unwrap_or_default(),
                None,
            ),
            None => vec![],
        };
        Ok(resp)
    }
}

#[async_trait]
impl FeatureProvider for LocalResolutionProvider {
    async fn initialize(&mut self, context: &EvaluationContext) {
        log::info!("Initializing LocalResolutionProvider...");
        {
            let mut status = self.status.write().await;
            *status = ProviderStatus::NotReady;
        }
        if (self.init(context.clone()).await).is_err() {
            let mut status = self.status.write().await;
            *status = ProviderStatus::Error;
            return;
        }

        log::info!("LocalResolutionProvider initialized successfully");
    }

    async fn resolve_bool_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<bool>> {
        self.resolve_bool(flag_key, evaluation_context.clone())
            .await
    }

    async fn resolve_string_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<String>> {
        self.resolve_string(flag_key, evaluation_context.clone())
            .await
    }

    async fn resolve_int_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<i64>> {
        self.resolve_int(flag_key, evaluation_context.clone()).await
    }

    async fn resolve_float_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<f64>> {
        self.resolve_float(flag_key, evaluation_context.clone())
            .await
    }

    async fn resolve_struct_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<StructValue>> {
        self.resolve_struct(flag_key, evaluation_context.clone())
            .await
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

#[async_trait]
impl SuperpositionDataSource for LocalResolutionProvider {
    async fn fetch_config(
        &self,
        _: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ConfigData>> {
        let cached = self.cached_config.read().await;
        match cached.as_ref() {
            Some(data) => Ok(FetchResponse::Data(data.clone())),
            None => Err(SuperpositionError::ConfigError(
                "No cached config available".into(),
            )),
        }
    }

    async fn fetch_filtered_config(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        last_fetched_at: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ConfigData>> {
        let resp = self.fetch_config(last_fetched_at).await?.map_data(|mut c| {
            let prefix = prefix_filter.map(HashSet::from_iter);
            c.config = c.config.filter(context.as_ref(), prefix.as_ref());

            c
        });

        Ok(resp)
    }

    async fn fetch_active_experiments(
        &self,
        _: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>> {
        if !self.supports_experiments() {
            return Err(SuperpositionError::ConfigError(
                "Experiments not supported by this provider".into(),
            ));
        }
        let cached = self.cached_experiments.read().await;
        match cached.clone() {
            Some(data) => Ok(FetchResponse::Data(data)),
            None => Err(SuperpositionError::ConfigError(
                "No cached experiments available".into(),
            )),
        }
    }

    async fn fetch_candidate_active_experiments(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        last_fetched_at: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>> {
        if !self.supports_experiments() {
            return Err(SuperpositionError::ConfigError(
                "Experiments not supported by this provider".into(),
            ));
        }

        let resp = self
            .fetch_active_experiments(last_fetched_at)
            .await?
            .map_data(|mut exp_data| {
                let context = context.unwrap_or_default();
                exp_data.data.experiments = get_satisfied_experiments(
                    exp_data.data.experiments,
                    &context,
                    prefix_filter,
                );
                exp_data.data.experiment_groups = FfiExperimentGroup::get_satisfied(
                    exp_data.data.experiment_groups,
                    &context,
                );
                exp_data
            });

        Ok(resp)
    }

    async fn fetch_matching_active_experiments(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        last_fetched_at: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>> {
        if !self.supports_experiments() {
            return Err(SuperpositionError::ConfigError(
                "Experiments not supported by this provider".into(),
            ));
        }

        let resp = self
            .fetch_active_experiments(last_fetched_at)
            .await?
            .map_data(|mut exp_data| {
                let context = context.unwrap_or_default();
                exp_data.data.experiments = filter_experiments_by_context(
                    exp_data.data.experiments,
                    &context,
                    prefix_filter,
                );
                exp_data.data.experiment_groups = FfiExperimentGroup::filter_by_eval(
                    exp_data.data.experiment_groups,
                    &context,
                );
                exp_data
            });

        Ok(resp)
    }

    fn supports_experiments(&self) -> bool {
        self.primary.supports_experiments()
    }

    async fn close(&self) -> Result<()> {
        Ok(())
    }
}
