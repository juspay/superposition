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
    config_checked_at: RwLock<Option<DateTime<Utc>>>,
    experiments_checked_at: RwLock<Option<DateTime<Utc>>>,
    background_task: RwLock<Option<JoinHandle<()>>>,
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
            config_checked_at: RwLock::new(None),
            experiments_checked_at: RwLock::new(None),
            background_task: RwLock::new(None),
            metadata: ProviderMetadata {
                name: "LocalResolutionProvider".to_string(),
            },
            status: RwLock::new(ProviderStatus::NotReady),
            global_context: RwLock::new(EvaluationContext::default()),
        }))
    }

    pub async fn init(&self, context: EvaluationContext) -> Result<()> {
        // Single-shot: a provider is initialized once and then served. Re-initializing a live
        // provider would strand the running background task (it holds a strong Arc, so it would
        // poll forever), so it is refused. A fresh (NotReady) or previously-failed (Error)
        // provider proceeds. The claim happens under the status lock so it is atomic.
        {
            let mut status = self.status.write().await;
            if matches!(*status, ProviderStatus::Ready | ProviderStatus::STALE) {
                log::warn!(
                    "LocalResolutionProvider already initialized; ignoring init(). \
                     Providers are single-shot — build a new instance."
                );
                return Ok(());
            }
            *status = ProviderStatus::NotReady;
        }

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
        *self.config_checked_at.write().await = Some(Utc::now());

        // Fetch experiments best-effort: try primary, else fallback
        let exp_data = if self.primary.supports_experiments() {
            match self.primary.fetch_active_experiments(None).await {
                Ok(exp_resp) => exp_resp.into_data(),
                Err(e) => {
                    log::warn!(
                        "LocalResolutionProvider: primary experiment fetch failed: {}",
                        e
                    );
                    if let Some(fallback) = &self.fallback {
                        if fallback.supports_experiments() {
                            match fallback.fetch_active_experiments(None).await {
                                Ok(exp_resp) => exp_resp.into_data(),
                                Err(fb_err) => {
                                    log::warn!(
                                        "LocalResolutionProvider: fallback experiment fetch also failed: {}",
                                        fb_err
                                    );
                                    return Err(SuperpositionError::ConfigError(format!(
                                        "Both primary and fallback experiment fetch failed. Primary: {}. Fallback: {}",
                                        e, fb_err
                                    )));
                                }
                            }
                        } else {
                            log::warn!(
                                "LocalResolutionProvider: fallback does not support experiments"
                            );
                            None
                        }
                    } else {
                        return Err(SuperpositionError::ConfigError(format!(
                            "Primary experiment fetch failed and no fallback configured: {}",
                            e
                        )));
                    }
                }
            }
        } else {
            None
        };

        if let Some(data) = exp_data {
            let mut cached = self.cached_experiments.write().await;
            *cached = Some(data);
        }
        if self.primary.supports_experiments() {
            *self.experiments_checked_at.write().await = Some(Utc::now());
        }

        // Start refresh strategy
        match &self.refresh_strategy {
            RefreshStrategy::Polling(polling_strategy) => {
                log::info!(
                    "LocalResolutionProvider: starting polling with interval={}ms",
                    polling_strategy.interval_ms()
                );
                let task = self.start_polling(polling_strategy.interval_ms()).await;
                let mut background_task = self.background_task.write().await;
                *background_task = Some(task);
            }
            RefreshStrategy::OnDemand(on_demand_strategy) => {
                log::info!(
                    "LocalResolutionProvider: using OnDemand strategy with ttl={}ms",
                    on_demand_strategy.ttl_ms()
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
                        let mut background_task = self.background_task.write().await;
                        *background_task = Some(task);
                    }
                    Ok(None) => {
                        return Err(SuperpositionError::ConfigError(
                            "Watch strategy selected but data source does not support watching".into(),
                        ));
                    }
                    Err(e) => {
                        return Err(SuperpositionError::ConfigError(format!(
                            "Failed to start watch: {}",
                            e
                        )));
                    }
                }
            }
            RefreshStrategy::Manual => {
                log::info!("LocalResolutionProvider: using Manual refresh strategy");
            }
        }

        {
            let mut global_context = self.global_context.write().await;
            *global_context = context;
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

    pub async fn close_provider(&self) -> Result<()> {
        // Abort background task
        {
            let mut background_task = self.background_task.write().await;
            if let Some(task) = background_task.take() {
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
        *self.config_checked_at.write().await = None;
        *self.experiments_checked_at.write().await = None;

        {
            let mut global_context = self.global_context.write().await;
            *global_context = EvaluationContext::default();
        }

        // Set status to NotReady
        {
            let mut status = self.status.write().await;
            *status = ProviderStatus::NotReady;
        }

        Ok(())
    }

    /// The timeout the configured strategy puts on a single refresh, if any.
    fn refresh_timeout(&self) -> Option<Duration> {
        match &self.refresh_strategy {
            RefreshStrategy::Polling(polling) => polling.timeout_ms(),
            RefreshStrategy::OnDemand(on_demand) => on_demand.timeout_ms(),
            RefreshStrategy::Watch(_) | RefreshStrategy::Manual => None,
        }
        .map(Duration::from_millis)
    }

    /// Runs a refresh, bounded by the strategy's timeout. Without this a data source that never
    /// answers would stall the poller — or, under OnDemand, the evaluating thread — indefinitely.
    ///
    /// Every refresh path funnels through here, so this is also where staleness is recorded.
    async fn do_refresh(&self) -> Result<()> {
        let result = match self.refresh_timeout() {
            Some(timeout) => {
                match tokio::time::timeout(timeout, self.refresh_once()).await {
                    Ok(result) => result,
                    Err(_) => {
                        log::warn!(
                        "LocalResolutionProvider: refresh timed out after {}ms, keeping last known good",
                        timeout.as_millis()
                    );
                        Err(SuperpositionError::RefreshError(format!(
                            "Refresh timed out after {}ms",
                            timeout.as_millis()
                        )))
                    }
                }
            }
            None => self.refresh_once().await,
        };

        self.record_refresh_outcome(result.is_ok()).await;
        result
    }

    /// A refresh that fails while the cache is still served leaves the provider STALE: the flags
    /// are frozen at their last known good values, and this is the only signal a consumer has that
    /// they stopped tracking the source of truth. The next successful refresh clears it.
    ///
    /// Only meaningful from Ready. A failure during init is an Error — there is no good data to be
    /// stale — and a provider that has been shut down stays NotReady.
    ///
    /// Evaluation is unaffected: the SDK's client never consults provider status, so a STALE
    /// provider keeps resolving flags from its cache.
    ///
    /// Unlike the Java and Python clients, this cannot notify anyone. `open_feature` 0.2.5 has no
    /// provider-event API, so the only way to observe staleness is to hold the provider and call
    /// [`FeatureProvider::status`]. Java emits PROVIDER_STALE and Python emits its equivalent, so
    /// consumers there can subscribe. Revisit when the crate grows events.
    async fn record_refresh_outcome(&self, succeeded: bool) {
        let mut status = self.status.write().await;
        match (&*status, succeeded) {
            (ProviderStatus::Ready, false) => {
                log::warn!("LocalResolutionProvider: refresh failed, serving stale data");
                *status = ProviderStatus::STALE;
            }
            (ProviderStatus::STALE, true) => {
                log::info!("LocalResolutionProvider: refresh recovered, no longer stale");
                *status = ProviderStatus::Ready;
            }
            _ => {}
        }
    }

    async fn refresh_once(&self) -> Result<()> {
        // Fetch config from primary; keep last known good on failure
        let last_fetched_at = {
            self.cached_config
                .read()
                .await
                .as_ref()
                .map(|data| data.fetched_at)
        };

        let config_resp = async {
            match self.primary.fetch_config(last_fetched_at).await {
                Ok(FetchResponse::Data(data)) => {
                    let mut cached = self.cached_config.write().await;
                    *cached = Some(data);
                    *self.config_checked_at.write().await = Some(Utc::now());
                    log::debug!("LocalResolutionProvider: config refreshed from primary");
                    Ok(())
                }
                Ok(FetchResponse::NotModified) => {
                    // A 304 is a successful check: the cache is confirmed current, so the TTL
                    // clock restarts. Without this the next evaluation would ask again at once.
                    *self.config_checked_at.write().await = Some(Utc::now());
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
            }
        };

        let exp_resp = async {
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
                        // Data or NotModified — both are a successful check, so the TTL restarts.
                        *self.experiments_checked_at.write().await = Some(Utc::now());
                        log::debug!(
                            "LocalResolutionProvider: experiments refreshed from primary"
                        );
                        Ok(())
                    }
                    Err(e) => {
                        log::warn!(
                            "LocalResolutionProvider: experiment refresh failed, keeping last known good: {}",
                            e
                        );
                        Err(e)
                    }
                }
            } else {
                Ok(())
            }
        };

        let (config_resp, exp_resp) = tokio::join!(config_resp, exp_resp);

        if config_resp.is_ok() {
            exp_resp
        } else {
            config_resp
        }
    }

    async fn start_polling(&self, interval_ms: u64) -> JoinHandle<()> {
        let provider = self.clone();
        tokio::spawn(async move {
            loop {
                sleep(Duration::from_millis(interval_ms)).await;
                let _ = provider.do_refresh().await;
            }
        })
    }

    async fn start_watching(
        &self,
        mut watch_stream: crate::types::WatchStream,
        debounce_ms: u64,
    ) -> JoinHandle<()> {
        let provider = self.clone();

        tokio::spawn(async move {
            loop {
                match watch_stream.receiver.recv().await {
                    Ok(()) => {
                        // Debounce: wait, then drain any queued events
                        sleep(Duration::from_millis(debounce_ms)).await;
                        while watch_stream.receiver.try_recv().is_ok() {}
                        let _ = provider.do_refresh().await;
                    }
                    Err(e) => {
                        log::error!(
                            "LocalResolutionProvider: watch channel error: {}",
                            e
                        );
                    }
                }
            }
        })
    }

    async fn ensure_fresh_data(&self) -> Result<()> {
        if let RefreshStrategy::OnDemand(on_demand) = &self.refresh_strategy {
            let ttl = on_demand.ttl_ms();
            let use_stale_on_error = on_demand.use_stale_on_error();

            let is_elapsed = |cached_at: DateTime<Utc>| {
                (chrono::Utc::now() - cached_at).num_milliseconds() > ttl as i64
            };

            // Never checked, or last checked before the TTL window opened.
            let should_refresh_config = self
                .config_checked_at
                .read()
                .await
                .map(is_elapsed)
                .unwrap_or(true);

            let should_refresh_experiments = self.primary.supports_experiments()
                && self
                    .experiments_checked_at
                    .read()
                    .await
                    .map(is_elapsed)
                    .unwrap_or(true);

            if should_refresh_config || should_refresh_experiments {
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
            Some(data) => data.data.dimensions.clone(),
            None => HashMap::new(),
        }
    }

    async fn get_merged_context(
        &self,
        mut context: EvaluationContext,
    ) -> (Map<String, Value>, Option<String>) {
        let global_context = self.global_context.read().await;
        context.merge_missing(&global_context);

        conversions::evaluation_context_to_query(context)
    }

    async fn eval_with_context(
        &self,
        context: EvaluationContext,
        prefix_filter: Option<Vec<String>>,
    ) -> Result<Map<String, Value>> {
        self.ensure_fresh_data().await?;

        let (mut query_data, targeting_key) = self.get_merged_context(context).await;
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
                    &targeting_key.unwrap_or_default(),
                    prefix_filter.clone(),
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
                (*config_data.data.default_configs).clone(),
                &config_data.data.contexts,
                &config_data.data.overrides,
                &config_data.data.dimensions,
                &query_data,
                MergeStrategy::MERGE,
                prefix_filter,
            )
            .map_err(|e| {
                SuperpositionError::ConfigError(format!(
                    "Failed to evaluate config: {}",
                    e
                ))
            }),
            None => Err(SuperpositionError::ProviderError(
                "Provider not initialized: no cached config available".into(),
            )),
        }
    }
}

#[async_trait]
impl AllFeatureProvider for LocalResolutionProvider {
    async fn resolve_all_features_with_filter(
        &self,
        context: EvaluationContext,
        prefix_filter: Option<Vec<String>>,
    ) -> Result<Map<String, Value>> {
        self.eval_with_context(context, prefix_filter).await
    }
}

#[async_trait]
impl FeatureExperimentMeta for LocalResolutionProvider {
    async fn get_applicable_variants(
        &self,
        context: EvaluationContext,
        prefix_filter: Option<Vec<String>>,
    ) -> Result<Vec<String>> {
        self.ensure_fresh_data().await?;

        let (query_data, targeting_key) = self.get_merged_context(context).await;
        let dimensions_info = self.get_dimensions_info().await;

        let cached_exp = self.cached_experiments.read().await;
        let resp = match cached_exp.as_ref() {
            Some(exp_data) => get_applicable_variants(
                &dimensions_info,
                exp_data.data.experiments.clone(),
                &exp_data.data.experiment_groups,
                &query_data,
                &targeting_key.unwrap_or_default(),
                prefix_filter,
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
        // init() owns the status transition: it refuses a re-init of a live provider and only
        // then moves to NotReady. Resetting the status here would defeat that guard.
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
            // need to do this as ProviderStatus neither implements Copy nor Clone
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
    async fn fetch_filtered_config(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ConfigData>> {
        if if_modified_since.is_some() {
            log::debug!("LocalResolutionProvider: ignoring if_modified_since for config, always returning cached data");
        }

        let mut config_data = {
            let cached = self.cached_config.read().await;
            match cached.as_ref() {
                Some(data) => data.clone(),
                None => {
                    return Err(SuperpositionError::DataSourceError(
                        "No cached config available".into(),
                    ))
                }
            }
        };

        let prefix = prefix_filter.map(HashSet::from_iter);
        config_data.data = config_data.data.filter(context.as_ref(), prefix.as_ref());

        Ok(FetchResponse::Data(config_data))
    }

    async fn fetch_active_experiments(
        &self,
        if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>> {
        if !self.supports_experiments() {
            return Err(SuperpositionError::DataSourceError(
                "Experiments not supported by this provider".into(),
            ));
        }
        if if_modified_since.is_some() {
            log::debug!("LocalResolutionProvider: ignoring if_modified_since for experiments, always returning cached data");
        }
        let cached = self.cached_experiments.read().await;
        match cached.clone() {
            Some(data) => Ok(FetchResponse::Data(data)),
            None => Err(SuperpositionError::DataSourceError(
                "No cached experiments available".into(),
            )),
        }
    }

    async fn fetch_candidate_active_experiments(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>> {
        if !self.supports_experiments() {
            return Err(SuperpositionError::DataSourceError(
                "Experiments not supported by this provider".into(),
            ));
        }

        let resp = self
            .fetch_active_experiments(if_modified_since)
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
        if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>> {
        if !self.supports_experiments() {
            return Err(SuperpositionError::DataSourceError(
                "Experiments not supported by this provider".into(),
            ));
        }

        let resp = self
            .fetch_active_experiments(if_modified_since)
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
        self.close_provider().await
    }
}
