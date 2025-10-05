use std::collections::HashMap;

mod local_provider;
mod remote_provider;

pub use local_provider::SuperpositionLocalProvider;
pub use remote_provider::SuperpositionRemoteProvider;

use crate::types::*;
use async_trait::async_trait;

use open_feature::{
    provider::FeatureProvider,
    provider::{ProviderMetadata, ProviderStatus, ResolutionDetails},
    EvaluationContext, EvaluationResult, StructValue,
};
use serde_json::Value;
use superposition_types::DimensionInfo;
use tokio::sync::RwLock;

use crate::client::{CacConfig, ExperimentationConfig};
use crate::types::*;
use crate::utils::ConversionUtils;

#[derive(Debug)]
pub struct SuperpositionRemoteProvider {
    metadata: ProviderMetadata,
    status: RwLock<ProviderStatus>,
    cac_config: Option<CacConfig>,
    exp_config: Option<ExperimentationConfig>,
}
impl SuperpositionRemoteProvider {
    pub fn new(provider_options: SuperpositionRemoteProviderOptions) -> Self {
        // Create CAC config
        let superposition_options = SuperpositionOptions::new(
            provider_options.endpoint,
            provider_options.token,
            provider_options.org_id,
            provider_options.workspace_id,
        );
        let cac_options = ConfigurationOptions::new(
            provider_options.refresh_strategy,
            provider_options.evaluation_cache,
            provider_options.fallback_config.clone(),
        );

        let cac_config =
            CacConfig::new(superposition_options.clone(), cac_options.clone());

        let exp_config =
            provider_options
                .experimentation_options
                .as_ref()
                .map(|exp_opts| {
                    ExperimentationConfig::new(
                        superposition_options.clone(),
                        exp_opts.clone(),
                    )
                });

        Self {
            metadata: ProviderMetadata {
                name: "SuperpositionRemoteProvider".to_string(),
            },
            status: RwLock::new(ProviderStatus::NotReady),
            cac_config: Some(cac_config),
            exp_config,
        }
    }

    fn get_context_from_evaluation_context(
        &self,
        evaluation_context: &EvaluationContext,
    ) -> (serde_json::Map<String, Value>, Option<String>) {
        let context = evaluation_context
            .custom_fields
            .iter()
            .map(|(k, v)| {
                (
                    k.clone(),
                    ConversionUtils::convert_evaluation_context_value_to_serde_value(v),
                )
            })
            .collect();

        (context, evaluation_context.targeting_key.clone())
    }

    async fn get_dimensions_info(&self) -> HashMap<String, DimensionInfo> {
        match &self.cac_config {
            Some(cac_config) => cac_config
                .get_cached_config()
                .await
                .map(|c| c.dimensions.clone())
                .unwrap_or_default(),
            None => HashMap::new(),
        }
    }

    pub async fn init(&self) -> Result<()> {
        // Initialize CAC config
        if let Some(cac_config) = &self.cac_config {
            match cac_config.create_config().await {
                Ok(_) => info!("CAC configuration initialized successfully"),
                Err(e) => {
                    error!("Failed to initialize CAC configuration: {}", e);
                    return Err(SuperpositionError::ConfigError(format!(
                        "Failed to initialize CAC configuration: {}",
                        e
                    )));
                }
            }
        }

        // Initialize experimentation config if available
        if let Some(exp_config) = &self.exp_config {
            match exp_config.create_config().await {
                Ok(_) => info!("Experimentation configuration initialized successfully"),
                Err(e) => {
                    error!("Failed to initialize experimentation configuration: {}", e);
                    return Err(SuperpositionError::ConfigError(format!(
                        "Failed to initialize experimentation configuration: {}",
                        e
                    )));
                }
            }
        };
        Ok(())
    }

    pub async fn resolve_full_config(
        &self,
        evaluation_context: &EvaluationContext,
    ) -> Result<serde_json::Map<String, Value>> {
        self.eval_config(evaluation_context).await
    }

    async fn eval_config(
        &self,
        evaluation_context: &EvaluationContext,
    ) -> Result<serde_json::Map<String, Value>> {
        // Get cached config from CAC
        let (mut context, targeting_key) =
            self.get_context_from_evaluation_context(evaluation_context);

        let dimensions_info = self.get_dimensions_info().await;
        let variant_ids = if let Some(exp_config) = &self.exp_config {
            exp_config
                .get_applicable_variants(&dimensions_info, &context, targeting_key)
                .await?
        } else {
            vec![]
        };

        context.insert(
            "variantIds".to_string(),
            Value::Array(variant_ids.into_iter().map(Value::String).collect()),
        );

        match &self.cac_config {
            Some(cac_config) => cac_config.evaluate_config(&context, None).await,
            None => Err(SuperpositionError::ConfigError(
                "CAC config not initialized".into(),
            )),
        }
    }
}
#[async_trait]
impl FeatureProvider for SuperpositionRemoteProvider {
    async fn initialize(&mut self, _context: &EvaluationContext) {
        info!("Initializing SuperpositionRemoteProvider...");
        {
            let mut status = self.status.write().await;
            *status = ProviderStatus::NotReady;
        }

        // Initialize CAC config
        if let Some(cac_config) = &self.cac_config {
            match cac_config.create_config().await {
                Ok(_) => info!("CAC configuration initialized successfully"),
                Err(e) => {
                    error!("Failed to initialize CAC configuration: {}", e);
                    let mut status = self.status.write().await;
                    *status = ProviderStatus::Error;
                    return;
                }
            }
        }
        if (self.init().await).is_err() {
            let mut status = self.status.write().await;
            *status = ProviderStatus::Error;
            return;
        }

        let mut status = self.status.write().await;
        *status = ProviderStatus::Ready;

        info!("SuperpositionRemoteProvider initialized successfully");
    }

    async fn resolve_bool_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<bool>> {
        match self.eval_config(evaluation_context).await {
            Ok(config) => {
                if let Some(value) = config.get(flag_key) {
                    if let Some(bool_val) = value.as_bool() {
                        return Ok(ResolutionDetails::new(bool_val));
                    }
                }
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
            Err(e) => {
                error!("Error evaluating boolean flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
        }
    }

    async fn resolve_string_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<String>> {
        match self.eval_config(evaluation_context).await {
            Ok(config) => {
                if let Some(value) = config.get(flag_key) {
                    if let Some(str_val) = value.as_str() {
                        return Ok(ResolutionDetails::new(str_val.to_owned()));
                    }
                }
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
            Err(e) => {
                error!("Error evaluating String flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
        }
    }

    async fn resolve_int_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<i64>> {
        match self.eval_config(evaluation_context).await {
            Ok(config) => {
                if let Some(value) = config.get(flag_key) {
                    if let Some(int_val) = value.as_i64() {
                        return Ok(ResolutionDetails::new(int_val));
                    }
                }
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
            Err(e) => {
                error!("Error evaluating integer flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
        }
    }

    async fn resolve_float_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<f64>> {
        match self.eval_config(evaluation_context).await {
            Ok(config) => {
                if let Some(value) = config.get(flag_key) {
                    if let Some(int_val) = value.as_f64() {
                        return Ok(ResolutionDetails::new(int_val));
                    }
                }
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
            Err(e) => {
                error!("Error evaluating float flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
        }
    }

    async fn resolve_struct_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<StructValue>> {
        match self.eval_config(evaluation_context).await {
            Ok(config) => {
                if let Some(value) = config.get(flag_key) {
                    // Use the conversion utility we added earlier
                    match ConversionUtils::serde_value_to_struct_value(value) {
                        Ok(struct_value) => {
                            return Ok(ResolutionDetails::new(struct_value));
                        }
                        Err(e) => {
                            error!("Error converting value to StructValue: {}", e);
                            return Err(EvaluationError {
                                code: EvaluationErrorCode::ParseError,
                                message: Some(format!(
                                    "Failed to parse struct value: {}",
                                    e
                                )),
                            });
                        }
                    }
                }
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
            Err(e) => {
                error!("Error evaluating Object flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
        }
    }

    fn metadata(&self) -> &ProviderMetadata {
        &self.metadata
    }

    fn status(&self) -> ProviderStatus {
        // Since we can't await in a non-async function, we need to handle this differently
        // We'll use try_read() which returns immediately
        match self.status.try_read() {
            Ok(status) => match *status {
                ProviderStatus::Ready => ProviderStatus::Ready,
                ProviderStatus::Error => ProviderStatus::Error,
                ProviderStatus::NotReady => ProviderStatus::NotReady,
                ProviderStatus::STALE => ProviderStatus::STALE,
            },
            Err(_) => ProviderStatus::NotReady, // Default if lock is held
        }
    }
}

#[derive(Debug)]
pub struct SuperpositionLocalProvider {
    metadata: ProviderMetadata,
    status: RwLock<ProviderStatus>,
    options: SuperpositionLocalProviderOptions,
    toml_config: Arc<RwLock<Option<SuperpositionToml>>>,
    _file_watcher: Option<RecommendedWatcher>,
}

impl SuperpositionLocalProvider {
    pub fn new(options: SuperpositionLocalProviderOptions) -> Self {
        Self {
            metadata: ProviderMetadata {
                name: "SuperpositionLocalProvider".to_string(),
            },
            status: RwLock::new(ProviderStatus::NotReady),
            options,
            toml_config: Arc::new(RwLock::new(None)),
            _file_watcher: None,
        }
    }

    async fn get_context_from_evaluation_context(
        &self,
        evaluation_context: &EvaluationContext,
    ) -> serde_json::Map<String, Value> {
        evaluation_context
            .custom_fields
            .iter()
            .map(|(k, v)| {
                (
                    k.clone(),
                    ConversionUtils::convert_evaluation_context_value_to_serde_value(v),
                )
            })
            .collect()
    }

    async fn eval_config(
        &self,
        evaluation_context: &EvaluationContext,
    ) -> Result<serde_json::Map<String, Value>> {
        let context = self
            .get_context_from_evaluation_context(evaluation_context)
            .await;

        // Load or reload config based on refresh strategy
        match self.options.refresh_strategy {
            LocalRefreshStrategy::OnDemand => {
                // Reload config for each evaluation
                match SuperpositionToml::parse(&self.options.file_path) {
                    Ok(toml_config) => {
                        let mut config = self.toml_config.write().await;
                        *config = Some(toml_config);
                    }
                    Err(_) => {
                        return Err(SuperpositionError::ConfigError(
                            "Failed to parse TOML file".into(),
                        ));
                    }
                }
            }
            LocalRefreshStrategy::FileWatch | LocalRefreshStrategy::Manual => {
                // Use cached config - for FileWatch, config is automatically updated by file watcher
                // For Manual, config is only updated through explicit reinitialization
            }
        }

        let config = self.toml_config.read().await;
        match config.as_ref() {
            Some(toml_config) => toml_config.get_resolved_config(&context).map_err(|e| {
                SuperpositionError::ConfigError(format!(
                    "Failed to resolve config: {}",
                    e
                ))
            }),
            None => Err(SuperpositionError::ConfigError(
                "No TOML config loaded".into(),
            )),
        }
    }

    async fn setup_file_watcher(&mut self) {
        let config_clone = Arc::clone(&self.toml_config);
        let file_path = self.options.file_path.clone();

        info!("Setting up file watcher for: {}", file_path);

        // Create a channel for file system events
        let (tx, mut rx) = mpsc::channel(100);

        // Set up the file watcher
        let mut watcher = match notify::recommended_watcher(move |res| {
            if let Ok(event) = res {
                if let Err(e) = tx.try_send(event) {
                    error!("Failed to send file watcher event: {}", e);
                }
            }
        }) {
            Ok(watcher) => watcher,
            Err(e) => {
                error!("Failed to create file watcher: {}", e);
                return;
            }
        };

        // Watch the file
        if let Err(e) = watcher.watch(Path::new(&file_path), RecursiveMode::NonRecursive)
        {
            error!("Failed to watch file {}: {}", file_path, e);
            return;
        }

        // Store the watcher to keep it alive
        self._file_watcher = Some(watcher);

        // Spawn a task to handle file change events
        let file_path_clone = file_path.clone();
        tokio::spawn(async move {
            while let Some(event) = rx.recv().await {
                match event.kind {
                    notify::EventKind::Modify(_) | notify::EventKind::Create(_) => {
                        info!(
                            "Detected file change, reloading configuration from: {}",
                            file_path_clone
                        );

                        // Reload the TOML configuration
                        match SuperpositionToml::parse(&file_path_clone) {
                            Ok(toml_config) => {
                                let mut config = config_clone.write().await;
                                *config = Some(toml_config);
                                info!("Configuration reloaded successfully");
                            }
                            Err(e) => {
                                error!("Failed to reload configuration: {:?}", e);
                            }
                        }
                    }
                    _ => {
                        // Ignore other event types like access, remove, etc.
                    }
                }
            }
        });

        info!("File watcher setup completed for: {}", file_path);
    }
}

#[async_trait]
impl FeatureProvider for SuperpositionLocalProvider {
    async fn initialize(&mut self, _context: &EvaluationContext) {
        info!("Initializing SuperpositionLocalProvider...");

        let mut status = self.status.write().await;
        *status = ProviderStatus::NotReady;
        drop(status);

        // Load initial TOML config
        match SuperpositionToml::parse(&self.options.file_path) {
            Ok(toml_config) => {
                let mut config = self.toml_config.write().await;
                *config = Some(toml_config);
                info!(
                    "TOML configuration loaded successfully from: {}",
                    self.options.file_path
                );

                // Set up file watcher for FileWatch strategy
                if matches!(
                    self.options.refresh_strategy,
                    LocalRefreshStrategy::FileWatch
                ) {
                    self.setup_file_watcher().await;
                }

                let mut status = self.status.write().await;
                *status = ProviderStatus::Ready;
            }
            Err(e) => {
                error!("Failed to load TOML configuration: {:?}", e);
                let mut status = self.status.write().await;
                *status = ProviderStatus::Error;
                return;
            }
        }

        info!("SuperpositionLocalProvider initialized successfully");
    }

    async fn resolve_bool_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<bool>> {
        match self.eval_config(evaluation_context).await {
            Ok(config) => {
                if let Some(value) = config.get(flag_key) {
                    if let Some(bool_val) = value.as_bool() {
                        return Ok(ResolutionDetails::new(bool_val));
                    }
                }
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
            Err(e) => {
                error!("Error evaluating boolean flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
        }
    }

    async fn resolve_string_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<String>> {
        match self.eval_config(evaluation_context).await {
            Ok(config) => {
                if let Some(value) = config.get(flag_key) {
                    if let Some(str_val) = value.as_str() {
                        return Ok(ResolutionDetails::new(str_val.to_owned()));
                    }
                }
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
            Err(e) => {
                error!("Error evaluating String flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
        }
    }

    async fn resolve_int_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<i64>> {
        match self.eval_config(evaluation_context).await {
            Ok(config) => {
                if let Some(value) = config.get(flag_key) {
                    if let Some(int_val) = value.as_i64() {
                        return Ok(ResolutionDetails::new(int_val));
                    }
                }
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
            Err(e) => {
                error!("Error evaluating integer flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
        }
    }

    async fn resolve_float_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<f64>> {
        match self.eval_config(evaluation_context).await {
            Ok(config) => {
                if let Some(value) = config.get(flag_key) {
                    if let Some(float_val) = value.as_f64() {
                        return Ok(ResolutionDetails::new(float_val));
                    }
                }
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
            Err(e) => {
                error!("Error evaluating float flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
        }
    }

    async fn resolve_struct_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<StructValue>> {
        match self.eval_config(evaluation_context).await {
            Ok(config) => {
                if let Some(value) = config.get(flag_key) {
                    match ConversionUtils::serde_value_to_struct_value(value) {
                        Ok(struct_value) => {
                            return Ok(ResolutionDetails::new(struct_value));
                        }
                        Err(e) => {
                            error!("Error converting value to StructValue: {}", e);
                            return Err(EvaluationError {
                                code: EvaluationErrorCode::ParseError,
                                message: Some(format!(
                                    "Failed to parse struct value: {}",
                                    e
                                )),
                            });
                        }
                    }
                }
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
            Err(e) => {
                error!("Error evaluating Object flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
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

/// Unified SuperpositionProvider that can wrap either Remote or Local providers
#[derive(Debug)]
pub enum SuperpositionProvider {
    Remote(SuperpositionRemoteProvider),
    Local(SuperpositionLocalProvider),
}

impl SuperpositionProvider {
    /// Create a new remote provider
    pub fn remote(options: SuperpositionRemoteProviderOptions) -> Self {
        Self::Remote(SuperpositionRemoteProvider::new(options))
    }

    /// Create a new local provider
    pub fn local(options: SuperpositionLocalProviderOptions) -> Self {
        Self::Local(SuperpositionLocalProvider::new(options))
    }

    /// Backwards compatibility constructor - creates a remote provider
    #[deprecated(since = "0.1.0", note = "Use SuperpositionProvider::remote() instead")]
    pub fn new(provider_options: SuperpositionRemoteProviderOptions) -> Self {
        Self::remote(provider_options)
    }
}

#[async_trait]
impl FeatureProvider for SuperpositionProvider {
    async fn initialize(&mut self, context: &EvaluationContext) {
        match self {
            SuperpositionProvider::Remote(provider) => provider.initialize(context).await,
            SuperpositionProvider::Local(provider) => provider.initialize(context).await,
        }
    }

    async fn resolve_bool_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<bool>> {
        match self {
            SuperpositionProvider::Remote(provider) => {
                provider
                    .resolve_bool_value(flag_key, evaluation_context)
                    .await
            }
            SuperpositionProvider::Local(provider) => {
                provider
                    .resolve_bool_value(flag_key, evaluation_context)
                    .await
            }
        }
    }

    async fn resolve_string_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<String>> {
        match self {
            SuperpositionProvider::Remote(provider) => {
                provider
                    .resolve_string_value(flag_key, evaluation_context)
                    .await
            }
            SuperpositionProvider::Local(provider) => {
                provider
                    .resolve_string_value(flag_key, evaluation_context)
                    .await
            }
        }
    }

    async fn resolve_int_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<i64>> {
        match self {
            SuperpositionProvider::Remote(provider) => {
                provider
                    .resolve_int_value(flag_key, evaluation_context)
                    .await
            }
            SuperpositionProvider::Local(provider) => {
                provider
                    .resolve_int_value(flag_key, evaluation_context)
                    .await
            }
        }
    }

    async fn resolve_float_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<f64>> {
        match self {
            SuperpositionProvider::Remote(provider) => {
                provider
                    .resolve_float_value(flag_key, evaluation_context)
                    .await
            }
            SuperpositionProvider::Local(provider) => {
                provider
                    .resolve_float_value(flag_key, evaluation_context)
                    .await
            }
        }
    }

    async fn resolve_struct_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<StructValue>> {
        match self {
            SuperpositionProvider::Remote(provider) => {
                provider
                    .resolve_struct_value(flag_key, evaluation_context)
                    .await
            }
            SuperpositionProvider::Local(provider) => {
                provider
                    .resolve_struct_value(flag_key, evaluation_context)
                    .await
            }
        }
    }

    fn metadata(&self) -> &ProviderMetadata {
        match self {
            SuperpositionProvider::Remote(provider) => provider.metadata(),
            SuperpositionProvider::Local(provider) => provider.metadata(),
        }
    }

    fn status(&self) -> ProviderStatus {
        match self {
            SuperpositionProvider::Remote(provider) => provider.status(),
            SuperpositionProvider::Local(provider) => provider.status(),
        }
    }
}

