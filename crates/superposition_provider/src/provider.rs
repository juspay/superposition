mod remote_provider;
mod local_provider;

pub use remote_provider::SuperpositionRemoteProvider;
pub use local_provider::SuperpositionLocalProvider;

use crate::types::*;
use async_trait::async_trait;

use open_feature::{
    provider::FeatureProvider,
    provider::{ProviderMetadata, ProviderStatus, ResolutionDetails},
    EvaluationContext, EvaluationResult,
    StructValue,
};

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