pub mod client;
pub mod data_source;
pub mod local_provider;
pub mod provider;
pub mod remote_provider;
pub mod traits;
pub mod types;
pub mod utils;

pub use client::*;
pub use data_source::{ConfigData, ExperimentData, SuperpositionDataSource};
pub use local_provider::LocalResolutionProvider;
pub use provider::*;
pub use remote_provider::SuperpositionAPIProvider;
pub use traits::*;
pub use types::*;

pub use open_feature::{
    provider::{ProviderMetadata, ProviderStatus, ResolutionDetails},
    EvaluationContext,
};

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_cac_config_creation() {
        let superposition_options = SuperpositionOptions::new(
            "http://localhost:8080".to_string(),
            "test-token".to_string(),
            "test-org".to_string(),
            "test-workspace".to_string(),
        );

        let config_options = ConfigurationOptions::new(
            RefreshStrategy::OnDemand(OnDemandStrategy::default()),
            None,
            None,
        );

        let cac_config = CacConfig::new(superposition_options, config_options);

        assert!(cac_config.get_cached_config().await.is_none());
    }

    #[tokio::test]
    async fn test_experimentation_config_creation() {
        let superposition_options = SuperpositionOptions::new(
            "http://localhost:8080".to_string(),
            "test-token".to_string(),
            "test-org".to_string(),
            "test-workspace".to_string(),
        );

        let exp_options = ExperimentationOptions::new(RefreshStrategy::OnDemand(
            OnDemandStrategy::default(),
        ));

        let exp_config = ExperimentationConfig::new(superposition_options, exp_options);

        // Test that we can get None for cached experiments initially
        assert!(exp_config.get_cached_experiments().await.is_none());
    }
}
