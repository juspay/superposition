use std::path::PathBuf;

use open_feature::EvaluationContext;
use superposition_provider::{
    data_source::file::FileDataSource,
    local_provider::LocalResolutionProvider,
    traits::AllFeatureProvider,
    OnDemandStrategy, RefreshStrategy,
};

#[tokio::main]
async fn main() {
    env_logger::init();

    let file_source = FileDataSource::new(PathBuf::from("./config.toml"));

    let provider = LocalResolutionProvider::new(
        Box::new(file_source),
        None,
        RefreshStrategy::OnDemand(OnDemandStrategy {
            ttl: 60,
            ..Default::default()
        }),
    );
    provider.init().await.unwrap();

    let context = EvaluationContext::default()
        .with_custom_field("os", "linux");

    let config = provider.resolve_all_features(&context).await.unwrap();
    println!("Config: {:?}", config);

    provider.close().await.unwrap();
}
