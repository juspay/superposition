use std::path::PathBuf;

use open_feature::EvaluationContext;
use superposition_provider::{
    data_source::file::FileDataSource, local_provider::LocalResolutionProvider,
    traits::AllFeatureProvider, RefreshStrategy, WatchStrategy,
};

#[tokio::main]
async fn main() {
    env_logger::init();

    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let config_path = manifest_dir.join("examples/config.toml");

    println!("Watching config file: {:?}", config_path);
    println!("Edit the file in another terminal to see changes.\n");

    let file_source = FileDataSource::new(config_path);

    let provider = LocalResolutionProvider::new(
        Box::new(file_source),
        None,
        RefreshStrategy::Watch(WatchStrategy::default()),
    );
    provider.init().await.unwrap();

    let context = EvaluationContext::default()
        .with_custom_field("os", "linux")
        .with_custom_field("city", "Boston");

    // Poll in a loop to show updated values after file changes
    loop {
        let config = provider.resolve_all_features(&context).await.unwrap();
        println!("Config: {:?}", config);
        tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;
    }
}
