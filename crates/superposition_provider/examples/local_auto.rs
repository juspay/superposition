//! Run with:
//! `SUPERPOSITION_CONFIG_FILE=crates/superposition_provider/examples/local_auto.super.toml \
//!  cargo run -p superposition_provider --example local_auto`
//!
//! No Superposition server is used. The provider loads the TOML file below and
//! watches it for edits.

use superposition_provider::{
    AllFeatureProvider, EvaluationContext, LocalResolutionProvider,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let provider = LocalResolutionProvider::auto()?;
    let app_context =
        EvaluationContext::default().with_custom_field("env", "development");
    provider.init(app_context).await?;

    let india = EvaluationContext::default().with_custom_field("country", "IN");
    println!(
        "Resolved configuration for country=IN:\n{}",
        serde_json::to_string_pretty(&provider.resolve_all_features(india).await?)?
    );
    println!("Edit the TOML and the provider will reload it. Press Ctrl-C to stop.");

    tokio::signal::ctrl_c().await?;
    Ok(())
}
