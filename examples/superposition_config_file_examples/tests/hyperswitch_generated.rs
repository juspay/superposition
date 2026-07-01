use serde_json::{Map, Value};
use std::fs;
use superposition_core::{eval_config, ConfigFormat, MergeStrategy, TomlFormat};

fn query(
    connector: &str,
    payment_method: &str,
    country: &str,
    currency: &str,
    capture_method: &str,
) -> Map<String, Value> {
    Map::from_iter([
        (
            "capture_method".to_string(),
            Value::String(capture_method.to_string()),
        ),
        (
            "connector".to_string(),
            Value::String(connector.to_string()),
        ),
        (
            "payment_method".to_string(),
            Value::String(payment_method.to_string()),
        ),
        ("country".to_string(), Value::String(country.to_string())),
        ("currency".to_string(), Value::String(currency.to_string())),
    ])
}

#[test]
fn generated_hyperswitch_payment_methods_supertoml_parses_and_resolves() {
    let manifest_dir = std::env!("CARGO_MANIFEST_DIR");
    let generated_path = format!("{manifest_dir}/hyperswitch-development.generated.toml");
    let toml =
        fs::read_to_string(generated_path).expect("generated SuperTOML should exist");
    let config =
        TomlFormat::parse_config(&toml).expect("generated SuperTOML should parse");
    let default_configs = (*config.default_configs).clone();
    let local_cohort_sources: Vec<(String, String)> = config
        .dimensions
        .iter()
        .filter_map(|(dimension, dimension_info)| {
            let dimension_type = format!("{:?}", dimension_info.dimension_type);
            dimension_type
                .strip_prefix("LocalCohort(\"")
                .and_then(|source| source.strip_suffix("\")"))
                .map(|source_dimension| (dimension.clone(), source_dimension.to_string()))
        })
        .collect();

    assert!(
        !local_cohort_sources.is_empty(),
        "generated SuperTOML should use local cohorts for compact UI-compatible contexts"
    );

    for context in &config.contexts {
        for (dimension, value) in context.condition.iter() {
            assert!(
                !value.is_array(),
                "context dimension {dimension} must be scalar for UI compatibility"
            );
        }

        for (cohort_dimension, source_dimension) in &local_cohort_sources {
            assert!(
                !(context.condition.contains_key(cohort_dimension)
                    && context.condition.contains_key(source_dimension)),
                "context must not mix cohort {cohort_dimension} with its source dimension {source_dimension}"
            );
        }
    }

    let adyen_ideal_auto = eval_config(
        default_configs.clone(),
        &config.contexts,
        &config.overrides,
        &config.dimensions,
        &query("adyen", "ideal", "NL", "EUR", "automatic"),
        MergeStrategy::MERGE,
        None,
    )
    .expect("adyen ideal automatic should resolve");
    assert_eq!(
        adyen_ideal_auto.get("payments_enabled"),
        Some(&Value::Bool(true))
    );

    let adyen_ideal_manual = eval_config(
        default_configs.clone(),
        &config.contexts,
        &config.overrides,
        &config.dimensions,
        &query("adyen", "ideal", "NL", "EUR", "manual"),
        MergeStrategy::MERGE,
        None,
    )
    .expect("adyen ideal manual should resolve");
    assert_eq!(
        adyen_ideal_manual.get("payments_enabled"),
        Some(&Value::Bool(false))
    );

    let adyen_ideal_wrong_country = eval_config(
        default_configs.clone(),
        &config.contexts,
        &config.overrides,
        &config.dimensions,
        &query("adyen", "ideal", "US", "EUR", "automatic"),
        MergeStrategy::MERGE,
        None,
    )
    .expect("adyen ideal wrong country should resolve");
    assert_eq!(
        adyen_ideal_wrong_country.get("payments_enabled"),
        Some(&Value::Bool(false))
    );

    let stripe_credit_mandate = eval_config(
        default_configs,
        &config.contexts,
        &config.overrides,
        &config.dimensions,
        &query(
            "stripe",
            "credit",
            "__unspecified__",
            "__unspecified__",
            "__unspecified__",
        ),
        MergeStrategy::MERGE,
        None,
    )
    .expect("stripe credit mandate support should resolve");
    assert_eq!(
        stripe_credit_mandate.get("payments_mandates_enabled"),
        Some(&Value::Bool(true))
    );
}
