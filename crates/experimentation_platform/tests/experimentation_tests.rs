use chrono::Utc;
use experimentation_platform::api::experiments::helpers;
use experimentation_platform::db::models::{Experiment, ExperimentStatusType};
use serde_json::{json, Map, Value};
use service_utils::helpers::extract_dimensions;
use service_utils::service::types::ExperimentationFlags;
use superposition_types::{
    result as superposition, Condition, Overrides, ValidationType,
};

enum Dimensions {
    Os(String),
    Client(String),
    VariantIds(String),
}

fn single_dimension_ctx_gen(value: Dimensions) -> Map<String, Value> {
    let mut map = Map::new();
    match value {
        Dimensions::Os(os) => map.insert(
            "==".to_string(),
            json!([
                {"var": "os"},
                os
            ]),
        ),
        Dimensions::Client(client_id) => map.insert(
            "==".to_string(),
            json!([
                client_id,
                {"var": "clientId"}
            ]),
        ),
        Dimensions::VariantIds(id) => map.insert(
            "in".to_string(),
            json!([
                id,
                {"var": "variantIds"},
            ]),
        ),
    };
    map
}

fn multiple_dimension_ctx_gen(values: Vec<Dimensions>) -> Map<String, Value> {
    let mut conditions: Vec<Map<String, Value>> = vec![];
    for val in values {
        conditions.push(single_dimension_ctx_gen(val));
    }
    Map::from_iter(vec![("and".to_string(), json!(conditions))])
}

fn experiment_gen(
    override_keys: &[String],
    context: &Map<String, Value>,
    status: ExperimentStatusType,
    variants: &Value,
) -> Experiment {
    Experiment {
        id: 123456789,
        created_at: Utc::now(),
        created_by: "test".to_string(),
        last_modified: Utc::now(),
        last_modified_by: "test".to_string(),
        name: "experiment-test".to_string(),
        traffic_percentage: 0,

        override_keys: override_keys.to_vec(),
        status,
        context: json!(context.clone()),
        variants: variants.clone(),
        chosen_variant: None,
    }
}

#[test]
fn test_duplicate_override_key_entries() {
    let override_keys = vec!["key1".to_string(), "key2".to_string(), "key1".to_string()];
    assert!(matches!(
        helpers::validate_override_keys(&override_keys),
        Err(superposition::AppError::BadArgument(_))
    ));
}

#[test]
fn test_unique_override_key_entries() {
    let override_keys = vec!["key1".to_string(), "key2".to_string()];
    assert!(matches!(
        helpers::validate_override_keys(&override_keys),
        Ok(())
    ));
}

#[test]
fn test_extract_dimensions() -> Result<(), superposition::AppError> {
    let context_a = multiple_dimension_ctx_gen(vec![
        Dimensions::Os("os1".to_string()),
        Dimensions::Client("testclient1".to_string()),
    ]);
    let context_a = Condition::new(context_a.clone(), ValidationType::DEFAULT)?;

    let context_b =
        single_dimension_ctx_gen(Dimensions::Client("testclient1".to_string()));
    let context_b = Condition::new(context_b.clone(), ValidationType::DEFAULT)?;

    let expected_dimensions_1 = serde_json::Map::from_iter(vec![
        ("os".to_string(), json!("os1")),
        ("clientId".to_string(), json!("testclient1")),
    ]);
    let expected_dimensions_2 =
        serde_json::Map::from_iter(vec![("clientId".to_string(), json!("testclient1"))]);

    // more than one dimension in context
    assert_eq!(extract_dimensions(&context_a)?, expected_dimensions_1);
    // only one dimension in context
    assert_eq!(extract_dimensions(&context_b)?, expected_dimensions_2);
    Ok(())
}

#[test]
fn test_are_overlapping_contexts() -> Result<(), superposition::AppError> {
    let context_a = multiple_dimension_ctx_gen(vec![
        Dimensions::Os("os1".to_string()),
        Dimensions::Client("testclient1".to_string()),
    ]);
    let context_a = Condition::new(context_a.clone(), ValidationType::DEFAULT)?;

    let context_b = multiple_dimension_ctx_gen(vec![
        Dimensions::Os("os1".to_string()),
        Dimensions::Client("testclient2".to_string()),
    ]);
    let context_b = Condition::new(context_b.clone(), ValidationType::DEFAULT)?;

    let context_c = single_dimension_ctx_gen(Dimensions::Os("os1".to_string()));
    let context_d = single_dimension_ctx_gen(Dimensions::Os("os2".to_string()));
    let context_c = Condition::new(context_c.clone(), ValidationType::DEFAULT)?;
    let context_d = Condition::new(context_d.clone(), ValidationType::DEFAULT)?;

    // both contexts with same dimensions
    assert!(helpers::are_overlapping_contexts(&context_a, &context_a)?);
    // contexts with one different dimension
    assert!(!(helpers::are_overlapping_contexts(&context_a, &context_b)?));
    // one context dimensions are subset of other
    assert!(helpers::are_overlapping_contexts(&context_a, &context_c)?);
    // one context dimensions not a subset of other but have less dimensions that other
    assert!(!(helpers::are_overlapping_contexts(&context_a, &context_d)?));
    // disjoint contexts
    assert!(!(helpers::are_overlapping_contexts(&context_c, &context_d)?));
    Ok(())
}

#[test]
fn test_check_variants_override_coverage() -> Result<(), superposition::AppError> {
    let override_keys = vec!["key1".to_string(), "key2".to_string()];
    let overrides = [
        Overrides::new(
            Map::from_iter(vec![
                ("key1".to_string(), json!("value1")),
                ("key2".to_string(), json!("value2")),
            ]),
            ValidationType::EXPERIMENTAL,
        ),
        // has one override key mi)ssing
        Overrides::new(
            Map::from_iter(vec![("key1".to_string(), json!("value1"))]),
            ValidationType::EXPERIMENTAL,
        ),
        // has an unknown override) key
        Overrides::new(
            Map::from_iter(vec![("key3".to_string(), json!("value3"))]),
            ValidationType::EXPERIMENTAL,
        ),
        // has an extra unknown ov)erride key
        Overrides::new(
            Map::from_iter(vec![
                ("key1".to_string(), json!("value1")),
                ("key2".to_string(), json!("value2")),
                ("key3".to_string(), json!("value3")),
            ]),
            ValidationType::EXPERIMENTAL,
        ),
    ]
    .into_iter()
    .collect::<superposition::Result<Vec<Overrides>>>()?;

    assert!(helpers::check_variant_override_coverage(
        &overrides[0],
        &override_keys
    ));
    assert!(!helpers::check_variant_override_coverage(
        &overrides[1],
        &override_keys
    ));
    assert!(!helpers::check_variant_override_coverage(
        &overrides[2],
        &override_keys
    ));
    assert!(!helpers::check_variant_override_coverage(
        &overrides[3],
        &override_keys
    ));
    Ok(())
}

/************************* No Restrictions *****************************************/

#[test]
fn test_is_valid_experiment_no_restrictions_overlapping_experiment(
) -> Result<(), superposition::AppError> {
    let experiment_context = multiple_dimension_ctx_gen(vec![
        Dimensions::Os("os1".to_string()),
        Dimensions::Client("testclient1".to_string()),
    ]);
    let experiment_context = Condition::new(experiment_context, ValidationType::DEFAULT)?;
    let experiment_override_keys = vec!["key1".to_string(), "key2".to_string()];
    let flags = ExperimentationFlags {
        allow_same_keys_overlapping_ctx: true,
        allow_diff_keys_overlapping_ctx: true,
        allow_same_keys_non_overlapping_ctx: true,
    };

    let active_experiments = vec![experiment_gen(
        &["key1".to_string(), "key2".to_string()],
        &experiment_context,
        ExperimentStatusType::CREATED,
        &json!(""),
    )];

    assert_eq!(
        helpers::is_valid_experiment(
            &experiment_context,
            &experiment_override_keys,
            &flags,
            &active_experiments
        )?,
        (true, "".to_string())
    );

    Ok(())
}

#[test]
fn test_is_valid_experiment_no_restrictions_non_overlapping_experiment(
) -> Result<(), superposition::AppError> {
    let experiment_context = multiple_dimension_ctx_gen(vec![
        Dimensions::Os("os1".to_string()),
        Dimensions::Client("testclient1".to_string()),
    ]);
    let experiment_context = Condition::new(experiment_context, ValidationType::DEFAULT)?;
    let experiment_override_keys = vec!["key1".to_string(), "key2".to_string()];
    let flags = ExperimentationFlags {
        allow_same_keys_overlapping_ctx: true,
        allow_diff_keys_overlapping_ctx: true,
        allow_same_keys_non_overlapping_ctx: true,
    };

    let active_experiments = vec![experiment_gen(
        &["key1".to_string(), "key2".to_string()],
        &multiple_dimension_ctx_gen(vec![
            Dimensions::Os("os2".to_string()),
            Dimensions::Client("testclient2".to_string()),
        ]),
        ExperimentStatusType::CREATED,
        &json!(""),
    )];

    assert_eq!(
        helpers::is_valid_experiment(
            &experiment_context,
            &experiment_override_keys,
            &flags,
            &active_experiments
        )?,
        (true, "".to_string())
    );

    Ok(())
}

/************************* Restrict Same Keys Overlapping Context *****************************************/

#[test]
fn test_is_valid_experiment_restrict_same_keys_overlapping_ctx_overlapping_experiment_same_keys(
) -> Result<(), superposition::AppError> {
    let experiment_context = multiple_dimension_ctx_gen(vec![
        Dimensions::Os("os1".to_string()),
        Dimensions::Client("testclient1".to_string()),
    ]);
    let experiment_context = Condition::new(experiment_context, ValidationType::DEFAULT)?;
    let experiment_override_keys = vec!["key1".to_string(), "key2".to_string()];
    let flags = ExperimentationFlags {
        allow_same_keys_overlapping_ctx: false,
        allow_diff_keys_overlapping_ctx: true,
        allow_same_keys_non_overlapping_ctx: true,
    };

    let active_experiments = vec![experiment_gen(
        &experiment_override_keys,
        &experiment_context,
        ExperimentStatusType::CREATED,
        &json!(""),
    )];

    assert_eq!(
        helpers::is_valid_experiment(
            &experiment_context,
            &experiment_override_keys,
            &flags,
            &active_experiments
        )?,
        (false, "This current context overlaps with an existing experiment or the keys in the context are overlapping".to_string())
    );

    Ok(())
}

#[test]
fn test_is_valid_experiment_restrict_same_keys_overlapping_ctx_overlapping_experiment_one_same_key(
) -> Result<(), superposition::AppError> {
    let experiment_context = multiple_dimension_ctx_gen(vec![
        Dimensions::Os("os1".to_string()),
        Dimensions::Client("testclient1".to_string()),
    ]);
    let experiment_context = Condition::new(experiment_context, ValidationType::DEFAULT)?;
    let experiment_override_keys = vec!["key1".to_string(), "key2".to_string()];
    let flags = ExperimentationFlags {
        allow_same_keys_overlapping_ctx: false,
        allow_diff_keys_overlapping_ctx: true,
        allow_same_keys_non_overlapping_ctx: true,
    };

    let active_experiments = vec![experiment_gen(
        &["key1".to_string(), "key3".to_string()],
        &experiment_context,
        ExperimentStatusType::CREATED,
        &json!(""),
    )];

    assert_eq!(
        helpers::is_valid_experiment(
            &experiment_context,
            &experiment_override_keys,
            &flags,
            &active_experiments
        )?,
        (false, "This current context overlaps with an existing experiment or the keys in the context are overlapping".to_string())
    );

    Ok(())
}

#[test]
fn test_is_valid_experiment_restrict_same_keys_overlapping_ctx_overlapping_experiment_diff_keys(
) -> Result<(), superposition::AppError> {
    let experiment_context = multiple_dimension_ctx_gen(vec![
        Dimensions::Os("os1".to_string()),
        Dimensions::Client("testclient1".to_string()),
    ]);
    let experiment_context = Condition::new(experiment_context, ValidationType::DEFAULT)?;
    let experiment_override_keys = vec!["key1".to_string(), "key2".to_string()];
    let flags = ExperimentationFlags {
        allow_same_keys_overlapping_ctx: false,
        allow_diff_keys_overlapping_ctx: true,
        allow_same_keys_non_overlapping_ctx: true,
    };

    let active_experiments = vec![experiment_gen(
        &["key3".to_string(), "key4".to_string()],
        &experiment_context,
        ExperimentStatusType::CREATED,
        &json!(""),
    )];

    assert_eq!(
        helpers::is_valid_experiment(
            &experiment_context,
            &experiment_override_keys,
            &flags,
            &active_experiments
        )?,
        (true, "".to_string())
    );

    Ok(())
}

/************************* Restrict Different Keys Overlapping Context *****************************************/

#[test]
fn test_is_valid_experiment_restrict_diff_keys_overlapping_ctx_overlapping_experiment_same_keys(
) -> Result<(), superposition::AppError> {
    let experiment_context = multiple_dimension_ctx_gen(vec![
        Dimensions::Os("os1".to_string()),
        Dimensions::Client("testclient1".to_string()),
    ]);
    let experiment_context = Condition::new(experiment_context, ValidationType::DEFAULT)?;
    let experiment_override_keys = vec!["key1".to_string(), "key2".to_string()];
    let flags = ExperimentationFlags {
        allow_same_keys_overlapping_ctx: true,
        allow_diff_keys_overlapping_ctx: false,
        allow_same_keys_non_overlapping_ctx: true,
    };

    let active_experiments = vec![experiment_gen(
        &experiment_override_keys,
        &experiment_context,
        ExperimentStatusType::CREATED,
        &json!(""),
    )];

    assert_eq!(
        helpers::is_valid_experiment(
            &experiment_context,
            &experiment_override_keys,
            &flags,
            &active_experiments
        )?,
        (true, "".to_string())
    );

    Ok(())
}

#[test]
fn test_is_valid_experiment_restrict_diff_keys_overlapping_ctx_overlapping_experiment_one_diff_key(
) -> Result<(), superposition::AppError> {
    let experiment_context = multiple_dimension_ctx_gen(vec![
        Dimensions::Os("os1".to_string()),
        Dimensions::Client("testclient1".to_string()),
    ]);
    let experiment_context = Condition::new(experiment_context, ValidationType::DEFAULT)?;
    let experiment_override_keys = vec!["key1".to_string(), "key2".to_string()];
    let flags = ExperimentationFlags {
        allow_same_keys_overlapping_ctx: true,
        allow_diff_keys_overlapping_ctx: false,
        allow_same_keys_non_overlapping_ctx: true,
    };

    let active_experiments = vec![experiment_gen(
        &["key1".to_string(), "key3".to_string()],
        &experiment_context,
        ExperimentStatusType::CREATED,
        &json!(""),
    )];

    assert_eq!(
        helpers::is_valid_experiment(
            &experiment_context,
            &experiment_override_keys,
            &flags,
            &active_experiments
        )?,
        (false, "This current context overlaps with an existing experiment or the keys in the context are overlapping".to_string())
    );

    Ok(())
}

#[test]
fn test_is_valid_experiment_restrict_diff_keys_overlapping_ctx_overlapping_experiment_diff_keys(
) -> Result<(), superposition::AppError> {
    let experiment_context = multiple_dimension_ctx_gen(vec![
        Dimensions::Os("os1".to_string()),
        Dimensions::Client("testclient1".to_string()),
    ]);
    let experiment_context = Condition::new(experiment_context, ValidationType::DEFAULT)?;
    let experiment_override_keys = vec!["key1".to_string(), "key2".to_string()];
    let flags = ExperimentationFlags {
        allow_same_keys_overlapping_ctx: true,
        allow_diff_keys_overlapping_ctx: false,
        allow_same_keys_non_overlapping_ctx: true,
    };

    let active_experiments = vec![experiment_gen(
        &["key3".to_string(), "key4".to_string()],
        &experiment_context,
        ExperimentStatusType::CREATED,
        &json!(""),
    )];

    assert_eq!(
        helpers::is_valid_experiment(
            &experiment_context,
            &experiment_override_keys,
            &flags,
            &active_experiments
        )?,
        (false, "This current context overlaps with an existing experiment or the keys in the context are overlapping".to_string())
    );

    Ok(())
}

/************************* Restrict Same Keys Non Overlapping Context *****************************************/

#[test]
fn test_is_valid_experiment_restrict_same_keys_non_overlapping_ctx_non_overlapping_experiment_same_keys(
) -> Result<(), superposition::AppError> {
    let experiment_context = multiple_dimension_ctx_gen(vec![
        Dimensions::Os("os1".to_string()),
        Dimensions::Client("testclient1".to_string()),
    ]);
    let experiment_context = Condition::new(experiment_context, ValidationType::DEFAULT)?;
    let experiment_override_keys = vec!["key1".to_string(), "key2".to_string()];
    let flags = ExperimentationFlags {
        allow_same_keys_overlapping_ctx: true,
        allow_diff_keys_overlapping_ctx: true,
        allow_same_keys_non_overlapping_ctx: false,
    };

    let active_experiments = vec![experiment_gen(
        &experiment_override_keys,
        &multiple_dimension_ctx_gen(vec![
            Dimensions::Os("os2".to_string()),
            Dimensions::Client("testclient2".to_string()),
        ]),
        ExperimentStatusType::CREATED,
        &json!(""),
    )];

    assert_eq!(
        helpers::is_valid_experiment(
            &experiment_context,
            &experiment_override_keys,
            &flags,
            &active_experiments
        )?,
        (false, "This current context overlaps with an existing experiment or the keys in the context are overlapping".to_string())
    );

    Ok(())
}

#[test]
fn test_is_valid_experiment_restrict_same_keys_non_overlapping_ctx_non_overlapping_experiment_one_diff_key(
) -> Result<(), superposition::AppError> {
    let experiment_context = multiple_dimension_ctx_gen(vec![
        Dimensions::Os("os1".to_string()),
        Dimensions::Client("testclient1".to_string()),
    ]);
    let experiment_context = Condition::new(experiment_context, ValidationType::DEFAULT)?;
    let experiment_override_keys = vec!["key1".to_string(), "key2".to_string()];
    let flags = ExperimentationFlags {
        allow_same_keys_overlapping_ctx: true,
        allow_diff_keys_overlapping_ctx: true,
        allow_same_keys_non_overlapping_ctx: false,
    };

    let active_experiments = vec![experiment_gen(
        &["key1".to_string(), "key3".to_string()],
        &multiple_dimension_ctx_gen(vec![
            Dimensions::Os("os2".to_string()),
            Dimensions::Client("testclient2".to_string()),
        ]),
        ExperimentStatusType::CREATED,
        &json!(""),
    )];

    assert_eq!(
        helpers::is_valid_experiment(
            &experiment_context,
            &experiment_override_keys,
            &flags,
            &active_experiments
        )?,
        (false, "This current context overlaps with an existing experiment or the keys in the context are overlapping".to_string())
    );

    Ok(())
}

#[test]
fn test_is_valid_experiment_restrict_same_keys_non_overlapping_ctx_non_overlapping_experiment_diff_keys(
) -> Result<(), superposition::AppError> {
    let experiment_context = multiple_dimension_ctx_gen(vec![
        Dimensions::Os("os1".to_string()),
        Dimensions::Client("testclient1".to_string()),
    ]);
    let experiment_context = Condition::new(experiment_context, ValidationType::DEFAULT)?;
    let experiment_override_keys = vec!["key1".to_string(), "key2".to_string()];
    let flags = ExperimentationFlags {
        allow_same_keys_overlapping_ctx: true,
        allow_diff_keys_overlapping_ctx: false,
        allow_same_keys_non_overlapping_ctx: true,
    };

    let active_experiments = vec![experiment_gen(
        &["key3".to_string(), "key4".to_string()],
        &multiple_dimension_ctx_gen(vec![
            Dimensions::Os("os2".to_string()),
            Dimensions::Client("testclient2".to_string()),
        ]),
        ExperimentStatusType::CREATED,
        &json!(""),
    )];

    assert_eq!(
        helpers::is_valid_experiment(
            &experiment_context,
            &experiment_override_keys,
            &flags,
            &active_experiments
        )?,
        (true, "".to_string())
    );

    Ok(())
}
