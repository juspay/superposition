use chrono::Utc;
use experimentation_platform::api::experiments::helpers;
use serde_json::{json, Map, Value};
use service_utils::service::types::ExperimentationFlags;
use superposition_types::{
    database::models::{
        experimentation::{
            Experiment, ExperimentStatusType, ExperimentType, TrafficPercentage, Variant,
            Variants,
        },
        ChangeReason, Description, Metrics,
    },
    result as superposition, Condition, Exp, Overrides,
};

enum Dimensions {
    Os(String),
    Client(String),
    #[allow(dead_code)]
    VariantIds(String),
}

#[cfg(feature = "jsonlogic")]
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

#[cfg(feature = "jsonlogic")]
fn multiple_dimension_ctx_gen(values: Vec<Dimensions>) -> Map<String, Value> {
    let mut conditions: Vec<Map<String, Value>> = vec![];
    for val in values {
        conditions.push(single_dimension_ctx_gen(val));
    }
    Map::from_iter(vec![("and".to_string(), json!(conditions))])
}

#[cfg(not(feature = "jsonlogic"))]
fn multiple_dimension_ctx_gen(values: Vec<Dimensions>) -> Map<String, Value> {
    values
        .into_iter()
        .map(|val| {
            let (key, value) = match val {
                Dimensions::Os(os) => ("os".to_string(), json!(os)),
                Dimensions::Client(client_id) => {
                    ("clientId".to_string(), json!(client_id))
                }
                Dimensions::VariantIds(id) => ("variantIds".to_string(), json!(id)),
            };
            (key, value)
        })
        .collect::<Map<String, Value>>()
}

fn experiment_gen(
    override_keys: &[String],
    context: &Condition,
    status: ExperimentStatusType,
    variants: &[Variant],
) -> Experiment {
    Experiment {
        id: 123456789,
        created_at: Utc::now(),
        created_by: "test".to_string(),
        last_modified: Utc::now(),
        last_modified_by: "test".to_string(),
        name: "experiment-test".to_string(),
        experiment_type: ExperimentType::Default,
        traffic_percentage: TrafficPercentage::default(),
        started_at: None,
        started_by: None,

        override_keys: override_keys.to_vec(),
        status,
        context: context.clone(),
        variants: Variants::new(variants.to_owned()),
        chosen_variant: None,
        description: Description::try_from(String::from("test")).unwrap(),
        change_reason: ChangeReason::try_from(String::from("test")).unwrap(),
        metrics: Metrics::default(),
        experiment_group_id: None,
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

#[cfg(feature = "jsonlogic")]
#[test]
fn test_extract_dimensions() -> Result<(), superposition::AppError> {
    use service_utils::helpers::extract_dimensions;
    use superposition_types::Cac;

    let context_a = multiple_dimension_ctx_gen(vec![
        Dimensions::Os("os1".to_string()),
        Dimensions::Client("testclient1".to_string()),
    ]);
    let context_a = Cac::<Condition>::try_from(context_a.clone())
        .map_err(superposition::AppError::BadArgument)?
        .into_inner();

    let context_b =
        multiple_dimension_ctx_gen(vec![Dimensions::Client("testclient1".to_string())]);
    let context_b = Cac::<Condition>::try_from(context_b.clone())
        .map_err(superposition::AppError::BadArgument)?
        .into_inner();

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
    let context_a = Exp::<Condition>::try_from(context_a.clone())
        .map_err(superposition::AppError::BadArgument)?
        .into_inner();

    let context_b = multiple_dimension_ctx_gen(vec![
        Dimensions::Os("os1".to_string()),
        Dimensions::Client("testclient2".to_string()),
    ]);
    let context_b = Exp::<Condition>::try_from(context_b.clone())
        .map_err(superposition::AppError::BadArgument)?
        .into_inner();

    let context_c = multiple_dimension_ctx_gen(vec![Dimensions::Os("os1".to_string())]);
    let context_d = multiple_dimension_ctx_gen(vec![Dimensions::Os("os2".to_string())]);
    let context_c = Exp::<Condition>::try_from(context_c.clone())
        .map_err(superposition::AppError::BadArgument)?
        .into_inner();
    let context_d = Exp::<Condition>::try_from(context_d.clone())
        .map_err(superposition::AppError::BadArgument)?
        .into_inner();

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
        Exp::<Overrides>::try_from(Map::from_iter(vec![
            ("key1".to_string(), json!("value1")),
            ("key2".to_string(), json!("value2")),
        ])),
        // has one override key missing
        Exp::<Overrides>::try_from(Map::from_iter(vec![(
            "key1".to_string(),
            json!("value1"),
        )])),
        // has an unknown override key
        Exp::<Overrides>::try_from(Map::from_iter(vec![(
            "key3".to_string(),
            json!("value3"),
        )])),
        // has an extra unknown override key
        Exp::<Overrides>::try_from(Map::from_iter(vec![
            ("key1".to_string(), json!("value1")),
            ("key2".to_string(), json!("value2")),
            ("key3".to_string(), json!("value3")),
        ])),
    ]
    .into_iter()
    .map(|a| a.map(|b| b.into_inner()))
    .collect::<Result<Vec<Overrides>, String>>()
    .map_err(superposition::AppError::BadArgument)?;

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
    let experiment_context = Exp::<Condition>::try_from(experiment_context.clone())
        .map_err(superposition::AppError::BadArgument)?
        .into_inner();
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
        &[],
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
    let experiment_context = Exp::<Condition>::try_from(experiment_context.clone())
        .map_err(superposition::AppError::BadArgument)?
        .into_inner();
    let experiment_override_keys = vec!["key1".to_string(), "key2".to_string()];
    let flags = ExperimentationFlags {
        allow_same_keys_overlapping_ctx: true,
        allow_diff_keys_overlapping_ctx: true,
        allow_same_keys_non_overlapping_ctx: true,
    };

    let active_experiments = vec![experiment_gen(
        &["key1".to_string(), "key2".to_string()],
        &Exp::<Condition>::try_from(multiple_dimension_ctx_gen(vec![
            Dimensions::Os("os2".to_string()),
            Dimensions::Client("testclient2".to_string()),
        ]))
        .map_err(superposition::AppError::BadArgument)?
        .into_inner(),
        ExperimentStatusType::CREATED,
        &[],
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
    let experiment_context = Exp::<Condition>::try_from(experiment_context.clone())
        .map_err(superposition::AppError::BadArgument)?
        .into_inner();
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
        &[],
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
    let experiment_context = Exp::<Condition>::try_from(experiment_context.clone())
        .map_err(superposition::AppError::BadArgument)?
        .into_inner();
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
        &[],
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
    let experiment_context = Exp::<Condition>::try_from(experiment_context.clone())
        .map_err(superposition::AppError::BadArgument)?
        .into_inner();
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
        &[],
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
    let experiment_context = Exp::<Condition>::try_from(experiment_context.clone())
        .map_err(superposition::AppError::BadArgument)?
        .into_inner();
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
        &[],
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
    let experiment_context = Exp::<Condition>::try_from(experiment_context.clone())
        .map_err(superposition::AppError::BadArgument)?
        .into_inner();
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
        &[],
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
    let experiment_context = Exp::<Condition>::try_from(experiment_context.clone())
        .map_err(superposition::AppError::BadArgument)?
        .into_inner();
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
        &[],
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
    let experiment_context = Exp::<Condition>::try_from(experiment_context.clone())
        .map_err(superposition::AppError::BadArgument)?
        .into_inner();
    let experiment_override_keys = vec!["key1".to_string(), "key2".to_string()];
    let flags = ExperimentationFlags {
        allow_same_keys_overlapping_ctx: true,
        allow_diff_keys_overlapping_ctx: true,
        allow_same_keys_non_overlapping_ctx: false,
    };

    let active_experiments = vec![experiment_gen(
        &experiment_override_keys,
        &Exp::<Condition>::try_from(multiple_dimension_ctx_gen(vec![
            Dimensions::Os("os2".to_string()),
            Dimensions::Client("testclient2".to_string()),
        ]))
        .map_err(superposition::AppError::BadArgument)?
        .into_inner(),
        ExperimentStatusType::CREATED,
        &[],
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
    let experiment_context = Exp::<Condition>::try_from(experiment_context.clone())
        .map_err(superposition::AppError::BadArgument)?
        .into_inner();
    let experiment_override_keys = vec!["key1".to_string(), "key2".to_string()];
    let flags = ExperimentationFlags {
        allow_same_keys_overlapping_ctx: true,
        allow_diff_keys_overlapping_ctx: true,
        allow_same_keys_non_overlapping_ctx: false,
    };

    let active_experiments = vec![experiment_gen(
        &["key1".to_string(), "key3".to_string()],
        &Exp::<Condition>::try_from(multiple_dimension_ctx_gen(vec![
            Dimensions::Os("os2".to_string()),
            Dimensions::Client("testclient2".to_string()),
        ]))
        .map_err(superposition::AppError::BadArgument)?
        .into_inner(),
        ExperimentStatusType::CREATED,
        &[],
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
    let experiment_context = Exp::<Condition>::try_from(experiment_context.clone())
        .map_err(superposition::AppError::BadArgument)?
        .into_inner();
    let experiment_override_keys = vec!["key1".to_string(), "key2".to_string()];
    let flags = ExperimentationFlags {
        allow_same_keys_overlapping_ctx: true,
        allow_diff_keys_overlapping_ctx: false,
        allow_same_keys_non_overlapping_ctx: true,
    };

    let active_experiments = vec![experiment_gen(
        &["key3".to_string(), "key4".to_string()],
        &Exp::<Condition>::try_from(multiple_dimension_ctx_gen(vec![
            Dimensions::Os("os2".to_string()),
            Dimensions::Client("testclient2".to_string()),
        ]))
        .map_err(superposition::AppError::BadArgument)?
        .into_inner(),
        ExperimentStatusType::CREATED,
        &[],
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
