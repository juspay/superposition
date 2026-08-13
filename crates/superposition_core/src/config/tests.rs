//! Tests for config evaluation.

use std::{borrow::Cow, collections::HashMap};

use serde_json::{from_value, json, Map, Value};
use superposition_types::{
    Cac, Condition, Config, Context, DimensionInfo, ExtendedMap, OverrideWithKeys,
    Overrides,
};

use super::*;

// ---------------------------------------------------------------------------
// helpers
// ---------------------------------------------------------------------------

fn value_map(values: Vec<(&str, Value)>) -> Map<String, Value> {
    values
        .into_iter()
        .map(|(key, value)| (key.to_string(), value))
        .collect()
}

fn condition(values: Vec<(&str, Value)>) -> Condition {
    Cac::<Condition>::try_from(value_map(values))
        .unwrap()
        .into_inner()
}

fn overrides(values: Vec<(&str, Value)>) -> Overrides {
    Cac::<Overrides>::try_from(value_map(values))
        .unwrap()
        .into_inner()
}

/// A context matching `conditions` and pointing at the override stored under
/// `override_key`.
fn context(id: &str, conditions: Vec<(&str, Value)>, override_key: &str) -> Context {
    Context {
        id: id.to_string(),
        condition: condition(conditions),
        priority: 0,
        weight: 0,
        override_with_keys: OverrideWithKeys::new(override_key.to_string()),
    }
}

fn overrides_map(entries: Vec<(&str, Vec<(&str, Value)>)>) -> HashMap<String, Overrides> {
    entries
        .into_iter()
        .map(|(key, values)| (key.to_string(), overrides(values)))
        .collect()
}

fn default_configs(values: Vec<(&str, Value)>) -> ExtendedMap {
    ExtendedMap::from(value_map(values))
}

/// `country` is a regular dimension that `tier`, a local cohort, is derived
/// from: `IN`/`US` land in the `metro` cohort, everything else in `otherwise`.
fn cohort_dimensions() -> HashMap<String, DimensionInfo> {
    from_value(json!({
        "country": {
            "schema": { "type": "string" },
            "dimension_type": { "REGULAR": {} },
            "position": 1,
            "dependency_graph": { "country": ["tier"] }
        },
        "tier": {
            "schema": {
                "type": "string",
                "enum": ["metro", "otherwise"],
                "definitions": {
                    "metro": { "in": [{ "var": "country" }, ["IN", "US"]] }
                }
            },
            "dimension_type": { "LOCAL_COHORT": "country" },
            "position": 2,
            "dependency_graph": {}
        }
    }))
    .unwrap()
}

// ---------------------------------------------------------------------------
// merge
// ---------------------------------------------------------------------------

#[test]
fn merge_replaces_scalars() {
    let mut doc = json!({ "a": 1 });
    merge(&mut doc, json!({ "a": 2 }));
    assert_eq!(doc, json!({ "a": 2 }));
}

#[test]
fn merge_adds_missing_keys_and_keeps_siblings() {
    let mut doc = json!({ "a": 1 });
    merge(&mut doc, json!({ "b": 2 }));
    assert_eq!(doc, json!({ "a": 1, "b": 2 }));
}

#[test]
fn merge_recurses_into_nested_objects() {
    let mut doc = json!({ "outer": { "kept": 1, "replaced": 2 } });
    merge(&mut doc, json!({ "outer": { "replaced": 3, "added": 4 } }));
    assert_eq!(
        doc,
        json!({ "outer": { "kept": 1, "replaced": 3, "added": 4 } })
    );
}

#[test]
fn merge_replaces_object_with_scalar_and_scalar_with_object() {
    let mut doc = json!({ "a": { "b": 1 } });
    merge(&mut doc, json!({ "a": 5 }));
    assert_eq!(doc, json!({ "a": 5 }));

    let mut doc = json!({ "a": 5 });
    merge(&mut doc, json!({ "a": { "b": 1 } }));
    assert_eq!(doc, json!({ "a": { "b": 1 } }));
}

#[test]
fn merge_replaces_arrays_wholesale() {
    // Arrays are values, not containers to recurse into.
    let mut doc = json!({ "a": [1, 2, 3] });
    merge(&mut doc, json!({ "a": [4] }));
    assert_eq!(doc, json!({ "a": [4] }));
}

#[test]
fn merge_patches_null_over_existing_value() {
    let mut doc = json!({ "a": { "b": 1 } });
    merge(&mut doc, json!({ "a": null }));
    assert_eq!(doc, json!({ "a": null }));
}

#[test]
fn merge_with_empty_patch_is_a_noop() {
    let mut doc = json!({ "a": 1, "b": { "c": 2 } });
    let original = doc.clone();
    merge(&mut doc, json!({}));
    assert_eq!(doc, original);
}

#[test]
fn merge_replaces_root_when_doc_is_not_an_object() {
    let mut doc = json!("scalar");
    merge(&mut doc, json!({ "a": 1 }));
    assert_eq!(doc, json!({ "a": 1 }));
}

// ---------------------------------------------------------------------------
// get_overrides
// ---------------------------------------------------------------------------

#[test]
fn get_overrides_returns_empty_when_no_context_matches() {
    let contexts = [context("c0", vec![("country", json!("IN"))], "o0")];
    let all_overrides = overrides_map(vec![("o0", vec![("key", json!(1))])]);

    let resolved = get_overrides(
        &value_map(vec![("country", json!("US"))]),
        contexts.iter().collect(),
        Cow::Borrowed(&all_overrides),
        &MergeStrategy::MERGE,
    );

    assert_eq!(resolved, Map::new());
}

#[test]
fn get_overrides_skips_context_when_query_lacks_the_dimension() {
    // `apply` requires an exact match, so a dimension absent from the query
    // data cannot satisfy the condition.
    let contexts = [context("c0", vec![("country", json!("IN"))], "o0")];
    let all_overrides = overrides_map(vec![("o0", vec![("key", json!(1))])]);

    let resolved = get_overrides(
        &Map::new(),
        contexts.iter().collect(),
        Cow::Borrowed(&all_overrides),
        &MergeStrategy::MERGE,
    );

    assert_eq!(resolved, Map::new());
}

#[test]
fn get_overrides_matches_variant_ids_by_containment() {
    let contexts = [context("c0", vec![("variantIds", json!("v1"))], "o0")];
    let all_overrides = overrides_map(vec![("o0", vec![("key", json!(1))])]);

    let resolved = get_overrides(
        &value_map(vec![("variantIds", json!(["v0", "v1"]))]),
        contexts.iter().collect(),
        Cow::Borrowed(&all_overrides),
        &MergeStrategy::MERGE,
    );

    assert_eq!(resolved, value_map(vec![("key", json!(1))]));

    let resolved = get_overrides(
        &value_map(vec![("variantIds", json!(["v2"]))]),
        contexts.iter().collect(),
        Cow::Borrowed(&all_overrides),
        &MergeStrategy::MERGE,
    );

    assert_eq!(resolved, Map::new());
}

#[test]
fn get_overrides_applies_contexts_in_order_so_the_last_one_wins() {
    let contexts = [
        context("c0", vec![("country", json!("IN"))], "o0"),
        context("c1", vec![("city", json!("BLR"))], "o1"),
    ];
    let all_overrides = overrides_map(vec![
        ("o0", vec![("shared", json!("first")), ("only0", json!(0))]),
        ("o1", vec![("shared", json!("second")), ("only1", json!(1))]),
    ]);
    let query_data = value_map(vec![("country", json!("IN")), ("city", json!("BLR"))]);

    let resolved = get_overrides(
        &query_data,
        contexts.iter().collect(),
        Cow::Borrowed(&all_overrides),
        &MergeStrategy::MERGE,
    );

    assert_eq!(
        resolved,
        value_map(vec![
            ("shared", json!("second")),
            ("only0", json!(0)),
            ("only1", json!(1)),
        ])
    );
}

#[test]
fn get_overrides_replace_strategy_overwrites_nested_objects_wholesale() {
    let contexts = [
        context("c0", vec![("country", json!("IN"))], "o0"),
        context("c1", vec![("city", json!("BLR"))], "o1"),
    ];
    let all_overrides = overrides_map(vec![
        ("o0", vec![("nested", json!({ "a": 1, "b": 2 }))]),
        ("o1", vec![("nested", json!({ "b": 3 }))]),
    ]);
    let query_data = value_map(vec![("country", json!("IN")), ("city", json!("BLR"))]);

    let resolved = get_overrides(
        &query_data,
        contexts.iter().collect(),
        Cow::Borrowed(&all_overrides),
        &MergeStrategy::REPLACE,
    );

    assert_eq!(resolved, value_map(vec![("nested", json!({ "b": 3 }))]));
}

#[test]
fn get_overrides_merge_strategy_deep_merges_nested_objects() {
    let contexts = [
        context("c0", vec![("country", json!("IN"))], "o0"),
        context("c1", vec![("city", json!("BLR"))], "o1"),
    ];
    let all_overrides = overrides_map(vec![
        ("o0", vec![("nested", json!({ "a": 1, "b": 2 }))]),
        ("o1", vec![("nested", json!({ "b": 3 }))]),
    ]);
    let query_data = value_map(vec![("country", json!("IN")), ("city", json!("BLR"))]);

    let resolved = get_overrides(
        &query_data,
        contexts.iter().collect(),
        Cow::Borrowed(&all_overrides),
        &MergeStrategy::MERGE,
    );

    assert_eq!(
        resolved,
        value_map(vec![("nested", json!({ "a": 1, "b": 3 }))])
    );
}

#[test]
fn get_overrides_skips_contexts_pointing_at_an_unknown_override_key() {
    let contexts = [
        context("c0", vec![("country", json!("IN"))], "missing"),
        context("c1", vec![("city", json!("BLR"))], "o1"),
    ];
    let all_overrides = overrides_map(vec![("o1", vec![("key", json!("present"))])]);
    let query_data = value_map(vec![("country", json!("IN")), ("city", json!("BLR"))]);

    let resolved = get_overrides(
        &query_data,
        contexts.iter().collect(),
        Cow::Borrowed(&all_overrides),
        &MergeStrategy::MERGE,
    );

    assert_eq!(resolved, value_map(vec![("key", json!("present"))]));
}

/// Override ids are content-derived, so several contexts can share one. Each
/// must apply it at its own position — the owned path moves the override out at
/// its *last* consumer only, and must still apply it at the earlier ones.
#[test]
fn get_overrides_applies_a_shared_override_at_every_matching_position() {
    let contexts = [
        context("c0", vec![("country", json!("IN"))], "shared"),
        context("c1", vec![("city", json!("BLR"))], "other"),
        context("c2", vec![("tier", json!("metro"))], "shared"),
    ];
    let all_overrides = overrides_map(vec![
        ("shared", vec![("key", json!("from-shared"))]),
        ("other", vec![("key", json!("from-other"))]),
    ]);
    let query_data = value_map(vec![
        ("country", json!("IN")),
        ("city", json!("BLR")),
        ("tier", json!("metro")),
    ]);

    // `shared` is applied last (at c2), so it wins over `other`.
    let expected = value_map(vec![("key", json!("from-shared"))]);

    let borrowed = get_overrides(
        &query_data,
        contexts.iter().collect(),
        Cow::Borrowed(&all_overrides),
        &MergeStrategy::MERGE,
    );
    let owned = get_overrides(
        &query_data,
        contexts.iter().collect(),
        Cow::Owned(all_overrides.clone()),
        &MergeStrategy::MERGE,
    );

    assert_eq!(borrowed, expected);
    // The owned path takes the move-out shortcut and must agree with it.
    assert_eq!(owned, expected);
}

/// The same shared-override case, but with the shared override placed first so
/// a premature move-out would silently drop it from the earlier position.
#[test]
fn get_overrides_shared_override_first_position_is_not_dropped() {
    let contexts = [
        context("c0", vec![("country", json!("IN"))], "shared"),
        context("c1", vec![("city", json!("BLR"))], "other"),
        context("c2", vec![("tier", json!("metro"))], "shared"),
    ];
    let all_overrides = overrides_map(vec![
        ("shared", vec![("a", json!("shared"))]),
        ("other", vec![("b", json!("other"))]),
    ]);
    let query_data = value_map(vec![
        ("country", json!("IN")),
        ("city", json!("BLR")),
        ("tier", json!("metro")),
    ]);

    let owned = get_overrides(
        &query_data,
        contexts.iter().collect(),
        Cow::Owned(all_overrides),
        &MergeStrategy::MERGE,
    );

    assert_eq!(
        owned,
        value_map(vec![("a", json!("shared")), ("b", json!("other"))])
    );
}

#[test]
fn get_overrides_borrowed_map_is_left_intact_for_reuse() {
    let contexts = [context("c0", vec![("country", json!("IN"))], "o0")];
    let all_overrides = overrides_map(vec![("o0", vec![("key", json!(1))])]);
    let query_data = value_map(vec![("country", json!("IN"))]);

    let first = get_overrides(
        &query_data,
        contexts.iter().collect(),
        Cow::Borrowed(&all_overrides),
        &MergeStrategy::MERGE,
    );
    let second = get_overrides(
        &query_data,
        contexts.iter().collect(),
        Cow::Borrowed(&all_overrides),
        &MergeStrategy::MERGE,
    );

    assert_eq!(first, second);
    assert!(all_overrides.contains_key("o0"));
}

#[test]
fn get_overrides_with_no_contexts_returns_empty() {
    let all_overrides = overrides_map(vec![("o0", vec![("key", json!(1))])]);

    let resolved = get_overrides(
        &value_map(vec![("country", json!("IN"))]),
        Vec::new(),
        Cow::Borrowed(&all_overrides),
        &MergeStrategy::MERGE,
    );

    assert_eq!(resolved, Map::new());
}

// ---------------------------------------------------------------------------
// apply_overrides_on_default_config
// ---------------------------------------------------------------------------

#[test]
fn apply_overrides_replaces_matching_default_keys() {
    let defaults = default_configs(vec![("a", json!(1)), ("b", json!(2))]);

    let result = apply_overrides_on_default_config(
        defaults,
        value_map(vec![("a", json!(10))]),
        MergeStrategy::REPLACE,
    );

    assert_eq!(
        result.into_inner(),
        value_map(vec![("a", json!(10)), ("b", json!(2))])
    );
}

#[test]
fn apply_overrides_replace_strategy_does_not_deep_merge() {
    let defaults = default_configs(vec![("a", json!({ "x": 1, "y": 2 }))]);

    let result = apply_overrides_on_default_config(
        defaults,
        value_map(vec![("a", json!({ "y": 3 }))]),
        MergeStrategy::REPLACE,
    );

    assert_eq!(
        result.into_inner(),
        value_map(vec![("a", json!({ "y": 3 }))])
    );
}

#[test]
fn apply_overrides_merge_strategy_deep_merges_into_defaults() {
    let defaults = default_configs(vec![("a", json!({ "x": 1, "y": 2 }))]);

    let result = apply_overrides_on_default_config(
        defaults,
        value_map(vec![("a", json!({ "y": 3, "z": 4 }))]),
        MergeStrategy::MERGE,
    );

    assert_eq!(
        result.into_inner(),
        value_map(vec![("a", json!({ "x": 1, "y": 3, "z": 4 }))])
    );
}

#[test]
fn apply_overrides_drops_keys_absent_from_default_config() {
    let defaults = default_configs(vec![("a", json!(1))]);

    let result = apply_overrides_on_default_config(
        defaults,
        value_map(vec![("a", json!(2)), ("unknown", json!("x"))]),
        MergeStrategy::MERGE,
    );

    assert_eq!(result.into_inner(), value_map(vec![("a", json!(2))]));
}

#[test]
fn apply_overrides_with_no_overrides_returns_defaults_untouched() {
    let defaults = default_configs(vec![("a", json!(1)), ("b", json!({ "c": 2 }))]);
    let expected = defaults.clone().into_inner();

    let result =
        apply_overrides_on_default_config(defaults, Map::new(), MergeStrategy::MERGE);

    assert_eq!(result.into_inner(), expected);
}

// ---------------------------------------------------------------------------
// eval_config
// ---------------------------------------------------------------------------

fn sample_config() -> Config {
    Config {
        default_configs: default_configs(vec![
            ("checkout.enabled", json!(false)),
            ("checkout.limits", json!({ "min": 1, "max": 10 })),
            ("search.enabled", json!(false)),
        ]),
        contexts: vec![
            context("c0", vec![("country", json!("IN"))], "o0"),
            context("c1", vec![("city", json!("BLR"))], "o1"),
        ],
        overrides: overrides_map(vec![
            (
                "o0",
                vec![
                    ("checkout.enabled", json!(true)),
                    ("checkout.limits", json!({ "max": 20 })),
                ],
            ),
            ("o1", vec![("search.enabled", json!(true))]),
        ]),
        dimensions: HashMap::new(),
    }
}

#[test]
fn eval_config_applies_matching_overrides() {
    let resolved = eval_config(
        sample_config(),
        value_map(vec![("country", json!("IN"))]),
        MergeStrategy::MERGE,
        None,
        None,
    );

    assert_eq!(
        resolved,
        value_map(vec![
            ("checkout.enabled", json!(true)),
            ("checkout.limits", json!({ "min": 1, "max": 20 })),
            ("search.enabled", json!(false)),
        ])
    );
}

#[test]
fn eval_config_replace_strategy_overwrites_nested_defaults() {
    let resolved = eval_config(
        sample_config(),
        value_map(vec![("country", json!("IN"))]),
        MergeStrategy::REPLACE,
        None,
        None,
    );

    assert_eq!(
        resolved,
        value_map(vec![
            ("checkout.enabled", json!(true)),
            ("checkout.limits", json!({ "max": 20 })),
            ("search.enabled", json!(false)),
        ])
    );
}

#[test]
fn eval_config_without_matching_context_returns_defaults() {
    let config = sample_config();
    let expected = config.default_configs.clone().into_inner();

    let resolved = eval_config(
        config,
        value_map(vec![("country", json!("US"))]),
        MergeStrategy::MERGE,
        None,
        None,
    );

    assert_eq!(resolved, expected);
}

#[test]
fn eval_config_with_empty_query_data_returns_defaults() {
    let config = sample_config();
    let expected = config.default_configs.clone().into_inner();

    let resolved = eval_config(config, Map::new(), MergeStrategy::MERGE, None, None);

    assert_eq!(resolved, expected);
}

#[test]
fn eval_config_filters_keys_by_include_prefix() {
    let resolved = eval_config(
        sample_config(),
        value_map(vec![("country", json!("IN")), ("city", json!("BLR"))]),
        MergeStrategy::MERGE,
        Some(vec!["checkout.".to_string()]),
        None,
    );

    // `search.*` is filtered out of the defaults, and the context whose
    // override only touched `search.*` no longer contributes anything.
    assert_eq!(
        resolved,
        value_map(vec![
            ("checkout.enabled", json!(true)),
            ("checkout.limits", json!({ "min": 1, "max": 20 })),
        ])
    );
}

#[test]
fn eval_config_filters_keys_by_exclude_prefix() {
    let resolved = eval_config(
        sample_config(),
        value_map(vec![("country", json!("IN")), ("city", json!("BLR"))]),
        MergeStrategy::MERGE,
        None,
        Some(vec!["checkout.".to_string()]),
    );

    assert_eq!(resolved, value_map(vec![("search.enabled", json!(true))]));
}

#[test]
fn eval_config_exclude_prefix_wins_over_include_prefix() {
    let resolved = eval_config(
        sample_config(),
        value_map(vec![("country", json!("IN")), ("city", json!("BLR"))]),
        MergeStrategy::MERGE,
        Some(vec!["checkout.".to_string()]),
        Some(vec!["checkout.limits".to_string()]),
    );

    assert_eq!(resolved, value_map(vec![("checkout.enabled", json!(true))]));
}

#[test]
fn eval_config_ignores_blank_prefixes() {
    let config = sample_config();
    let unfiltered = eval_config(
        config,
        value_map(vec![("country", json!("IN"))]),
        MergeStrategy::MERGE,
        None,
        None,
    );

    let with_blanks = eval_config(
        sample_config(),
        value_map(vec![("country", json!("IN"))]),
        MergeStrategy::MERGE,
        Some(vec!["".to_string(), "   ".to_string()]),
        Some(vec!["".to_string()]),
    );

    assert_eq!(with_blanks, unfiltered);
}

#[test]
fn eval_config_resolves_local_cohort_dimensions() {
    let config = Config {
        default_configs: default_configs(vec![("shipping.free", json!(false))]),
        contexts: vec![context("c0", vec![("tier", json!("metro"))], "o0")],
        overrides: overrides_map(vec![("o0", vec![("shipping.free", json!(true))])]),
        dimensions: cohort_dimensions(),
    };

    // `country: IN` resolves the `tier` cohort to `metro`, which the context
    // matches even though `tier` was never supplied.
    let resolved = eval_config(
        config,
        value_map(vec![("country", json!("IN"))]),
        MergeStrategy::MERGE,
        None,
        None,
    );

    assert_eq!(resolved, value_map(vec![("shipping.free", json!(true))]));
}

#[test]
fn eval_config_unresolved_local_cohort_falls_back_to_otherwise() {
    let config = Config {
        default_configs: default_configs(vec![("shipping.free", json!(false))]),
        contexts: vec![context("c0", vec![("tier", json!("metro"))], "o0")],
        overrides: overrides_map(vec![("o0", vec![("shipping.free", json!(true))])]),
        dimensions: cohort_dimensions(),
    };

    let resolved = eval_config(
        config,
        value_map(vec![("country", json!("FR"))]),
        MergeStrategy::MERGE,
        None,
        None,
    );

    assert_eq!(resolved, value_map(vec![("shipping.free", json!(false))]));
}

// ---------------------------------------------------------------------------
// eval
// ---------------------------------------------------------------------------

/// query data, strategy, include prefixes, exclude prefixes
type EvalCase = (
    Map<String, Value>,
    MergeStrategy,
    Option<Vec<String>>,
    Option<Vec<String>>,
);

/// `eval` is the borrowing twin of `eval_config`; the two must agree.
fn eval_from(
    config: &Config,
    query_data: Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
    filter_exclude_prefixes: Option<Vec<String>>,
) -> Map<String, Value> {
    eval(
        config.default_configs.clone(),
        &config.contexts,
        &config.overrides,
        &config.dimensions,
        query_data,
        merge_strategy,
        filter_prefixes,
        filter_exclude_prefixes,
    )
}

#[test]
fn eval_applies_matching_overrides() {
    let config = sample_config();

    let resolved = eval_from(
        &config,
        value_map(vec![("country", json!("IN"))]),
        MergeStrategy::MERGE,
        None,
        None,
    );

    assert_eq!(
        resolved,
        value_map(vec![
            ("checkout.enabled", json!(true)),
            ("checkout.limits", json!({ "min": 1, "max": 20 })),
            ("search.enabled", json!(false)),
        ])
    );
}

#[test]
fn eval_matches_eval_config_across_strategies_and_filters() {
    let cases: Vec<EvalCase> = vec![
        (
            value_map(vec![("country", json!("IN"))]),
            MergeStrategy::MERGE,
            None,
            None,
        ),
        (
            value_map(vec![("country", json!("IN"))]),
            MergeStrategy::REPLACE,
            None,
            None,
        ),
        (Map::new(), MergeStrategy::MERGE, None, None),
        (
            value_map(vec![("country", json!("IN")), ("city", json!("BLR"))]),
            MergeStrategy::MERGE,
            Some(vec!["checkout.".to_string()]),
            None,
        ),
        (
            value_map(vec![("country", json!("IN")), ("city", json!("BLR"))]),
            MergeStrategy::MERGE,
            None,
            Some(vec!["checkout.".to_string()]),
        ),
    ];

    let config = sample_config();

    for (query_data, strategy, prefixes, exclude_prefixes) in cases {
        let borrowed = eval_from(
            &config,
            query_data.clone(),
            strategy,
            prefixes.clone(),
            exclude_prefixes.clone(),
        );
        let owned = eval_config(
            sample_config(),
            query_data.clone(),
            strategy,
            prefixes.clone(),
            exclude_prefixes.clone(),
        );

        assert_eq!(borrowed, owned, "mismatch for query data {query_data:?}");
    }
}

#[test]
fn eval_leaves_the_borrowed_config_reusable() {
    let config = sample_config();
    let query_data = value_map(vec![("country", json!("IN")), ("city", json!("BLR"))]);

    let first = eval_from(
        &config,
        query_data.clone(),
        MergeStrategy::MERGE,
        None,
        None,
    );
    // A second run over the same borrowed data must not observe anything
    // consumed by the first.
    let second = eval_from(
        &config,
        query_data.clone(),
        MergeStrategy::MERGE,
        None,
        None,
    );

    assert_eq!(first, second);
    assert_eq!(config.overrides.len(), 2);
    assert_eq!(config.contexts.len(), 2);
}

#[test]
fn eval_without_matching_context_returns_defaults() {
    let config = sample_config();

    let resolved = eval_from(
        &config,
        value_map(vec![("country", json!("US"))]),
        MergeStrategy::MERGE,
        None,
        None,
    );

    assert_eq!(resolved, config.default_configs.clone().into_inner());
}

#[test]
fn eval_resolves_local_cohort_dimensions() {
    let config = Config {
        default_configs: default_configs(vec![("shipping.free", json!(false))]),
        contexts: vec![context("c0", vec![("tier", json!("metro"))], "o0")],
        overrides: overrides_map(vec![("o0", vec![("shipping.free", json!(true))])]),
        dimensions: cohort_dimensions(),
    };

    let resolved = eval_from(
        &config,
        value_map(vec![("country", json!("US"))]),
        MergeStrategy::MERGE,
        None,
        None,
    );

    assert_eq!(resolved, value_map(vec![("shipping.free", json!(true))]));
}

#[test]
fn eval_filters_keys_by_prefix() {
    let config = sample_config();

    let resolved = eval_from(
        &config,
        value_map(vec![("country", json!("IN")), ("city", json!("BLR"))]),
        MergeStrategy::MERGE,
        Some(vec!["search.".to_string()]),
        None,
    );

    assert_eq!(resolved, value_map(vec![("search.enabled", json!(true))]));
}

#[test]
fn eval_with_empty_config_returns_empty_map() {
    let resolved = eval(
        ExtendedMap::default(),
        &[],
        &HashMap::new(),
        &HashMap::new(),
        value_map(vec![("country", json!("IN"))]),
        MergeStrategy::MERGE,
        None,
        None,
    );

    assert_eq!(resolved, Map::new());
}

// ---------------------------------------------------------------------------
// ConfigRef
// ---------------------------------------------------------------------------

#[test]
fn config_ref_round_trips_through_into_parts_and_from_parts() {
    let config = sample_config();
    let config_ref = ConfigRef {
        default_configs: config.default_configs.clone(),
        contexts: config.contexts.iter().collect(),
        overrides: Cow::Borrowed(&config.overrides),
        dimensions: &config.dimensions,
    };

    let (contexts, overrides, defaults, dimensions) = config_ref.into_parts();
    let rebuilt = ConfigRef::from_parts(contexts, overrides, defaults, dimensions);

    assert_eq!(rebuilt.contexts.len(), config.contexts.len());
    assert_eq!(rebuilt.overrides.len(), config.overrides.len());
    assert_eq!(
        rebuilt.default_configs.clone().into_inner(),
        config.default_configs.clone().into_inner()
    );
    assert!(matches!(rebuilt.overrides, Cow::Borrowed(_)));
}

#[test]
fn config_ref_filter_by_prefix_drops_contexts_left_without_overrides() {
    let config = sample_config();
    let config_ref = ConfigRef {
        default_configs: config.default_configs.clone(),
        contexts: config.contexts.iter().collect(),
        overrides: Cow::Borrowed(&config.overrides),
        dimensions: &config.dimensions,
    };

    let filtered = config_ref.filter_by_prefix(
        &PrefixList::from_iter(vec!["checkout.".to_string()]),
        &PrefixList::new(),
    );

    // Only `c0` overrides a `checkout.*` key, so `c1` is dropped along with its
    // now-empty override.
    assert_eq!(filtered.contexts.len(), 1);
    assert_eq!(filtered.contexts[0].id, "c0");
    assert!(filtered.overrides.contains_key("o0"));
    assert!(!filtered.overrides.contains_key("o1"));
    assert_eq!(
        filtered.default_configs.into_inner(),
        value_map(vec![
            ("checkout.enabled", json!(false)),
            ("checkout.limits", json!({ "min": 1, "max": 10 })),
        ])
    );
}
