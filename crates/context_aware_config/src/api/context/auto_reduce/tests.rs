//! Tests for auto-reduce.

use std::collections::HashMap;

use bigdecimal::BigDecimal;
use chrono::Utc;
use serde_json::{Map, Value, json};
use service_utils::service::types::{OrganisationId, WorkspaceId};
use superposition_types::{
    Cac, Condition, Config, Context, ExtendedMap, OverrideWithKeys, Overrides,
    database::models::{
        ChangeReason, Description, Metrics, NonEmptyString, Workspace, WorkspaceStatus,
        cac::Context as DbContext,
    },
};

use super::*;

// helpers

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

/// List contexts lowest weight first, the order resolution applies them in.
fn context(id: &str, conditions: Vec<(&str, Value)>, override_key: &str) -> Context {
    Context {
        id: id.to_string(),
        condition: condition(conditions),
        priority: 0,
        weight: 0,
        override_with_keys: OverrideWithKeys::new(override_key.to_string()),
    }
}

fn config(
    contexts: Vec<Context>,
    override_entries: Vec<(&str, Vec<(&str, Value)>)>,
    defaults: Vec<(&str, Value)>,
) -> Config {
    let overrides_map: HashMap<String, Overrides> = override_entries
        .into_iter()
        .map(|(key, values)| (key.to_string(), overrides(values)))
        .collect();
    Config {
        contexts,
        overrides: overrides_map,
        default_configs: ExtendedMap::from(value_map(defaults)),
        dimensions: HashMap::new(),
    }
}

fn reducer(
    contexts: Vec<Context>,
    override_entries: Vec<(&str, Vec<(&str, Value)>)>,
    defaults: Vec<(&str, Value)>,
) -> AutoReducer {
    AutoReducer::from_config(config(contexts, override_entries, defaults))
}

fn db_context(
    id: &str,
    conditions: Vec<(&str, Value)>,
    override_values: Vec<(&str, Value)>,
) -> DbContext {
    let now = Utc::now();
    DbContext {
        id: id.to_string(),
        value: condition(conditions),
        override_id: "override-id".to_string(),
        created_at: now,
        created_by: "test@superposition.io".to_string(),
        override_: overrides(override_values),
        last_modified_at: now,
        last_modified_by: "test@superposition.io".to_string(),
        weight: BigDecimal::from(1),
        description: Description::try_from("test".to_string()).unwrap(),
        change_reason: ChangeReason::try_from("test".to_string()).unwrap(),
    }
}

fn schema() -> SchemaName {
    SchemaName("test".to_string())
}

// reduce

#[test]
fn drops_a_key_a_broader_context_already_resolves_to_the_same_value() {
    let reducer = reducer(
        vec![context("broad", vec![("os", json!("android"))], "o1")],
        vec![("o1", vec![("colour", json!("red"))])],
        vec![("colour", json!("blue"))],
    );

    let reduction = reducer.reduce(
        "new",
        &condition(vec![("os", json!("android")), ("city", json!("bangalore"))]),
        &overrides(vec![("colour", json!("red"))]),
    );

    assert_eq!(reduction.dropped, vec!["colour".to_string()]);
    assert!(reduction.kept.is_empty());
    assert!(reduction.is_fully_redundant());
}

#[test]
fn keeps_a_key_that_differs_from_what_the_condition_resolves_to() {
    let reducer = reducer(
        vec![context("broad", vec![("os", json!("android"))], "o1")],
        vec![("o1", vec![("colour", json!("red"))])],
        vec![("colour", json!("blue"))],
    );

    let reduction = reducer.reduce(
        "new",
        &condition(vec![("os", json!("android")), ("city", json!("bangalore"))]),
        &overrides(vec![("colour", json!("green"))]),
    );

    assert!(reduction.dropped.is_empty());
    assert_eq!(reduction.kept, value_map(vec![("colour", json!("green"))]));
    assert!(!reduction.is_fully_redundant());
}

#[test]
fn drops_a_key_that_only_restates_the_default_config() {
    let reducer = reducer(vec![], vec![], vec![("colour", json!("blue"))]);

    let reduction = reducer.reduce(
        "new",
        &condition(vec![("os", json!("android"))]),
        &overrides(vec![("colour", json!("blue"))]),
    );

    assert_eq!(reduction.dropped, vec!["colour".to_string()]);
    assert!(reduction.is_fully_redundant());
}

#[test]
fn keeps_only_the_redundant_half_of_a_mixed_override() {
    let reducer = reducer(
        vec![context("broad", vec![("os", json!("android"))], "o1")],
        vec![("o1", vec![("colour", json!("red"))])],
        vec![("colour", json!("blue")), ("size", json!(10))],
    );

    let reduction = reducer.reduce(
        "new",
        &condition(vec![("os", json!("android")), ("city", json!("bangalore"))]),
        &overrides(vec![("colour", json!("red")), ("size", json!(42))]),
    );

    assert_eq!(reduction.dropped, vec!["colour".to_string()]);
    assert_eq!(reduction.kept, value_map(vec![("size", json!(42))]));
    assert!(!reduction.is_fully_redundant());
}

#[test]
fn a_context_is_excluded_from_the_resolution_that_judges_it() {
    // Left in, "self" would be the highest-weight exact match and match itself.
    let reducer = reducer(
        vec![context("self", vec![("os", json!("android"))], "o1")],
        vec![("o1", vec![("colour", json!("red"))])],
        vec![("colour", json!("blue"))],
    );

    let reduction = reducer.reduce(
        "self",
        &condition(vec![("os", json!("android"))]),
        &overrides(vec![("colour", json!("red"))]),
    );

    assert!(reduction.dropped.is_empty());
    assert_eq!(reduction.kept, value_map(vec![("colour", json!("red"))]));
}

#[test]
fn keeps_a_key_that_is_redundant_under_only_one_merge_strategy() {
    // REPLACE resolves to {"b":2} (redundant), MERGE to {"a":1,"b":2} (not).
    let reducer = reducer(
        vec![
            context("low", vec![("os", json!("android"))], "o1"),
            context(
                "high",
                vec![("os", json!("android")), ("city", json!("bangalore"))],
                "o2",
            ),
        ],
        vec![
            ("o1", vec![("flags", json!({"a": 1}))]),
            ("o2", vec![("flags", json!({"b": 2}))]),
        ],
        vec![("flags", json!({}))],
    );

    let reduction = reducer.reduce(
        "new",
        &condition(vec![
            ("os", json!("android")),
            ("city", json!("bangalore")),
            ("tier", json!("t1")),
        ]),
        &overrides(vec![("flags", json!({"b": 2}))]),
    );

    assert!(
        reduction.dropped.is_empty(),
        "a key that is load-bearing under MERGE must survive"
    );
    assert_eq!(reduction.kept, value_map(vec![("flags", json!({"b": 2}))]));
}

// apply

#[test]
fn apply_skips_variant_contexts() {
    // Trimming would leave the experiment pointing at a stale override.
    let reducer = reducer(vec![], vec![], vec![("colour", json!("blue"))]);
    let ctx = db_context(
        "variant-ctx",
        vec![("os", json!("android")), ("variantIds", json!("variant-1"))],
        vec![("colour", json!("blue"))],
    );
    let original = ctx.override_.clone();

    match apply(Some(&reducer), ctx, &schema()).unwrap() {
        ReducedContext::Unchanged(ctx) => assert_eq!(ctx.override_, original),
        _ => panic!("variant contexts must never be reduced"),
    }
}

#[test]
fn apply_trims_the_override_and_rehashes_the_override_id() {
    let reducer = reducer(
        vec![context("broad", vec![("os", json!("android"))], "o1")],
        vec![("o1", vec![("colour", json!("red"))])],
        vec![("colour", json!("blue")), ("size", json!(10))],
    );
    let ctx = db_context(
        "new",
        vec![("os", json!("android")), ("city", json!("bangalore"))],
        vec![("colour", json!("red")), ("size", json!(42))],
    );
    let stale_override_id = ctx.override_id.clone();

    match apply(Some(&reducer), ctx, &schema()).unwrap() {
        ReducedContext::Trimmed { context, dropped } => {
            assert_eq!(dropped, vec!["colour".to_string()]);
            assert_eq!(
                context.override_,
                overrides(vec![("size", json!(42))]),
                "the redundant key must be gone from what gets written"
            );
            assert_ne!(
                context.override_id, stale_override_id,
                "override_id is what resolution looks the override up by, so it \
                 must be re-hashed for the trimmed map"
            );
            assert_eq!(
                context.override_id,
                superposition_core::helpers::hash(&Value::Object(value_map(vec![(
                    "size",
                    json!(42)
                )])))
            );
        }
        _ => panic!("expected the context to be trimmed"),
    }
}

#[test]
fn apply_reports_a_fully_redundant_context_without_touching_it() {
    let reducer = reducer(
        vec![context("broad", vec![("os", json!("android"))], "o1")],
        vec![("o1", vec![("colour", json!("red"))])],
        vec![("colour", json!("blue"))],
    );
    let ctx = db_context(
        "new",
        vec![("os", json!("android")), ("city", json!("bangalore"))],
        vec![("colour", json!("red"))],
    );

    match apply(Some(&reducer), ctx, &schema()).unwrap() {
        ReducedContext::FullyRedundant { context, dropped } => {
            assert_eq!(dropped, vec!["colour".to_string()]);
            assert_eq!(
                context.override_,
                overrides(vec![("colour", json!("red"))]),
                "the reported context keeps what was submitted; it is just not written"
            );
        }
        _ => panic!("expected the context to be reported as fully redundant"),
    }
}

// is_enabled

fn workspace_context(setting: bool) -> WorkspaceContext {
    let now = Utc::now();
    WorkspaceContext {
        workspace_id: WorkspaceId("test".to_string()),
        organisation_id: OrganisationId("org".to_string()),
        schema_name: schema(),
        settings: Workspace {
            organisation_id: "org".to_string(),
            organisation_name: NonEmptyString::try_from("org".to_string()).unwrap(),
            workspace_name: "test".to_string(),
            workspace_schema_name: "test".to_string(),
            workspace_status: WorkspaceStatus::ENABLED,
            workspace_admin_email: "test@superposition.io".to_string(),
            config_version: None,
            created_by: "test@superposition.io".to_string(),
            last_modified_by: "test@superposition.io".to_string(),
            last_modified_at: now,
            created_at: now,
            mandatory_dimensions: None,
            metrics: Metrics::default(),
            allow_experiment_self_approval: false,
            auto_populate_control: false,
            enable_context_validation: false,
            enable_change_reason_validation: false,
            enable_auto_reduce: setting,
            encryption_key: String::new(),
            key_rotated_at: None,
            workspace_lock_id: None,
            workspace_lock_operation: None,
            workspace_locked_by: None,
            workspace_lock_acquired_at: None,
            workspace_lock_expires_at: None,
        },
    }
}

#[test]
fn no_header_falls_back_to_the_workspace_setting() {
    assert!(is_enabled(None, &workspace_context(true)));
    assert!(!is_enabled(None, &workspace_context(false)));
}

#[test]
fn the_header_overrides_the_workspace_setting_both_ways() {
    // What experimentation relies on: header false wins over a workspace set true.
    assert!(!is_enabled(Some(false), &workspace_context(true)));
    assert!(is_enabled(Some(true), &workspace_context(false)));
}
