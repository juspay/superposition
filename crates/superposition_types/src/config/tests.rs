pub(crate) mod map;

use std::collections::HashMap;

use map::{with_dimensions, without_dimensions};
use serde_json::{from_value, json, Map, Number, Value};

use super::Config;
use crate::{ExtendedMap, PrefixList};

pub(crate) fn get_dimension_data1() -> Map<String, Value> {
    Map::from_iter(vec![(String::from("test3"), Value::Bool(true))])
}

pub(crate) fn get_dimension_data2() -> Map<String, Value> {
    Map::from_iter(vec![
        (String::from("test3"), Value::Bool(false)),
        (String::from("test"), Value::String(String::from("key"))),
    ])
}

pub(crate) fn get_dimension_data3() -> Map<String, Value> {
    Map::from_iter(vec![
        (String::from("test3"), Value::Bool(false)),
        (String::from("test"), Value::String(String::from("key"))),
        (String::from("test2"), Value::Number(Number::from(12))),
    ])
}

pub(crate) fn get_dimension_filtered_config3_with_dimension() -> Config {
    let config_json = json!(  {
        "contexts": [],
        "overrides": {},
        "default_configs": {
            "key1": false,
            "test.test.test1": 1,
            "test.test1": 12,
            "test2.key": false,
            "test2.test": "def_val"
        },
        "dimensions" : {
            "test3": {
                "schema": {
                    "type": "boolean"
                },
                "dimension_type": {"REGULAR": {}},
                "position": 3,
                "dependency_graph": {}
            },
            "test2": {
                "schema": {
                    "type": "integer"
                },
                "dimension_type": {"REGULAR": {}},
                "position": 2,
                "dependency_graph": {}
            },
            "test": {
                "schema": {
                    "pattern": ".*",
                    "type" : "string"
                },
                "dimension_type": {"REGULAR": {}},
                "position": 1,
                "dependency_graph": {}
            }
        }
    });

    from_value(config_json).unwrap()
}

pub(crate) fn get_dimension_filtered_config3_without_dimension() -> Config {
    let config_json = json!(  {
        "contexts": [],
        "overrides": {},
        "default_configs": {
            "key1": false,
            "test.test.test1": 1,
            "test.test1": 12,
            "test2.key": false,
            "test2.test": "def_val"
        }
    });

    from_value(config_json).unwrap()
}

#[test]
fn filter_by_dimensions_with_dimension() {
    let config = with_dimensions::get_config();

    assert_eq!(
        config.clone().filter_by_dimensions(get_dimension_data1()),
        with_dimensions::get_dimension_filtered_config1()
    );

    assert_eq!(
        config.clone().filter_by_dimensions(get_dimension_data2()),
        with_dimensions::get_dimension_filtered_config2()
    );

    assert_eq!(
        config.filter_by_dimensions(get_dimension_data3()),
        get_dimension_filtered_config3_with_dimension()
    );
}

#[test]
fn filter_by_dimensions_without_dimension() {
    let config = without_dimensions::get_config();

    assert_eq!(
        config.clone().filter_by_dimensions(get_dimension_data1()),
        without_dimensions::get_dimension_filtered_config1()
    );

    assert_eq!(
        config.clone().filter_by_dimensions(get_dimension_data2()),
        without_dimensions::get_dimension_filtered_config2()
    );

    assert_eq!(
        config.filter_by_dimensions(get_dimension_data3()),
        get_dimension_filtered_config3_without_dimension()
    );
}

#[test]
fn filter_default_by_prefix_with_dimension() {
    let config = with_dimensions::get_config();

    let prefix_list = PrefixList::from_iter(vec![String::from("test.")]);

    assert_eq!(
        config.filter_default_by_prefix(&prefix_list, &PrefixList::new()),
        json!({
            "test.test.test1": 1,
            "test.test1": 12,
        })
        .as_object()
        .unwrap()
        .clone()
        .into()
    );

    let prefix_list = PrefixList::from_iter(vec![String::from("test3")]);

    assert_eq!(
        config.filter_default_by_prefix(&prefix_list, &PrefixList::new()),
        ExtendedMap(Map::new())
    );
}

#[test]
fn filter_default_by_prefix_without_dimension() {
    let config = without_dimensions::get_config();

    let prefix_list = PrefixList::from_iter(vec![String::from("test.")]);

    assert_eq!(
        config.filter_default_by_prefix(&prefix_list, &PrefixList::new()),
        json!({
            "test.test.test1": 1,
            "test.test1": 12,
        })
        .as_object()
        .unwrap()
        .clone()
        .into()
    );

    let prefix_list = PrefixList::from_iter(vec![String::from("test3")]);

    assert_eq!(
        config.filter_default_by_prefix(&prefix_list, &PrefixList::new()),
        ExtendedMap(Map::new())
    );
}

#[test]
fn filter_by_prefix_with_dimension() {
    let config = with_dimensions::get_config();

    let prefix_list = PrefixList::from_iter(vec![String::from("test.")]);

    assert_eq!(
        config
            .clone()
            .filter_by_prefix(&prefix_list, &PrefixList::new()),
        with_dimensions::get_prefix_filtered_config1()
    );

    let prefix_list =
        PrefixList::from_iter(vec![String::from("test."), String::from("test2.")]);

    assert_eq!(
        config
            .clone()
            .filter_by_prefix(&prefix_list, &PrefixList::new()),
        with_dimensions::get_prefix_filtered_config2()
    );

    let prefix_list = PrefixList::from_iter(vec![String::from("abcd")]);

    let dimensions = config.dimensions.clone();
    assert_eq!(
        config.filter_by_prefix(&prefix_list, &PrefixList::new()),
        Config {
            contexts: Vec::new(),
            overrides: HashMap::new(),
            default_configs: Map::new().into(),
            dimensions,
        }
    );
}

#[test]
fn filter_by_prefix_without_dimension() {
    let config = without_dimensions::get_config();

    let prefix_list = PrefixList::from_iter(vec![String::from("test.")]);

    assert_eq!(
        config
            .clone()
            .filter_by_prefix(&prefix_list, &PrefixList::new()),
        without_dimensions::get_prefix_filtered_config1()
    );

    let prefix_list =
        PrefixList::from_iter(vec![String::from("test."), String::from("test2.")]);

    assert_eq!(
        config
            .clone()
            .filter_by_prefix(&prefix_list, &PrefixList::new()),
        without_dimensions::get_prefix_filtered_config2()
    );

    let prefix_list = PrefixList::from_iter(vec![String::from("abcd")]);

    let dimensions = config.dimensions.clone();
    assert_eq!(
        config.filter_by_prefix(&prefix_list, &PrefixList::new()),
        Config {
            contexts: Vec::new(),
            overrides: HashMap::new(),
            default_configs: Map::new().into(),
            dimensions,
        }
    );
}

#[test]
fn filter_default_by_prefix_with_exclude() {
    let config = without_dimensions::get_config();

    // Exclude-only: empty allow-list keeps every key not matching an exclude.
    let exclude_list = PrefixList::from_iter(vec![String::from("test2.")]);
    assert_eq!(
        config.filter_default_by_prefix(&PrefixList::new(), &exclude_list),
        json!({
            "key1": false,
            "test.test.test1": 1,
            "test.test1": 12,
        })
        .as_object()
        .unwrap()
        .clone()
        .into()
    );

    // Allow + exclude combined: allow `test.`, then drop the `test.test.` subtree.
    let prefix_list = PrefixList::from_iter(vec![String::from("test.")]);
    let exclude_list = PrefixList::from_iter(vec![String::from("test.test.")]);
    assert_eq!(
        config.filter_default_by_prefix(&prefix_list, &exclude_list),
        json!({ "test.test1": 12 })
            .as_object()
            .unwrap()
            .clone()
            .into()
    );
}

#[test]
fn filter_by_excluded_prefix_removes_only_matching_keys() {
    let config = without_dimensions::get_config();
    let prefix_list =
        PrefixList::from_iter([String::from("test."), String::from("test2.")]);

    let filtered = config.filter_by_prefix(&PrefixList::new(), &prefix_list);

    assert_eq!(
        filtered.default_configs,
        json!({ "key1": false }).as_object().unwrap().clone().into()
    );
    assert_eq!(filtered.contexts.len(), 1);
    assert_eq!(filtered.overrides.len(), 1);
    assert!(filtered
        .overrides
        .values()
        .all(|overrides| overrides.contains_key("key1")));
}

#[test]
fn filter_by_excluded_prefix_keeps_non_matching_keys_and_empty_filter_is_noop() {
    let config = with_dimensions::get_config();

    let non_matching = PrefixList::from_iter([String::from("unknown.")]);
    assert_eq!(
        config
            .clone()
            .filter_by_prefix(&PrefixList::new(), &non_matching),
        config
    );

    assert_eq!(
        config
            .clone()
            .filter_by_prefix(&PrefixList::new(), &PrefixList::new()),
        config
    );
}

#[test]
fn prefix_allow_list_is_applied_before_excluded_prefixes() {
    let config = without_dimensions::get_config();
    let allowed = PrefixList::from_iter([String::from("test.")]);
    let excluded = PrefixList::from_iter([String::from("test.test.")]);

    let filtered = config.filter_by_prefix(&allowed, &excluded);

    assert_eq!(
        filtered.default_configs,
        json!({ "test.test1": 12 })
            .as_object()
            .unwrap()
            .clone()
            .into()
    );
    assert_eq!(filtered.contexts.len(), 1);
}

#[test]
fn filter_by_prefix_ignores_blank_prefixes() {
    // Blank/whitespace-only entries (e.g. from a trailing comma in the query
    // string) must be treated as absent: a blank exclude must not wipe the
    // config, and a blank allow-list must still mean "allow everything".
    let config = without_dimensions::get_config();

    // Blank-only allow-list + exclude with a blank alongside a real prefix.
    let allow = PrefixList::from_iter([String::new(), String::from("   ")]);
    let exclude = PrefixList::from_iter([String::new(), String::from("test.")]);

    let filtered = config.clone().filter_by_prefix(&allow, &exclude);

    // "test." keys dropped by the real exclude; everything else retained.
    assert_eq!(
        filtered.default_configs,
        json!({
            "key1": false,
            "test2.key": false,
            "test2.test": "def_val",
        })
        .as_object()
        .unwrap()
        .clone()
        .into()
    );

    // Blank-only lists on both sides are a complete no-op.
    let blanks = PrefixList::from_iter([String::new(), String::from("  ")]);
    assert_eq!(config.clone().filter_by_prefix(&blanks, &blanks), config);
}

#[test]
fn excluding_the_allowed_prefix_empties_the_config() {
    // When the exclude-list covers everything the allow-list permitted, the
    // result has no default keys and every now-empty context is dropped.
    let config = without_dimensions::get_config();
    let allowed = PrefixList::from_iter([String::from("test.")]);
    let excluded = PrefixList::from_iter([String::from("test.")]);

    let dimensions = config.dimensions.clone();
    assert_eq!(
        config.filter_by_prefix(&allowed, &excluded),
        Config {
            contexts: Vec::new(),
            overrides: HashMap::new(),
            default_configs: Map::new().into(),
            dimensions,
        }
    );
}
