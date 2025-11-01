use std::collections::HashMap;

use crate::{
    logic::{
        dimensions_to_start_from, evaluate_local_cohort_dimension, evaluate_local_cohorts,
    },
    DimensionInfo,
};
use serde_json::{json, Map};

#[test]
// Tests dimensions_to_start_from when query data exists for dimensions at different levels
// Verifies "closest to root" selection and LocalCohort fallback behavior
fn test_dimensions_to_start_from_with_query_data() {
    let dimensions_json = json!({
        "test": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 1,
            "dependency_graph": {
                "test_remote": ["testdep"],
                "test": ["testtest", "test_remote"],
                "testdep": [],
                "testtest": []
            }
        },
        "test_remote": {
            "dimension_type": { "REMOTE_COHORT": "remote_endpoint" },
            "schema": {},
            "position": 2,
            "dependency_graph": {
                "test_remote": ["testdep"],
                "testdep": []
            }
        },
        "testdep": {
            "dimension_type": { "LOCAL_COHORT": "test_remote" },
            "schema": {},
            "position": 3,
            "dependency_graph": {
                "testdep": []
            }
        },
        "testtest": {
            "dimension_type": { "LOCAL_COHORT": "test" },
            "schema": {},
            "position": 4,
            "dependency_graph": {
                "testtest": []
            }
        }
    });

    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();

    let query_data = json!({
        "testtest": "value1"
    })
    .as_object()
    .unwrap()
    .clone();

    let start_dims = dimensions_to_start_from(&dimensions, &query_data);
    assert_eq!(start_dims.len(), 2);
    assert!(start_dims.contains(&"testtest".to_string()));
    assert!(start_dims.contains(&"testdep".to_string()));
}

#[test]
// Tests LocalCohort selection as starting point when no query data is available
// Verifies fallback behavior when traversing from Regular dimension to LocalCohort
fn test_dimensions_to_start_from_with_local_cohort() {
    let dimensions_json = json!({
        "test": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 1,
            "dependency_graph": {
                "test": ["testdep"],
                "testdep": []
            }
        },
        "testdep": {
            "dimension_type": { "LOCAL_COHORT": "test_remote" },
            "schema": {},
            "position": 2,
            "dependency_graph": {
                "testdep": []
            }
        }
    });

    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();

    let query_data = Map::new();

    let start_dims = dimensions_to_start_from(&dimensions, &query_data);
    assert_eq!(start_dims, vec!["testdep".to_string()]);
}

#[test]
// Tests behavior with empty dimensions HashMap
// Verifies graceful handling of edge case with no dimension configuration
fn test_dimensions_to_start_from_empty_dimensions() {
    let dimensions_json = json!({});
    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();
    let query_data = json!({
        "some_key": "some_value"
    })
    .as_object()
    .unwrap()
    .clone();

    let start_dims = dimensions_to_start_from(&dimensions, &query_data);
    assert_eq!(start_dims, Vec::<String>::new());
}

#[test]
// Tests processing of multiple independent dependency trees
// Verifies algorithm correctly handles separate root dimensions and their branches
fn test_dimensions_to_start_from_multiple_trees() {
    let dimensions_json = json!({
        "dim1": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 1,
            "dependency_graph": {
                "dim1": ["dim2"],
                "dim2": []
            }
        },
        "dim2": {
            "dimension_type": { "LOCAL_COHORT": "cohort_based_on" },
            "schema": {},
            "position": 2,
            "dependency_graph": {
                "dim2": []
            }
        },
        "dim3": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 3,
            "dependency_graph": {
                "dim3": ["dim4"],
                "dim4": []
            }
        },
        "dim4": {
            "dimension_type": { "LOCAL_COHORT": "some_var" },
            "schema": {},
            "position": 4,
            "dependency_graph": {
                "dim4": []
            }
        }
    });

    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();

    let query_data = json!({
        "dim1": "value1",
        "dim4": "value4"
    })
    .as_object()
    .unwrap()
    .clone();

    let start_dims = dimensions_to_start_from(&dimensions, &query_data);
    assert_eq!(start_dims.len(), 2);
    assert!(start_dims.contains(&"dim1".to_string()));
    assert!(start_dims.contains(&"dim4".to_string()));
}

#[test]
// Tests evaluate_local_cohorts with empty dimensions HashMap
// Verifies function returns query_data unchanged when no dimensions are configured
fn test_evaluate_local_cohorts_empty_dimensions() {
    let dimensions_json = json!({});
    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();
    let query_data = json!({
        "key1": "value1"
    })
    .as_object()
    .unwrap()
    .clone();

    let result = evaluate_local_cohorts(&dimensions, &query_data);
    assert_eq!(result, query_data);
}

#[test]
// Tests evaluate_local_cohorts with Regular dimension
// Verifies Regular dimensions pass through their query_data values unchanged
fn test_evaluate_local_cohorts_with_regular_dimension() {
    let dimensions_json = json!({
        "regular_dim": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 1,
            "dependency_graph": {
                "regular_dim": []
            }
        }
    });

    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();

    let query_data = json!({
        "regular_dim": "value1"
    })
    .as_object()
    .unwrap()
    .clone();

    let result = evaluate_local_cohorts(&dimensions, &query_data);
    assert_eq!(result.get("regular_dim"), Some(&json!("value1")));
}

#[test]
// Tests LocalCohort evaluation defaulting to "otherwise" when no query data matches conditions
// Verifies fallback behavior when JSON Logic conditions don't match
fn test_evaluate_local_cohorts_with_local_cohort_otherwise() {
    let dimensions_json = json!({
        "local_cohort_dim": {
            "dimension_type": { "LOCAL_COHORT": "user_id" },
            "schema": {
                "enum": ["cohort1", "cohort2", "otherwise"],
                "definitions": {
                    "cohort1": {
                        "==": [{"var": "user_id"}, 123]
                    },
                    "cohort2": {
                        "==": [{"var": "user_id"}, 456]
                    }
                }
            },
            "position": 1,
            "dependency_graph": {
                "local_cohort_dim": []
            }
        }
    });

    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();

    let query_data = Map::new();

    let result = evaluate_local_cohorts(&dimensions, &query_data);
    assert_eq!(result.get("local_cohort_dim"), Some(&json!("otherwise")));
}

#[test]
// Tests dependency evaluation chain: Regular dimension -> LocalCohort
// Verifies LocalCohort evaluates using parent dimension's value via JSON Logic
fn test_evaluate_local_cohorts_with_dependency() {
    let dimensions_json = json!({
        "test": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 1,
            "dependency_graph": {
                "test": ["testdep"],
                "testdep": []
            }
        },
        "testdep": {
            "dimension_type": { "LOCAL_COHORT": "test" },
            "schema": {
                "enum": ["cohort1", "cohort2", "otherwise"],
                "definitions": {
                    "cohort1": {
                        "==": [{"var": "test"}, "trigger_value"]
                    },
                    "cohort2": {
                        "==": [{"var": "test"}, "other_value"]
                    }
                }
            },
            "position": 2,
            "dependency_graph": {
                "testdep": []
            }
        }
    });

    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();

    let query_data = json!({
        "test": "trigger_value"
    })
    .as_object()
    .unwrap()
    .clone();

    let result = evaluate_local_cohorts(&dimensions, &query_data);
    assert_eq!(result.get("test"), Some(&json!("trigger_value")));
    assert_eq!(result.get("testdep"), Some(&json!("cohort1")));
}

#[test]
// Tests direct evaluation of evaluate_local_cohort_dimension function
// Verifies JSON Logic evaluation with matching and non-matching conditions
fn test_evaluate_local_cohort_dimension_function() {
    let schema = json!({
        "enum": ["exactly_42", "otherwise"],
        "definitions": {
            "exactly_42": {
                "==": [{"var": "test_value"}, 42]
            }
        }
    })
    .as_object()
    .unwrap()
    .clone();

    let result = evaluate_local_cohort_dimension("test_value", &json!(42), &schema);
    assert_eq!(result, "exactly_42");

    let result = evaluate_local_cohort_dimension("test_value", &json!(100), &schema);
    assert_eq!(result, "otherwise");

    let result = evaluate_local_cohort_dimension("test_value", &json!("string"), &schema);
    assert_eq!(result, "otherwise");
}

#[test]
// Tests complex real-world dependency graph scenario
// Verifies complete end-to-end behavior with multiple dimension types and dependency chains
fn test_sample_dependency_graph() {
    let dimensions_json = json!({
        "test": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 1,
            "dependency_graph": {
                "test_remote": ["testdep"],
                "test": ["testtest", "test_remote"],
                "testdep": [],
                "testtest": []
            }
        },
        "test_remote": {
            "dimension_type": { "REMOTE_COHORT": "remote_endpoint" },
            "schema": {},
            "position": 2,
            "dependency_graph": {
                "test_remote": ["testdep"],
                "testdep": []
            }
        },
        "testdep": {
            "dimension_type": { "LOCAL_COHORT": "test_remote" },
            "schema": {
                "enum": ["group_a", "group_b", "otherwise"],
                "definitions": {
                    "group_a": {
                        "==": [{"var": "test_remote"}, "remote_value_a"]
                    },
                    "group_b": {
                        "==": [{"var": "test_remote"}, "remote_value_b"]
                    }
                }
            },
            "position": 3,
            "dependency_graph": {
                "testdep": []
            }
        },
        "testtest": {
            "dimension_type": { "LOCAL_COHORT": "test" },
            "schema": {},
            "position": 4,
            "dependency_graph": {
                "testtest": []
            }
        }
    });

    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();

    let query_data = json!({
        "test": "main_value",
        "test_remote": "remote_value_a"
    })
    .as_object()
    .unwrap()
    .clone();

    let start_dims = dimensions_to_start_from(&dimensions, &query_data);
    assert_eq!(start_dims.len(), 1);
    assert!(start_dims.contains(&"test".to_string()));

    let result = evaluate_local_cohorts(&dimensions, &query_data);
    assert_eq!(result.get("test"), Some(&json!("main_value")));
    assert_eq!(result.get("test_remote"), Some(&json!("remote_value_a")));
    assert_eq!(result.get("testdep"), Some(&json!("group_a")));
    assert_eq!(result.get("testtest"), Some(&json!("otherwise")));

    let query_data2 = json!({
        "testtest": "leaf_value"
    })
    .as_object()
    .unwrap()
    .clone();

    let start_dims2 = dimensions_to_start_from(&dimensions, &query_data2);
    assert_eq!(start_dims2.len(), 2);
    assert!(start_dims2.contains(&"testtest".to_string()));
    assert!(start_dims2.contains(&"testdep".to_string()));

    let result2 = evaluate_local_cohorts(&dimensions, &query_data2);
    assert_eq!(result2.get("testtest"), Some(&json!("leaf_value")));
    assert_eq!(result2.get("testdep"), Some(&json!("otherwise")));
}

#[test]
// Tests algorithm doesn't return duplicate dimensions in starting points
// Verifies deduplication logic when processing multiple independent trees
fn test_no_duplicates_in_dimensions_to_start_from() {
    let dimensions_json = json!({
        "root1": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 1,
            "dependency_graph": {
                "root1": ["dep1"],
                "dep1": []
            }
        },
        "root2": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 2,
            "dependency_graph": {
                "root2": ["dep2"],
                "dep2": []
            }
        },
        "dep1": {
            "dimension_type": { "LOCAL_COHORT": "some_var" },
            "schema": {},
            "position": 3,
            "dependency_graph": {
                "dep1": []
            }
        },
        "dep2": {
            "dimension_type": { "LOCAL_COHORT": "some_other_var" },
            "schema": {},
            "position": 4,
            "dependency_graph": {
                "dep2": []
            }
        }
    });

    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();

    let query_data = json!({
        "dep1": "some_value",
        "dep2": "another_value"
    })
    .as_object()
    .unwrap()
    .clone();

    let start_dims = dimensions_to_start_from(&dimensions, &query_data);
    assert_eq!(start_dims.len(), 2);
    assert!(start_dims.contains(&"dep1".to_string()));
    assert!(start_dims.contains(&"dep2".to_string()));
}

#[test]
// Tests deep hierarchical dependency chains with query data at different levels
// Verifies "closest to root" selection across 4-level deep tree with mixed dimension types
fn test_dimensions_to_start_from_deep_tree_with_query_at_different_levels() {
    let dimensions_json = json!({
        "root": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 1,
            "dependency_graph": {
                "root": ["level1"],
                "level1": ["level2"],
                "level2": ["level3"],
                "level3": []
            }
        },
        "level1": {
            "dimension_type": { "REMOTE_COHORT": "remote_endpoint" },
            "schema": {},
            "position": 2,
            "dependency_graph": {
                "level1": ["level2"],
                "level2": ["level3"],
                "level3": []
            }
        },
        "level2": {
            "dimension_type": { "REMOTE_COHORT": "another_endpoint" },
            "schema": {},
            "position": 3,
            "dependency_graph": {
                "level2": ["level3"],
                "level3": []
            }
        },
        "level3": {
            "dimension_type": { "LOCAL_COHORT": "level2" },
            "schema": {},
            "position": 4,
            "dependency_graph": {
                "level3": []
            }
        }
    });

    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();

    // Test: Query data at root level - should select root
    let query_data1 = json!({
        "root": "root_value"
    })
    .as_object()
    .unwrap()
    .clone();

    let start_dims1 = dimensions_to_start_from(&dimensions, &query_data1);
    assert_eq!(start_dims1, vec!["root".to_string()]);

    // Test: Query data at middle level - should select middle level (closest to root)
    let query_data2 = json!({
        "level1": "level1_value",
        "level3": "level3_value"
    })
    .as_object()
    .unwrap()
    .clone();

    let start_dims2 = dimensions_to_start_from(&dimensions, &query_data2);
    assert_eq!(start_dims2, vec!["level1".to_string()]);

    // Test: Query data only at leaf level - should select leaf
    let query_data3 = json!({
        "level3": "level3_value"
    })
    .as_object()
    .unwrap()
    .clone();

    let start_dims3 = dimensions_to_start_from(&dimensions, &query_data3);
    assert_eq!(start_dims3, vec!["level3".to_string()]);

    // Test: No query data - should select LocalCohort fallback
    let query_data4 = Map::new();

    let start_dims4 = dimensions_to_start_from(&dimensions, &query_data4);
    assert_eq!(start_dims4, vec!["level3".to_string()]);
}

#[test]
// Tests behavior when Regular dimension has empty dependency graph
// Verifies algorithm handles missing dependency information gracefully
fn test_dimensions_to_start_from_missing_dependency_graph() {
    let dimensions_json = json!({
        "root": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 1,
            "dependency_graph": {}
        }
    });

    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();

    let query_data = json!({
        "nonexistent": "value"
    })
    .as_object()
    .unwrap()
    .clone();

    let start_dims = dimensions_to_start_from(&dimensions, &query_data);
    // When no dependencies and no query data matches, algorithm doesn't traverse further
    // and doesn't add the root as a starting point
    assert_eq!(start_dims, Vec::<String>::new());

    // Test when root itself has query data
    let query_data_with_root = json!({
        "root": "root_value"
    })
    .as_object()
    .unwrap()
    .clone();

    let start_dims_with_root =
        dimensions_to_start_from(&dimensions, &query_data_with_root);
    assert_eq!(start_dims_with_root, vec!["root".to_string()]);
}

#[test]
// Tests LocalCohort evaluation with malformed schema missing "definitions" field
// Verifies graceful fallback to "otherwise" when schema is incomplete
fn test_evaluate_local_cohorts_missing_schema_definitions() {
    let dimensions_json = json!({
        "root": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 1,
            "dependency_graph": {
                "root": ["local_cohort"],
                "local_cohort": []
            }
        },
        "local_cohort": {
            "dimension_type": { "LOCAL_COHORT": "root" },
            "schema": {
                "enum": ["group1", "otherwise"]
                // Missing "definitions" field
            },
            "position": 2,
            "dependency_graph": {
                "local_cohort": []
            }
        }
    });

    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();

    let query_data = json!({
        "root": "some_value"
    })
    .as_object()
    .unwrap()
    .clone();

    let result = evaluate_local_cohorts(&dimensions, &query_data);
    assert_eq!(result.get("root"), Some(&json!("some_value")));
    assert_eq!(result.get("local_cohort"), Some(&json!("otherwise")));
}

#[test]
// Tests LocalCohort evaluation with invalid JSON Logic expressions
// Verifies error handling defaults to "otherwise" when JSON Logic evaluation fails
fn test_evaluate_local_cohorts_invalid_json_logic() {
    let dimensions_json = json!({
        "root": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 1,
            "dependency_graph": {
                "root": ["local_cohort"],
                "local_cohort": []
            }
        },
        "local_cohort": {
            "dimension_type": { "LOCAL_COHORT": "root" },
            "schema": {
                "enum": ["group1", "otherwise"],
                "definitions": {
                    "group1": {
                        "invalid_operator": [{"var": "root"}, "trigger_value"]
                    }
                }
            },
            "position": 2,
            "dependency_graph": {
                "local_cohort": []
            }
        }
    });

    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();

    let query_data = json!({
        "root": "trigger_value"
    })
    .as_object()
    .unwrap()
    .clone();

    let result = evaluate_local_cohorts(&dimensions, &query_data);
    assert_eq!(result.get("root"), Some(&json!("trigger_value")));
    assert_eq!(result.get("local_cohort"), Some(&json!("otherwise")));
}

#[test]
// Tests LocalCohort evaluation with complex JSON Logic using numerical comparisons
// Verifies advanced conditional logic with range-based cohort assignment
fn test_evaluate_local_cohorts_complex_json_logic() {
    let dimensions_json = json!({
        "user": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 1,
            "dependency_graph": {
                "user": ["cohort"],
                "cohort": []
            }
        },
        "cohort": {
            "dimension_type": { "LOCAL_COHORT": "user" },
            "schema": {
                "enum": ["premium", "standard", "basic", "otherwise"],
                "definitions": {
                    "premium": {
                        "and": [
                            {">=": [{"var": "user"}, 1000]},
                            {"<": [{"var": "user"}, 10000]}
                        ]
                    },
                    "standard": {
                        "and": [
                            {">=": [{"var": "user"}, 100]},
                            {"<": [{"var": "user"}, 1000]}
                        ]
                    },
                    "basic": {
                        "and": [
                            {">=": [{"var": "user"}, 1]},
                            {"<": [{"var": "user"}, 100]}
                        ]
                    }
                }
            },
            "position": 2,
            "dependency_graph": {
                "cohort": []
            }
        }
    });

    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();

    let test_cases = vec![
        (5000, "premium"),
        (500, "standard"),
        (50, "basic"),
        (50000, "otherwise"),
        (0, "otherwise"),
    ];

    for (user_value, expected_cohort) in test_cases {
        let query_data = json!({
            "user": user_value
        })
        .as_object()
        .unwrap()
        .clone();

        let result = evaluate_local_cohorts(&dimensions, &query_data);
        assert_eq!(result.get("user"), Some(&json!(user_value)));
        assert_eq!(result.get("cohort"), Some(&json!(expected_cohort)));
    }
}

#[test]
// Tests RemoteCohort behavior as passthrough node in dependency chain
// Verifies RemoteCohorts preserve query_data values and enable LocalCohort evaluation
fn test_evaluate_local_cohorts_remote_cohort_passthrough() {
    let dimensions_json = json!({
        "root": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 1,
            "dependency_graph": {
                "root": ["remote"],
                "remote": ["local"],
                "local": []
            }
        },
        "remote": {
            "dimension_type": { "REMOTE_COHORT": "endpoint" },
            "schema": {},
            "position": 2,
            "dependency_graph": {
                "remote": ["local"],
                "local": []
            }
        },
        "local": {
            "dimension_type": { "LOCAL_COHORT": "remote" },
            "schema": {
                "enum": ["group_a", "group_b", "otherwise"],
                "definitions": {
                    "group_a": {
                        "==": [{"var": "remote"}, "remote_value_a"]
                    },
                    "group_b": {
                        "==": [{"var": "remote"}, "remote_value_b"]
                    }
                }
            },
            "position": 3,
            "dependency_graph": {
                "local": []
            }
        }
    });

    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();

    let query_data = json!({
        "root": "root_value",
        "remote": "remote_value_a"
    })
    .as_object()
    .unwrap()
    .clone();

    let result = evaluate_local_cohorts(&dimensions, &query_data);
    assert_eq!(result.get("root"), Some(&json!("root_value")));
    assert_eq!(result.get("remote"), Some(&json!("remote_value_a")));
    assert_eq!(result.get("local"), Some(&json!("group_a")));
}

#[test]
// Tests filling missing LocalCohorts with "otherwise" value
// Verifies orphaned LocalCohorts (not reached by traversal) get default values
fn test_evaluate_local_cohorts_fills_missing_local_cohorts() {
    let dimensions_json = json!({
        "orphan_cohort": {
            "dimension_type": { "LOCAL_COHORT": "nonexistent" },
            "schema": {
                "enum": ["group1", "otherwise"],
                "definitions": {
                    "group1": {
                        "==": [{"var": "nonexistent"}, "value"]
                    }
                }
            },
            "position": 1,
            "dependency_graph": {
                "orphan_cohort": []
            }
        }
    });

    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();

    let query_data = json!({
        "other_dimension": "value"
    })
    .as_object()
    .unwrap()
    .clone();

    let result = evaluate_local_cohorts(&dimensions, &query_data);
    assert_eq!(result.get("orphan_cohort"), Some(&json!("otherwise")));
}

#[test]
// Tests processing multiple Regular root dimensions with LocalCohort fallbacks
// Verifies algorithm handles multiple independent trees and selects appropriate fallbacks
fn test_dimensions_to_start_from_with_multiple_regular_roots() {
    let dimensions_json = json!({
        "root1": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 1,
            "dependency_graph": {
                "root1": ["child1"],
                "child1": []
            }
        },
        "root2": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 2,
            "dependency_graph": {
                "root2": ["child2"],
                "child2": []
            }
        },
        "child1": {
            "dimension_type": { "LOCAL_COHORT": "root1" },
            "schema": {},
            "position": 3,
            "dependency_graph": {
                "child1": []
            }
        },
        "child2": {
            "dimension_type": { "LOCAL_COHORT": "root2" },
            "schema": {},
            "position": 4,
            "dependency_graph": {
                "child2": []
            }
        }
    });

    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();

    // Test: No query data - should get LocalCohort fallbacks from both trees
    let query_data = Map::new();

    let start_dims = dimensions_to_start_from(&dimensions, &query_data);
    assert_eq!(start_dims.len(), 2);
    assert!(start_dims.contains(&"child1".to_string()));
    assert!(start_dims.contains(&"child2".to_string()));
}

#[test]
// Tests evaluate_local_cohort_dimension function with various edge cases
// Verifies error handling for malformed schemas, missing fields, and invalid data types
fn test_evaluate_local_cohort_dimension_edge_cases() {
    // Test with empty schema
    let empty_schema = Map::new();
    let result = evaluate_local_cohort_dimension("test", &json!("value"), &empty_schema);
    assert_eq!(result, "otherwise");

    // Test with schema missing enum
    let schema_no_enum = json!({
        "definitions": {
            "group1": {
                "==": [{"var": "test"}, "value"]
            }
        }
    })
    .as_object()
    .unwrap()
    .clone();

    let result =
        evaluate_local_cohort_dimension("test", &json!("value"), &schema_no_enum);
    assert_eq!(result, "otherwise");

    // Test with schema missing definitions
    let schema_no_definitions = json!({
        "enum": ["group1", "otherwise"]
    })
    .as_object()
    .unwrap()
    .clone();

    let result =
        evaluate_local_cohort_dimension("test", &json!("value"), &schema_no_definitions);
    assert_eq!(result, "otherwise");

    // Test with malformed enum (not array)
    let schema_bad_enum = json!({
        "enum": "not_an_array",
        "definitions": {
            "group1": {
                "==": [{"var": "test"}, "value"]
            }
        }
    })
    .as_object()
    .unwrap()
    .clone();

    let result =
        evaluate_local_cohort_dimension("test", &json!("value"), &schema_bad_enum);
    assert_eq!(result, "otherwise");
}

#[test]
// Tests LocalCohort dimension that depends on another LocalCohort dimension
// Verifies cascading LocalCohort evaluation and proper dependency resolution
fn test_local_cohort_based_on_local_cohort() {
    let dimensions_json = json!({
        "user_tier": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 1,
            "dependency_graph": {
                "user_tier": ["user_category"],
                "user_category": ["user_segment"],
                "user_segment": []
            }
        },
        "user_category": {
            "dimension_type": { "LOCAL_COHORT": "user_tier" },
            "schema": {
                "enum": ["premium", "standard", "basic", "otherwise"],
                "definitions": {
                    "premium": {
                        "==": [{"var": "user_tier"}, 1500]
                    },
                    "standard": {
                        "==": [{"var": "user_tier"}, 500]
                    },
                    "basic": {
                        "==": [{"var": "user_tier"}, 50]
                    }
                }
            },
            "position": 2,
            "dependency_graph": {
                "user_category": ["user_segment"],
                "user_segment": []
            }
        },
        "user_segment": {
            "dimension_type": { "LOCAL_COHORT": "user_category" },
            "schema": {
                "enum": ["vip", "regular", "new", "otherwise"],
                "definitions": {
                    "vip": {
                        "==": [{"var": "user_category"}, "premium"]
                    },
                    "regular": {
                        "in": [{"var": "user_category"}, ["standard", "basic"]]
                    },
                    "new": {
                        "==": [{"var": "user_category"}, "otherwise"]
                    }
                }
            },
            "position": 3,
            "dependency_graph": {
                "user_segment": []
            }
        }
    });

    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();

    // Test case 1: Premium user -> VIP segment
    let query_data1 = json!({
        "user_tier": 1500
    })
    .as_object()
    .unwrap()
    .clone();

    let result1 = evaluate_local_cohorts(&dimensions, &query_data1);
    assert_eq!(result1.get("user_tier"), Some(&json!(1500)));
    assert_eq!(result1.get("user_category"), Some(&json!("premium")));
    assert_eq!(result1.get("user_segment"), Some(&json!("vip")));

    // Test case 2: Standard user -> Regular segment
    let query_data2 = json!({
        "user_tier": 500
    })
    .as_object()
    .unwrap()
    .clone();

    let result2 = evaluate_local_cohorts(&dimensions, &query_data2);
    assert_eq!(result2.get("user_tier"), Some(&json!(500)));
    assert_eq!(result2.get("user_category"), Some(&json!("standard")));
    assert_eq!(result2.get("user_segment"), Some(&json!("regular")));

    // Test case 3: Basic user -> Regular segment
    let query_data3 = json!({
        "user_tier": 50
    })
    .as_object()
    .unwrap()
    .clone();

    let result3 = evaluate_local_cohorts(&dimensions, &query_data3);
    assert_eq!(result3.get("user_tier"), Some(&json!(50)));
    assert_eq!(result3.get("user_category"), Some(&json!("basic")));
    assert_eq!(result3.get("user_segment"), Some(&json!("regular")));

    // Test case 4: Invalid tier -> otherwise -> new segment
    let query_data4 = json!({
        "user_tier": 999
    })
    .as_object()
    .unwrap()
    .clone();

    let result4 = evaluate_local_cohorts(&dimensions, &query_data4);
    assert_eq!(result4.get("user_tier"), Some(&json!(999)));
    assert_eq!(result4.get("user_category"), Some(&json!("otherwise")));
    assert_eq!(result4.get("user_segment"), Some(&json!("new")));
}

#[test]
// Tests LocalCohort chain evaluation when starting from intermediate LocalCohort
// Verifies proper evaluation when query data exists for a LocalCohort in the middle of chain
fn test_local_cohort_chain_with_intermediate_query_data() {
    let dimensions_json = json!({
        "region": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 1,
            "dependency_graph": {
                "region": ["country_group"],
                "country_group": ["market_segment"],
                "market_segment": []
            }
        },
        "country_group": {
            "dimension_type": { "LOCAL_COHORT": "region" },
            "schema": {
                "enum": ["asia", "europe", "americas", "otherwise"],
                "definitions": {
                    "asia": {
                        "in": [{"var": "region"}, ["india", "china", "japan"]]
                    },
                    "europe": {
                        "in": [{"var": "region"}, ["uk", "germany", "france"]]
                    },
                    "americas": {
                        "in": [{"var": "region"}, ["usa", "canada", "brazil"]]
                    }
                }
            },
            "position": 2,
            "dependency_graph": {
                "country_group": ["market_segment"],
                "market_segment": []
            }
        },
        "market_segment": {
            "dimension_type": { "LOCAL_COHORT": "country_group" },
            "schema": {
                "enum": ["emerging", "developed", "mixed", "otherwise"],
                "definitions": {
                    "emerging": {
                        "==": [{"var": "country_group"}, "asia"]
                    },
                    "developed": {
                        "==": [{"var": "country_group"}, "europe"]
                    },
                    "mixed": {
                        "==": [{"var": "country_group"}, "americas"]
                    }
                }
            },
            "position": 3,
            "dependency_graph": {
                "market_segment": []
            }
        }
    });

    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();

    // Test: Query data for intermediate LocalCohort (country_group)
    let query_data = json!({
        "country_group": "asia"
    })
    .as_object()
    .unwrap()
    .clone();

    let start_dims = dimensions_to_start_from(&dimensions, &query_data);
    assert_eq!(start_dims.len(), 1);
    assert!(start_dims.contains(&"country_group".to_string()));

    let result = evaluate_local_cohorts(&dimensions, &query_data);
    assert_eq!(result.get("country_group"), Some(&json!("asia")));
    assert_eq!(result.get("market_segment"), Some(&json!("emerging")));
    // region should not be evaluated since it's not in the starting path
    assert!(!result.contains_key("region"));
}

#[test]
// Tests LocalCohort dependency chain with complex branching
// Verifies multiple LocalCohorts depending on the same parent LocalCohort
fn test_local_cohort_complex_branching() {
    let dimensions_json = json!({
        "device_type": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 1,
            "dependency_graph": {
                "device_type": ["device_category"],
                "device_category": ["ui_theme", "feature_set"],
                "ui_theme": [],
                "feature_set": []
            }
        },
        "device_category": {
            "dimension_type": { "LOCAL_COHORT": "device_type" },
            "schema": {
                "enum": ["mobile", "desktop", "tablet", "otherwise"],
                "definitions": {
                    "mobile": {
                        "in": [{"var": "device_type"}, ["android", "ios"]]
                    },
                    "desktop": {
                        "in": [{"var": "device_type"}, ["windows", "mac", "linux"]]
                    },
                    "tablet": {
                        "in": [{"var": "device_type"}, ["ipad", "android_tablet"]]
                    }
                }
            },
            "position": 2,
            "dependency_graph": {
                "device_category": ["ui_theme", "feature_set"],
                "ui_theme": [],
                "feature_set": []
            }
        },
        "ui_theme": {
            "dimension_type": { "LOCAL_COHORT": "device_category" },
            "schema": {
                "enum": ["dark", "light", "auto", "otherwise"],
                "definitions": {
                    "dark": {
                        "==": [{"var": "device_category"}, "mobile"]
                    },
                    "light": {
                        "==": [{"var": "device_category"}, "desktop"]
                    },
                    "auto": {
                        "==": [{"var": "device_category"}, "tablet"]
                    }
                }
            },
            "position": 3,
            "dependency_graph": {
                "ui_theme": []
            }
        },
        "feature_set": {
            "dimension_type": { "LOCAL_COHORT": "device_category" },
            "schema": {
                "enum": ["basic", "advanced", "premium", "otherwise"],
                "definitions": {
                    "basic": {
                        "==": [{"var": "device_category"}, "mobile"]
                    },
                    "advanced": {
                        "==": [{"var": "device_category"}, "tablet"]
                    },
                    "premium": {
                        "==": [{"var": "device_category"}, "desktop"]
                    }
                }
            },
            "position": 4,
            "dependency_graph": {
                "feature_set": []
            }
        }
    });

    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();

    // Test: Mobile device evaluation
    let query_data = json!({
        "device_type": "android"
    })
    .as_object()
    .unwrap()
    .clone();

    let result = evaluate_local_cohorts(&dimensions, &query_data);
    assert_eq!(result.get("device_type"), Some(&json!("android")));
    assert_eq!(result.get("device_category"), Some(&json!("mobile")));
    assert_eq!(result.get("ui_theme"), Some(&json!("dark")));
    assert_eq!(result.get("feature_set"), Some(&json!("basic")));

    // Test: Desktop device evaluation
    let query_data2 = json!({
        "device_type": "windows"
    })
    .as_object()
    .unwrap()
    .clone();

    let result2 = evaluate_local_cohorts(&dimensions, &query_data2);
    assert_eq!(result2.get("device_type"), Some(&json!("windows")));
    assert_eq!(result2.get("device_category"), Some(&json!("desktop")));
    assert_eq!(result2.get("ui_theme"), Some(&json!("light")));
    assert_eq!(result2.get("feature_set"), Some(&json!("premium")));
}

#[test]
// Tests multiple independent trees with complex branching and query data from random branches
// Verifies algorithm handles large multi-tree scenarios with selective query data
fn test_multi_tree_complex_branching() {
    let dimensions_json = json!({
        // Tree 1: User Management
        "user_id": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 1,
            "dependency_graph": {
                "user_id": ["user_tier", "user_preferences"],
                "user_tier": ["subscription_level"],
                "user_preferences": ["notification_settings"],
                "subscription_level": [],
                "notification_settings": []
            }
        },
        "user_tier": {
            "dimension_type": { "LOCAL_COHORT": "user_id" },
            "schema": {
                "enum": ["premium", "standard", "basic", "otherwise"],
                "definitions": {
                    "premium": { ">=": [{"var": "user_id"}, 1000] },
                    "standard": { "and": [{">=": [{"var": "user_id"}, 100]}, {"<": [{"var": "user_id"}, 1000]}] },
                    "basic": { "<": [{"var": "user_id"}, 100] }
                }
            },
            "position": 2,
            "dependency_graph": {
                "user_tier": ["subscription_level"],
                "subscription_level": []
            }
        },
        "user_preferences": {
            "dimension_type": { "LOCAL_COHORT": "user_id" },
            "schema": {
                "enum": ["detailed", "minimal", "otherwise"],
                "definitions": {
                    "detailed": { ">=": [{"var": "user_id"}, 500] },
                    "minimal": { "<": [{"var": "user_id"}, 500] }
                }
            },
            "position": 3,
            "dependency_graph": {
                "user_preferences": ["notification_settings"],
                "notification_settings": []
            }
        },
        "subscription_level": {
            "dimension_type": { "LOCAL_COHORT": "user_tier" },
            "schema": {
                "enum": ["pro", "plus", "free", "otherwise"],
                "definitions": {
                    "pro": { "==": [{"var": "user_tier"}, "premium"] },
                    "plus": { "==": [{"var": "user_tier"}, "standard"] },
                    "free": { "in": [{"var": "user_tier"}, ["basic", "otherwise"]] }
                }
            },
            "position": 4,
            "dependency_graph": {
                "subscription_level": []
            }
        },
        "notification_settings": {
            "dimension_type": { "LOCAL_COHORT": "user_preferences" },
            "schema": {
                "enum": ["all", "important", "none", "otherwise"],
                "definitions": {
                    "all": { "==": [{"var": "user_preferences"}, "detailed"] },
                    "important": { "==": [{"var": "user_preferences"}, "minimal"] }
                }
            },
            "position": 5,
            "dependency_graph": {
                "notification_settings": []
            }
        },
        // Tree 2: Location Services
        "location": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 6,
            "dependency_graph": {
                "location": ["region", "timezone"],
                "region": ["language"],
                "timezone": ["currency"],
                "language": [],
                "currency": []
            }
        },
        "region": {
            "dimension_type": { "LOCAL_COHORT": "location" },
            "schema": {
                "enum": ["north", "south", "east", "west", "otherwise"],
                "definitions": {
                    "north": { "in": [{"var": "location"}, ["canada", "alaska"]] },
                    "south": { "in": [{"var": "location"}, ["mexico", "brazil"]] },
                    "east": { "in": [{"var": "location"}, ["india", "china"]] },
                    "west": { "in": [{"var": "location"}, ["usa", "uk"]] }
                }
            },
            "position": 7,
            "dependency_graph": {
                "region": ["language"],
                "language": []
            }
        },
        "timezone": {
            "dimension_type": { "LOCAL_COHORT": "location" },
            "schema": {
                "enum": ["utc_minus", "utc_plus", "utc", "otherwise"],
                "definitions": {
                    "utc_minus": { "in": [{"var": "location"}, ["usa", "canada", "mexico"]] },
                    "utc_plus": { "in": [{"var": "location"}, ["india", "china"]] },
                    "utc": { "in": [{"var": "location"}, ["uk"]] }
                }
            },
            "position": 8,
            "dependency_graph": {
                "timezone": ["currency"],
                "currency": []
            }
        },
        "language": {
            "dimension_type": { "LOCAL_COHORT": "region" },
            "schema": {
                "enum": ["english", "spanish", "chinese", "hindi", "otherwise"],
                "definitions": {
                    "english": { "in": [{"var": "region"}, ["north", "west"]] },
                    "spanish": { "==": [{"var": "region"}, "south"] },
                    "chinese": { "==": [{"var": "region"}, "east"] }
                }
            },
            "position": 9,
            "dependency_graph": {
                "language": []
            }
        },
        "currency": {
            "dimension_type": { "LOCAL_COHORT": "timezone" },
            "schema": {
                "enum": ["usd", "inr", "cny", "gbp", "otherwise"],
                "definitions": {
                    "usd": { "==": [{"var": "timezone"}, "utc_minus"] },
                    "inr": { "==": [{"var": "timezone"}, "utc_plus"] },
                    "gbp": { "==": [{"var": "timezone"}, "utc"] }
                }
            },
            "position": 10,
            "dependency_graph": {
                "currency": []
            }
        }
    });

    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();

    // Test 1: Query data from deep branch in tree 1, shallow branch in tree 2
    let query_data1 = json!({
        "user_preferences": "detailed",
        "location": "usa"
    })
    .as_object()
    .unwrap()
    .clone();

    let start_dims1 = dimensions_to_start_from(&dimensions, &query_data1);
    assert_eq!(start_dims1.len(), 3);
    assert!(start_dims1.contains(&"user_preferences".to_string()));
    assert!(start_dims1.contains(&"location".to_string()));
    assert!(start_dims1.contains(&"user_tier".to_string()));

    let result1 = evaluate_local_cohorts(&dimensions, &query_data1);
    assert_eq!(result1.get("user_preferences"), Some(&json!("detailed")));
    assert_eq!(result1.get("location"), Some(&json!("usa")));
    assert_eq!(result1.get("region"), Some(&json!("west")));
    assert_eq!(result1.get("timezone"), Some(&json!("utc_minus")));
    assert_eq!(result1.get("language"), Some(&json!("english")));
    assert_eq!(result1.get("currency"), Some(&json!("usd")));
    assert_eq!(result1.get("notification_settings"), Some(&json!("all")));
    assert_eq!(result1.get("user_tier"), Some(&json!("otherwise")));
    assert_eq!(result1.get("subscription_level"), Some(&json!("otherwise")));
    assert_eq!(result1.len(), 9);

    // Test 2: Query data from multiple branches, different depths
    let query_data2 = json!({
        "user_id": 1500,
        "timezone": "utc_plus"
    })
    .as_object()
    .unwrap()
    .clone();

    let start_dims2 = dimensions_to_start_from(&dimensions, &query_data2);
    assert_eq!(start_dims2.len(), 3);
    assert!(start_dims2.contains(&"user_id".to_string()));
    assert!(start_dims2.contains(&"timezone".to_string()));
    assert!(start_dims2.contains(&"region".to_string()));

    let result2 = evaluate_local_cohorts(&dimensions, &query_data2);
    assert_eq!(result2.get("user_id"), Some(&json!(1500)));
    assert_eq!(result2.get("user_tier"), Some(&json!("premium")));
    assert_eq!(result2.get("user_preferences"), Some(&json!("detailed")));
    assert_eq!(result2.get("subscription_level"), Some(&json!("pro")));
    assert_eq!(result2.get("notification_settings"), Some(&json!("all")));
    assert_eq!(result2.get("timezone"), Some(&json!("utc_plus")));
    assert_eq!(result2.get("currency"), Some(&json!("inr")));
    assert_eq!(result2.get("region"), Some(&json!("otherwise")));
    assert_eq!(result2.get("language"), Some(&json!("otherwise")));
    assert_eq!(result2.len(), 9);
}

#[test]
// Tests big tree with random query data from different branches at various depths
// Verifies "closest to root" selection in complex hierarchical structures
fn test_big_tree_random_branch_queries() {
    let dimensions_json = json!({
        "root": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 1,
            "dependency_graph": {
                "root": ["level1_a", "level1_b"],
                "level1_a": ["level2_a", "level2_b"],
                "level1_b": ["level2_c"],
                "level2_a": ["level3_a"],
                "level2_b": ["level3_b", "level3_c"],
                "level2_c": ["level3_d"],
                "level3_a": ["level4_a"],
                "level3_b": [],
                "level3_c": ["level4_b"],
                "level3_d": ["level4_c"],
                "level4_a": [],
                "level4_b": [],
                "level4_c": []
            }
        },
        "level1_a": {
            "dimension_type": { "REMOTE_COHORT": "remote_a" },
            "schema": {},
            "position": 2,
            "dependency_graph": {
                "level1_a": ["level2_a", "level2_b"],
                "level2_a": ["level3_a"],
                "level2_b": ["level3_b", "level3_c"],
                "level3_a": ["level4_a"],
                "level3_b": [],
                "level3_c": ["level4_b"],
                "level4_a": [],
                "level4_b": []
            }
        },
        "level1_b": {
            "dimension_type": { "REMOTE_COHORT": "remote_b" },
            "schema": {},
            "position": 3,
            "dependency_graph": {
                "level1_b": ["level2_c"],
                "level2_c": ["level3_d"],
                "level3_d": ["level4_c"],
                "level4_c": []
            }
        },
        "level2_a": {
            "dimension_type": { "LOCAL_COHORT": "level1_a" },
            "schema": {
                "enum": ["group_a", "otherwise"],
                "definitions": {
                    "group_a": { "==": [{"var": "level1_a"}, "value_a"] }
                }
            },
            "position": 4,
            "dependency_graph": {
                "level2_a": ["level3_a"],
                "level3_a": ["level4_a"],
                "level4_a": []
            }
        },
        "level2_b": {
            "dimension_type": { "LOCAL_COHORT": "level1_a" },
            "schema": {
                "enum": ["group_b", "otherwise"],
                "definitions": {
                    "group_b": { "==": [{"var": "level1_a"}, "value_b"] }
                }
            },
            "position": 5,
            "dependency_graph": {
                "level2_b": ["level3_b", "level3_c"],
                "level3_b": [],
                "level3_c": ["level4_b"],
                "level4_b": []
            }
        },
        "level2_c": {
            "dimension_type": { "LOCAL_COHORT": "level1_b" },
            "schema": {
                "enum": ["group_c", "otherwise"],
                "definitions": {
                    "group_c": { "==": [{"var": "level1_b"}, "value_c"] }
                }
            },
            "position": 6,
            "dependency_graph": {
                "level2_c": ["level3_d"],
                "level3_d": ["level4_c"],
                "level4_c": []
            }
        },
        "level3_a": { "dimension_type": { "LOCAL_COHORT": "level2_a" }, "schema": {}, "position": 7, "dependency_graph": { "level3_a": ["level4_a"], "level4_a": [] } },
        "level3_b": { "dimension_type": { "LOCAL_COHORT": "level2_b" }, "schema": {}, "position": 8, "dependency_graph": { "level3_b": [] } },
        "level3_c": { "dimension_type": { "LOCAL_COHORT": "level2_b" }, "schema": {}, "position": 9, "dependency_graph": { "level3_c": ["level4_b"], "level4_b": [] } },
        "level3_d": { "dimension_type": { "LOCAL_COHORT": "level2_c" }, "schema": {}, "position": 10, "dependency_graph": { "level3_d": ["level4_c"], "level4_c": [] } },
        "level4_a": { "dimension_type": { "LOCAL_COHORT": "level3_a" }, "schema": {}, "position": 11, "dependency_graph": { "level4_a": [] } },
        "level4_b": { "dimension_type": { "LOCAL_COHORT": "level3_c" }, "schema": {}, "position": 12, "dependency_graph": { "level4_b": [] } },
        "level4_c": { "dimension_type": { "LOCAL_COHORT": "level3_d" }, "schema": {}, "position": 13, "dependency_graph": { "level4_c": [] } }
    });

    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();

    // Test 1: Query data at root + deep leaf - root should win
    let query_data1 = json!({
        "root": "root_value",
        "level4_a": "deep_value",
        "level3_d": "mid_value"
    })
    .as_object()
    .unwrap()
    .clone();

    let start_dims1 = dimensions_to_start_from(&dimensions, &query_data1);
    assert_eq!(start_dims1.len(), 1);
    assert!(start_dims1.contains(&"root".to_string()));

    // Test 2: Query data from different branches at same level
    let query_data2 = json!({
        "level2_a": "branch_a",
        "level2_c": "branch_c"
    })
    .as_object()
    .unwrap()
    .clone();

    let start_dims2 = dimensions_to_start_from(&dimensions, &query_data2);
    assert_eq!(start_dims2.len(), 3);
    assert!(start_dims2.contains(&"level2_a".to_string()));
    assert!(start_dims2.contains(&"level2_b".to_string()));
    assert!(start_dims2.contains(&"level2_c".to_string()));

    // Test 3: Query data from random deep branches - test LocalCohort selection
    let query_data3 = json!({
        "level3_b": "value_3b"
    })
    .as_object()
    .unwrap()
    .clone();

    let start_dims3 = dimensions_to_start_from(&dimensions, &query_data3);
    assert_eq!(start_dims3.len(), 3);
    assert!(start_dims3.contains(&"level2_a".to_string()));
    assert!(start_dims3.contains(&"level2_b".to_string()));
    assert!(start_dims3.contains(&"level2_c".to_string()));

    // Test 4: No query data - should get all leaf LocalCohorts as fallbacks
    let query_data4 = Map::new();

    let start_dims4 = dimensions_to_start_from(&dimensions, &query_data4);
    assert_eq!(start_dims4.len(), 3);
    assert!(start_dims4.contains(&"level2_a".to_string()));
    assert!(start_dims4.contains(&"level2_b".to_string()));
    assert!(start_dims4.contains(&"level2_c".to_string()));
}

#[test]
// Tests override scenario where query provides LocalCohort value but Regular dimension is also provided
// Verifies that computed LocalCohort value from Regular dimension overrides query LocalCohort value
fn test_regular_overrides_local_cohort_in_query() {
    let dimensions_json = json!({
        "user_score": {
            "dimension_type": { "REGULAR": {} },
            "schema": {},
            "position": 1,
            "dependency_graph": {
                "user_score": ["user_category"],
                "user_category": ["reward_tier"],
                "reward_tier": []
            }
        },
        "user_category": {
            "dimension_type": { "LOCAL_COHORT": "user_score" },
            "schema": {
                "enum": ["gold", "silver", "bronze", "otherwise"],
                "definitions": {
                    "gold": { ">=": [{"var": "user_score"}, 900] },
                    "silver": { "and": [{">=": [{"var": "user_score"}, 500]}, {"<": [{"var": "user_score"}, 900]}] },
                    "bronze": { "and": [{">=": [{"var": "user_score"}, 100]}, {"<": [{"var": "user_score"}, 500]}] }
                }
            },
            "position": 2,
            "dependency_graph": {
                "user_category": ["reward_tier"],
                "reward_tier": []
            }
        },
        "reward_tier": {
            "dimension_type": { "LOCAL_COHORT": "user_category" },
            "schema": {
                "enum": ["premium", "standard", "basic", "otherwise"],
                "definitions": {
                    "premium": { "==": [{"var": "user_category"}, "gold"] },
                    "standard": { "==": [{"var": "user_category"}, "silver"] },
                    "basic": { "in": [{"var": "user_category"}, ["bronze", "otherwise"]] }
                }
            },
            "position": 3,
            "dependency_graph": {
                "reward_tier": []
            }
        }
    });

    let dimensions: HashMap<String, DimensionInfo> =
        serde_json::from_value(dimensions_json).unwrap();

    // Test 1: Query provides wrong LocalCohort values, but Regular dimension should override
    let query_data1 = json!({
        "user_score": 950,
        "user_category": "bronze",
        "reward_tier": "basic"
    })
    .as_object()
    .unwrap()
    .clone();

    let start_dims1 = dimensions_to_start_from(&dimensions, &query_data1);
    assert_eq!(start_dims1.len(), 1);
    assert!(start_dims1.contains(&"user_score".to_string()));

    let result1 = evaluate_local_cohorts(&dimensions, &query_data1);
    assert_eq!(result1.get("user_score"), Some(&json!(950)));
    assert_eq!(result1.get("user_category"), Some(&json!("gold")));
    assert_eq!(result1.get("reward_tier"), Some(&json!("premium")));

    // Test 2: Query provides LocalCohort value but no Regular dimension
    let query_data2 = json!({
        "user_category": "silver",
        "reward_tier": "basic"
    })
    .as_object()
    .unwrap()
    .clone();

    let start_dims2 = dimensions_to_start_from(&dimensions, &query_data2);
    assert_eq!(start_dims2.len(), 1);
    assert!(start_dims2.contains(&"user_category".to_string()));

    let result2 = evaluate_local_cohorts(&dimensions, &query_data2);
    assert_eq!(result2.get("user_category"), Some(&json!("silver")));
    assert_eq!(result2.get("reward_tier"), Some(&json!("standard")));

    // Test 3: Query provides only leaf LocalCohort value (no computation path available)
    let query_data3 = json!({
        "reward_tier": "premium"
    })
    .as_object()
    .unwrap()
    .clone();

    let start_dims3 = dimensions_to_start_from(&dimensions, &query_data3);
    assert_eq!(start_dims3.len(), 1);
    assert!(start_dims3.contains(&"user_category".to_string()));

    let result3 = evaluate_local_cohorts(&dimensions, &query_data3);
    assert_eq!(result3.get("user_category"), Some(&json!("otherwise")));
    assert_eq!(result3.get("reward_tier"), Some(&json!("otherwise")));

    // Test 4: Complex override scenario with multiple trees
    let query_data4 = json!({
        "user_score": 300,
        "user_category": "gold",
        "reward_tier": "premium"
    })
    .as_object()
    .unwrap()
    .clone();

    let result4 = evaluate_local_cohorts(&dimensions, &query_data4);
    assert_eq!(result4.get("user_score"), Some(&json!(300)));
    assert_eq!(result4.get("user_category"), Some(&json!("bronze")));
    assert_eq!(result4.get("reward_tier"), Some(&json!("basic")));
}
