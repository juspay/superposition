//! Integration test for action registry
//!
//! Run with: cargo test --test action_registry_test -p service_utils

use service_utils::registry::ActionRegistry;

#[test]
fn test_actions_are_registered() {
    let actions = ActionRegistry::get_all();

    // Verify structure is correct (basic smoke test)
    for action in &actions {
        assert!(
            !action.action_name.is_empty(),
            "Action name should not be empty"
        );
        assert!(
            !action.handler_name.is_empty(),
            "Handler name should not be empty"
        );
        assert!(
            !action.source_file.is_empty(),
            "Source file should not be empty"
        );
    }
}

#[test]
fn test_grouping_by_resource() {
    let grouped = ActionRegistry::group_by_resource();

    // Verify grouping is correct
    for (resource, resource_actions) in &grouped {
        for action in resource_actions {
            assert_eq!(
                &action.resource_type, resource,
                "Action should be in correct resource group"
            );
        }
    }
}
