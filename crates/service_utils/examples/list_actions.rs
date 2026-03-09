//! Example: List all registered actions
//!
//! This example demonstrates how to use the action registry to collect
//! and display all actions registered via the #[authorized] macro.
//!
//! Run with: cargo run --example list_actions -p service_utils

use service_utils::registry::ActionRegistry;

fn main() {
    println!("=== All Registered Actions ===\n");

    let actions = ActionRegistry::get_all();
    println!("Total actions registered: {}\n", actions.len());

    for action in &actions {
        println!(
            "Handler: {} | Action: {} | File: {} | Resource: {}",
            action.handler_name,
            action.action_name,
            action.source_file,
            action.resource_type
        );
    }

    println!("\n=== Actions Grouped by Resource ===\n");

    let grouped = ActionRegistry::group_by_resource();
    for (resource, resource_actions) in grouped {
        println!("{}:", resource);
        for action in resource_actions {
            println!(
                "  - {} (handler: {})",
                action.action_name, action.handler_name
            );
        }
    }

    println!("\n=== Registry as JSON ===\n");

    println!(
        "{}",
        serde_json::to_string(&ActionRegistry::group_by_resource()).unwrap_or_default()
    );
}
