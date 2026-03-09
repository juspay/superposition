//! Generate complete action registry JSON
//!
//! This example depends on superposition which links all crates with #[authorized] handlers.
//! Run with: cargo run --example generate_actions_registry -p superposition > actions_registry.json
//!
//! Note: This must be run as a binary that links with all crates containing handlers.

// These imports ensure the crates with handlers are linked into the binary
// The actual types don't matter - we just need the crates linked so inventory collects all items
extern crate context_aware_config;
extern crate experimentation_platform;

use service_utils::registry::ActionRegistry;

fn main() {
    let actions = ActionRegistry::group_by_resource();

    let json = serde_json::json!({
        "metadata": {
            "total_actions": actions.values().map(|v| v.len()).sum::<usize>(),
            "generated_at": chrono::Local::now().to_rfc3339(),
        },
        "actions": actions,
    });

    if let Ok(output) = serde_json::to_string_pretty(&json) {
        println!("{}", output);
    } else {
        eprintln!("Failed to generate JSON");
    }
}
