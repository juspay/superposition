use std::collections::HashMap;

use inventory::collect;
use serde::{Deserialize, Serialize};
use superposition_types::Resource;

/// Represents a single authorized action registered in the system
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ActionRegistry {
    /// Source file where the action is defined (relative path from workspace root)
    pub source_file: &'static str,
    /// Name of the handler function (e.g., "create_handler")
    pub handler_name: &'static str,
    /// Action name derived from function name or explicitly provided
    pub action_name: &'static str,
    /// Resource type that this action operates on (from declare_resource! or override)
    pub resource_type: Resource,
}

collect!(ActionRegistry);

impl ActionRegistry {
    /// Creates a new action registry entry
    pub const fn new(
        source_file: &'static str,
        handler_name: &'static str,
        action_name: &'static str,
        resource_type: Resource,
    ) -> Self {
        Self {
            source_file,
            handler_name,
            action_name,
            resource_type,
        }
    }

    /// Collects all registered actions from the inventory
    pub fn get_all() -> Vec<Self> {
        inventory::iter::<Self>().cloned().collect::<Vec<_>>()
    }

    pub fn get_by_resource(resource: Resource) -> Vec<Self> {
        Self::get_all()
            .into_iter()
            .filter(|action| action.resource_type == resource)
            .collect()
    }

    pub fn group_by_resource() -> HashMap<Resource, Vec<Self>> {
        let mut map = HashMap::new();

        for action in Self::get_all() {
            map.entry(action.resource_type)
                .or_insert_with(Vec::new)
                .push(action);
        }

        map
    }
}
