use crate::mcp_service::{value_to_hashmap, value_to_document, McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;
use superposition_sdk::types::{Variant, VariantType};

pub struct CreateExperimentTool;

impl MCPTool for CreateExperimentTool {
    fn get_definition() -> Tool {
        Tool {
            name: "create_experiment".to_string(),
            description: "Create a new experiment".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "name": {"type": "string", "description": "Experiment name"},
                    "context": {"type": "object", "description": "Experiment context"},
                    "variants": {"type": "array", "description": "Experiment variants"},
                    "change_reason": {"type": "string", "description": "Reason for creating experiment"}
                },
                "required": ["org_id", "workspace_id", "name", "context", "variants", "change_reason"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        let org_id = arguments["org_id"].as_str().unwrap_or("");
        let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
        let name = arguments["name"].as_str().unwrap_or("");
        let context = &arguments["context"];
        let variants = &arguments["variants"];
        let change_reason = arguments["change_reason"].as_str().unwrap_or("");

        let context_hashmap = if let Some(context_map) = value_to_hashmap(context.clone()) {
            context_map
        } else {
            return Err("Invalid context format".into());
        };

        let mut experiment_variants = Vec::new();
        if let Some(variants_array) = variants.as_array() {
            for variant_value in variants_array {
                if let Some(variant_obj) = variant_value.as_object() {
                    let variant_id = variant_obj.get("id").and_then(|v| v.as_str()).unwrap_or("");
                    let variant_type = match variant_obj.get("variant_type").and_then(|v| v.as_str()).unwrap_or("CONTROL") {
                        "EXPERIMENTAL" => VariantType::Experimental,
                        _ => VariantType::Control,
                    };
                    let override_keys = variant_obj.get("override_keys").cloned().unwrap_or(json!({}));
                    let override_document = value_to_document(&override_keys);

                    let variant = Variant::builder()
                        .id(variant_id)
                        .variant_type(variant_type)
                        .set_overrides(Some(override_document))
                        .build()
                        .map_err(|e| format!("Failed to build variant: {}", e))?;
                    experiment_variants.push(variant);
                }
            }
        }

        let mut builder = service
            .superposition_client
            .create_experiment()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .name(name)
            .set_context(Some(context_hashmap))
            .change_reason(change_reason);

        for variant in experiment_variants {
            builder = builder.variants(variant);
        }

        builder
            .send()
            .await
            .map(|_| json!({"status": "experiment_created"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "create_experiment"
    }
}
