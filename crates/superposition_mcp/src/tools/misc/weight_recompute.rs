use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct WeightRecomputeTool;

impl MCPTool for WeightRecomputeTool {
    fn get_definition() -> Tool {
        Tool {
            name: "weight_recompute".to_string(),
            description: "Recompute weights for configurations".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "config_keys": {"type": "array", "items": {"type": "string"}, "description": "Configuration keys to recompute weights for"}
                },
                "required": ["org_id", "workspace_id"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        let org_id = arguments["org_id"].as_str().unwrap_or("");
        let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
        let mut builder = service
            .superposition_client
            .weight_recompute()
            .workspace_id(workspace_id)
            .org_id(org_id);
        
        if let Some(config_keys_array) = arguments["config_keys"].as_array() {
            let config_keys: Vec<String> = config_keys_array
                .iter()
                .filter_map(|v| v.as_str())
                .map(|s| s.to_string())
                .collect();
            let config_keys_str = config_keys.join(",");
            builder = builder.config_tags(config_keys_str);
        }
        
        builder
            .send()
            .await
            .map(|_| json!({"status": "weights recomputed"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "weight_recompute"
    }
}