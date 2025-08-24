use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct MoveContextTool;

impl MCPTool for MoveContextTool {
    fn get_definition() -> Tool {
        Tool {
            name: "move_context".to_string(),
            description: "Move a context to a different position".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "context_id": {
                        "type": "string",
                        "description": "ID of the context to move"
                    },
                    "dest_position": {
                        "type": "integer",
                        "description": "Destination position for the context"
                    }
                },
                "required": ["context_id", "dest_position"]
            }),
        }
    }

    async fn execute(
        _service: &McpService,
        _arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        Err(format!("move_context implementation needs parameter verification").into())
    }

    fn name() -> &'static str {
        "move_context"
    }
}