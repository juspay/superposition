// Example of ToolsGroup that composes individual MCPTool implementations
// This demonstrates the new architecture where ToolsGroup aggregates MCPTool instances

use super::individual_tools::{CreateDefaultConfigTool, GetResolvedConfigTool};
use super::{MCPTool, ToolsGroup};
use crate::mcp_service::{McpService, Tool};
use serde_json::Value;
use std::error::Error;

/// Example ToolsGroup that composes individual MCPTool implementations
pub struct ComposedConfigTools;

impl ToolsGroup for ComposedConfigTools {
    fn get_tool_definitions() -> Vec<Tool> {
        vec![
            GetResolvedConfigTool::get_definition(),
            CreateDefaultConfigTool::get_definition(),
            // Add more individual tools here...
        ]
    }

    async fn execute_tool(
        service: &McpService,
        tool_name: &str,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        match tool_name {
            tool_name if tool_name == GetResolvedConfigTool::name() => {
                GetResolvedConfigTool::execute(service, arguments).await
            }
            tool_name if tool_name == CreateDefaultConfigTool::name() => {
                CreateDefaultConfigTool::execute(service, arguments).await
            }
            _ => Err(format!("Unknown tool: {}", tool_name).into()),
        }
    }
}
