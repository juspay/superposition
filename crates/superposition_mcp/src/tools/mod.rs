// Macro definitions (must be declared before usage)
pub mod macros;

// New structured tool directories
pub mod audit;
pub mod config;
pub mod context;
pub mod dimension;
pub mod experiment;
pub mod function;
pub mod misc;
pub mod organisation;
pub mod types;
pub mod webhook;
pub mod workspace;

use crate::mcp_service::{McpService, Tool};
use serde_json::Value;
use std::error::Error;

/// Trait for individual MCP tools
pub trait MCPTool {
    /// Get the tool definition for this specific tool
    fn get_definition() -> Tool;

    /// Execute this specific tool
    async fn execute(
        service: &McpService,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>>;

    /// Get the tool name
    fn name() -> &'static str;
}

/// Trait for groups of MCP tools (previously ToolsModule)
pub trait MCPToolsGroup {
    fn get_tool_definitions() -> Vec<Tool>;
    async fn execute_tool(
        service: &McpService,
        tool_name: &str,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>>;
}

pub fn get_all_tools() -> Vec<Tool> {
    let mut tools = Vec::new();

    // New structured tools
    tools.extend(config::ConfigTools::get_tool_definitions());
    tools.extend(experiment::ExperimentTools::get_tool_definitions());
    tools.extend(organisation::OrganisationTools::get_tool_definitions());

    // Legacy tools (to be migrated)
    tools.extend(dimension::DimensionTools::get_tool_definitions());
    tools.extend(context::ContextTools::get_tool_definitions());
    tools.extend(function::FunctionTools::get_tool_definitions());
    tools.extend(workspace::WorkspaceTools::get_tool_definitions());
    tools.extend(webhook::WebhookTools::get_tool_definitions());
    tools.extend(types::TypeTools::get_tool_definitions());
    tools.extend(audit::AuditTools::get_tool_definitions());
    tools.extend(misc::MiscTools::get_tool_definitions());

    tools
}

pub async fn execute_any_tool(
    service: &McpService,
    tool_name: &str,
    arguments: &Value,
) -> Result<Value, Box<dyn Error>> {
    // Try new structured tools first
    if let Ok(result) =
        config::ConfigTools::execute_tool(service, tool_name, arguments).await
    {
        return Ok(result);
    }

    if let Ok(result) =
        experiment::ExperimentTools::execute_tool(service, tool_name, arguments).await
    {
        return Ok(result);
    }

    if let Ok(result) =
        organisation::OrganisationTools::execute_tool(service, tool_name, arguments).await
    {
        return Ok(result);
    }

    // Try legacy tools
    if let Ok(result) =
        dimension::DimensionTools::execute_tool(service, tool_name, arguments).await
    {
        return Ok(result);
    }

    if let Ok(result) =
        context::ContextTools::execute_tool(service, tool_name, arguments).await
    {
        return Ok(result);
    }

    if let Ok(result) =
        function::FunctionTools::execute_tool(service, tool_name, arguments).await
    {
        return Ok(result);
    }

    if let Ok(result) =
        workspace::WorkspaceTools::execute_tool(service, tool_name, arguments).await
    {
        return Ok(result);
    }

    if let Ok(result) =
        webhook::WebhookTools::execute_tool(service, tool_name, arguments).await
    {
        return Ok(result);
    }

    if let Ok(result) =
        types::TypeTools::execute_tool(service, tool_name, arguments).await
    {
        return Ok(result);
    }

    if let Ok(result) =
        audit::AuditTools::execute_tool(service, tool_name, arguments).await
    {
        return Ok(result);
    }

    if let Ok(result) = misc::MiscTools::execute_tool(service, tool_name, arguments).await
    {
        return Ok(result);
    }

    Err(format!("Unknown tool: {}", tool_name).into())
}

