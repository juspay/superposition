/// Macro to help create MCPToolsGroup implementations from MCPTool implementations
/// This reduces boilerplate code when composing individual tools into groups
#[macro_export]
macro_rules! compose_tools_group {
    ($group_name:ident, $($tool:ty),*) => {
        pub struct $group_name;

        impl $crate::tools::MCPToolsGroup for $group_name {
            fn get_tool_definitions() -> Vec<$crate::mcp_service::Tool> {
                vec![
                    $(<$tool>::get_definition(),)*
                ]
            }

            async fn execute_tool(
                service: &$crate::mcp_service::McpService,
                tool_name: &str,
                arguments: &serde_json::Value,
                token: Option<&str>,
            ) -> Result<serde_json::Value, Box<dyn std::error::Error>> {
                match tool_name {
                    $(
                        tool_name if tool_name == <$tool>::name() => {
                            <$tool>::execute(service, arguments, token).await
                        }
                    )*
                    _ => Err(format!("Unknown tool: {}", tool_name).into()),
                }
            }
        }
    };
}


