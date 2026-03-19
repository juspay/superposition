use rmcp::model::*;
use schemars::JsonSchema;
use serde::Deserialize;
use crate::helpers::*;
use crate::SuperpositionMcpServer;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct CreateFunctionParams { pub function_name: String, pub description: String, pub change_reason: String, pub function: String, pub runtime_version: String, pub function_type: String }
#[derive(Debug, Deserialize, JsonSchema)]
pub struct GetFunctionParams { pub function_name: String }
#[derive(Debug, Deserialize, JsonSchema)]
pub struct ListFunctionsParams { pub count: Option<i32>, pub page: Option<i32>, pub all: Option<bool>, pub function_type: Option<Vec<String>> }
#[derive(Debug, Deserialize, JsonSchema)]
pub struct UpdateFunctionParams { pub function_name: String, pub change_reason: String, pub description: Option<String>, pub function: Option<String>, pub runtime_version: Option<String> }
#[derive(Debug, Deserialize, JsonSchema)]
pub struct DeleteFunctionParams { pub function_name: String }
#[derive(Debug, Deserialize, JsonSchema)]
pub struct PublishFunctionParams { pub function_name: String, pub change_reason: String }
#[derive(Debug, Deserialize, JsonSchema)]
pub struct TestFunctionParams { pub function_name: String, pub stage: String, pub request: serde_json::Value }

fn parse_ft(s: &str) -> superposition_sdk::types::FunctionTypes {
    match s.to_uppercase().as_str() { "VALUE_COMPUTE" => superposition_sdk::types::FunctionTypes::ValueCompute, "CONTEXT_VALIDATION" => superposition_sdk::types::FunctionTypes::ContextValidation, "CHANGE_REASON_VALIDATION" => superposition_sdk::types::FunctionTypes::ChangeReasonValidation, _ => superposition_sdk::types::FunctionTypes::ValueValidation }
}

impl SuperpositionMcpServer {
    pub async fn create_function_impl(&self, args: CreateFunctionParams) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self.client.create_function().workspace_id(&self.config.workspace_id).org_id(&self.config.org_id)
            .function_name(args.function_name).description(args.description).change_reason(args.change_reason)
            .function(args.function).runtime_version(superposition_sdk::types::FunctionRuntimeVersion::V1).function_type(parse_ft(&args.function_type))
            .send().await.map_err(|e| mcp_err(e))?;
        Ok(CallToolResult::success(vec![Content::text(serde_json::to_string_pretty(&function_to_json!(resp)).map_err(mcp_err)?)]))
    }
    pub async fn get_function_impl(&self, args: GetFunctionParams) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self.client.get_function().workspace_id(&self.config.workspace_id).org_id(&self.config.org_id).function_name(args.function_name).send().await.map_err(|e| mcp_err(e))?;
        Ok(CallToolResult::success(vec![Content::text(serde_json::to_string_pretty(&function_to_json!(resp)).map_err(mcp_err)?)]))
    }
    pub async fn list_functions_impl(&self, args: ListFunctionsParams) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self.client.list_function().workspace_id(&self.config.workspace_id).org_id(&self.config.org_id);
        if let Some(c) = args.count { req = req.count(c); }
        if let Some(p) = args.page { req = req.page(p); }
        if let Some(a) = args.all { req = req.all(a); }
        if let Some(types) = args.function_type { for t in types { req = req.function_type(parse_ft(&t)); } }
        let resp = req.send().await.map_err(|e| mcp_err(e))?;
        let items: Vec<serde_json::Value> = resp.data.iter().map(|r| function_to_json!(r)).collect();
        Ok(CallToolResult::success(vec![Content::text(serde_json::to_string_pretty(&serde_json::json!({"total_pages": resp.total_pages, "total_items": resp.total_items, "data": items})).map_err(mcp_err)?)]))
    }
    pub async fn update_function_impl(&self, args: UpdateFunctionParams) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self.client.update_function().workspace_id(&self.config.workspace_id).org_id(&self.config.org_id).function_name(args.function_name).change_reason(args.change_reason);
        if let Some(d) = args.description { req = req.description(d); }
        if let Some(f) = args.function { req = req.function(f); }
        if args.runtime_version.is_some() { req = req.runtime_version(superposition_sdk::types::FunctionRuntimeVersion::V1); }
        let resp = req.send().await.map_err(|e| mcp_err(e))?;
        Ok(CallToolResult::success(vec![Content::text(serde_json::to_string_pretty(&function_to_json!(resp)).map_err(mcp_err)?)]))
    }
    pub async fn delete_function_impl(&self, args: DeleteFunctionParams) -> Result<CallToolResult, rmcp::ErrorData> {
        self.client.delete_function().workspace_id(&self.config.workspace_id).org_id(&self.config.org_id).function_name(args.function_name).send().await.map_err(|e| mcp_err(e))?;
        Ok(CallToolResult::success(vec![Content::text("Function deleted successfully")]))
    }
    pub async fn publish_function_impl(&self, args: PublishFunctionParams) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self.client.publish().workspace_id(&self.config.workspace_id).org_id(&self.config.org_id).function_name(args.function_name).change_reason(args.change_reason).send().await.map_err(|e| mcp_err(e))?;
        Ok(CallToolResult::success(vec![Content::text(serde_json::to_string_pretty(&function_to_json!(resp)).map_err(mcp_err)?)]))
    }
    pub async fn test_function_impl(&self, args: TestFunctionParams) -> Result<CallToolResult, rmcp::ErrorData> {
        let result = serde_json::json!({"message": "Function testing requires a typed FunctionExecutionRequest. Use the Superposition UI or SDK directly.", "function_name": args.function_name, "stage": args.stage, "request": args.request});
        Ok(CallToolResult::success(vec![Content::text(serde_json::to_string_pretty(&result).map_err(mcp_err)?)]))
    }
}
