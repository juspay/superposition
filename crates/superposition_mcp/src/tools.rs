use smithy_mcp_runtime::{McpError, ToolInfo};
use crate::types::*;
use superposition_sdk::Client;

pub async fn handle_add_members_to_group(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: ModifyMembersToGroupRequest = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::add_members_to_group::builders::AddMembersToGroupInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ExperimentGroupResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_add_members_to_group() -> ToolInfo {
    ToolInfo {
        name: "AddMembersToGroup".to_string(),
        description: "Adds members to an existing experiment group.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "id": { "type": "string" },
        "change_reason": { "type": "string" },
        "member_experiment_ids": {
            "type": "array",
            "items": { "type": "string" }
        }
    },
    "required": ["workspace_id", "org_id", "id", "change_reason", "member_experiment_ids"]
}),
    }
}

pub async fn handle_applicable_variants(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: ApplicableVariantsInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::applicable_variants::builders::ApplicableVariantsInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ApplicableVariantsOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_applicable_variants() -> ToolInfo {
    ToolInfo {
        name: "ApplicableVariants".to_string(),
        description: "Determines which experiment variants are applicable to a given context, used for experiment evaluation and variant selection.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "context": {
            "type": "object",
            "additionalProperties": { "type": "object" }
        },
        "identifier": { "type": "string" },
        "prefix": {
            "type": "array",
            "items": { "type": "string" }
        }
    },
    "required": ["workspace_id", "org_id", "context", "identifier"]
}),
    }
}

pub async fn handle_bulk_operation(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: BulkOperationInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::bulk_operation::builders::BulkOperationInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: BulkOperationOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_bulk_operation() -> ToolInfo {
    ToolInfo {
        name: "BulkOperation".to_string(),
        description: "Executes multiple context operations (PUT, REPLACE, DELETE, MOVE) in a single atomic transaction for efficient batch processing.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "config_tags": { "type": "string" },
        "operations": {
            "type": "array",
            "items": {
                "oneOf": [
                    {
                        "type": "object",
                        "properties": {
                            "PUT": {
                                "type": "object",
                                "properties": {
                                    "context": {
                                        "type": "object",
                                        "additionalProperties": { "type": "object" }
                                    },
                                    "override": {
                                        "type": "object",
                                        "additionalProperties": { "type": "object" }
                                    },
                                    "description": { "type": "string" },
                                    "change_reason": { "type": "string" }
                                },
                                "required": ["context", "override", "change_reason"]
                            }
                        },
                        "required": ["PUT"]
                    },
                    {
                        "type": "object",
                        "properties": {
                            "REPLACE": {
                                "type": "object",
                                "properties": {
                                    "context": {
                                        "oneOf": [
                                            {
                                                "type": "object",
                                                "properties": {
                                                    "id": { "type": "string" }
                                                },
                                                "required": ["id"]
                                            },
                                            {
                                                "type": "object",
                                                "properties": {
                                                    "context": {
                                                        "type": "object",
                                                        "additionalProperties": { "type": "object" }
                                                    }
                                                },
                                                "required": ["context"]
                                            }
                                        ]
                                    },
                                    "override": {
                                        "type": "object",
                                        "additionalProperties": { "type": "object" }
                                    },
                                    "description": { "type": "string" },
                                    "change_reason": { "type": "string" }
                                },
                                "required": ["context", "override", "change_reason"]
                            }
                        },
                        "required": ["REPLACE"]
                    },
                    {
                        "type": "object",
                        "properties": {
                            "DELETE": { "type": "string" }
                        },
                        "required": ["DELETE"]
                    },
                    {
                        "type": "object",
                        "properties": {
                            "MOVE": {
                                "type": "object",
                                "properties": {
                                    "id": { "type": "string" },
                                    "request": {
                                        "type": "object",
                                        "properties": {
                                            "context": {
                                                "type": "object",
                                                "additionalProperties": { "type": "object" }
                                            },
                                            "description": { "type": "string" },
                                            "change_reason": { "type": "string" }
                                        },
                                        "required": ["context", "change_reason"]
                                    }
                                },
                                "required": ["id", "request"]
                            }
                        },
                        "required": ["MOVE"]
                    }
                ]
            }
        }
    },
    "required": ["workspace_id", "org_id", "operations"]
}),
    }
}

pub async fn handle_conclude_experiment(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: ConcludeExperimentInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::conclude_experiment::builders::ConcludeExperimentInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ExperimentResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_conclude_experiment() -> ToolInfo {
    ToolInfo {
        name: "ConcludeExperiment".to_string(),
        description: "Concludes an inprogress experiment by selecting a winning variant and transitioning the experiment to a concluded state.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "id": { "type": "string" },
        "chosen_variant": { "type": "string" },
        "description": { "type": "string" },
        "change_reason": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "id", "chosen_variant", "change_reason"]
}),
    }
}

pub async fn handle_create_context(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: CreateContextInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::create_context::builders::CreateContextInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ContextResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_create_context() -> ToolInfo {
    ToolInfo {
        name: "CreateContext".to_string(),
        description: "Creates a new context with specified conditions and overrides. Contexts define conditional rules for config management.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "config_tags": { "type": "string" },
        "request": {
            "type": "object",
            "properties": {
                "context": {
                    "type": "object",
                    "additionalProperties": { "type": "object" }
                },
                "override": {
                    "type": "object",
                    "additionalProperties": { "type": "object" }
                },
                "description": { "type": "string" },
                "change_reason": { "type": "string" }
            },
            "required": ["context", "override", "change_reason"]
        }
    },
    "required": ["workspace_id", "org_id", "request"]
}),
    }
}

pub async fn handle_create_default_config(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: CreateDefaultConfigInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::create_default_config::builders::CreateDefaultConfigInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: DefaultConfigResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_create_default_config() -> ToolInfo {
    ToolInfo {
        name: "CreateDefaultConfig".to_string(),
        description: "Creates a new default config entry with specified key, value, schema, and metadata. Default configs serve as fallback values when no specific context matches.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "key": { "type": "string" },
        "value": { "type": "object" },
        "schema": {
            "type": "object",
            "additionalProperties": { "type": "object" }
        },
        "description": { "type": "string" },
        "change_reason": { "type": "string" },
        "value_validation_function_name": { "type": "string" },
        "value_compute_function_name": { "type": "string" },
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" }
    },
    "required": ["key", "value", "schema", "description", "change_reason", "workspace_id", "org_id"]
}),
    }
}

pub async fn handle_create_dimension(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: CreateDimensionInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::create_dimension::builders::CreateDimensionInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: DimensionResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_create_dimension() -> ToolInfo {
    ToolInfo {
        name: "CreateDimension".to_string(),
        description: "Creates a new dimension with the specified json schema. Dimensions define categorical attributes used for context-based config management.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "dimension": { "type": "string" },
        "position": { "type": "integer" },
        "schema": {
            "type": "object",
            "additionalProperties": { "type": "object" }
        },
        "value_validation_function_name": { "type": "string" },
        "description": { "type": "string" },
        "change_reason": { "type": "string" },
        "dimension_type": {
            "oneOf": [
                {
                    "type": "object",
                    "properties": {
                        "REGULAR": { "type": "null" }
                    },
                    "required": ["REGULAR"]
                },
                {
                    "type": "object",
                    "properties": {
                        "LOCAL_COHORT": { "type": "string" }
                    },
                    "required": ["LOCAL_COHORT"]
                },
                {
                    "type": "object",
                    "properties": {
                        "REMOTE_COHORT": { "type": "string" }
                    },
                    "required": ["REMOTE_COHORT"]
                }
            ]
        },
        "value_compute_function_name": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "dimension", "position", "schema", "description", "change_reason"]
}),
    }
}

pub async fn handle_create_experiment(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: CreateExperimentRequest = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::create_experiment::builders::CreateExperimentInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ExperimentResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_create_experiment() -> ToolInfo {
    ToolInfo {
        name: "CreateExperiment".to_string(),
        description: "Creates a new experiment with variants, context and conditions. You can optionally specify metrics and experiment group for tracking and analysis.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "name": { "type": "string" },
        "experiment_type": {
            "type": "string",
            "enum": ["DEFAULT", "DELETE_OVERRIDES"]
        },
        "context": {
            "type": "object",
            "additionalProperties": { "type": "object" }
        },
        "variants": {
            "type": "array",
            "items": {
                "type": "object",
                "properties": {
                    "id": { "type": "string" },
                    "variant_type": {
                        "type": "string",
                        "enum": ["CONTROL", "EXPERIMENTAL"]
                    },
                    "context_id": { "type": "string" },
                    "override_id": { "type": "string" },
                    "overrides": {
                        "type": "object",
                        "additionalProperties": { "type": "object" }
                    }
                },
                "required": ["id", "variant_type", "overrides"]
            }
        },
        "description": { "type": "string" },
        "change_reason": { "type": "string" },
        "metrics": { "type": "object" },
        "experiment_group_id": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "name", "context", "variants", "description", "change_reason"]
}),
    }
}

pub async fn handle_create_experiment_group(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: CreateExperimentGroupRequest = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::create_experiment_group::builders::CreateExperimentGroupInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ExperimentGroupResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_create_experiment_group() -> ToolInfo {
    ToolInfo {
        name: "CreateExperimentGroup".to_string(),
        description: "Creates a new experiment group.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "name": { "type": "string" },
        "description": { "type": "string" },
        "change_reason": { "type": "string" },
        "context": {
            "type": "object",
            "additionalProperties": { "type": "object" }
        },
        "traffic_percentage": { "type": "integer" },
        "member_experiment_ids": {
            "type": "array",
            "items": { "type": "string" }
        }
    },
    "required": ["workspace_id", "org_id", "name", "description", "change_reason", "context", "traffic_percentage"]
}),
    }
}

pub async fn handle_create_function(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: CreateFunctionRequest = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::create_function::builders::CreateFunctionInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: FunctionResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_create_function() -> ToolInfo {
    ToolInfo {
        name: "CreateFunction".to_string(),
        description: "Creates a new custom function for value_validation, value_compute, context_validation or change_reason_validation with specified code, runtime version, and function type.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "function_name": { "type": "string" },
        "description": { "type": "string" },
        "change_reason": { "type": "string" },
        "function": { "type": "string" },
        "runtime_version": {
            "type": "string",
            "enum": ["1.0"]
        },
        "function_type": {
            "type": "string",
            "enum": ["VALUE_VALIDATION", "VALUE_COMPUTE", "CONTEXT_VALIDATION", "CHANGE_REASON_VALIDATION"]
        }
    },
    "required": ["workspace_id", "org_id", "function_name", "description", "change_reason", "function", "runtime_version", "function_type"]
}),
    }
}

pub async fn handle_create_organisation(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: CreateOrganisationRequest = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::create_organisation::builders::CreateOrganisationInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: OrganisationResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_create_organisation() -> ToolInfo {
    ToolInfo {
        name: "CreateOrganisation".to_string(),
        description: "Creates a new organisation with specified name and administrator email. This is the top-level entity that contains workspaces and manages organizational-level settings.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "country_code": { "type": "string" },
        "contact_email": { "type": "string" },
        "contact_phone": { "type": "string" },
        "admin_email": { "type": "string" },
        "sector": { "type": "string" },
        "name": { "type": "string" }
    },
    "required": ["admin_email", "name"]
}),
    }
}

pub async fn handle_create_secret(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: CreateSecretInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::create_secret::builders::CreateSecretInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: SecretResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_create_secret() -> ToolInfo {
    ToolInfo {
        name: "CreateSecret".to_string(),
        description: "Creates a new encrypted secret with the specified name and value. The secret is encrypted with the workspace's current encryption key. Secret values are never returned in responses for security.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "name": { "type": "string" },
        "value": { "type": "string" },
        "description": { "type": "string" },
        "change_reason": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "name", "value", "description", "change_reason"]
}),
    }
}

pub async fn handle_create_type_templates(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: CreateTypeTemplatesRequest = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::create_type_templates::builders::CreateTypeTemplatesInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: TypeTemplatesResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_create_type_templates() -> ToolInfo {
    ToolInfo {
        name: "CreateTypeTemplates".to_string(),
        description: "Creates a new type template with specified schema definition, providing reusable type definitions for config validation.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "type_name": { "type": "string" },
        "type_schema": {
            "type": "object",
            "additionalProperties": { "type": "object" }
        },
        "description": { "type": "string" },
        "change_reason": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "type_name", "type_schema", "description", "change_reason"]
}),
    }
}

pub async fn handle_create_variable(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: CreateVariableInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::create_variable::builders::CreateVariableInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: VariableResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_create_variable() -> ToolInfo {
    ToolInfo {
        name: "CreateVariable".to_string(),
        description: "Creates a new variable with the specified name and value.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "name": { "type": "string" },
        "value": { "type": "string" },
        "description": { "type": "string" },
        "change_reason": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "name", "value", "description", "change_reason"]
}),
    }
}

pub async fn handle_create_webhook(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: CreateWebhookInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::create_webhook::builders::CreateWebhookInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: WebhookResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_create_webhook() -> ToolInfo {
    ToolInfo {
        name: "CreateWebhook".to_string(),
        description: "Creates a new webhook config to receive HTTP notifications when specified events occur in the system.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "name": { "type": "string" },
        "description": { "type": "string" },
        "enabled": { "type": "boolean" },
        "url": { "type": "string" },
        "method": {
            "type": "string",
            "enum": ["GET", "POST", "PUT", "PATCH", "DELETE", "HEAD"]
        },
        "version": {
            "type": "string",
            "enum": ["V1"]
        },
        "custom_headers": {
            "type": "object",
            "additionalProperties": { "type": "object" }
        },
        "events": {
            "type": "array",
            "items": { "type": "string" }
        },
        "change_reason": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "name", "description", "enabled", "url", "method", "events", "change_reason"]
}),
    }
}

pub async fn handle_create_workspace(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: CreateWorkspaceRequest = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::create_workspace::builders::CreateWorkspaceInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: WorkspaceResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_create_workspace() -> ToolInfo {
    ToolInfo {
        name: "CreateWorkspace".to_string(),
        description: "Creates a new workspace within an organisation, including database schema setup and isolated environment for config management with specified admin and settings.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "org_id": { "type": "string" },
        "workspace_admin_email": { "type": "string" },
        "workspace_name": { "type": "string" },
        "workspace_status": {
            "type": "string",
            "enum": ["ENABLED", "DISABLED"]
        },
        "metrics": { "type": "object" },
        "allow_experiment_self_approval": { "type": "boolean" },
        "auto_populate_control": { "type": "boolean" },
        "enable_context_validation": { "type": "boolean" },
        "enable_change_reason_validation": { "type": "boolean" }
    },
    "required": ["org_id", "workspace_admin_email", "workspace_name"]
}),
    }
}

pub async fn handle_delete_context(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: DeleteContextInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::delete_context::builders::DeleteContextInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: Unit = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_delete_context() -> ToolInfo {
    ToolInfo {
        name: "DeleteContext".to_string(),
        description: "Permanently removes a context from the workspace. This operation cannot be undone and will affect config resolution.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "id": { "type": "string" },
        "config_tags": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "id"]
}),
    }
}

pub async fn handle_delete_default_config(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: DeleteDefaultConfigInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::delete_default_config::builders::DeleteDefaultConfigInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: Unit = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_delete_default_config() -> ToolInfo {
    ToolInfo {
        name: "DeleteDefaultConfig".to_string(),
        description: "Permanently removes a default config entry from the workspace. This operation cannot be performed if it affects config resolution for contexts that rely on this fallback value.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "key": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "key"]
}),
    }
}

pub async fn handle_delete_dimension(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: DeleteDimensionInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::delete_dimension::builders::DeleteDimensionInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: Unit = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_delete_dimension() -> ToolInfo {
    ToolInfo {
        name: "DeleteDimension".to_string(),
        description: "Permanently removes a dimension from the workspace. This operation will fail if the dimension has active dependencies or is referenced by existing configurations.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "dimension": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "dimension"]
}),
    }
}

pub async fn handle_delete_experiment_group(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: DeleteExperimentGroupInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::delete_experiment_group::builders::DeleteExperimentGroupInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ExperimentGroupResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_delete_experiment_group() -> ToolInfo {
    ToolInfo {
        name: "DeleteExperimentGroup".to_string(),
        description: "Deletes an experiment group.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "id": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "id"]
}),
    }
}

pub async fn handle_delete_function(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: DeleteFunctionInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::delete_function::builders::DeleteFunctionInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: Unit = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_delete_function() -> ToolInfo {
    ToolInfo {
        name: "DeleteFunction".to_string(),
        description: "Permanently removes a function from the workspace, deleting both draft and published versions along with all associated code. It fails if already in use".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "function_name": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "function_name"]
}),
    }
}

pub async fn handle_delete_secret(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: DeleteSecretInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::delete_secret::builders::DeleteSecretInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: SecretResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_delete_secret() -> ToolInfo {
    ToolInfo {
        name: "DeleteSecret".to_string(),
        description: "Permanently deletes a secret from the workspace. The encrypted value is removed and cannot be recovered.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "name": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "name"]
}),
    }
}

pub async fn handle_delete_type_templates(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: DeleteTypeTemplatesInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::delete_type_templates::builders::DeleteTypeTemplatesInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: TypeTemplatesResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_delete_type_templates() -> ToolInfo {
    ToolInfo {
        name: "DeleteTypeTemplates".to_string(),
        description: "Permanently removes a type template from the workspace. No checks performed while deleting".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "type_name": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "type_name"]
}),
    }
}

pub async fn handle_delete_variable(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: DeleteVariableInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::delete_variable::builders::DeleteVariableInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: VariableResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_delete_variable() -> ToolInfo {
    ToolInfo {
        name: "DeleteVariable".to_string(),
        description: "Permanently deletes a variable from the workspace.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "name": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "name"]
}),
    }
}

pub async fn handle_delete_webhook(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: DeleteWebhookInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::delete_webhook::builders::DeleteWebhookInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: Unit = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_delete_webhook() -> ToolInfo {
    ToolInfo {
        name: "DeleteWebhook".to_string(),
        description: "Permanently removes a webhook config from the workspace, stopping all future event notifications to that endpoint.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "name": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "name"]
}),
    }
}

pub async fn handle_discard_experiment(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: DiscardExperimentInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::discard_experiment::builders::DiscardExperimentInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ExperimentResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_discard_experiment() -> ToolInfo {
    ToolInfo {
        name: "DiscardExperiment".to_string(),
        description: "Discards an experiment without selecting a winner, effectively canceling the experiment and removing its effects.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "id": { "type": "string" },
        "change_reason": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "id", "change_reason"]
}),
    }
}

pub async fn handle_get_config(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: GetConfigInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::get_config::builders::GetConfigInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: GetConfigOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_get_config() -> ToolInfo {
    ToolInfo {
        name: "GetConfig".to_string(),
        description: "Retrieves config data with context evaluation, including applicable contexts, overrides, and default values based on provided conditions.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "prefix": {
            "type": "array",
            "items": { "type": "string" }
        },
        "version": { "type": "string" },
        "if_modified_since": { "type": "string", "format": "date-time" },
        "context": {
            "type": "object",
            "additionalProperties": { "type": "object" }
        }
    },
    "required": ["workspace_id", "org_id"]
}),
    }
}

pub async fn handle_get_config_json(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: GetConfigJsonInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::get_config_json::builders::GetConfigJsonInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: GetConfigJsonOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_get_config_json() -> ToolInfo {
    ToolInfo {
        name: "GetConfigJson".to_string(),
        description: "Retrieves the full config in JSON format, including default configs with schemas, dimensions, and overrides. This endpoint is optimized for clients that prefer JSON format for configuration management.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "if_modified_since": { "type": "string", "format": "date-time" }
    },
    "required": ["workspace_id", "org_id"]
}),
    }
}

pub async fn handle_get_config_toml(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: GetConfigTomlInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::get_config_toml::builders::GetConfigTomlInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: GetConfigTomlOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_get_config_toml() -> ToolInfo {
    ToolInfo {
        name: "GetConfigToml".to_string(),
        description: "Retrieves the full config in TOML format, including default configs with schemas, dimensions, and overrides. This endpoint is optimized for clients that prefer TOML format for configuration management.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "if_modified_since": { "type": "string", "format": "date-time" }
    },
    "required": ["workspace_id", "org_id"]
}),
    }
}

pub async fn handle_get_context(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: GetContextInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::get_context::builders::GetContextInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ContextResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_get_context() -> ToolInfo {
    ToolInfo {
        name: "GetContext".to_string(),
        description: "Retrieves detailed information about a specific context by its unique identifier, including conditions, overrides, and metadata.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "id": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "id"]
}),
    }
}

pub async fn handle_get_context_from_condition(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: GetContextFromConditionInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::get_context_from_condition::builders::GetContextFromConditionInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ContextResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_get_context_from_condition() -> ToolInfo {
    ToolInfo {
        name: "GetContextFromCondition".to_string(),
        description: "Retrieves context information by matching against provided conditions. Used to find contexts that would apply to specific scenarios.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "context": { "type": "object" }
    },
    "required": ["workspace_id", "org_id"]
}),
    }
}

pub async fn handle_get_default_config(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: GetDefaultConfigInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::get_default_config::builders::GetDefaultConfigInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: DefaultConfigResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_get_default_config() -> ToolInfo {
    ToolInfo {
        name: "GetDefaultConfig".to_string(),
        description: "Retrieves a specific default config entry by its key, including its value, schema, function mappings, and metadata.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "key": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "key"]
}),
    }
}

pub async fn handle_get_dimension(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: GetDimensionInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::get_dimension::builders::GetDimensionInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: DimensionResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_get_dimension() -> ToolInfo {
    ToolInfo {
        name: "GetDimension".to_string(),
        description: "Retrieves detailed information about a specific dimension, including its schema, cohort dependency graph, and configuration metadata.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "dimension": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "dimension"]
}),
    }
}

pub async fn handle_get_experiment(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: GetExperimentInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::get_experiment::builders::GetExperimentInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ExperimentResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_get_experiment() -> ToolInfo {
    ToolInfo {
        name: "GetExperiment".to_string(),
        description: "Retrieves detailed information about a specific experiment, including its config, variants, status, and metrics.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "id": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "id"]
}),
    }
}

pub async fn handle_get_experiment_config(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: GetExperimentConfigInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::get_experiment_config::builders::GetExperimentConfigInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: GetExperimentConfigOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_get_experiment_config() -> ToolInfo {
    ToolInfo {
        name: "GetExperimentConfig".to_string(),
        description: "Retrieves the experiment configuration for a given workspace and organization. The response includes details of all experiment groups and experiments that match the specified filters.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "if_modified_since": { "type": "string", "format": "date-time" },
        "prefix": {
            "type": "array",
            "items": { "type": "string" }
        },
        "context": {
            "type": "object",
            "additionalProperties": { "type": "object" }
        },
        "dimension_match_strategy": {
            "type": "string",
            "enum": ["exact", "subset"]
        }
    },
    "required": ["workspace_id", "org_id"]
}),
    }
}

pub async fn handle_get_experiment_group(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: GetExperimentGroupInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::get_experiment_group::builders::GetExperimentGroupInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ExperimentGroupResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_get_experiment_group() -> ToolInfo {
    ToolInfo {
        name: "GetExperimentGroup".to_string(),
        description: "Retrieves an existing experiment group by its ID.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "id": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "id"]
}),
    }
}

pub async fn handle_get_function(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: GetFunctionInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::get_function::builders::GetFunctionInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: FunctionResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_get_function() -> ToolInfo {
    ToolInfo {
        name: "GetFunction".to_string(),
        description: "Retrieves detailed information about a specific function including its published and draft versions, code, and metadata.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "function_name": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "function_name"]
}),
    }
}

pub async fn handle_get_organisation(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: GetOrganisationInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::get_organisation::builders::GetOrganisationInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: OrganisationResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_get_organisation() -> ToolInfo {
    ToolInfo {
        name: "GetOrganisation".to_string(),
        description: "Retrieves detailed information about a specific organisation including its status, contact details, and administrative metadata.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "id": { "type": "string" }
    },
    "required": ["id"]
}),
    }
}

pub async fn handle_get_resolved_config(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: GetResolvedConfigInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::get_resolved_config::builders::GetResolvedConfigInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: GetResolvedConfigOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_get_resolved_config() -> ToolInfo {
    ToolInfo {
        name: "GetResolvedConfig".to_string(),
        description: "Resolves and merges config values based on context conditions, applying overrides and merge strategies to produce the final configuration.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "prefix": {
            "type": "array",
            "items": { "type": "string" }
        },
        "version": { "type": "string" },
        "show_reasoning": { "type": "boolean" },
        "merge_strategy": {
            "type": "string",
            "enum": ["MERGE", "REPLACE"]
        },
        "context_id": { "type": "string" },
        "resolve_remote": { "type": "boolean" },
        "context": {
            "type": "object",
            "additionalProperties": { "type": "object" }
        }
    },
    "required": ["workspace_id", "org_id"]
}),
    }
}

pub async fn handle_get_resolved_config_with_identifier(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: GetResolvedConfigWithIdentifierInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::get_resolved_config_with_identifier::builders::GetResolvedConfigWithIdentifierInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: GetResolvedConfigWithIdentifierOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_get_resolved_config_with_identifier() -> ToolInfo {
    ToolInfo {
        name: "GetResolvedConfigWithIdentifier".to_string(),
        description: "Resolves and merges config values based on context conditions and identifier, applying overrides and merge strategies to produce the final configuration.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "prefix": {
            "type": "array",
            "items": { "type": "string" }
        },
        "version": { "type": "string" },
        "show_reasoning": { "type": "boolean" },
        "merge_strategy": {
            "type": "string",
            "enum": ["MERGE", "REPLACE"]
        },
        "context_id": { "type": "string" },
        "resolve_remote": { "type": "boolean" },
        "context": {
            "type": "object",
            "additionalProperties": { "type": "object" }
        },
        "identifier": { "type": "string" }
    },
    "required": ["workspace_id", "org_id"]
}),
    }
}

pub async fn handle_get_secret(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: GetSecretInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::get_secret::builders::GetSecretInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: SecretResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_get_secret() -> ToolInfo {
    ToolInfo {
        name: "GetSecret".to_string(),
        description: "Retrieves detailed information about a specific secret by its name. The value is masked for security.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "name": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "name"]
}),
    }
}

pub async fn handle_get_type_template(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: GetTypeTemplateInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::get_type_template::builders::GetTypeTemplateInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: TypeTemplatesResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_get_type_template() -> ToolInfo {
    ToolInfo {
        name: "GetTypeTemplate".to_string(),
        description: "Retrieves detailed information about a specific type template including its schema and metadata.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "type_name": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "type_name"]
}),
    }
}

pub async fn handle_get_type_templates_list(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: GetTypeTemplatesListInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::get_type_templates_list::builders::GetTypeTemplatesListInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: GetTypeTemplatesListOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_get_type_templates_list() -> ToolInfo {
    ToolInfo {
        name: "GetTypeTemplatesList".to_string(),
        description: "Retrieves a paginated list of all type templates in the workspace, including their schemas and metadata for type management.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "count": { "type": "integer" },
        "page": { "type": "integer" },
        "all": { "type": "boolean" },
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" }
    },
    "required": ["workspace_id", "org_id"]
}),
    }
}

pub async fn handle_get_variable(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: GetVariableInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::get_variable::builders::GetVariableInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: VariableResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_get_variable() -> ToolInfo {
    ToolInfo {
        name: "GetVariable".to_string(),
        description: "Retrieves detailed information about a specific variable by its name.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "name": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "name"]
}),
    }
}

pub async fn handle_get_version(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: GetVersionInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::get_version::builders::GetVersionInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: GetVersionResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_get_version() -> ToolInfo {
    ToolInfo {
        name: "GetVersion".to_string(),
        description: "Retrieves a specific config version along with its metadata for audit and rollback purposes.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "id": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "id"]
}),
    }
}

pub async fn handle_get_webhook(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: GetWebhookInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::get_webhook::builders::GetWebhookInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: WebhookResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_get_webhook() -> ToolInfo {
    ToolInfo {
        name: "GetWebhook".to_string(),
        description: "Retrieves detailed information about a specific webhook config, including its events, headers, and trigger history.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "name": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "name"]
}),
    }
}

pub async fn handle_get_webhook_by_event(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: GetWebhookByEventInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::get_webhook_by_event::builders::GetWebhookByEventInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: WebhookResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_get_webhook_by_event() -> ToolInfo {
    ToolInfo {
        name: "GetWebhookByEvent".to_string(),
        description: "Retrieves a webhook configuration based on a specific event type, allowing users to find which webhook is set to trigger for that event.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "event": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "event"]
}),
    }
}

pub async fn handle_get_workspace(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: GetWorkspaceInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::get_workspace::builders::GetWorkspaceInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: WorkspaceResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_get_workspace() -> ToolInfo {
    ToolInfo {
        name: "GetWorkspace".to_string(),
        description: "Retrieves detailed information about a specific workspace including its configuration and metadata.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "org_id": { "type": "string" },
        "workspace_name": { "type": "string" }
    },
    "required": ["org_id", "workspace_name"]
}),
    }
}

pub async fn handle_list_audit_logs(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: ListAuditLogsInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::list_audit_logs::builders::ListAuditLogsInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ListAuditLogsOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_list_audit_logs() -> ToolInfo {
    ToolInfo {
        name: "ListAuditLogs".to_string(),
        description: "Retrieves a paginated list of audit logs with support for filtering by date range, table names, actions, and usernames for compliance and monitoring purposes.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "count": { "type": "integer" },
        "page": { "type": "integer" },
        "all": { "type": "boolean" },
        "from_date": { "type": "string", "format": "date-time" },
        "to_date": { "type": "string", "format": "date-time" },
        "tables": {
            "type": "array",
            "items": { "type": "string" }
        },
        "action": {
            "type": "array",
            "items": {
                "type": "string",
                "enum": ["INSERT", "UPDATE", "DELETE"]
            }
        },
        "username": { "type": "string" },
        "sort_by": {
            "type": "string",
            "enum": ["desc", "asc"]
        }
    },
    "required": ["workspace_id", "org_id"]
}),
    }
}

pub async fn handle_list_contexts(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: ListContextsInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::list_contexts::builders::ListContextsInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ListContextsOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_list_contexts() -> ToolInfo {
    ToolInfo {
        name: "ListContexts".to_string(),
        description: "Retrieves a paginated list of contexts with support for filtering by creation date, modification date, weight, and other criteria.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "count": { "type": "integer" },
        "page": { "type": "integer" },
        "all": { "type": "boolean" },
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "prefix": {
            "type": "array",
            "items": { "type": "string" }
        },
        "sort_on": {
            "type": "string",
            "enum": ["last_modified_at", "created_at", "weight"]
        },
        "sort_by": {
            "type": "string",
            "enum": ["desc", "asc"]
        },
        "created_by": {
            "type": "array",
            "items": { "type": "string" }
        },
        "last_modified_by": {
            "type": "array",
            "items": { "type": "string" }
        },
        "plaintext": { "type": "string" },
        "dimension_match_strategy": {
            "type": "string",
            "enum": ["exact", "subset"]
        }
    },
    "required": ["workspace_id", "org_id"]
}),
    }
}

pub async fn handle_list_default_configs(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: ListDefaultConfigsInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::list_default_configs::builders::ListDefaultConfigsInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ListDefaultConfigsOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_list_default_configs() -> ToolInfo {
    ToolInfo {
        name: "ListDefaultConfigs".to_string(),
        description: "Retrieves a paginated list of all default config entries in the workspace, including their values, schemas, and metadata.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "count": { "type": "integer" },
        "page": { "type": "integer" },
        "all": { "type": "boolean" },
        "name": { "type": "string" }
    },
    "required": ["workspace_id", "org_id"]
}),
    }
}

pub async fn handle_list_dimensions(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: ListDimensionsInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::list_dimensions::builders::ListDimensionsInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ListDimensionsOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_list_dimensions() -> ToolInfo {
    ToolInfo {
        name: "ListDimensions".to_string(),
        description: "Retrieves a paginated list of all dimensions in the workspace. Dimensions are returned with their details and metadata.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "count": { "type": "integer" },
        "page": { "type": "integer" },
        "all": { "type": "boolean" },
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" }
    },
    "required": ["workspace_id", "org_id"]
}),
    }
}

pub async fn handle_list_experiment(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: ListExperimentInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::list_experiment::builders::ListExperimentInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ListExperimentOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_list_experiment() -> ToolInfo {
    ToolInfo {
        name: "ListExperiment".to_string(),
        description: "Retrieves a paginated list of experiments with support for filtering by status, date range, name, creator, and experiment group.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "count": { "type": "integer" },
        "page": { "type": "integer" },
        "all": { "type": "boolean" },
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "if_modified_since": { "type": "string", "format": "date-time" },
        "status": {
            "type": "array",
            "items": {
                "type": "string",
                "enum": ["CREATED", "CONCLUDED", "INPROGRESS", "DISCARDED", "PAUSED"]
            }
        },
        "from_date": { "type": "string", "format": "date-time" },
        "to_date": { "type": "string", "format": "date-time" },
        "experiment_name": { "type": "string" },
        "experiment_ids": {
            "type": "array",
            "items": { "type": "string" }
        },
        "experiment_group_ids": {
            "type": "array",
            "items": { "type": "string" }
        },
        "created_by": {
            "type": "array",
            "items": { "type": "string" }
        },
        "sort_on": {
            "type": "string",
            "enum": ["last_modified_at", "created_at"]
        },
        "sort_by": {
            "type": "string",
            "enum": ["desc", "asc"]
        },
        "global_experiments_only": { "type": "boolean" },
        "dimension_match_strategy": {
            "type": "string",
            "enum": ["exact", "subset"]
        },
        "prefix": {
            "type": "array",
            "items": { "type": "string" }
        },
        "context": {
            "type": "object",
            "additionalProperties": { "type": "object" }
        }
    },
    "required": ["workspace_id", "org_id"]
}),
    }
}

pub async fn handle_list_experiment_groups(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: ListExperimentGroupsInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::list_experiment_groups::builders::ListExperimentGroupsInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ListExperimentGroupsOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_list_experiment_groups() -> ToolInfo {
    ToolInfo {
        name: "ListExperimentGroups".to_string(),
        description: "Lists experiment groups, with support for filtering and pagination.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "count": { "type": "integer" },
        "page": { "type": "integer" },
        "all": { "type": "boolean" },
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "if_modified_since": { "type": "string", "format": "date-time" },
        "name": { "type": "string" },
        "created_by": { "type": "string" },
        "last_modified_by": { "type": "string" },
        "sort_on": {
            "type": "string",
            "enum": ["name", "created_at", "last_modified_at"]
        },
        "sort_by": {
            "type": "string",
            "enum": ["desc", "asc"]
        },
        "group_type": {
            "type": "array",
            "items": {
                "type": "string",
                "enum": ["USER_CREATED", "SYSTEM_GENERATED"]
            }
        },
        "dimension_match_strategy": {
            "type": "string",
            "enum": ["exact", "subset"]
        },
        "context": {
            "type": "object",
            "additionalProperties": { "type": "object" }
        }
    },
    "required": ["workspace_id", "org_id"]
}),
    }
}

pub async fn handle_list_function(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: ListFunctionInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::list_function::builders::ListFunctionInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ListFunctionOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_list_function() -> ToolInfo {
    ToolInfo {
        name: "ListFunction".to_string(),
        description: "Retrieves a paginated list of all functions in the workspace with their basic information and current status.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "count": { "type": "integer" },
        "page": { "type": "integer" },
        "all": { "type": "boolean" },
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "function_type": {
            "type": "array",
            "items": {
                "type": "string",
                "enum": ["VALUE_VALIDATION", "VALUE_COMPUTE", "CONTEXT_VALIDATION", "CHANGE_REASON_VALIDATION"]
            }
        }
    },
    "required": ["workspace_id", "org_id"]
}),
    }
}

pub async fn handle_list_organisation(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: ListOrganisationInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::list_organisation::builders::ListOrganisationInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ListOrganisationOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_list_organisation() -> ToolInfo {
    ToolInfo {
        name: "ListOrganisation".to_string(),
        description: "Retrieves a paginated list of all organisations with their basic information, creation details, and current status.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "count": { "type": "integer" },
        "page": { "type": "integer" },
        "all": { "type": "boolean" }
    },
    "required": []
}),
    }
}

pub async fn handle_list_secrets(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: ListSecretsInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::list_secrets::builders::ListSecretsInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ListSecretsOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_list_secrets() -> ToolInfo {
    ToolInfo {
        name: "ListSecrets".to_string(),
        description: "Retrieves a paginated list of all secrets in the workspace with optional filtering and sorting. All secret values are masked.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "count": { "type": "integer" },
        "page": { "type": "integer" },
        "all": { "type": "boolean" },
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "name": {
            "type": "array",
            "items": { "type": "string" }
        },
        "created_by": {
            "type": "array",
            "items": { "type": "string" }
        },
        "last_modified_by": {
            "type": "array",
            "items": { "type": "string" }
        },
        "sort_on": {
            "type": "string",
            "enum": ["name", "created_at", "last_modified_at"]
        },
        "sort_by": {
            "type": "string",
            "enum": ["desc", "asc"]
        }
    },
    "required": ["workspace_id", "org_id"]
}),
    }
}

pub async fn handle_list_variables(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: ListVariablesInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::list_variables::builders::ListVariablesInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ListVariablesOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_list_variables() -> ToolInfo {
    ToolInfo {
        name: "ListVariables".to_string(),
        description: "Retrieves a paginated list of all variables in the workspace with optional filtering and sorting.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "count": { "type": "integer" },
        "page": { "type": "integer" },
        "all": { "type": "boolean" },
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "name": {
            "type": "array",
            "items": { "type": "string" }
        },
        "created_by": {
            "type": "array",
            "items": { "type": "string" }
        },
        "last_modified_by": {
            "type": "array",
            "items": { "type": "string" }
        },
        "sort_on": {
            "type": "string",
            "enum": ["name", "created_at", "last_modified_at"]
        },
        "sort_by": {
            "type": "string",
            "enum": ["desc", "asc"]
        }
    },
    "required": ["workspace_id", "org_id"]
}),
    }
}

pub async fn handle_list_versions(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: ListVersionsInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::list_versions::builders::ListVersionsInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ListVersionsOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_list_versions() -> ToolInfo {
    ToolInfo {
        name: "ListVersions".to_string(),
        description: "Retrieves a paginated list of config versions with their metadata, hash values, and creation timestamps for audit and rollback purposes.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "count": { "type": "integer" },
        "page": { "type": "integer" }
    },
    "required": ["workspace_id", "org_id"]
}),
    }
}

pub async fn handle_list_webhook(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: ListWebhookInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::list_webhook::builders::ListWebhookInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ListWebhookOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_list_webhook() -> ToolInfo {
    ToolInfo {
        name: "ListWebhook".to_string(),
        description: "Retrieves a paginated list of all webhook configs in the workspace, including their status and config details.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "count": { "type": "integer" },
        "page": { "type": "integer" },
        "all": { "type": "boolean" },
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" }
    },
    "required": ["workspace_id", "org_id"]
}),
    }
}

pub async fn handle_list_workspace(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: ListWorkspaceInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::list_workspace::builders::ListWorkspaceInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ListWorkspaceOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_list_workspace() -> ToolInfo {
    ToolInfo {
        name: "ListWorkspace".to_string(),
        description: "Retrieves a paginated list of all workspaces with optional filtering by workspace name, including their status, config details, and administrative information.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "count": { "type": "integer" },
        "page": { "type": "integer" },
        "all": { "type": "boolean" },
        "org_id": { "type": "string" }
    },
    "required": ["org_id"]
}),
    }
}

pub async fn handle_migrate_workspace_schema(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: WorkspaceSelectorRequest = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::migrate_workspace_schema::builders::MigrateWorkspaceSchemaInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: WorkspaceResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_migrate_workspace_schema() -> ToolInfo {
    ToolInfo {
        name: "MigrateWorkspaceSchema".to_string(),
        description: "Migrates the workspace database schema to the new version of the template".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "org_id": { "type": "string" },
        "workspace_name": { "type": "string" }
    },
    "required": ["org_id", "workspace_name"]
}),
    }
}

pub async fn handle_move_context(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: MoveContextInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::move_context::builders::MoveContextInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ContextResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_move_context() -> ToolInfo {
    ToolInfo {
        name: "MoveContext".to_string(),
        description: "Updates the condition of the mentioned context, if a context with the new condition already exists, it merges the override and effectively deleting the old context".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "id": { "type": "string" },
        "request": {
            "type": "object",
            "properties": {
                "context": {
                    "type": "object",
                    "additionalProperties": { "type": "object" }
                },
                "description": { "type": "string" },
                "change_reason": { "type": "string" }
            },
            "required": ["context", "change_reason"]
        }
    },
    "required": ["workspace_id", "org_id", "id", "request"]
}),
    }
}

pub async fn handle_pause_experiment(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: PauseExperimentInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::pause_experiment::builders::PauseExperimentInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ExperimentResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_pause_experiment() -> ToolInfo {
    ToolInfo {
        name: "PauseExperiment".to_string(),
        description: "Temporarily pauses an inprogress experiment, suspending its effects while preserving the experiment config for later resumption.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "id": { "type": "string" },
        "change_reason": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "id", "change_reason"]
}),
    }
}

pub async fn handle_publish(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: PublishInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::publish::builders::PublishInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: FunctionResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_publish() -> ToolInfo {
    ToolInfo {
        name: "Publish".to_string(),
        description: "Publishes the draft version of a function, making it the active version used for value_validation, value_compute, context_validation or change_reason_validation in the system.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "function_name": { "type": "string" },
        "change_reason": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "function_name", "change_reason"]
}),
    }
}

pub async fn handle_ramp_experiment(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: RampExperimentInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::ramp_experiment::builders::RampExperimentInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ExperimentResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_ramp_experiment() -> ToolInfo {
    ToolInfo {
        name: "RampExperiment".to_string(),
        description: "Adjusts the traffic percentage allocation for an in-progress experiment, allowing gradual rollout or rollback of experimental features.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "id": { "type": "string" },
        "change_reason": { "type": "string" },
        "traffic_percentage": { "type": "integer" }
    },
    "required": ["workspace_id", "org_id", "id", "change_reason", "traffic_percentage"]
}),
    }
}

pub async fn handle_remove_members_from_group(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: ModifyMembersToGroupRequest = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::remove_members_from_group::builders::RemoveMembersFromGroupInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ExperimentGroupResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_remove_members_from_group() -> ToolInfo {
    ToolInfo {
        name: "RemoveMembersFromGroup".to_string(),
        description: "Removes members from an existing experiment group.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "id": { "type": "string" },
        "change_reason": { "type": "string" },
        "member_experiment_ids": {
            "type": "array",
            "items": { "type": "string" }
        }
    },
    "required": ["workspace_id", "org_id", "id", "change_reason", "member_experiment_ids"]
}),
    }
}

pub async fn handle_resume_experiment(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: ResumeExperimentInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::resume_experiment::builders::ResumeExperimentInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ExperimentResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_resume_experiment() -> ToolInfo {
    ToolInfo {
        name: "ResumeExperiment".to_string(),
        description: "Resumes a previously paused experiment, restoring its in-progress state and re-enabling variant evaluation.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "id": { "type": "string" },
        "change_reason": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "id", "change_reason"]
}),
    }
}

pub async fn handle_rotate_master_encryption_key(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: Unit = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::rotate_master_encryption_key::builders::RotateMasterEncryptionKeyInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: RotateMasterEncryptionKeyOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_rotate_master_encryption_key() -> ToolInfo {
    ToolInfo {
        name: "RotateMasterEncryptionKey".to_string(),
        description: "Rotates the master encryption key across all workspaces".to_string(),
        input_schema: serde_json::json!({ "type": "null" }),
    }
}

pub async fn handle_rotate_workspace_encryption_key(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: WorkspaceSelectorRequest = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::rotate_workspace_encryption_key::builders::RotateWorkspaceEncryptionKeyInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: RotateWorkspaceEncryptionKeyOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_rotate_workspace_encryption_key() -> ToolInfo {
    ToolInfo {
        name: "RotateWorkspaceEncryptionKey".to_string(),
        description: "Rotates the workspace encryption key. Generates a new encryption key and re-encrypts all secrets with the new key. This is a critical operation that should be done during low-traffic periods.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "org_id": { "type": "string" },
        "workspace_name": { "type": "string" }
    },
    "required": ["org_id", "workspace_name"]
}),
    }
}

pub async fn handle_test(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: TestInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::test::builders::TestInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: FunctionExecutionResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_test() -> ToolInfo {
    ToolInfo {
        name: "Test".to_string(),
        description: "Executes a function in test mode with provided input parameters to validate its behavior before publishing or deployment.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "function_name": { "type": "string" },
        "stage": {
            "type": "string",
            "enum": ["draft", "published"]
        },
        "request": {
            "oneOf": [
                {
                    "type": "object",
                    "properties": {
                        "value_validate": {
                            "type": "object",
                            "properties": {
                                "key": { "type": "string" },
                                "value": { "type": "object" },
                                "type": { "type": "string" },
                                "environment": { "type": "object" }
                            },
                            "required": ["key", "value", "type", "environment"]
                        }
                    },
                    "required": ["value_validate"]
                },
                {
                    "type": "object",
                    "properties": {
                        "value_compute": {
                            "type": "object",
                            "properties": {
                                "name": { "type": "string" },
                                "prefix": { "type": "string" },
                                "type": { "type": "string" },
                                "environment": { "type": "object" }
                            },
                            "required": ["name", "prefix", "type", "environment"]
                        }
                    },
                    "required": ["value_compute"]
                },
                {
                    "type": "object",
                    "properties": {
                        "context_validate": {
                            "type": "object",
                            "properties": {
                                "environment": { "type": "object" }
                            },
                            "required": ["environment"]
                        }
                    },
                    "required": ["context_validate"]
                },
                {
                    "type": "object",
                    "properties": {
                        "change_reason_validate": {
                            "type": "object",
                            "properties": {
                                "change_reason": { "type": "string" }
                            },
                            "required": ["change_reason"]
                        }
                    },
                    "required": ["change_reason_validate"]
                }
            ]
        }
    },
    "required": ["workspace_id", "org_id", "function_name", "stage", "request"]
}),
    }
}

pub async fn handle_update_default_config(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: UpdateDefaultConfigInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::update_default_config::builders::UpdateDefaultConfigInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: DefaultConfigResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_update_default_config() -> ToolInfo {
    ToolInfo {
        name: "UpdateDefaultConfig".to_string(),
        description: "Updates an existing default config entry. Allows modification of value, schema, function mappings, and description while preserving the key identifier.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "key": { "type": "string" },
        "change_reason": { "type": "string" },
        "value": { "type": "object" },
        "schema": {
            "type": "object",
            "additionalProperties": { "type": "object" }
        },
        "value_validation_function_name": { "type": "string" },
        "description": { "type": "string" },
        "value_compute_function_name": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "key", "change_reason"]
}),
    }
}

pub async fn handle_update_dimension(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: UpdateDimensionInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::update_dimension::builders::UpdateDimensionInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: DimensionResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_update_dimension() -> ToolInfo {
    ToolInfo {
        name: "UpdateDimension".to_string(),
        description: "Updates an existing dimension's configuration. Allows modification of schema, position, function mappings, and other properties while maintaining dependency relationships.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "dimension": { "type": "string" },
        "schema": {
            "type": "object",
            "additionalProperties": { "type": "object" }
        },
        "position": { "type": "integer" },
        "value_validation_function_name": { "type": "string" },
        "description": { "type": "string" },
        "change_reason": { "type": "string" },
        "value_compute_function_name": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "dimension", "change_reason"]
}),
    }
}

pub async fn handle_update_experiment_group(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: UpdateExperimentGroupRequest = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::update_experiment_group::builders::UpdateExperimentGroupInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ExperimentGroupResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_update_experiment_group() -> ToolInfo {
    ToolInfo {
        name: "UpdateExperimentGroup".to_string(),
        description: "Updates an existing experiment group. Allows partial updates to specified fields.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "id": { "type": "string" },
        "change_reason": { "type": "string" },
        "description": { "type": "string" },
        "traffic_percentage": { "type": "integer" }
    },
    "required": ["workspace_id", "org_id", "id", "change_reason"]
}),
    }
}

pub async fn handle_update_function(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: UpdateFunctionRequest = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::update_function::builders::UpdateFunctionInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: FunctionResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_update_function() -> ToolInfo {
    ToolInfo {
        name: "UpdateFunction".to_string(),
        description: "Updates the draft version of an existing function with new code, runtime version, or description while preserving the published version.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "function_name": { "type": "string" },
        "description": { "type": "string" },
        "change_reason": { "type": "string" },
        "function": { "type": "string" },
        "runtime_version": {
            "type": "string",
            "enum": ["1.0"]
        }
    },
    "required": ["workspace_id", "org_id", "function_name", "change_reason"]
}),
    }
}

pub async fn handle_update_organisation(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: UpdateOrganisationRequest = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::update_organisation::builders::UpdateOrganisationInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: OrganisationResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_update_organisation() -> ToolInfo {
    ToolInfo {
        name: "UpdateOrganisation".to_string(),
        description: "Updates an existing organisation's information including contact details, status, and administrative properties.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "country_code": { "type": "string" },
        "contact_email": { "type": "string" },
        "contact_phone": { "type": "string" },
        "admin_email": { "type": "string" },
        "sector": { "type": "string" },
        "id": { "type": "string" },
        "status": {
            "type": "string",
            "enum": ["Active", "Inactive", "PendingKyb"]
        }
    },
    "required": ["id"]
}),
    }
}

pub async fn handle_update_override(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: UpdateOverrideInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::update_override::builders::UpdateOverrideInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ContextResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_update_override() -> ToolInfo {
    ToolInfo {
        name: "UpdateOverride".to_string(),
        description: "Updates the overrides for an existing context. Allows modification of override values while maintaining the context's conditions.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "config_tags": { "type": "string" },
        "request": {
            "type": "object",
            "properties": {
                "context": {
                    "oneOf": [
                        {
                            "type": "object",
                            "properties": {
                                "id": { "type": "string" }
                            },
                            "required": ["id"]
                        },
                        {
                            "type": "object",
                            "properties": {
                                "context": {
                                    "type": "object",
                                    "additionalProperties": { "type": "object" }
                                }
                            },
                            "required": ["context"]
                        }
                    ]
                },
                "override": {
                    "type": "object",
                    "additionalProperties": { "type": "object" }
                },
                "description": { "type": "string" },
                "change_reason": { "type": "string" }
            },
            "required": ["context", "override", "change_reason"]
        }
    },
    "required": ["workspace_id", "org_id", "request"]
}),
    }
}

pub async fn handle_update_overrides_experiment(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: UpdateOverrideRequest = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::update_overrides_experiment::builders::UpdateOverridesExperimentInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: ExperimentResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_update_overrides_experiment() -> ToolInfo {
    ToolInfo {
        name: "UpdateOverridesExperiment".to_string(),
        description: "Updates the overrides for specific variants within an experiment, allowing modification of experiment behavior Updates the overrides for specific variants within an experiment, allowing modification of experiment behavior while it is in the created state.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "id": { "type": "string" },
        "variant_list": {
            "type": "array",
            "items": {
                "type": "object",
                "properties": {
                    "id": { "type": "string" },
                    "overrides": {
                        "type": "object",
                        "additionalProperties": { "type": "object" }
                    }
                },
                "required": ["id", "overrides"]
            }
        },
        "description": { "type": "string" },
        "change_reason": { "type": "string" },
        "metrics": { "type": "object" },
        "experiment_group_id": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "id", "variant_list", "change_reason"]
}),
    }
}

pub async fn handle_update_secret(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: UpdateSecretInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::update_secret::builders::UpdateSecretInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: SecretResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_update_secret() -> ToolInfo {
    ToolInfo {
        name: "UpdateSecret".to_string(),
        description: "Updates an existing secret's value or description. The value is re-encrypted with the current workspace encryption key. Returns masked value.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "name": { "type": "string" },
        "value": { "type": "string" },
        "description": { "type": "string" },
        "change_reason": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "name", "change_reason"]
}),
    }
}

pub async fn handle_update_type_templates(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: UpdateTypeTemplatesRequest = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::update_type_templates::builders::UpdateTypeTemplatesInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: TypeTemplatesResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_update_type_templates() -> ToolInfo {
    ToolInfo {
        name: "UpdateTypeTemplates".to_string(),
        description: "Updates an existing type template's schema definition and metadata while preserving its identifier and usage history.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "type_name": { "type": "string" },
        "type_schema": {
            "type": "object",
            "additionalProperties": { "type": "object" }
        },
        "description": { "type": "string" },
        "change_reason": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "type_name", "type_schema", "change_reason"]
}),
    }
}

pub async fn handle_update_variable(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: UpdateVariableInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::update_variable::builders::UpdateVariableInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: VariableResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_update_variable() -> ToolInfo {
    ToolInfo {
        name: "UpdateVariable".to_string(),
        description: "Updates an existing variable's value, description, or tags.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "name": { "type": "string" },
        "value": { "type": "string" },
        "description": { "type": "string" },
        "change_reason": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "name", "change_reason"]
}),
    }
}

pub async fn handle_update_webhook(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: UpdateWebhookInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::update_webhook::builders::UpdateWebhookInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: WebhookResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_update_webhook() -> ToolInfo {
    ToolInfo {
        name: "UpdateWebhook".to_string(),
        description: "Updates an existing webhook config, allowing modification of URL, events, headers, and other webhook properties.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "name": { "type": "string" },
        "description": { "type": "string" },
        "enabled": { "type": "boolean" },
        "url": { "type": "string" },
        "method": {
            "type": "string",
            "enum": ["GET", "POST", "PUT", "PATCH", "DELETE", "HEAD"]
        },
        "version": {
            "type": "string",
            "enum": ["V1"]
        },
        "custom_headers": {
            "type": "object",
            "additionalProperties": { "type": "object" }
        },
        "events": {
            "type": "array",
            "items": { "type": "string" }
        },
        "change_reason": { "type": "string" }
    },
    "required": ["workspace_id", "org_id", "name", "change_reason"]
}),
    }
}

pub async fn handle_update_workspace(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: UpdateWorkspaceRequest = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::update_workspace::builders::UpdateWorkspaceInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: WorkspaceResponse = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_update_workspace() -> ToolInfo {
    ToolInfo {
        name: "UpdateWorkspace".to_string(),
        description: "Updates an existing workspace configuration, allowing modification of admin settings, mandatory dimensions, and workspace properties. Validates config version existence if provided.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "org_id": { "type": "string" },
        "workspace_name": { "type": "string" },
        "workspace_admin_email": { "type": "string" },
        "config_version": { "type": "string" },
        "mandatory_dimensions": {
            "type": "array",
            "items": { "type": "string" }
        },
        "workspace_status": {
            "type": "string",
            "enum": ["ENABLED", "DISABLED"]
        },
        "metrics": { "type": "object" },
        "allow_experiment_self_approval": { "type": "boolean" },
        "auto_populate_control": { "type": "boolean" },
        "enable_context_validation": { "type": "boolean" },
        "enable_change_reason_validation": { "type": "boolean" }
    },
    "required": ["org_id", "workspace_name"]
}),
    }
}

pub async fn handle_validate_context(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: ValidateContextInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::validate_context::builders::ValidateContextInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: Unit = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_validate_context() -> ToolInfo {
    ToolInfo {
        name: "ValidateContext".to_string(),
        description: "Validates if a given context condition is well-formed".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "context": {
            "type": "object",
            "additionalProperties": { "type": "object" }
        }
    },
    "required": ["workspace_id", "org_id", "context"]
}),
    }
}

pub async fn handle_weight_recompute(client: &Client, params: serde_json::Value) -> Result<serde_json::Value, McpError> {
    let input: WeightRecomputeInput = serde_json::from_value(params)?;
    let builder: superposition_sdk::operation::weight_recompute::builders::WeightRecomputeInputBuilder = input.into();
    let sdk_output = builder.send_with(client).await
        .map_err(|e| McpError::internal(e.to_string()))?;
    let mcp_output: WeightRecomputeOutput = sdk_output.into();
    Ok(serde_json::to_value(mcp_output)?)
}

pub fn tool_info_weight_recompute() -> ToolInfo {
    ToolInfo {
        name: "WeightRecompute".to_string(),
        description: "Recalculates and updates the priority weights for all contexts in the workspace based on their dimensions.".to_string(),
        input_schema: serde_json::json!({
    "type": "object",
    "properties": {
        "workspace_id": { "type": "string" },
        "org_id": { "type": "string" },
        "config_tags": { "type": "string" }
    },
    "required": ["workspace_id", "org_id"]
}),
    }
}

