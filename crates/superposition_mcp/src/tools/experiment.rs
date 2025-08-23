use super::ToolsModule;
use crate::mcp_service::{value_to_hashmap, value_to_document, McpService, Tool};
use serde_json::{json, Value};
use std::error::Error;
use superposition_sdk::types::{Variant, VariantType};

pub struct ExperimentTools;

impl ToolsModule for ExperimentTools {
    fn get_tool_definitions() -> Vec<Tool> {
        vec![
            Tool {
                name: "list_experiment".to_string(),
                description: "List all experiments".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {}
                }),
            },
            Tool {
                name: "create_experiment".to_string(),
                description: "Create a new experiment".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "name": {"type": "string", "description": "Experiment name"},
                        "context": {"type": "object", "description": "Experiment context"},
                        "variants": {"type": "array", "description": "Experiment variants"},
                        "change_reason": {"type": "string", "description": "Reason for creating experiment"}
                    },
                    "required": ["name", "context", "variants", "change_reason"]
                }),
            },
            Tool {
                name: "get_experiment".to_string(),
                description: "Get an experiment by ID".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "experiment_id": {"type": "string", "description": "Experiment ID"}
                    },
                    "required": ["experiment_id"]
                }),
            },
            Tool {
                name: "conclude_experiment".to_string(),
                description: "Conclude an experiment".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "experiment_id": {"type": "string", "description": "Experiment ID"},
                        "change_reason": {"type": "string", "description": "Reason for concluding experiment"}
                    },
                    "required": ["experiment_id", "change_reason"]
                }),
            },
            Tool {
                name: "discard_experiment".to_string(),
                description: "Discard an experiment".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "experiment_id": {"type": "string", "description": "Experiment ID"},
                        "change_reason": {"type": "string", "description": "Reason for discarding experiment"}
                    },
                    "required": ["experiment_id", "change_reason"]
                }),
            },
            Tool {
                name: "pause_experiment".to_string(),
                description: "Pause an experiment".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "experiment_id": {"type": "string", "description": "Experiment ID"},
                        "change_reason": {"type": "string", "description": "Reason for pausing experiment"}
                    },
                    "required": ["experiment_id", "change_reason"]
                }),
            },
            Tool {
                name: "resume_experiment".to_string(),
                description: "Resume an experiment".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "experiment_id": {"type": "string", "description": "Experiment ID"},
                        "change_reason": {"type": "string", "description": "Reason for resuming experiment"}
                    },
                    "required": ["experiment_id", "change_reason"]
                }),
            },
            Tool {
                name: "ramp_experiment".to_string(),
                description: "Ramp an experiment".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "experiment_id": {"type": "string", "description": "Experiment ID"},
                        "ramp_percentage": {"type": "number", "description": "Ramp percentage"},
                        "change_reason": {"type": "string", "description": "Reason for ramping experiment"}
                    },
                    "required": ["experiment_id", "ramp_percentage", "change_reason"]
                }),
            },
            Tool {
                name: "create_experiment_group".to_string(),
                description: "Create an experiment group".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "name": {"type": "string", "description": "Group name"},
                        "description": {"type": "string", "description": "Group description"},
                        "change_reason": {"type": "string", "description": "Reason for creating group"}
                    },
                    "required": ["name", "description", "change_reason"]
                }),
            },
            Tool {
                name: "get_experiment_group".to_string(),
                description: "Get an experiment group by ID".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "group_id": {"type": "string", "description": "Group ID"}
                    },
                    "required": ["group_id"]
                }),
            },
            Tool {
                name: "list_experiment_groups".to_string(),
                description: "List all experiment groups".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {}
                }),
            },
            Tool {
                name: "update_experiment_group".to_string(),
                description: "Update an experiment group".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "group_id": {"type": "string", "description": "Group ID"},
                        "name": {"type": "string", "description": "Updated group name"},
                        "description": {"type": "string", "description": "Updated group description"},
                        "change_reason": {"type": "string", "description": "Reason for updating group"}
                    },
                    "required": ["group_id", "change_reason"]
                }),
            },
            Tool {
                name: "delete_experiment_group".to_string(),
                description: "Delete an experiment group".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "group_id": {"type": "string", "description": "Group ID"}
                    },
                    "required": ["group_id"]
                }),
            },
            Tool {
                name: "add_members_to_group".to_string(),
                description: "Add members to an experiment group".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "group_id": {"type": "string", "description": "Group ID"},
                        "member_ids": {"type": "array", "items": {"type": "string"}, "description": "Member IDs"},
                        "change_reason": {"type": "string", "description": "Reason for adding members"}
                    },
                    "required": ["group_id", "member_ids", "change_reason"]
                }),
            },
            Tool {
                name: "remove_members_from_group".to_string(),
                description: "Remove members from an experiment group".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "group_id": {"type": "string", "description": "Group ID"},
                        "member_ids": {"type": "array", "items": {"type": "string"}, "description": "Member IDs"},
                        "change_reason": {"type": "string", "description": "Reason for removing members"}
                    },
                    "required": ["group_id", "member_ids", "change_reason"]
                }),
            },
            Tool {
                name: "update_overrides_experiment".to_string(),
                description: "Update overrides for an experiment".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "experiment_id": {"type": "string", "description": "Experiment ID"},
                        "overrides": {"type": "object", "description": "Override configuration"},
                        "change_reason": {"type": "string", "description": "Reason for updating overrides"}
                    },
                    "required": ["experiment_id", "overrides", "change_reason"]
                }),
            },
        ]
    }

    async fn execute_tool(
        service: &McpService,
        tool_name: &str,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        match tool_name {
            "list_experiment" => {
                service
                    .superposition_client
                    .list_experiment()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .send()
                    .await
                    .map(|output| {
                        json!({
                            "count": output.data().len(),
                            "message": "Experiments found (detailed data not available due to serialization constraints)"
                        })
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "create_experiment" => {
                let name = arguments["name"].as_str().unwrap_or("");
                let context = &arguments["context"];
                let variants_array = arguments["variants"].as_array().ok_or("variants must be an array")?;
                let change_reason = arguments["change_reason"].as_str().unwrap_or("");

                // Convert context to hashmap format expected by SDK
                let context_hashmap = if let Some(context_map) = value_to_hashmap(context.clone()) {
                    context_map
                } else {
                    return Err("Invalid context format".into());
                };

                // Convert variants JSON array to Vec<Variant>
                let variants: Result<Vec<Variant>, Box<dyn Error>> = variants_array
                    .iter()
                    .map(|variant_value| {
                        let variant_obj = variant_value.as_object()
                            .ok_or("Each variant must be an object")?;
                        
                        let id = variant_obj.get("id")
                            .and_then(|v| v.as_str())
                            .ok_or("Variant must have an 'id' field")?;
                        
                        let variant_type_str = variant_obj.get("variant_type")
                            .and_then(|v| v.as_str())
                            .unwrap_or("EXPERIMENTAL");
                        
                        let variant_type = match variant_type_str.to_uppercase().as_str() {
                            "CONTROL" => VariantType::Control,
                            "EXPERIMENTAL" => VariantType::Experimental,
                            _ => VariantType::Experimental, // Default fallback
                        };
                        
                        let context_id = variant_obj.get("context_id")
                            .and_then(|v| v.as_str())
                            .map(|s| s.to_string());
                        
                        let override_id = variant_obj.get("override_id")
                            .and_then(|v| v.as_str())
                            .map(|s| s.to_string());
                        
                        let default_overrides = serde_json::json!({});
                        let overrides = variant_obj.get("overrides")
                            .unwrap_or(&default_overrides);
                        let overrides_doc = value_to_document(overrides);
                        
                        let variant = Variant::builder()
                            .id(id)
                            .variant_type(variant_type)
                            .set_context_id(context_id)
                            .set_override_id(override_id)
                            .overrides(overrides_doc)
                            .build()
                            .map_err(|e| format!("Failed to build variant: {}", e))?;
                        
                        Ok(variant)
                    })
                    .collect();
                
                let variants = variants?;

                // Build the experiment with all variants
                let mut builder = service
                    .superposition_client
                    .create_experiment()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .name(name)
                    .change_reason(change_reason)
                    .set_context(Some(context_hashmap));

                // Add each variant to the builder
                for variant in variants {
                    builder = builder.variants(variant);
                }

                builder
                    .send()
                    .await
                    .map(|_| json!({"status": "experiment_created"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "get_experiment" => {
                let experiment_id = arguments["experiment_id"].as_str().unwrap_or("");

                service
                    .superposition_client
                    .get_experiment()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .id(experiment_id)
                    .send()
                    .await
                    .map(|_| json!({"status": "found"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "conclude_experiment" => {
                let experiment_id = arguments["experiment_id"].as_str().unwrap_or("");
                let change_reason = arguments["change_reason"].as_str().unwrap_or("");

                service
                    .superposition_client
                    .conclude_experiment()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .id(experiment_id)
                    .change_reason(change_reason)
                    .send()
                    .await
                    .map(|_| json!({"status": "concluded"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "discard_experiment" => {
                let experiment_id = arguments["experiment_id"].as_str().unwrap_or("");
                let change_reason = arguments["change_reason"].as_str().unwrap_or("");

                service
                    .superposition_client
                    .discard_experiment()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .id(experiment_id)
                    .change_reason(change_reason)
                    .send()
                    .await
                    .map(|_| json!({"status": "discarded"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "pause_experiment" => {
                let experiment_id = arguments["experiment_id"].as_str().unwrap_or("");
                let change_reason = arguments["change_reason"].as_str().unwrap_or("");

                service
                    .superposition_client
                    .pause_experiment()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .id(experiment_id)
                    .change_reason(change_reason)
                    .send()
                    .await
                    .map(|_| json!({"status": "paused"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "resume_experiment" => {
                let experiment_id = arguments["experiment_id"].as_str().unwrap_or("");
                let change_reason = arguments["change_reason"].as_str().unwrap_or("");

                service
                    .superposition_client
                    .resume_experiment()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .id(experiment_id)
                    .change_reason(change_reason)
                    .send()
                    .await
                    .map(|_| json!({"status": "resumed"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "ramp_experiment" => {
                let experiment_id = arguments["experiment_id"].as_str().unwrap_or("");
                let ramp_percentage = arguments["ramp_percentage"].as_f64().unwrap_or(0.0);
                let change_reason = arguments["change_reason"].as_str().unwrap_or("");

                service
                    .superposition_client
                    .ramp_experiment()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .id(experiment_id)
                    .traffic_percentage(ramp_percentage as i32)
                    .change_reason(change_reason)
                    .send()
                    .await
                    .map(|_| json!({"status": "ramped"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "create_experiment_group" => {
                let name = arguments["name"].as_str().unwrap_or("");
                let description = arguments["description"].as_str().unwrap_or("");
                let change_reason = arguments["change_reason"].as_str().unwrap_or("");

                service
                    .superposition_client
                    .create_experiment_group()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .name(name)
                    .description(description)
                    .change_reason(change_reason)
                    .send()
                    .await
                    .map(|_| json!({"status": "created"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "get_experiment_group" => {
                let group_id = arguments["group_id"].as_str().unwrap_or("");

                service
                    .superposition_client
                    .get_experiment_group()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .id(group_id)
                    .send()
                    .await
                    .map(|_| json!({"status": "found"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "list_experiment_groups" => {
                service
                    .superposition_client
                    .list_experiment_groups()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .send()
                    .await
                    .map(|output| {
                        json!({
                            "count": output.data().len(),
                            "message": "Experiment groups found"
                        })
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "update_experiment_group" => {
                let group_id = arguments["group_id"].as_str().unwrap_or("");
                let change_reason = arguments["change_reason"].as_str().unwrap_or("");

                service
                    .superposition_client
                    .update_experiment_group()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .id(group_id)
                    .change_reason(change_reason)
                    .send()
                    .await
                    .map(|_| json!({"status": "updated"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "delete_experiment_group" => {
                let group_id = arguments["group_id"].as_str().unwrap_or("");

                service
                    .superposition_client
                    .delete_experiment_group()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .id(group_id)
                    .send()
                    .await
                    .map(|_| json!({"status": "deleted"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "add_members_to_group" => {
                let group_id = arguments["group_id"].as_str().unwrap_or("");
                let empty_vec = vec![];
                let member_ids = arguments["member_ids"].as_array().unwrap_or(&empty_vec);
                let change_reason = arguments["change_reason"].as_str().unwrap_or("");

                let member_strings: Vec<String> = member_ids
                    .iter()
                    .filter_map(|v| v.as_str())
                    .map(|s| s.to_string())
                    .collect();

                service
                    .superposition_client
                    .add_members_to_group()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .id(group_id)
                    .set_member_experiment_ids(Some(member_strings))
                    .change_reason(change_reason)
                    .send()
                    .await
                    .map(|_| json!({"status": "members added"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "remove_members_from_group" => {
                let group_id = arguments["group_id"].as_str().unwrap_or("");
                let empty_vec = vec![];
                let member_ids = arguments["member_ids"].as_array().unwrap_or(&empty_vec);
                let change_reason = arguments["change_reason"].as_str().unwrap_or("");

                let member_strings: Vec<String> = member_ids
                    .iter()
                    .filter_map(|v| v.as_str())
                    .map(|s| s.to_string())
                    .collect();

                service
                    .superposition_client
                    .remove_members_from_group()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .id(group_id)
                    .set_member_experiment_ids(Some(member_strings))
                    .change_reason(change_reason)
                    .send()
                    .await
                    .map(|_| json!({"status": "members removed"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "update_overrides_experiment" => {
                let experiment_id = arguments["experiment_id"].as_str().unwrap_or("");
                let change_reason = arguments["change_reason"].as_str().unwrap_or("");

                service
                    .superposition_client
                    .update_overrides_experiment()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .id(experiment_id)
                    .change_reason(change_reason)
                    .send()
                    .await
                    .map(|_| json!({"status": "overrides updated"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            _ => Err(format!("Unknown experiment tool: {}", tool_name).into()),
        }
    }
}
