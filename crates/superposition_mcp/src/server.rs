use std::sync::Arc;
use smithy_mcp_runtime::{McpError, Router};
use crate::tools;
use crate::types::*;

pub struct McpServer {
    router: Router,
}

impl McpServer {
    pub fn new(client: superposition_sdk::Client) -> Self {
        let client = Arc::new(client);
        let mut router = Router::new();

        let c = client.clone();
        router.register_tool(tools::tool_info_add_members_to_group(), move |params| {
            let client = c.clone();
            async move { tools::handle_add_members_to_group(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_applicable_variants(), move |params| {
            let client = c.clone();
            async move { tools::handle_applicable_variants(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_bulk_operation(), move |params| {
            let client = c.clone();
            async move { tools::handle_bulk_operation(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_conclude_experiment(), move |params| {
            let client = c.clone();
            async move { tools::handle_conclude_experiment(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_create_context(), move |params| {
            let client = c.clone();
            async move { tools::handle_create_context(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_create_default_config(), move |params| {
            let client = c.clone();
            async move { tools::handle_create_default_config(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_create_dimension(), move |params| {
            let client = c.clone();
            async move { tools::handle_create_dimension(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_create_experiment(), move |params| {
            let client = c.clone();
            async move { tools::handle_create_experiment(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_create_experiment_group(), move |params| {
            let client = c.clone();
            async move { tools::handle_create_experiment_group(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_create_function(), move |params| {
            let client = c.clone();
            async move { tools::handle_create_function(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_create_organisation(), move |params| {
            let client = c.clone();
            async move { tools::handle_create_organisation(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_create_secret(), move |params| {
            let client = c.clone();
            async move { tools::handle_create_secret(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_create_type_templates(), move |params| {
            let client = c.clone();
            async move { tools::handle_create_type_templates(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_create_variable(), move |params| {
            let client = c.clone();
            async move { tools::handle_create_variable(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_create_webhook(), move |params| {
            let client = c.clone();
            async move { tools::handle_create_webhook(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_create_workspace(), move |params| {
            let client = c.clone();
            async move { tools::handle_create_workspace(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_delete_context(), move |params| {
            let client = c.clone();
            async move { tools::handle_delete_context(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_delete_default_config(), move |params| {
            let client = c.clone();
            async move { tools::handle_delete_default_config(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_delete_dimension(), move |params| {
            let client = c.clone();
            async move { tools::handle_delete_dimension(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_delete_experiment_group(), move |params| {
            let client = c.clone();
            async move { tools::handle_delete_experiment_group(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_delete_function(), move |params| {
            let client = c.clone();
            async move { tools::handle_delete_function(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_delete_secret(), move |params| {
            let client = c.clone();
            async move { tools::handle_delete_secret(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_delete_type_templates(), move |params| {
            let client = c.clone();
            async move { tools::handle_delete_type_templates(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_delete_variable(), move |params| {
            let client = c.clone();
            async move { tools::handle_delete_variable(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_delete_webhook(), move |params| {
            let client = c.clone();
            async move { tools::handle_delete_webhook(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_discard_experiment(), move |params| {
            let client = c.clone();
            async move { tools::handle_discard_experiment(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_get_config(), move |params| {
            let client = c.clone();
            async move { tools::handle_get_config(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_get_config_json(), move |params| {
            let client = c.clone();
            async move { tools::handle_get_config_json(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_get_config_toml(), move |params| {
            let client = c.clone();
            async move { tools::handle_get_config_toml(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_get_context(), move |params| {
            let client = c.clone();
            async move { tools::handle_get_context(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_get_context_from_condition(), move |params| {
            let client = c.clone();
            async move { tools::handle_get_context_from_condition(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_get_default_config(), move |params| {
            let client = c.clone();
            async move { tools::handle_get_default_config(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_get_dimension(), move |params| {
            let client = c.clone();
            async move { tools::handle_get_dimension(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_get_experiment(), move |params| {
            let client = c.clone();
            async move { tools::handle_get_experiment(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_get_experiment_config(), move |params| {
            let client = c.clone();
            async move { tools::handle_get_experiment_config(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_get_experiment_group(), move |params| {
            let client = c.clone();
            async move { tools::handle_get_experiment_group(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_get_function(), move |params| {
            let client = c.clone();
            async move { tools::handle_get_function(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_get_organisation(), move |params| {
            let client = c.clone();
            async move { tools::handle_get_organisation(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_get_resolved_config(), move |params| {
            let client = c.clone();
            async move { tools::handle_get_resolved_config(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_get_resolved_config_with_identifier(), move |params| {
            let client = c.clone();
            async move { tools::handle_get_resolved_config_with_identifier(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_get_secret(), move |params| {
            let client = c.clone();
            async move { tools::handle_get_secret(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_get_type_template(), move |params| {
            let client = c.clone();
            async move { tools::handle_get_type_template(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_get_type_templates_list(), move |params| {
            let client = c.clone();
            async move { tools::handle_get_type_templates_list(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_get_variable(), move |params| {
            let client = c.clone();
            async move { tools::handle_get_variable(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_get_version(), move |params| {
            let client = c.clone();
            async move { tools::handle_get_version(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_get_webhook(), move |params| {
            let client = c.clone();
            async move { tools::handle_get_webhook(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_get_webhook_by_event(), move |params| {
            let client = c.clone();
            async move { tools::handle_get_webhook_by_event(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_get_workspace(), move |params| {
            let client = c.clone();
            async move { tools::handle_get_workspace(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_list_audit_logs(), move |params| {
            let client = c.clone();
            async move { tools::handle_list_audit_logs(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_list_contexts(), move |params| {
            let client = c.clone();
            async move { tools::handle_list_contexts(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_list_default_configs(), move |params| {
            let client = c.clone();
            async move { tools::handle_list_default_configs(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_list_dimensions(), move |params| {
            let client = c.clone();
            async move { tools::handle_list_dimensions(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_list_experiment(), move |params| {
            let client = c.clone();
            async move { tools::handle_list_experiment(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_list_experiment_groups(), move |params| {
            let client = c.clone();
            async move { tools::handle_list_experiment_groups(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_list_function(), move |params| {
            let client = c.clone();
            async move { tools::handle_list_function(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_list_organisation(), move |params| {
            let client = c.clone();
            async move { tools::handle_list_organisation(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_list_secrets(), move |params| {
            let client = c.clone();
            async move { tools::handle_list_secrets(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_list_variables(), move |params| {
            let client = c.clone();
            async move { tools::handle_list_variables(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_list_versions(), move |params| {
            let client = c.clone();
            async move { tools::handle_list_versions(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_list_webhook(), move |params| {
            let client = c.clone();
            async move { tools::handle_list_webhook(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_list_workspace(), move |params| {
            let client = c.clone();
            async move { tools::handle_list_workspace(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_migrate_workspace_schema(), move |params| {
            let client = c.clone();
            async move { tools::handle_migrate_workspace_schema(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_move_context(), move |params| {
            let client = c.clone();
            async move { tools::handle_move_context(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_pause_experiment(), move |params| {
            let client = c.clone();
            async move { tools::handle_pause_experiment(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_publish(), move |params| {
            let client = c.clone();
            async move { tools::handle_publish(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_ramp_experiment(), move |params| {
            let client = c.clone();
            async move { tools::handle_ramp_experiment(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_remove_members_from_group(), move |params| {
            let client = c.clone();
            async move { tools::handle_remove_members_from_group(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_resume_experiment(), move |params| {
            let client = c.clone();
            async move { tools::handle_resume_experiment(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_rotate_master_encryption_key(), move |params| {
            let client = c.clone();
            async move { tools::handle_rotate_master_encryption_key(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_rotate_workspace_encryption_key(), move |params| {
            let client = c.clone();
            async move { tools::handle_rotate_workspace_encryption_key(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_test(), move |params| {
            let client = c.clone();
            async move { tools::handle_test(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_update_default_config(), move |params| {
            let client = c.clone();
            async move { tools::handle_update_default_config(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_update_dimension(), move |params| {
            let client = c.clone();
            async move { tools::handle_update_dimension(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_update_experiment_group(), move |params| {
            let client = c.clone();
            async move { tools::handle_update_experiment_group(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_update_function(), move |params| {
            let client = c.clone();
            async move { tools::handle_update_function(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_update_organisation(), move |params| {
            let client = c.clone();
            async move { tools::handle_update_organisation(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_update_override(), move |params| {
            let client = c.clone();
            async move { tools::handle_update_override(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_update_overrides_experiment(), move |params| {
            let client = c.clone();
            async move { tools::handle_update_overrides_experiment(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_update_secret(), move |params| {
            let client = c.clone();
            async move { tools::handle_update_secret(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_update_type_templates(), move |params| {
            let client = c.clone();
            async move { tools::handle_update_type_templates(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_update_variable(), move |params| {
            let client = c.clone();
            async move { tools::handle_update_variable(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_update_webhook(), move |params| {
            let client = c.clone();
            async move { tools::handle_update_webhook(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_update_workspace(), move |params| {
            let client = c.clone();
            async move { tools::handle_update_workspace(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_validate_context(), move |params| {
            let client = c.clone();
            async move { tools::handle_validate_context(&client, params).await }
        });

        let c = client.clone();
        router.register_tool(tools::tool_info_weight_recompute(), move |params| {
            let client = c.clone();
            async move { tools::handle_weight_recompute(&client, params).await }
        });

        Self { router }
    }

    pub async fn serve_stdio(self) -> Result<(), Box<dyn std::error::Error>> {
        smithy_mcp_runtime::serve_stdio(self.router).await
    }

    pub fn into_router(self) -> Router {
        self.router
    }
}
