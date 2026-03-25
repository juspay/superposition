use rmcp::handler::server::tool::ToolRouter;
use rmcp::handler::server::wrapper::Parameters;
use rmcp::model::*;
use rmcp::{ServerHandler, tool, tool_router};

use crate::config::McpServerConfig;
use crate::tools::{
    audit_log::*, config::*, context::*, default_config::*, dimension::*, experiment::*,
    experiment_group::*, function::*, organisation::*, secret::*, type_template::*,
    variable::*, webhook::*, workspace::*,
};

/// The Superposition MCP Server — exposes all Superposition API operations as MCP tools.
#[derive(Clone)]
pub struct SuperpositionMcpServer {
    pub(crate) client: superposition_sdk::Client,
    /// Server configuration (public for testing)
    pub config: McpServerConfig,
    #[allow(dead_code)]
    tool_router: ToolRouter<Self>,
}

impl SuperpositionMcpServer {
    pub fn new(config: McpServerConfig) -> Self {
        let client = config.build_sdk_client();
        let tool_router = Self::tool_router();
        Self {
            client,
            config,
            tool_router,
        }
    }
}

#[tool_router]
impl SuperpositionMcpServer {
    // ===== Configuration Management =====
    #[tool(
        name = "config.get",
        description = "Retrieves config data with context evaluation, including applicable contexts, overrides, and default values based on provided conditions."
    )]
    async fn config_get(
        &self,
        Parameters(args): Parameters<GetConfigParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.get_config_impl(args).await
    }

    #[tool(
        name = "config.resolve",
        description = "Resolves and merges config values based on context conditions, applying overrides and merge strategies to produce the final configuration."
    )]
    async fn config_resolve(
        &self,
        Parameters(args): Parameters<ResolveConfigParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.resolve_config_impl(args).await
    }

    #[tool(
        name = "config.resolve_with_identifier",
        description = "Resolves and merges config values like config.resolve, but also accepts an identifier (e.g. user ID) for cohort-based resolution and returns an audit_id."
    )]
    async fn config_resolve_with_identifier(
        &self,
        Parameters(args): Parameters<ResolveConfigWithIdentifierParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.resolve_config_with_identifier_impl(args).await
    }

    #[tool(
        name = "config.get_fast",
        description = "Retrieves the latest raw config with no processing for high-performance access."
    )]
    async fn config_get_fast(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        self.get_config_fast_impl().await
    }

    #[tool(
        name = "config.get_toml",
        description = "Retrieves the full config in TOML format including default configs, dimensions, and overrides."
    )]
    async fn config_get_toml(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        self.get_config_toml_impl().await
    }

    #[tool(
        name = "config.get_json",
        description = "Retrieves the full config in JSON format including default configs, dimensions, and overrides."
    )]
    async fn config_get_json(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        self.get_config_json_impl().await
    }

    #[tool(
        name = "config.get_version",
        description = "Retrieves a specific config version along with its metadata for audit and rollback purposes."
    )]
    async fn config_get_version(
        &self,
        Parameters(args): Parameters<GetVersionParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.get_version_impl(args).await
    }

    #[tool(
        name = "config.list_versions",
        description = "Retrieves a paginated list of config versions with metadata, hash values, and creation timestamps."
    )]
    async fn config_list_versions(
        &self,
        Parameters(args): Parameters<ListVersionsParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.list_versions_impl(args).await
    }

    // ===== Default Config =====
    #[tool(
        name = "default_config.create",
        description = "Creates a new default config entry with key, value, JSON schema, and metadata. Default configs serve as fallback values when no context matches."
    )]
    async fn default_config_create(
        &self,
        Parameters(args): Parameters<CreateDefaultConfigParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.create_default_config_impl(args).await
    }

    #[tool(
        name = "default_config.get",
        description = "Retrieves a specific default config entry by its key, including its value, schema, and metadata."
    )]
    async fn default_config_get(
        &self,
        Parameters(args): Parameters<GetDefaultConfigParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.get_default_config_impl(args).await
    }

    #[tool(
        name = "default_config.list",
        description = "Retrieves a paginated list of all default config entries in the workspace."
    )]
    async fn default_config_list(
        &self,
        Parameters(args): Parameters<ListDefaultConfigsParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.list_default_configs_impl(args).await
    }

    #[tool(
        name = "default_config.update",
        description = "Updates an existing default config entry's value, schema, or description."
    )]
    async fn default_config_update(
        &self,
        Parameters(args): Parameters<UpdateDefaultConfigParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.update_default_config_impl(args).await
    }

    #[tool(
        name = "default_config.delete",
        description = "Permanently removes a default config entry from the workspace."
    )]
    async fn default_config_delete(
        &self,
        Parameters(args): Parameters<DeleteDefaultConfigParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.delete_default_config_impl(args).await
    }

    // ===== Dimensions =====
    #[tool(
        name = "dimension.create",
        description = "Creates a new dimension with a JSON schema. Dimensions define categorical attributes used for context-based config management."
    )]
    async fn dimension_create(
        &self,
        Parameters(args): Parameters<CreateDimensionParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.create_dimension_impl(args).await
    }

    #[tool(
        name = "dimension.get",
        description = "Retrieves detailed information about a specific dimension including its schema and dependency graph."
    )]
    async fn dimension_get(
        &self,
        Parameters(args): Parameters<GetDimensionParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.get_dimension_impl(args).await
    }

    #[tool(
        name = "dimension.list",
        description = "Retrieves a paginated list of all dimensions in the workspace."
    )]
    async fn dimension_list(
        &self,
        Parameters(args): Parameters<ListDimensionsParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.list_dimensions_impl(args).await
    }

    #[tool(
        name = "dimension.update",
        description = "Updates an existing dimension's schema, position, or function mappings."
    )]
    async fn dimension_update(
        &self,
        Parameters(args): Parameters<UpdateDimensionParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.update_dimension_impl(args).await
    }

    #[tool(
        name = "dimension.delete",
        description = "Permanently removes a dimension from the workspace."
    )]
    async fn dimension_delete(
        &self,
        Parameters(args): Parameters<DeleteDimensionParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.delete_dimension_impl(args).await
    }

    // ===== Contexts =====
    #[tool(
        name = "context.create",
        description = "Creates a new context with conditions and overrides. Contexts define conditional rules for config management."
    )]
    async fn context_create(
        &self,
        Parameters(args): Parameters<CreateContextParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.create_context_impl(args).await
    }

    #[tool(
        name = "context.get",
        description = "Retrieves detailed information about a specific context by its ID."
    )]
    async fn context_get(
        &self,
        Parameters(args): Parameters<GetContextParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.get_context_impl(args).await
    }

    #[tool(
        name = "context.list",
        description = "Retrieves a paginated list of contexts with filtering and sorting support."
    )]
    async fn context_list(
        &self,
        Parameters(args): Parameters<ListContextsParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.list_contexts_impl(args).await
    }

    #[tool(
        name = "context.delete",
        description = "Permanently removes a context from the workspace."
    )]
    async fn context_delete(
        &self,
        Parameters(args): Parameters<DeleteContextParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.delete_context_impl(args).await
    }

    #[tool(
        name = "context.update_override",
        description = "Updates the overrides for an existing context while maintaining the context's conditions."
    )]
    async fn context_update_override(
        &self,
        Parameters(args): Parameters<UpdateOverrideParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.update_override_impl(args).await
    }

    #[tool(
        name = "context.move",
        description = "Moves a context to a new condition. If a context with the new condition already exists, it merges the override."
    )]
    async fn context_move(
        &self,
        Parameters(args): Parameters<MoveContextParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.move_context_impl(args).await
    }

    #[tool(
        name = "context.get_by_condition",
        description = "Retrieves context information by matching against provided conditions."
    )]
    async fn context_get_by_condition(
        &self,
        Parameters(args): Parameters<GetContextFromConditionParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.get_context_from_condition_impl(args).await
    }

    #[tool(
        name = "context.validate",
        description = "Validates a context's conditions without creating it. Returns success if the context is valid, or an error describing why it is invalid."
    )]
    async fn context_validate(
        &self,
        Parameters(args): Parameters<ValidateContextParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.validate_context_impl(args).await
    }

    #[tool(
        name = "context.weight_recompute",
        description = "Recalculates priority weights for all contexts in the workspace."
    )]
    async fn context_weight_recompute(
        &self,
        Parameters(args): Parameters<WeightRecomputeParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.weight_recompute_impl(args).await
    }

    #[tool(
        name = "context.bulk_operation",
        description = "Executes multiple context operations (PUT, REPLACE, DELETE, MOVE) in a single batch."
    )]
    async fn context_bulk_operation(
        &self,
        Parameters(args): Parameters<BulkOperationParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.bulk_operation_impl(args).await
    }

    // ===== Experiments =====
    #[tool(
        name = "experiment.create",
        description = "Creates a new A/B experiment with variants, context conditions, and optional metrics."
    )]
    async fn experiment_create(
        &self,
        Parameters(args): Parameters<CreateExperimentParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.create_experiment_impl(args).await
    }

    #[tool(
        name = "experiment.get",
        description = "Retrieves detailed information about a specific experiment including variants, status, and metrics."
    )]
    async fn experiment_get(
        &self,
        Parameters(args): Parameters<GetExperimentParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.get_experiment_impl(args).await
    }

    #[tool(
        name = "experiment.list",
        description = "Retrieves a paginated list of experiments with filtering by status, date, name, and group."
    )]
    async fn experiment_list(
        &self,
        Parameters(args): Parameters<ListExperimentsParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.list_experiments_impl(args).await
    }

    #[tool(
        name = "experiment.update_overrides",
        description = "Updates the overrides for specific variants within an experiment."
    )]
    async fn experiment_update_overrides(
        &self,
        Parameters(args): Parameters<UpdateExperimentOverridesParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.update_experiment_overrides_impl(args).await
    }

    #[tool(
        name = "experiment.conclude",
        description = "Concludes an in-progress experiment by selecting a winning variant."
    )]
    async fn experiment_conclude(
        &self,
        Parameters(args): Parameters<ConcludeExperimentParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.conclude_experiment_impl(args).await
    }

    #[tool(
        name = "experiment.discard",
        description = "Discards an experiment without selecting a winner."
    )]
    async fn experiment_discard(
        &self,
        Parameters(args): Parameters<DiscardExperimentParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.discard_experiment_impl(args).await
    }

    #[tool(
        name = "experiment.ramp",
        description = "Adjusts the traffic percentage allocation for an in-progress experiment."
    )]
    async fn experiment_ramp(
        &self,
        Parameters(args): Parameters<RampExperimentParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.ramp_experiment_impl(args).await
    }

    #[tool(
        name = "experiment.pause",
        description = "Temporarily pauses an in-progress experiment, preserving its config for later resumption."
    )]
    async fn experiment_pause(
        &self,
        Parameters(args): Parameters<PauseResumeExperimentParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.pause_experiment_impl(args).await
    }

    #[tool(
        name = "experiment.resume",
        description = "Resumes a previously paused experiment, restoring its in-progress state."
    )]
    async fn experiment_resume(
        &self,
        Parameters(args): Parameters<PauseResumeExperimentParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.resume_experiment_impl(args).await
    }

    #[tool(
        name = "experiment.applicable_variants",
        description = "Determines which experiment variants are applicable to a given context and identifier."
    )]
    async fn experiment_applicable_variants(
        &self,
        Parameters(args): Parameters<ApplicableVariantsParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.applicable_variants_impl(args).await
    }

    // ===== Experiment Groups =====
    #[tool(
        name = "experiment_group.create",
        description = "Creates a new experiment group for managing multiple experiments together."
    )]
    async fn experiment_group_create(
        &self,
        Parameters(args): Parameters<CreateExperimentGroupParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.create_experiment_group_impl(args).await
    }

    #[tool(
        name = "experiment_group.get",
        description = "Retrieves an experiment group by its ID."
    )]
    async fn experiment_group_get(
        &self,
        Parameters(args): Parameters<GetExperimentGroupParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.get_experiment_group_impl(args).await
    }

    #[tool(
        name = "experiment_group.list",
        description = "Lists experiment groups with filtering and pagination."
    )]
    async fn experiment_group_list(
        &self,
        Parameters(args): Parameters<ListExperimentGroupsParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.list_experiment_groups_impl(args).await
    }

    #[tool(
        name = "experiment_group.update",
        description = "Updates an experiment group's description or traffic percentage."
    )]
    async fn experiment_group_update(
        &self,
        Parameters(args): Parameters<UpdateExperimentGroupParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.update_experiment_group_impl(args).await
    }

    #[tool(
        name = "experiment_group.delete",
        description = "Deletes an experiment group."
    )]
    async fn experiment_group_delete(
        &self,
        Parameters(args): Parameters<DeleteExperimentGroupParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.delete_experiment_group_impl(args).await
    }

    #[tool(
        name = "experiment_group.add_members",
        description = "Adds experiments to an existing experiment group."
    )]
    async fn experiment_group_add_members(
        &self,
        Parameters(args): Parameters<ModifyGroupMembersParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.add_members_to_group_impl(args).await
    }

    #[tool(
        name = "experiment_group.remove_members",
        description = "Removes experiments from an existing experiment group."
    )]
    async fn experiment_group_remove_members(
        &self,
        Parameters(args): Parameters<ModifyGroupMembersParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.remove_members_from_group_impl(args).await
    }

    // ===== Functions =====
    #[tool(
        name = "function.create",
        description = "Creates a new custom function for value validation, value compute, context validation, or change reason validation."
    )]
    async fn function_create(
        &self,
        Parameters(args): Parameters<CreateFunctionParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.create_function_impl(args).await
    }

    #[tool(
        name = "function.get",
        description = "Retrieves detailed information about a function including its published and draft versions."
    )]
    async fn function_get(
        &self,
        Parameters(args): Parameters<GetFunctionParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.get_function_impl(args).await
    }

    #[tool(
        name = "function.list",
        description = "Retrieves a paginated list of all functions in the workspace."
    )]
    async fn function_list(
        &self,
        Parameters(args): Parameters<ListFunctionsParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.list_functions_impl(args).await
    }

    #[tool(
        name = "function.update",
        description = "Updates the draft version of a function with new code or description."
    )]
    async fn function_update(
        &self,
        Parameters(args): Parameters<UpdateFunctionParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.update_function_impl(args).await
    }

    #[tool(
        name = "function.delete",
        description = "Permanently removes a function from the workspace."
    )]
    async fn function_delete(
        &self,
        Parameters(args): Parameters<DeleteFunctionParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.delete_function_impl(args).await
    }

    #[tool(
        name = "function.publish",
        description = "Publishes the draft version of a function, making it the active version."
    )]
    async fn function_publish(
        &self,
        Parameters(args): Parameters<PublishFunctionParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.publish_function_impl(args).await
    }

    #[tool(
        name = "function.test",
        description = "Executes a function in test mode with provided input parameters."
    )]
    async fn function_test(
        &self,
        Parameters(args): Parameters<TestFunctionParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.test_function_impl(args).await
    }

    // ===== Type Templates =====
    #[tool(
        name = "type_template.create",
        description = "Creates a new type template with a JSON schema definition."
    )]
    async fn type_template_create(
        &self,
        Parameters(args): Parameters<CreateTypeTemplateParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.create_type_template_impl(args).await
    }

    #[tool(
        name = "type_template.get",
        description = "Retrieves a type template by name including its schema and metadata."
    )]
    async fn type_template_get(
        &self,
        Parameters(args): Parameters<GetTypeTemplateParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.get_type_template_impl(args).await
    }

    #[tool(
        name = "type_template.list",
        description = "Retrieves a paginated list of all type templates in the workspace."
    )]
    async fn type_template_list(
        &self,
        Parameters(args): Parameters<ListTypeTemplatesParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.list_type_templates_impl(args).await
    }

    #[tool(
        name = "type_template.update",
        description = "Updates an existing type template's schema definition."
    )]
    async fn type_template_update(
        &self,
        Parameters(args): Parameters<UpdateTypeTemplateParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.update_type_template_impl(args).await
    }

    #[tool(
        name = "type_template.delete",
        description = "Permanently removes a type template from the workspace."
    )]
    async fn type_template_delete(
        &self,
        Parameters(args): Parameters<DeleteTypeTemplateParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.delete_type_template_impl(args).await
    }

    // ===== Organisations =====
    #[tool(
        name = "organisation.create",
        description = "Creates a new organisation with a name and admin email."
    )]
    async fn organisation_create(
        &self,
        Parameters(args): Parameters<CreateOrganisationParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.create_organisation_impl(args).await
    }

    #[tool(
        name = "organisation.get",
        description = "Retrieves detailed information about a specific organisation."
    )]
    async fn organisation_get(
        &self,
        Parameters(args): Parameters<GetOrganisationParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.get_organisation_impl(args).await
    }

    #[tool(
        name = "organisation.list",
        description = "Retrieves a paginated list of all organisations."
    )]
    async fn organisation_list(
        &self,
        Parameters(args): Parameters<ListOrganisationsParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.list_organisations_impl(args).await
    }

    #[tool(
        name = "organisation.update",
        description = "Updates an organisation's contact details, status, or admin email."
    )]
    async fn organisation_update(
        &self,
        Parameters(args): Parameters<UpdateOrganisationParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.update_organisation_impl(args).await
    }

    // ===== Workspaces =====
    #[tool(
        name = "workspace.create",
        description = "Creates a new workspace within an organisation with isolated config management."
    )]
    async fn workspace_create(
        &self,
        Parameters(args): Parameters<CreateWorkspaceParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.create_workspace_impl(args).await
    }

    #[tool(
        name = "workspace.get",
        description = "Retrieves detailed information about a specific workspace."
    )]
    async fn workspace_get(
        &self,
        Parameters(args): Parameters<GetWorkspaceParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.get_workspace_impl(args).await
    }

    #[tool(
        name = "workspace.list",
        description = "Retrieves a paginated list of all workspaces."
    )]
    async fn workspace_list(
        &self,
        Parameters(args): Parameters<ListWorkspacesParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.list_workspaces_impl(args).await
    }

    #[tool(
        name = "workspace.update",
        description = "Updates an existing workspace's admin email, status, or mandatory dimensions."
    )]
    async fn workspace_update(
        &self,
        Parameters(args): Parameters<UpdateWorkspaceParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.update_workspace_impl(args).await
    }

    // ===== Audit Logs =====
    #[tool(
        name = "audit_log.list",
        description = "Retrieves a paginated list of audit logs with filtering by date, table, action, and username."
    )]
    async fn audit_log_list(
        &self,
        Parameters(args): Parameters<ListAuditLogsParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.list_audit_logs_impl(args).await
    }

    // ===== Variables =====
    #[tool(
        name = "variable.create",
        description = "Creates a new key-value variable."
    )]
    async fn variable_create(
        &self,
        Parameters(args): Parameters<CreateVariableParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.create_variable_impl(args).await
    }

    #[tool(
        name = "variable.get",
        description = "Retrieves a specific variable by name."
    )]
    async fn variable_get(
        &self,
        Parameters(args): Parameters<GetVariableParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.get_variable_impl(args).await
    }

    #[tool(
        name = "variable.list",
        description = "Retrieves a paginated list of all variables in the workspace."
    )]
    async fn variable_list(
        &self,
        Parameters(args): Parameters<ListVariablesParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.list_variables_impl(args).await
    }

    #[tool(
        name = "variable.update",
        description = "Updates an existing variable's value or description."
    )]
    async fn variable_update(
        &self,
        Parameters(args): Parameters<UpdateVariableParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.update_variable_impl(args).await
    }

    #[tool(
        name = "variable.delete",
        description = "Permanently deletes a variable from the workspace."
    )]
    async fn variable_delete(
        &self,
        Parameters(args): Parameters<DeleteVariableParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.delete_variable_impl(args).await
    }

    // ===== Webhooks =====
    #[tool(
        name = "webhook.create",
        description = "Creates a new webhook to receive HTTP notifications on specified events."
    )]
    async fn webhook_create(
        &self,
        Parameters(args): Parameters<CreateWebhookParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.create_webhook_impl(args).await
    }

    #[tool(
        name = "webhook.get",
        description = "Retrieves a specific webhook's configuration."
    )]
    async fn webhook_get(
        &self,
        Parameters(args): Parameters<GetWebhookParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.get_webhook_impl(args).await
    }

    #[tool(
        name = "webhook.list",
        description = "Retrieves a paginated list of all webhooks in the workspace."
    )]
    async fn webhook_list(
        &self,
        Parameters(args): Parameters<ListWebhooksParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.list_webhooks_impl(args).await
    }

    #[tool(
        name = "webhook.update",
        description = "Updates an existing webhook's URL, events, headers, or other properties."
    )]
    async fn webhook_update(
        &self,
        Parameters(args): Parameters<UpdateWebhookParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.update_webhook_impl(args).await
    }

    #[tool(
        name = "webhook.delete",
        description = "Permanently removes a webhook from the workspace."
    )]
    async fn webhook_delete(
        &self,
        Parameters(args): Parameters<DeleteWebhookParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.delete_webhook_impl(args).await
    }

    #[tool(
        name = "webhook.get_by_event",
        description = "Retrieves the webhook configured for a specific event type."
    )]
    async fn webhook_get_by_event(
        &self,
        Parameters(args): Parameters<GetWebhookByEventParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.get_webhook_by_event_impl(args).await
    }

    // ===== Secrets =====
    #[tool(
        name = "secret.create",
        description = "Creates a new encrypted secret with the specified name and value."
    )]
    async fn secret_create(
        &self,
        Parameters(args): Parameters<CreateSecretParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.create_secret_impl(args).await
    }

    #[tool(
        name = "secret.get",
        description = "Retrieves secret metadata by name (values are always masked)."
    )]
    async fn secret_get(
        &self,
        Parameters(args): Parameters<GetSecretParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.get_secret_impl(args).await
    }

    #[tool(
        name = "secret.list",
        description = "Retrieves a paginated list of all secrets in the workspace (values are masked)."
    )]
    async fn secret_list(
        &self,
        Parameters(args): Parameters<ListSecretsParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.list_secrets_impl(args).await
    }

    #[tool(
        name = "secret.update",
        description = "Updates a secret's value or description. The value is re-encrypted with the current key."
    )]
    async fn secret_update(
        &self,
        Parameters(args): Parameters<UpdateSecretParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.update_secret_impl(args).await
    }

    #[tool(
        name = "secret.delete",
        description = "Permanently deletes a secret from the workspace."
    )]
    async fn secret_delete(
        &self,
        Parameters(args): Parameters<DeleteSecretParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.delete_secret_impl(args).await
    }
}

impl ServerHandler for SuperpositionMcpServer {
    fn get_info(&self) -> ServerInfo {
        ServerInfo::new(ServerCapabilities::builder().enable_tools().build())
            .with_instructions(
                "Superposition MCP Server — provides tools for managing feature flags, \
                 A/B experiments, configuration contexts, dimensions, and more via the \
                 Superposition platform."
                    .to_string(),
            )
    }
}
