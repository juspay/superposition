use std::sync::Arc;

use smithy_mcp_runtime::Router;
use superposition_mcp::tools;
use superposition_sdk::{config::Builder, Client};

use crate::auth::{shared_basic, shared_bearer, FallbackScheme, TaskLocalAuthSchemePlugin};
use crate::config::{Config, Defaults, Mode, StaticCreds};
use crate::dispatch;

pub fn build_client(cfg: &Config, mode: Mode) -> Client {
    build_client_with(cfg, mode, |b| b)
}

/// Construct the SDK client, allowing the caller to apply additional
/// modifications to the [`Builder`] before `.build()`. Used by integration
/// tests to swap in a non-TLS HTTP client that can talk to wiremock; the
/// production binary uses [`build_client`] which preserves the default
/// HTTPS connector.
pub fn build_client_with<F>(cfg: &Config, mode: Mode, customise: F) -> Client
where
    F: FnOnce(Builder) -> Builder,
{
    let bearer_fallback = match (&cfg.creds, mode) {
        (Some(StaticCreds::Bearer(t)), Mode::Stdio)
        | (Some(StaticCreds::Bearer(t)), Mode::HttpWithStaticFallback) => Some(t.clone()),
        _ => None,
    };
    let basic_fallback = match (&cfg.creds, mode) {
        (Some(StaticCreds::Basic { user, pass }), Mode::Stdio)
        | (Some(StaticCreds::Basic { user, pass }), Mode::HttpWithStaticFallback) => {
            Some((user.clone(), pass.clone()))
        }
        _ => None,
    };

    // The fallback scheme is the auth scheme used when the per-request
    // `SUPERPOSITION_AUTH` task-local is unset — i.e., stdio mode (where the
    // task-local is set process-wide from env creds) and HTTP + `--allow-static-auth`
    // when no `Authorization` header is supplied. The scheme is derived from
    // which credential variant the operator configured.
    let fallback_scheme = cfg.creds.as_ref().map(|c| match c {
        StaticCreds::Bearer(_) => FallbackScheme::Bearer,
        StaticCreds::Basic { .. } => FallbackScheme::Basic,
    });

    let bearer = shared_bearer(bearer_fallback);
    let basic = shared_basic(basic_fallback);

    let builder = Builder::new()
        .behavior_version_latest()
        .endpoint_url(&cfg.endpoint)
        .bearer_token_resolver(bearer)
        .basic_auth_login_resolver(basic)
        .runtime_plugin(TaskLocalAuthSchemePlugin::new(fallback_scheme));

    Client::from_conf(customise(builder).build())
}

pub fn build_router(client: Client, defaults: Defaults) -> Router {
    let client = Arc::new(client);
    let defaults = Arc::new(defaults);
    let mut router = Router::new();

    macro_rules! register {
        ($info_fn:ident, $handle_fn:ident) => {{
            let c = client.clone();
            let d = defaults.clone();
            router.register_tool(tools::$info_fn(), move |params| {
                let c = c.clone();
                let d = d.clone();
                async move {
                    let params = dispatch::inject_defaults(params, &d);
                    tools::$handle_fn(&c, params).await
                }
            });
        }};
    }

    // BEGIN GENERATED TOOL REGISTRATIONS
    //
    // Regenerate with:
    //   grep -E "^pub async fn handle_" crates/superposition_mcp/src/tools.rs \
    //     | awk "{print \$4}" | sed "s/(.*//" | sort \
    //     | sed "s/^handle_(.*)$/    register!(tool_info_<m>, handle_<m>);/"
    // (replace <m> with the regex backreference for your shell; see plan doc).
    register!(tool_info_add_members_to_group, handle_add_members_to_group);
    register!(tool_info_applicable_variants, handle_applicable_variants);
    register!(tool_info_bulk_operation, handle_bulk_operation);
    register!(tool_info_conclude_experiment, handle_conclude_experiment);
    register!(tool_info_create_context, handle_create_context);
    register!(tool_info_create_default_config, handle_create_default_config);
    register!(tool_info_create_dimension, handle_create_dimension);
    register!(tool_info_create_experiment, handle_create_experiment);
    register!(tool_info_create_experiment_group, handle_create_experiment_group);
    register!(tool_info_create_function, handle_create_function);
    register!(tool_info_create_organisation, handle_create_organisation);
    register!(tool_info_create_secret, handle_create_secret);
    register!(tool_info_create_type_templates, handle_create_type_templates);
    register!(tool_info_create_variable, handle_create_variable);
    register!(tool_info_create_webhook, handle_create_webhook);
    register!(tool_info_create_workspace, handle_create_workspace);
    register!(tool_info_delete_context, handle_delete_context);
    register!(tool_info_delete_default_config, handle_delete_default_config);
    register!(tool_info_delete_dimension, handle_delete_dimension);
    register!(tool_info_delete_experiment_group, handle_delete_experiment_group);
    register!(tool_info_delete_function, handle_delete_function);
    register!(tool_info_delete_secret, handle_delete_secret);
    register!(tool_info_delete_type_templates, handle_delete_type_templates);
    register!(tool_info_delete_variable, handle_delete_variable);
    register!(tool_info_delete_webhook, handle_delete_webhook);
    register!(tool_info_discard_experiment, handle_discard_experiment);
    register!(tool_info_get_config, handle_get_config);
    register!(tool_info_get_config_json, handle_get_config_json);
    register!(tool_info_get_config_toml, handle_get_config_toml);
    register!(tool_info_get_context, handle_get_context);
    register!(tool_info_get_context_from_condition, handle_get_context_from_condition);
    register!(tool_info_get_default_config, handle_get_default_config);
    register!(tool_info_get_dimension, handle_get_dimension);
    register!(tool_info_get_experiment, handle_get_experiment);
    register!(tool_info_get_experiment_config, handle_get_experiment_config);
    register!(tool_info_get_experiment_group, handle_get_experiment_group);
    register!(tool_info_get_function, handle_get_function);
    register!(tool_info_get_organisation, handle_get_organisation);
    register!(tool_info_get_resolved_config, handle_get_resolved_config);
    register!(tool_info_get_resolved_config_with_identifier, handle_get_resolved_config_with_identifier);
    register!(tool_info_get_secret, handle_get_secret);
    register!(tool_info_get_type_template, handle_get_type_template);
    register!(tool_info_get_type_templates_list, handle_get_type_templates_list);
    register!(tool_info_get_variable, handle_get_variable);
    register!(tool_info_get_version, handle_get_version);
    register!(tool_info_get_webhook, handle_get_webhook);
    register!(tool_info_get_webhook_by_event, handle_get_webhook_by_event);
    register!(tool_info_get_workspace, handle_get_workspace);
    register!(tool_info_list_audit_logs, handle_list_audit_logs);
    register!(tool_info_list_contexts, handle_list_contexts);
    register!(tool_info_list_default_configs, handle_list_default_configs);
    register!(tool_info_list_dimensions, handle_list_dimensions);
    register!(tool_info_list_experiment, handle_list_experiment);
    register!(tool_info_list_experiment_groups, handle_list_experiment_groups);
    register!(tool_info_list_function, handle_list_function);
    register!(tool_info_list_organisation, handle_list_organisation);
    register!(tool_info_list_secrets, handle_list_secrets);
    register!(tool_info_list_variables, handle_list_variables);
    register!(tool_info_list_versions, handle_list_versions);
    register!(tool_info_list_webhook, handle_list_webhook);
    register!(tool_info_list_workspace, handle_list_workspace);
    register!(tool_info_migrate_workspace_schema, handle_migrate_workspace_schema);
    register!(tool_info_move_context, handle_move_context);
    register!(tool_info_pause_experiment, handle_pause_experiment);
    register!(tool_info_publish, handle_publish);
    register!(tool_info_ramp_experiment, handle_ramp_experiment);
    register!(tool_info_remove_members_from_group, handle_remove_members_from_group);
    register!(tool_info_resume_experiment, handle_resume_experiment);
    register!(tool_info_rotate_master_encryption_key, handle_rotate_master_encryption_key);
    register!(tool_info_rotate_workspace_encryption_key, handle_rotate_workspace_encryption_key);
    register!(tool_info_test, handle_test);
    register!(tool_info_update_default_config, handle_update_default_config);
    register!(tool_info_update_dimension, handle_update_dimension);
    register!(tool_info_update_experiment_group, handle_update_experiment_group);
    register!(tool_info_update_function, handle_update_function);
    register!(tool_info_update_organisation, handle_update_organisation);
    register!(tool_info_update_override, handle_update_override);
    register!(tool_info_update_overrides_experiment, handle_update_overrides_experiment);
    register!(tool_info_update_secret, handle_update_secret);
    register!(tool_info_update_type_templates, handle_update_type_templates);
    register!(tool_info_update_variable, handle_update_variable);
    register!(tool_info_update_webhook, handle_update_webhook);
    register!(tool_info_update_workspace, handle_update_workspace);
    register!(tool_info_validate_context, handle_validate_context);
    register!(tool_info_weight_recompute, handle_weight_recompute);
    // END GENERATED TOOL REGISTRATIONS

    router
}
