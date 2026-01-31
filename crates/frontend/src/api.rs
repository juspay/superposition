use reqwest::header::HeaderMap;
use serde_json::{Map, Value};
use superposition_types::{
    Config, PaginatedResponse,
    api::{
        config::{ConfigQuery, ResolveConfigQuery},
        context::ContextListFilters,
        experiments::{
            ExperimentListFilters, ExperimentResponse, ExperimentStateChangeRequest,
        },
        functions::{
            FunctionEnvironment, FunctionExecutionRequest, FunctionExecutionResponse,
            KeyType, ListFunctionFilters, Stage,
        },
        webhook::{CreateWebhookRequest, UpdateWebhookRequest, WebhookName},
    },
    custom_query::{DimensionQuery, PaginationParams, QueryMap, QueryParam},
    database::models::{
        ChangeReason, Description, Metrics, NonEmptyString,
        cac::{Context, Function, TypeTemplate},
        others::{CustomHeaders, HttpMethod, PayloadVersion, Webhook, WebhookEvent},
    },
};

use crate::utils::{
    construct_request_headers, parse_json_response, request, use_host_server,
};

pub mod snapshots {
    use superposition_types::database::models::cac::{
        ConfigVersion, ConfigVersionListItem,
    };

    use super::*;

    pub async fn fetch_all(
        filters: &PaginationParams,
        workspace: &str,
        org_id: &str,
    ) -> Result<PaginatedResponse<ConfigVersionListItem>, String> {
        let host = use_host_server();
        let url = format!("{host}/config/versions?{}", filters.to_query_param());

        let response = request(
            url,
            reqwest::Method::GET,
            None::<()>,
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }

    pub async fn fetch(
        id: &str,
        workspace: &str,
        org_id: &str,
    ) -> Result<ConfigVersion, String> {
        let host = use_host_server();
        let url = format!("{host}/config/version/{id}");

        let response = request(
            url,
            reqwest::Method::GET,
            None::<()>,
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }
}

pub mod dimensions {
    use superposition_types::{
        ExtendedMap,
        api::dimension::{
            CreateRequest, DimensionName, DimensionResponse, UpdateRequest,
        },
        database::models::cac::{DimensionType, Position},
    };

    use super::*;

    pub async fn fetch(
        filters: &PaginationParams,
        workspace: &str,
        org_id: &str,
    ) -> Result<PaginatedResponse<DimensionResponse>, String> {
        let host = use_host_server();
        let url = format!("{}/dimension?{}", host, filters.to_query_param());

        let response = request(
            url,
            reqwest::Method::GET,
            None::<()>,
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }

    pub async fn get(
        name: &str,
        workspace: &str,
        org_id: &str,
    ) -> Result<DimensionResponse, String> {
        let host = use_host_server();
        let url = format!("{host}/dimension/{name}");

        let response = request(
            url,
            reqwest::Method::GET,
            None::<()>,
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }

    #[allow(clippy::too_many_arguments)]
    pub async fn create(
        dimension: String,
        position: u32,
        schema: Value,
        value_validation_function_name: Option<String>,
        value_compute_function_name: Option<String>,
        description: String,
        change_reason: String,
        dimension_type: DimensionType,
        workspace: &str,
        org_id: &str,
    ) -> Result<DimensionResponse, String> {
        let payload = CreateRequest {
            dimension: DimensionName::try_from(dimension)?,
            position: Position::from(position),
            schema: ExtendedMap::try_from(schema)?,
            value_validation_function_name,
            value_compute_function_name,
            description: Description::try_from(description)?,
            change_reason: ChangeReason::try_from(change_reason)?,
            dimension_type,
        };

        let host = use_host_server();
        let url = format!("{host}/dimension");

        let response = request(
            url,
            reqwest::Method::POST,
            Some(payload),
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }

    pub async fn update(
        dimension_name: String,
        payload: UpdateRequest,
        workspace: &str,
        org_id: &str,
    ) -> Result<DimensionResponse, String> {
        let host = use_host_server();
        let url = format!("{host}/dimension/{dimension_name}");

        let response = request(
            url,
            reqwest::Method::PATCH,
            Some(payload),
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }

    pub async fn delete(
        name: String,
        workspace: &str,
        org_id: &str,
    ) -> Result<(), String> {
        let host = use_host_server();
        let url = format!("{host}/dimension/{name}");

        request(
            url,
            reqwest::Method::DELETE,
            None::<()>,
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        Ok(())
    }
}

pub async fn delete_context(
    context_id: String,
    workspace: &str,
    org_id: &str,
) -> Result<(), String> {
    let host = use_host_server();
    let url = format!("{}/context/{}", host, context_id);

    request(
        url,
        reqwest::Method::DELETE,
        None::<()>,
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    Ok(())
}

pub async fn fetch_experiments(
    filters: &ExperimentListFilters,
    pagination: &PaginationParams,
    dimension_params: &DimensionQuery<QueryMap>,
    workspace: &str,
    org_id: &str,
) -> Result<PaginatedResponse<ExperimentResponse>, String> {
    let host = use_host_server();
    let url = format!(
        "{}/experiments?{}&{}&{}",
        host,
        filters.to_query_param(),
        pagination.to_query_param(),
        dimension_params.to_query_param()
    );

    let response = request(
        url,
        reqwest::Method::GET,
        None::<()>,
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn fetch_functions(
    pagination: &PaginationParams,
    filters: &ListFunctionFilters,
    workspace: &str,
    org_id: &str,
) -> Result<PaginatedResponse<Function>, String> {
    let host = use_host_server();
    let url = format!(
        "{}/function?{}&{}",
        host,
        filters.to_query_param(),
        pagination.to_query_param()
    );

    let response = request(
        url,
        reqwest::Method::GET,
        None::<()>,
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn fetch_function(
    function_name: String,
    workspace: &str,
    org_id: &str,
) -> Result<Function, String> {
    let host = use_host_server();
    let url = format!("{}/function/{}", host, function_name);

    let response = request(
        url,
        reqwest::Method::GET,
        None::<()>,
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

// #[server(GetConfig, "/fxn", "GetJson")]
pub async fn fetch_config(
    context: &DimensionQuery<QueryMap>,
    config_params: &ConfigQuery,
    workspace: &str,
    org_id: &str,
) -> Result<Config, String> {
    let host = use_host_server();
    let url = format!(
        "{host}/config?{}&{}",
        context.to_query_param(),
        config_params.to_query_param()
    );

    let response = request(
        url,
        reqwest::Method::GET,
        None::<()>,
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn fetch_context(
    pagination: &PaginationParams,
    context_filters: &ContextListFilters,
    dimension_params: &DimensionQuery<QueryMap>,
    workspace: &str,
    org_id: &str,
) -> Result<PaginatedResponse<Context>, String> {
    let host = use_host_server();
    let url = format!(
        "{host}/context?{}&{}&{}",
        pagination.to_query_param(),
        context_filters.to_query_param(),
        dimension_params.to_query_param()
    );

    let response = request(
        url,
        reqwest::Method::GET,
        None::<()>,
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

// #[server(GetExperiment, "/fxn", "GetJson")]
pub async fn fetch_experiment(
    exp_id: String,
    workspace: &str,
    org_id: &str,
) -> Result<ExperimentResponse, String> {
    let host = use_host_server();
    let url = format!("{}/experiments/{}", host, exp_id);

    let response = request(
        url,
        reqwest::Method::GET,
        None::<()>,
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub mod default_configs {
    use superposition_types::{
        api::default_config::DefaultConfigFilters, database::models::cac::DefaultConfig,
    };

    use super::*;

    pub async fn get(
        key_name: &str,
        workspace: &str,
        org_id: &str,
    ) -> Result<DefaultConfig, String> {
        let host = use_host_server();
        let url = format!("{host}/default-config/{key_name}");

        let response = request(
            url,
            reqwest::Method::GET,
            None::<()>,
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }

    pub async fn delete(
        key: String,
        workspace: &str,
        org_id: &str,
    ) -> Result<(), String> {
        let host = use_host_server();
        let url = format!("{host}/default-config/{key}");

        request(
            url,
            reqwest::Method::DELETE,
            None::<()>,
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        Ok(())
    }

    pub async fn fetch(
        pagination: &PaginationParams,
        filters: &DefaultConfigFilters,
        workspace: &str,
        org_id: &str,
    ) -> Result<PaginatedResponse<DefaultConfig>, String> {
        let host = use_host_server();
        let url = format!(
            "{}/default-config?{}&{}",
            host,
            pagination.to_query_param(),
            filters.to_query_param()
        );

        let response = request(
            url,
            reqwest::Method::GET,
            None::<()>,
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }
}

pub async fn fetch_organisations() -> Result<Vec<String>, String> {
    let host = use_host_server();
    let url = format!("{host}/organisations");

    let response =
        request(url, reqwest::Method::GET, None::<()>, HeaderMap::new()).await?;

    parse_json_response(response).await
}

pub async fn fetch_types(
    filters: &PaginationParams,
    workspace: &str,
    org_id: &str,
) -> Result<PaginatedResponse<TypeTemplate>, String> {
    let host = use_host_server();
    let url = format!("{host}/types?{}", filters.to_query_param());

    let response = request::<()>(
        url,
        reqwest::Method::GET,
        None,
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub mod workspaces {
    use superposition_types::{
        api::workspace::{
            CreateWorkspaceRequest, KeyRotationResponse, UpdateWorkspaceRequest,
            WorkspaceResponse,
        },
        database::models::WorkspaceStatus,
    };

    use super::*;

    pub async fn fetch_all(
        filters: &PaginationParams,
        org_id: &str,
    ) -> Result<PaginatedResponse<WorkspaceResponse>, String> {
        let host = use_host_server();
        let url = format!("{}/workspaces?{}", host, filters.to_query_param());

        let response = request::<()>(
            url,
            reqwest::Method::GET,
            None,
            construct_request_headers(&[("x-org-id", org_id)])?,
        )
        .await?;

        parse_json_response(response).await
    }

    pub async fn create(
        payload: CreateWorkspaceRequest,
        org_id: &str,
    ) -> Result<serde_json::Value, String> {
        let host = use_host_server();
        let url = format!("{}/workspaces", host);

        let response = request(
            url,
            reqwest::Method::POST,
            Some(payload),
            construct_request_headers(&[("x-org-id", org_id)])?,
        )
        .await?;

        parse_json_response(response).await
    }

    #[allow(clippy::too_many_arguments)]
    pub fn try_update_payload(
        workspace_admin_email: String,
        config_version: Value,
        workspace_status: WorkspaceStatus,
        mandatory_dimensions: Vec<String>,
        metrics: Metrics,
        allow_experiment_self_approval: bool,
        auto_populate_control: bool,
        enable_context_validation: bool,
        enable_change_reason_validation: bool,
    ) -> Result<UpdateWorkspaceRequest, String> {
        Ok(UpdateWorkspaceRequest {
            workspace_admin_email,
            config_version: Some(
                serde_json::from_value(config_version)
                    .map_err(|e| format!("Invalid config version: {}", e))?,
            ),
            workspace_status: Some(workspace_status),
            mandatory_dimensions: Some(mandatory_dimensions),
            metrics: Some(metrics),
            allow_experiment_self_approval: Some(allow_experiment_self_approval),
            auto_populate_control: Some(auto_populate_control),
            enable_context_validation: Some(enable_context_validation),
            enable_change_reason_validation: Some(enable_change_reason_validation),
        })
    }

    pub async fn update(
        workspace_key: &str,
        payload: UpdateWorkspaceRequest,
        org_id: &str,
    ) -> Result<serde_json::Value, String> {
        let host = use_host_server();
        let url = format!("{}/workspaces/{}", host, workspace_key);

        let response = request(
            url,
            reqwest::Method::PATCH,
            Some(payload),
            construct_request_headers(&[("x-org-id", org_id)])?,
        )
        .await?;

        parse_json_response(response).await
    }

    pub async fn rotate_key(
        workspace: &str,
        org_id: &str,
    ) -> Result<KeyRotationResponse, String> {
        let host = use_host_server();
        let url = format!("{host}/workspaces/{workspace}/rotate-encryption-key");

        let response = request(
            url,
            reqwest::Method::POST,
            None::<()>,
            construct_request_headers(&[("x-org-id", org_id)])?,
        )
        .await?;

        parse_json_response(response).await
    }
}

#[allow(clippy::too_many_arguments)]
pub async fn create_webhook(
    name: String,
    description: String,
    enabled: bool,
    url: String,
    method: HttpMethod,
    payload_version: PayloadVersion,
    custom_headers: CustomHeaders,
    events: Vec<WebhookEvent>,
    change_reason: String,
    workspace: &str,
    org_id: &str,
) -> Result<Webhook, String> {
    let payload = CreateWebhookRequest {
        name: WebhookName::try_from(name)?,
        description: Description::try_from(description)?,
        enabled,
        url: NonEmptyString::try_from(url)?,
        method,
        payload_version: Some(payload_version),
        custom_headers: Some(custom_headers),
        events,
        change_reason: ChangeReason::try_from(change_reason)?,
    };
    let host = use_host_server();
    let url = format!("{host}/webhook");

    let response = request(
        url,
        reqwest::Method::POST,
        Some(payload),
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn update_webhook(
    webhook_name: String,
    payload: UpdateWebhookRequest,
    workspace: &str,
    org_id: &str,
) -> Result<Webhook, String> {
    let host = use_host_server();
    let url = format!("{host}/webhook/{webhook_name}");

    let response = request(
        url,
        reqwest::Method::PATCH,
        Some(payload),
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn fetch_webhooks(
    filters: &PaginationParams,
    workspace: &str,
    org_id: &str,
) -> Result<PaginatedResponse<Webhook>, String> {
    let host = use_host_server();
    let url = format!("{}/webhook?{}", host, filters.to_query_param());

    let response = request(
        url,
        reqwest::Method::GET,
        None::<()>,
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn delete_webhooks(
    name: String,
    workspace: &str,
    org_id: &str,
) -> Result<(), String> {
    let host = use_host_server();
    let url = format!("{host}/webhook/{name}");

    request(
        url,
        reqwest::Method::DELETE,
        None::<()>,
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    Ok(())
}

pub async fn resolve_config(
    context: &DimensionQuery<QueryMap>,
    resolve_params: &ResolveConfigQuery,
    workspace: &str,
    org_id: &str,
) -> Result<Map<String, Value>, String> {
    let host = use_host_server();
    let url = format!(
        "{host}/config/resolve?{}&{}",
        context.to_query_param(),
        resolve_params.to_query_param()
    );

    let response = request(
        url,
        reqwest::Method::GET,
        None::<()>,
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn pause_experiment(
    exp_id: &str,
    change_reason: String,
    workspace: &str,
    org_id: &str,
) -> Result<ExperimentResponse, String> {
    let payload = ExperimentStateChangeRequest {
        change_reason: ChangeReason::try_from(change_reason)?,
    };

    let host = use_host_server();
    let url = format!("{host}/experiments/{exp_id}/pause");

    let response = request(
        url,
        reqwest::Method::PATCH,
        Some(payload),
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn resume_experiment(
    exp_id: &str,
    change_reason: String,
    workspace: &str,
    org_id: &str,
) -> Result<ExperimentResponse, String> {
    let payload = ExperimentStateChangeRequest {
        change_reason: ChangeReason::try_from(change_reason)?,
    };

    let host = use_host_server();
    let url = format!("{host}/experiments/{exp_id}/resume");

    let response = request(
        url,
        reqwest::Method::PATCH,
        Some(payload),
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn discard_experiment(
    exp_id: &str,
    change_reason: String,
    workspace: &str,
    org_id: &str,
) -> Result<ExperimentResponse, String> {
    let payload = ExperimentStateChangeRequest {
        change_reason: ChangeReason::try_from(change_reason)?,
    };

    let host = use_host_server();
    let url = format!("{host}/experiments/{exp_id}/discard");

    let response = request(
        url,
        reqwest::Method::PATCH,
        Some(payload),
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn get_context(
    context_id: &str,
    workspace: &str,
    org_id: &str,
) -> Result<Context, String> {
    let host = use_host_server();
    let url = format!("{host}/context/{context_id}");

    let response = request(
        url,
        reqwest::Method::GET,
        None::<()>,
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn get_context_from_condition(
    condition: &Map<String, Value>,
    workspace: &str,
    org_id: &str,
) -> Result<Context, String> {
    let host = use_host_server();
    let url = format!("{host}/context/get");

    let response = request(
        url,
        reqwest::Method::POST,
        Some(condition),
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn execute_value_compute_function(
    name: String,
    value: String,
    r#type: KeyType,
    environment: FunctionEnvironment,
    fn_name: &str,
    workspace: &str,
    org_id: &str,
) -> Result<Vec<String>, String> {
    let host = use_host_server();
    let url = format!("{}/function/{}/{}/test", host, fn_name, Stage::Published);
    let payload = FunctionExecutionRequest::ValueComputeFunctionRequest {
        name,
        prefix: value,
        r#type,
        environment,
    };
    let resp = request(
        url,
        reqwest::Method::POST,
        Some(payload),
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    let function_execution_response =
        parse_json_response::<FunctionExecutionResponse>(resp).await?;

    let result = function_execution_response
        .fn_output
        .as_array()
        .unwrap_or(&Vec::new())
        .iter()
        .map(|v| {
            v.as_str()
                .unwrap_or("Could not parse function execution response")
                .to_string()
        })
        .collect();
    Ok(result)
}

pub async fn get_type_template(
    name: &str,
    workspace: &str,
    org_id: &str,
) -> Result<TypeTemplate, String> {
    let host = use_host_server();
    let url = format!("{host}/types/{name}");

    let response = request(
        url,
        reqwest::Method::GET,
        None::<()>,
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn get_webhook(
    name: &str,
    workspace: &str,
    org_id: &str,
) -> Result<Webhook, String> {
    let host = use_host_server();
    let url = format!("{host}/webhook/{name}");

    let response = request(
        url,
        reqwest::Method::GET,
        None::<()>,
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub mod variables {
    use superposition_types::{
        api::variables::{CreateVariableRequest, UpdateVariableRequest, VariableFilters},
        database::models::others::{Variable, VariableName},
    };

    use super::*;

    pub async fn fetch(
        filters: &VariableFilters,
        pagination_params: &PaginationParams,
        workspace: &str,
        org_id: &str,
    ) -> Result<PaginatedResponse<Variable>, String> {
        let host = use_host_server();
        let url = format!(
            "{host}/variables?{}&{}",
            filters.to_query_param(),
            pagination_params.to_query_param()
        );

        let response = request(
            url,
            reqwest::Method::GET,
            None::<()>,
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }

    pub async fn create(
        name: String,
        value: String,
        description: String,
        change_reason: String,
        workspace: &str,
        org_id: &str,
    ) -> Result<Variable, String> {
        let payload = CreateVariableRequest {
            name: VariableName::try_from(name)?,
            value,
            description: Description::try_from(description)?,
            change_reason: ChangeReason::try_from(change_reason)?,
        };

        let host = use_host_server();
        let url = format!("{host}/variables");

        let response = request(
            url,
            reqwest::Method::POST,
            Some(payload),
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }

    pub async fn get(
        variable_name: &str,
        workspace: &str,
        org_id: &str,
    ) -> Result<Variable, String> {
        let host = use_host_server();
        let url = format!("{host}/variables/{}", variable_name);

        let response = request(
            url,
            reqwest::Method::GET,
            None::<()>,
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }

    pub async fn update(
        variable_name: String,
        payload: UpdateVariableRequest,
        workspace: &str,
        org_id: &str,
    ) -> Result<Variable, String> {
        let host = use_host_server();
        let url = format!("{host}/variables/{variable_name}");

        let response = request(
            url,
            reqwest::Method::PATCH,
            Some(payload),
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }

    pub async fn delete(
        variable_name: String,
        workspace: &str,
        org_id: &str,
    ) -> Result<(), String> {
        let host = use_host_server();
        let url = format!("{host}/variables/{variable_name}");

        request(
            url,
            reqwest::Method::DELETE,
            None::<()>,
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        Ok(())
    }
}

pub mod secrets {
    use superposition_types::{
        api::secrets::{
            CreateSecretRequest, SecretFilters, SecretResponse, UpdateSecretRequest,
        },
        database::models::others::SecretName,
    };

    use super::*;

    pub async fn fetch(
        filters: &SecretFilters,
        pagination_params: &PaginationParams,
        workspace: &str,
        org_id: &str,
    ) -> Result<PaginatedResponse<SecretResponse>, String> {
        let host = use_host_server();
        let url = format!(
            "{host}/secrets?{}&{}",
            filters.to_query_param(),
            pagination_params.to_query_param()
        );

        let response = request(
            url,
            reqwest::Method::GET,
            None::<()>,
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }

    pub async fn create(
        name: String,
        value: String,
        description: String,
        change_reason: String,
        workspace: &str,
        org_id: &str,
    ) -> Result<SecretResponse, String> {
        let payload = CreateSecretRequest {
            name: SecretName::try_from(name)?,
            value,
            description: Description::try_from(description)?,
            change_reason: ChangeReason::try_from(change_reason)?,
        };

        let host = use_host_server();
        let url = format!("{host}/secrets");

        let response = request(
            url,
            reqwest::Method::POST,
            Some(payload),
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }

    pub async fn get(
        secret_name: &str,
        workspace: &str,
        org_id: &str,
    ) -> Result<SecretResponse, String> {
        let host = use_host_server();
        let url = format!("{host}/secrets/{}", secret_name);

        let response = request(
            url,
            reqwest::Method::GET,
            None::<()>,
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }

    pub async fn update(
        secret_name: String,
        payload: UpdateSecretRequest,
        workspace: &str,
        org_id: &str,
    ) -> Result<SecretResponse, String> {
        let host = use_host_server();
        let url = format!("{host}/secrets/{secret_name}");

        let response = request(
            url,
            reqwest::Method::PATCH,
            Some(payload),
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }

    pub async fn delete(
        secret_name: String,
        workspace: &str,
        org_id: &str,
    ) -> Result<SecretResponse, String> {
        let host = use_host_server();
        let url = format!("{host}/secrets/{secret_name}");

        let response = request(
            url,
            reqwest::Method::DELETE,
            None::<()>,
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }
}

pub mod experiment_groups {
    use superposition_types::{
        Condition, Exp,
        api::experiment_groups::{
            ExpGroupCreateRequest, ExpGroupFilters, ExpGroupMemberRequest,
            ExpGroupUpdateRequest,
        },
        database::models::experimentation::{ExperimentGroup, TrafficPercentage},
    };

    use crate::logic::Conditions;

    use super::*;

    pub async fn fetch_all(
        filters: &ExpGroupFilters,
        pagination: &PaginationParams,
        workspace: &str,
        org_id: &str,
    ) -> Result<PaginatedResponse<ExperimentGroup>, String> {
        let host = use_host_server();

        let url = format!(
            "{}/experiment-groups?{}&{}",
            host,
            filters.to_query_param(),
            pagination.to_query_param()
        );
        let response = request::<()>(
            url,
            reqwest::Method::GET,
            None,
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }

    pub async fn fetch(
        group_id: &str,
        workspace: &str,
        org_id: &str,
    ) -> Result<ExperimentGroup, String> {
        let host = use_host_server();
        let url = format!("{}/experiment-groups/{}", host, group_id);

        let headers = construct_request_headers(&[
            ("x-workspace", workspace),
            ("x-org-id", org_id),
        ])?;

        let response = request(url, reqwest::Method::GET, None::<()>, headers).await?;

        parse_json_response(response).await
    }

    #[allow(clippy::too_many_arguments)]
    pub async fn create(
        name: String,
        description: String,
        change_reason: String,
        traffic_percentage: i32,
        member_experiment_ids: Option<Vec<i64>>,
        conditions: Conditions,
        workspace: &str,
        org_id: &str,
    ) -> Result<ExperimentGroup, String> {
        let payload = ExpGroupCreateRequest {
            name,
            description: Description::try_from(description)?,
            change_reason: ChangeReason::try_from(change_reason)?,
            traffic_percentage: TrafficPercentage::try_from(traffic_percentage)?,
            member_experiment_ids,
            context: Exp::<Condition>::try_from(conditions.as_context_json())
                .map_err(|e| e.to_string())?,
        };
        let host = use_host_server();
        let url = format!("{host}/experiment-groups");

        let response = request(
            url,
            reqwest::Method::POST,
            Some(payload),
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }

    pub fn try_update_payload(
        traffic_percentage: i32,
        description: String,
        change_reason: String,
    ) -> Result<ExpGroupUpdateRequest, String> {
        Ok(ExpGroupUpdateRequest {
            traffic_percentage: Some(TrafficPercentage::try_from(traffic_percentage)?),
            description: Some(Description::try_from(description)?),
            change_reason: ChangeReason::try_from(change_reason)?,
        })
    }

    pub async fn update(
        group_id: &str,
        payload: ExpGroupUpdateRequest,
        workspace: &str,
        org_id: &str,
    ) -> Result<ExperimentGroup, String> {
        let host = use_host_server();
        let url = format!("{}/experiment-groups/{}", host, group_id);

        let response = request(
            url,
            reqwest::Method::PATCH,
            Some(payload),
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }

    pub async fn delete(
        group_id: &str,
        workspace: &str,
        org_id: &str,
    ) -> Result<ExperimentGroup, String> {
        let host = use_host_server();
        let url = format!("{}/experiment-groups/{}", host, group_id);

        let response = request(
            url,
            reqwest::Method::DELETE,
            None::<()>,
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }

    pub async fn add_members(
        group_id: &str,
        payload: ExpGroupMemberRequest,
        workspace: &str,
        org_id: &str,
    ) -> Result<ExperimentGroup, String> {
        let host = use_host_server();
        let url = format!("{}/experiment-groups/{}/add-members", host, group_id);

        let response = request(
            url,
            reqwest::Method::PATCH,
            Some(payload),
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }

    pub async fn remove_members(
        group_id: &str,
        payload: &ExpGroupMemberRequest,
        workspace: &str,
        org_id: &str,
    ) -> Result<ExperimentGroup, String> {
        let host = use_host_server();
        let url = format!("{}/experiment-groups/{}/remove-members", host, group_id);

        let response = request(
            url,
            reqwest::Method::PATCH,
            Some(payload),
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }
}

pub mod audit_log {
    use superposition_types::{
        api::audit_log::AuditQueryFilters, database::models::cac::EventLog,
    };

    use super::*;

    pub async fn fetch(
        filters: &AuditQueryFilters,
        pagination: &PaginationParams,
        workspace: &str,
        org_id: &str,
    ) -> Result<PaginatedResponse<EventLog>, String> {
        let host = use_host_server();
        let url = format!(
            "{}/audit?{}&{}",
            host,
            filters.to_query_param(),
            pagination.to_query_param(),
        );

        let response = request(
            url,
            reqwest::Method::GET,
            None::<()>,
            construct_request_headers(&[
                ("x-workspace", workspace),
                ("x-org-id", org_id),
            ])?,
        )
        .await?;

        parse_json_response(response).await
    }
}

pub mod master_encryption_key {
    use super::*;
    use superposition_types::api::secrets::MasterEncryptionKeyRotationResponse;

    pub async fn rotate() -> Result<MasterEncryptionKeyRotationResponse, String> {
        let host = use_host_server();
        let url = format!("{host}/master-encryption-key/rotate");

        let response =
            request(url, reqwest::Method::POST, None::<()>, HeaderMap::new()).await?;

        parse_json_response(response).await
    }
}
