use std::ops::Deref;

use serde_json::Value;
use service_utils::service::types::{EncryptionKey, SchemaName, WorkspaceContext};
use superposition_core::validations::{try_into_jsonschema, validation_err_to_str};
use superposition_macros::{bad_argument, unexpected_error, validation_error};
use superposition_types::{
    api::{
        default_config::{
            DefaultConfigAction, DefaultConfigCreateRequest, DefaultConfigUpdateRequest,
        },
        functions::{FunctionEnvironment, FunctionExecutionRequest, KeyType},
    },
    database::models::{
        cac::{DefaultConfig, FunctionType},
        ChangeReason,
    },
    result as superposition, DBConnection, User,
};

use crate::{
    api::{
        context::helpers::validation_function_executor,
        functions::{
            helpers::{check_fn_published, get_published_function_code},
            types::FunctionInfo,
        },
    },
    helpers::validate_change_reason,
};

use super::handlers::{fetch_default_key, get_key_usage_context_ids};

pub(crate) fn validate_fn_published(
    function: &Option<String>,
    function_type: FunctionType,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<()> {
    let Some(function_name) = function else {
        return Ok(());
    };
    check_fn_published(function_name, function_type, conn, schema_name)
}

pub(crate) async fn validate_default_config_with_function(
    workspace_context: &WorkspaceContext,
    conn: &mut DBConnection,
    function_name: &Option<String>,
    key: &str,
    value: &Value,
    master_encryption_key: &Option<EncryptionKey>,
) -> superposition::Result<()> {
    let Some(function_name) = function_name else {
        return Ok(());
    };

    let FunctionInfo {
        published_code,
        published_runtime_version,
        ..
    } = get_published_function_code(
        conn,
        function_name,
        FunctionType::ValueValidation,
        &workspace_context.schema_name,
    )
    .map_err(|_| {
        bad_argument!(
            "Function {}'s published code does not exist.",
            function_name
        )
    })?;

    if let (Some(code), Some(version)) = (published_code, published_runtime_version) {
        validation_function_executor(
            workspace_context,
            function_name,
            &code,
            &FunctionExecutionRequest::ValueValidationFunctionRequest {
                key: key.to_string(),
                value: value.clone(),
                r#type: KeyType::ConfigKey,
                environment: FunctionEnvironment::default(),
            },
            version,
            conn,
            master_encryption_key,
        )
        .await?;
    }

    Ok(())
}

pub(super) async fn validate_create_request(
    request: &DefaultConfigCreateRequest,
    workspace_context: &WorkspaceContext,
    conn: &mut DBConnection,
    master_encryption_key: &Option<EncryptionKey>,
) -> superposition::Result<()> {
    if request.schema.is_empty() {
        return Err(bad_argument!("Schema cannot be empty."));
    }

    validate_change_reason(
        workspace_context,
        &request.change_reason,
        conn,
        master_encryption_key,
    )
    .await?;
    validate_value(&request.schema, &request.value)?;
    validate_default_config_with_function(
        workspace_context,
        conn,
        &request.value_validation_function_name,
        request.key.deref(),
        &request.value,
        master_encryption_key,
    )
    .await?;
    validate_fn_published(
        &request.value_compute_function_name,
        FunctionType::ValueCompute,
        conn,
        &workspace_context.schema_name,
    )
}

pub(super) async fn validate_update_request(
    key: &str,
    request: &DefaultConfigUpdateRequest,
    existing: &DefaultConfig,
    workspace_context: &WorkspaceContext,
    conn: &mut DBConnection,
    master_encryption_key: &Option<EncryptionKey>,
) -> superposition::Result<()> {
    validate_change_reason(
        workspace_context,
        &request.change_reason,
        conn,
        master_encryption_key,
    )
    .await?;

    let value = request.value.as_ref().unwrap_or(&existing.value);
    let schema = request.schema.as_ref().unwrap_or(&existing.schema);
    validate_value(schema, value)?;

    let validation_function = request
        .value_validation_function_name
        .as_ref()
        .unwrap_or(&existing.value_validation_function_name);
    validate_default_config_with_function(
        workspace_context,
        conn,
        validation_function,
        key,
        value,
        master_encryption_key,
    )
    .await?;

    let compute_function = request
        .value_compute_function_name
        .as_ref()
        .unwrap_or(&existing.value_compute_function_name);
    validate_fn_published(
        compute_function,
        FunctionType::ValueCompute,
        conn,
        &workspace_context.schema_name,
    )
}

pub(super) fn validate_delete_request(
    key: &str,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<()> {
    let context_ids = get_key_usage_context_ids(key, conn, schema_name)?;
    if context_ids.is_empty() {
        Ok(())
    } else {
        Err(bad_argument!(
            "Given key already in use in contexts: {}",
            context_ids.join(",")
        ))
    }
}

pub(super) async fn validate_bulk_operation(
    operation: &DefaultConfigAction,
    workspace_context: &WorkspaceContext,
    conn: &mut DBConnection,
    master_encryption_key: &Option<EncryptionKey>,
    user: &User,
) -> superposition::Result<ChangeReason> {
    match operation {
        DefaultConfigAction::Create(request) => {
            validate_create_request(
                request,
                workspace_context,
                conn,
                master_encryption_key,
            )
            .await?;
            Ok(request.change_reason.clone())
        }
        DefaultConfigAction::Update { key, request } => {
            let existing =
                fetch_default_key(key.deref(), conn, &workspace_context.schema_name)?;
            validate_update_request(
                key.deref(),
                request,
                &existing,
                workspace_context,
                conn,
                master_encryption_key,
            )
            .await?;
            Ok(request.change_reason.clone())
        }
        DefaultConfigAction::Delete(key) => {
            validate_delete_request(key.deref(), conn, &workspace_context.schema_name)?;
            ChangeReason::try_from(format!(
                "Default config deleted by {}",
                user.get_email()
            ))
            .map_err(|error| unexpected_error!(error))
        }
    }
}

fn validate_value(
    schema: &superposition_types::ExtendedMap,
    value: &Value,
) -> superposition::Result<()> {
    let compiled = try_into_jsonschema(&Value::from(schema))
        .map_err(|_| bad_argument!("Invalid JSON schema (failed to compile)"))?;

    compiled.validate(value).map_err(|errors| {
        validation_error!(
            "Schema validation failed: {}",
            validation_err_to_str(errors.collect())
                .first()
                .cloned()
                .unwrap_or_default()
        )
    })
}
