use std::collections::HashMap;
use std::str;

use cac_client::utils::json_to_sorted_string;
use chrono::Utc;
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper};
use serde_json::{Map, Value};
use service_utils::service::types::{SchemaName, WorkspaceContext};
use superposition_macros::{unexpected_error, validation_error};
use superposition_types::{
    api::{
        context::PutRequest,
        functions::{
            FunctionEnvironment, FunctionExecutionRequest, FunctionExecutionResponse,
            KeyType, CONTEXT_VALIDATION_FN_NAME,
        },
    },
    database::{
        models::{
            cac::{Context, FunctionCode, FunctionRuntimeVersion, FunctionType},
            Description,
        },
        schema::{contexts, default_configs::dsl, dimensions},
    },
    logic::dimensions_to_start_from,
    result as superposition, Cac, Condition, DBConnection, DimensionInfo, Overrides,
    User,
};

use crate::api::functions::helpers::get_first_function_by_type;
use crate::{
    api::dimension::fetch_dimensions_info_map, helpers::calculate_context_weight,
};
use crate::{
    api::functions::{helpers::get_published_functions_by_names, types::FunctionInfo},
    validation_functions::execute_fn,
};

use super::validations::{validate_dimensions, validate_override_with_default_configs};

pub fn hash(val: &Value) -> String {
    let sorted_str: String = json_to_sorted_string(val);
    blake3::hash(sorted_str.as_bytes()).to_string()
}

pub fn validate_condition_with_mandatory_dimensions(
    context_map: &Map<String, Value>,
    mandatory_dimensions: &Vec<String>,
) -> superposition::Result<()> {
    let dimensions_list: Vec<String> = context_map.keys().cloned().collect();
    let all_mandatory_present = mandatory_dimensions
        .iter()
        .all(|dimension| dimensions_list.contains(dimension));
    if !all_mandatory_present {
        return Err(validation_error!(
            "The context should contain all the mandatory dimensions : {:?}.",
            mandatory_dimensions,
        ));
    }
    Ok(())
}

/// Given a set of dimensions and a context map, validate that dependent dimensions,
///  of the given dimensions in context, are not present
fn validate_condition_with_dependent_dimensions(
    dimensions: &HashMap<String, DimensionInfo>,
    context_map: &Map<String, Value>,
) -> superposition::Result<()> {
    let required_dimensions = dimensions_to_start_from(dimensions, context_map);

    let invalid_dimensions = context_map
        .keys()
        .filter(|dimension_key| !required_dimensions.contains(dimension_key))
        .cloned()
        .collect::<Vec<_>>();

    let mut error_messages = Vec::new();
    for dim_key in required_dimensions {
        if let Some(dependents) = dimensions
            .get(&dim_key)
            .map(|d| d.dependency_graph.keys().cloned().collect::<Vec<_>>())
        {
            for invalid_dimension in &invalid_dimensions {
                if dependents.contains(invalid_dimension) {
                    error_messages.push(format!(
                        "{} can be derived from {} dimension",
                        invalid_dimension, dim_key
                    ));
                }
            }
        }
    }

    if !error_messages.is_empty() {
        return Err(validation_error!(
            "Cohort Dimension(s): [ {} ] using the cohort definitions. Hence, usage of this/these dimension(s) is not allowed.",
            error_messages.join(", ")
        ));
    }

    Ok(())
}

pub fn validate_condition_with_functions(
    conn: &mut DBConnection,
    context_map: &Map<String, Value>,
    override_: &Map<String, Value>,
    is_context_validation_enabled: bool,
    schema_name: &SchemaName,
) -> superposition::Result<()> {
    use dimensions::dsl;
    let dimensions_list: Vec<String> = context_map.keys().cloned().collect();
    let keys_function_array: Vec<(String, Option<String>)> = dsl::dimensions
        .filter(dsl::dimension.eq_any(dimensions_list))
        .select((dsl::dimension, dsl::value_validation_function_name))
        .schema_name(schema_name)
        .load(conn)?;
    let new_keys_function_array: Vec<(String, String)> = keys_function_array
        .into_iter()
        .filter_map(|(key, f_name)| f_name.map(|func| (key, func)))
        .collect();

    let context_validation_function =
        get_first_function_by_type(FunctionType::ContextValidation, conn, schema_name)?;

    let environment = FunctionEnvironment {
        context: context_map.clone(),
        overrides: override_.clone(),
    };

    // workspace_setting check
    if is_context_validation_enabled {
        if let (Some(function_code), Some(published_runtime_version)) = (
            context_validation_function.published_code,
            context_validation_function.published_runtime_version,
        ) {
            validation_function_executor(
                CONTEXT_VALIDATION_FN_NAME,
                &function_code,
                &FunctionExecutionRequest::ContextValidationFunctionRequest {
                    environment: environment.clone(),
                },
                published_runtime_version,
                conn,
                schema_name,
            )?;
        }
    }

    let dimension_functions_map =
        get_functions_map(conn, new_keys_function_array, schema_name)?;
    for (key, value) in context_map.iter() {
        if let Some(functions_map) = dimension_functions_map.get(key) {
            if let (function_name, Some(function_code), Some(published_runtime_version)) = (
                functions_map.function_name.clone(),
                functions_map.published_code.clone(),
                functions_map.published_runtime_version,
            ) {
                validation_function_executor(
                    &function_name,
                    &function_code,
                    &FunctionExecutionRequest::ValueValidationFunctionRequest {
                        key: key.clone(),
                        value: value.to_owned(),
                        r#type: KeyType::Dimension,
                        environment: environment.clone(),
                    },
                    published_runtime_version,
                    conn,
                    schema_name,
                )?;
            }
        }
    }
    Ok(())
}

pub fn validate_override_with_functions(
    conn: &mut DBConnection,
    override_: &Map<String, Value>,
    context: &Map<String, Value>,
    schema_name: &SchemaName,
) -> superposition::Result<()> {
    let default_config_keys: Vec<String> = override_.keys().cloned().collect();
    let keys_function_array: Vec<(String, Option<String>)> = dsl::default_configs
        .filter(dsl::key.eq_any(default_config_keys))
        .select((dsl::key, dsl::value_validation_function_name))
        .schema_name(schema_name)
        .load(conn)?;
    let new_keys_function_array: Vec<(String, String)> = keys_function_array
        .into_iter()
        .filter_map(|(key_, f_name)| f_name.map(|func| (key_, func)))
        .collect();

    let default_config_functions_map =
        get_functions_map(conn, new_keys_function_array, schema_name)?;
    for (key, value) in override_.iter() {
        if let Some(functions_map) = default_config_functions_map.get(key) {
            if let (function_name, Some(function_code), Some(published_runtime_version)) = (
                functions_map.function_name.clone(),
                functions_map.published_code.clone(),
                functions_map.published_runtime_version,
            ) {
                validation_function_executor(
                    &function_name,
                    &function_code,
                    &FunctionExecutionRequest::ValueValidationFunctionRequest {
                        key: key.clone(),
                        value: value.to_owned(),
                        r#type: KeyType::ConfigKey,
                        environment: FunctionEnvironment {
                            context: context.clone(),
                            overrides: override_.clone(),
                        },
                    },
                    published_runtime_version,
                    conn,
                    schema_name,
                )?;
            }
        }
    }
    Ok(())
}

fn get_functions_map(
    conn: &mut DBConnection,
    keys_function_array: Vec<(String, String)>,
    schema_name: &SchemaName,
) -> superposition::Result<HashMap<String, FunctionInfo>> {
    let functions_map: HashMap<String, FunctionInfo> = get_published_functions_by_names(
        conn,
        keys_function_array
            .iter()
            .map(|(_, f_name)| f_name.clone())
            .collect(),
        schema_name,
    )?
    .into_iter()
    .map(|functions_info| (functions_info.function_name.clone(), functions_info))
    .collect();

    // primitives here either imply dimensions or default configs based on who is calling it
    let function_to_primitives_map: HashMap<String, FunctionInfo> = keys_function_array
        .into_iter()
        .filter_map(|(key, function_name)| {
            functions_map
                .get(&function_name)
                .cloned()
                .map(|func_info| (key, func_info))
        })
        .collect();

    Ok(function_to_primitives_map)
}

pub fn validation_function_executor(
    fun_name: &str,
    function: &FunctionCode,
    args: &FunctionExecutionRequest,
    runtime_version: FunctionRuntimeVersion,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<()> {
    match execute_fn(function, args, runtime_version, conn, schema_name) {
        Err((err, stdout)) => {
            let stdout = stdout.unwrap_or_default();
            let key = args.function_identifier();
            log::error!("function validation failed for {key} with error: {err}");
            Err(validation_error!(
                "Function {fun_name} validation failed for {} with error {}. {}",
                key,
                err,
                stdout
            ))
        }
        Ok(FunctionExecutionResponse {
            fn_output, stdout, ..
        }) => {
            log::debug!("Function execution returned: {:?}", fn_output);
            if fn_output.as_bool().unwrap_or_default() {
                Ok(())
            } else {
                log::error!(
                    "Validation function {fun_name} returned false, logs are {stdout}"
                );
                Err(validation_error!(
                    "Validation function {fun_name} returned false, please check your inputs",
                ))
            }
        }
    }
}

pub fn query_description(
    context: Value,
    transaction_conn: &mut diesel::PgConnection,
    schema_name: &SchemaName,
) -> Result<Description, superposition::AppError> {
    use superposition_types::database::schema::contexts::dsl::{
        contexts as contexts_table, id as context_id,
    };

    let context_id_value = hash(&context);

    let existing_context = contexts_table
        .filter(context_id.eq(context_id_value))
        .schema_name(schema_name)
        .first::<Context>(transaction_conn)?;

    Ok(existing_context.description)
}

pub fn create_ctx_from_put_req(
    req: PutRequest,
    req_description: Description,
    conn: &mut DBConnection,
    user: &User,
    workspace_request: &WorkspaceContext,
) -> superposition::Result<Context> {
    let ctx_condition = req.context.to_owned().into_inner();
    let condition_val = Value::Object(ctx_condition.clone().into());
    let r_override = req.r#override.clone().into_inner();
    let ctx_override = Value::Object(r_override.clone().into());

    let dimension_data_map = validate_ctx(
        conn,
        workspace_request,
        ctx_condition.clone(),
        r_override.clone(),
    )?;
    let change_reason = req.change_reason.clone();

    validate_override_with_default_configs(
        conn,
        &r_override,
        &workspace_request.schema_name,
    )?;
    validate_override_with_functions(
        conn,
        &r_override,
        &ctx_condition.clone(),
        &workspace_request.schema_name,
    )?;

    let weight = calculate_context_weight(&condition_val, &dimension_data_map)
        .map_err(|_| unexpected_error!("Something Went Wrong"))?;

    let context_id = hash(&condition_val);
    let override_id = hash(&ctx_override);
    Ok(Context {
        id: context_id,
        value: ctx_condition,
        override_id,
        override_: r_override,
        created_at: Utc::now(),
        created_by: user.get_email(),
        last_modified_at: Utc::now(),
        last_modified_by: user.get_email(),
        weight,
        description: req_description,
        change_reason,
    })
}

fn db_update_override(
    conn: &mut DBConnection,
    ctx: Context,
    user: &User,
    schema_name: &SchemaName,
) -> superposition::Result<Context> {
    use contexts::dsl;
    let update_resp = diesel::update(dsl::contexts)
        .filter(dsl::id.eq(ctx.id.clone()))
        .set((
            dsl::override_.eq(ctx.override_),
            dsl::override_id.eq(ctx.override_id),
            dsl::last_modified_at.eq(Utc::now()),
            dsl::last_modified_by.eq(user.get_email()),
            dsl::description.eq(ctx.description),
            dsl::change_reason.eq(ctx.change_reason),
        ))
        .returning(Context::as_returning())
        .schema_name(schema_name)
        .get_result::<Context>(conn)?;
    Ok(update_resp)
}

pub fn replace_override_of_existing_ctx(
    conn: &mut DBConnection,
    ctx: Context,
    user: &User,
    schema_name: &SchemaName,
) -> superposition::Result<Context> {
    let new_override = ctx.override_;
    let new_override_id = hash(&Value::Object(new_override.clone().into()));
    let new_ctx = Context {
        override_: new_override,
        override_id: new_override_id,
        ..ctx
    };
    db_update_override(conn, new_ctx, user, schema_name)
}

pub fn update_override_of_existing_ctx(
    conn: &mut DBConnection,
    ctx: Context,
    user: &User,
    schema_name: &SchemaName,
) -> superposition::Result<Context> {
    use contexts::dsl;
    let mut new_override: Value = dsl::contexts
        .filter(dsl::id.eq(ctx.id.clone()))
        .select(dsl::override_)
        .schema_name(schema_name)
        .first(conn)?;
    cac_client::merge(
        &mut new_override,
        &Value::Object(ctx.override_.clone().into()),
    );
    let new_override_id = hash(&new_override);
    let new_ctx = Context {
        override_: Cac::<Overrides>::validate_db_data(
            new_override.as_object().cloned().unwrap_or(Map::new()),
        )
        .map_err(|err| {
            log::error!(
                "update_override_of_existing_ctx : failed to decode context from db {}",
                err
            );
            unexpected_error!(err)
        })?
        .into_inner(),
        override_id: new_override_id,
        ..ctx
    };
    db_update_override(conn, new_ctx, user, schema_name)
}

pub fn validate_ctx(
    conn: &mut DBConnection,
    workspace_request: &WorkspaceContext,
    condition: Condition,
    override_: Overrides,
) -> superposition::Result<HashMap<String, DimensionInfo>> {
    validate_condition_with_mandatory_dimensions(
        &condition,
        workspace_request
            .settings
            .mandatory_dimensions
            .as_ref()
            .unwrap_or(&vec![]),
    )?;

    let dimension_info_map =
        fetch_dimensions_info_map(conn, &workspace_request.schema_name)?;
    validate_condition_with_dependent_dimensions(&dimension_info_map, &condition)?;
    validate_dimensions(&condition, &dimension_info_map)?;
    validate_condition_with_functions(
        conn,
        &condition,
        &override_,
        workspace_request.settings.enable_context_validation,
        &workspace_request.schema_name,
    )?;
    Ok(dimension_info_map)
}
