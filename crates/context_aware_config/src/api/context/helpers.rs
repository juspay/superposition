use std::collections::HashMap;
use std::str;

use actix_web::web::Json;
use cac_client::utils::json_to_sorted_string;
use chrono::Utc;
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper};
use serde_json::{Map, Value};
use service_utils::{helpers::extract_dimensions, service::types::SchemaName};
use superposition_macros::{unexpected_error, validation_error};
use superposition_types::{
    api::functions::{FunctionExecutionRequest, FunctionExecutionResponse},
    database::{
        models::cac::{Context, FunctionCode, FunctionTypes},
        schema::{contexts, default_configs::dsl, dimensions},
    },
    result as superposition, Cac, Condition, DBConnection, Overrides, User,
};

use crate::validation_functions::execute_fn;
use crate::{
    api::functions::helpers::get_published_functions_by_names, helpers::get_workspace,
};
use crate::{
    api::{
        context::types::FunctionsInfo,
        dimension::{get_dimension_data, get_dimension_data_map},
    },
    helpers::calculate_context_weight,
};

use super::{
    types::PutResp,
    validations::{validate_dimensions, validate_override_with_default_configs},
    PutReq,
};

pub fn hash(val: &Value) -> String {
    let sorted_str: String = json_to_sorted_string(val);
    blake3::hash(sorted_str.as_bytes()).to_string()
}

pub fn validate_condition_with_mandatory_dimensions(
    context: &Condition,
    mandatory_dimensions: &Vec<String>,
) -> superposition::Result<()> {
    let context_map = extract_dimensions(context)?;
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

pub fn validate_condition_with_functions(
    conn: &mut DBConnection,
    context: &Condition,
    schema_name: &SchemaName,
) -> superposition::Result<()> {
    use dimensions::dsl;
    let context = extract_dimensions(context)?;
    let dimensions_list: Vec<String> = context.keys().cloned().collect();
    let keys_function_array: Vec<(String, Option<String>)> = dsl::dimensions
        .filter(dsl::dimension.eq_any(dimensions_list))
        .select((dsl::dimension, dsl::function_name))
        .schema_name(schema_name)
        .load(conn)?;
    let new_keys_function_array: Vec<(String, String)> = keys_function_array
        .into_iter()
        .filter_map(|(key_, f_name)| f_name.map(|func| (key_, func)))
        .collect();

    let dimension_functions_map =
        get_functions_map(conn, new_keys_function_array, schema_name)?;
    for (key, value) in context.iter() {
        if let Some(functions_map) = dimension_functions_map.get(key) {
            if let (function_name, Some(function_code)) =
                (functions_map.name.clone(), functions_map.code.clone())
            {
                validate_value_with_function(&function_name, &function_code, key, value)?;
            }
        }
    }
    Ok(())
}

pub fn validate_override_with_functions(
    conn: &mut DBConnection,
    override_: &Map<String, Value>,
    schema_name: &SchemaName,
) -> superposition::Result<()> {
    let default_config_keys: Vec<String> = override_.keys().cloned().collect();
    let keys_function_array: Vec<(String, Option<String>)> = dsl::default_configs
        .filter(dsl::key.eq_any(default_config_keys))
        .select((dsl::key, dsl::function_name))
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
            if let (function_name, Some(function_code)) =
                (functions_map.name.clone(), functions_map.code.clone())
            {
                validate_value_with_function(&function_name, &function_code, key, value)?;
            }
        }
    }
    Ok(())
}

fn get_functions_map(
    conn: &mut DBConnection,
    keys_function_array: Vec<(String, String)>,
    schema_name: &SchemaName,
) -> superposition::Result<HashMap<String, FunctionsInfo>> {
    let functions_map: HashMap<String, Option<FunctionCode>> =
        get_published_functions_by_names(
            conn,
            keys_function_array
                .iter()
                .map(|(_, f_name)| f_name.clone())
                .collect(),
            schema_name,
        )?
        .into_iter()
        .collect();

    let default_config_functions_map: HashMap<String, FunctionsInfo> =
        keys_function_array
            .into_iter()
            .map(|(key, function_name)| {
                (
                    key.clone(),
                    FunctionsInfo {
                        name: function_name.clone(),
                        code: functions_map.get(&function_name).cloned().flatten(),
                    },
                )
            })
            .collect();
    Ok(default_config_functions_map)
}

pub fn validate_value_with_function(
    _fun_name: &str,
    function: &FunctionCode,
    key: &String,
    value: &Value,
) -> superposition::Result<()> {
    match execute_fn(
        function,
        &FunctionExecutionRequest::ValidateFunctionRequest {
            key: key.clone(),
            value: value.to_owned(),
        },
    ) {
        Err((err, stdout)) => {
            let stdout = stdout.unwrap_or(String::new());
            log::error!("function validation failed for {key} with error: {err}");
            Err(validation_error!(
                "Function validation failed for {} with error {}. {}",
                key,
                err,
                stdout
            ))
        }
        Ok(FunctionExecutionResponse {
            fn_output,
            stdout,
            function_type,
        }) => match function_type {
            FunctionTypes::Validation => {
                log::debug!("Function execution returned: {:?}", fn_output);
                if fn_output.is_boolean() && fn_output == Value::Bool(true) {
                    Ok(())
                } else {
                    log::error!("Validation function returned false, logs are {stdout}");
                    Err(validation_error!(
                            "The validation function returned false, please check your inputs",
                        ))
                }
            }
            FunctionTypes::Autocomplete => Ok(()),
        },
    }
}

pub fn query_description(
    context: Value,
    transaction_conn: &mut diesel::PgConnection,
    schema_name: &SchemaName,
) -> Result<String, superposition::AppError> {
    use superposition_types::database::schema::contexts::dsl::{
        contexts as contexts_table, id as context_id,
    };

    let context_id_value = hash(&context);

    // Perform the database query
    let existing_context = contexts_table
        .filter(context_id.eq(context_id_value))
        .schema_name(schema_name)
        .first::<Context>(transaction_conn);

    match existing_context {
        Ok(ctx) => Ok(ctx.description), // If the context is found, return the description
        Err(diesel::result::Error::NotFound) => Err(superposition::AppError::NotFound(
            "Description not found in the existing context".to_string(),
        )),
        Err(e) => {
            log::error!("Database error while fetching context: {:?}", e);
            Err(superposition::AppError::DbError(e)) // Use the `DbError` variant for other Diesel-related errors
        }
    }
}

pub fn create_ctx_from_put_req(
    req: Json<PutReq>,
    req_description: String,
    conn: &mut DBConnection,
    user: &User,
    schema_name: &SchemaName,
) -> superposition::Result<Context> {
    let ctx_condition = req.context.to_owned().into_inner();
    let condition_val = Value::Object(ctx_condition.clone().into());
    let r_override = req.r#override.clone().into_inner();
    let ctx_override = Value::Object(r_override.clone().into());

    let workspace_settings = get_workspace(schema_name, conn)?;

    let change_reason = req.change_reason.clone();
    validate_condition_with_mandatory_dimensions(
        &ctx_condition,
        &workspace_settings.mandatory_dimensions.unwrap_or_default(),
    )?;
    validate_override_with_default_configs(conn, &r_override, schema_name)?;
    validate_condition_with_functions(conn, &ctx_condition, schema_name)?;
    validate_override_with_functions(conn, &r_override, schema_name)?;

    let dimension_data = get_dimension_data(conn, schema_name)?;
    let dimension_data_map = get_dimension_data_map(&dimension_data)?;
    validate_dimensions("context", &condition_val, &dimension_data_map)?;

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
        last_modified_at: Utc::now().naive_utc(),
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
) -> superposition::Result<PutResp> {
    use contexts::dsl;
    let update_resp = diesel::update(dsl::contexts)
        .filter(dsl::id.eq(ctx.id.clone()))
        .set((
            dsl::override_.eq(ctx.override_),
            dsl::override_id.eq(ctx.override_id),
            dsl::last_modified_at.eq(Utc::now().naive_utc()),
            dsl::last_modified_by.eq(user.get_email()),
            dsl::description.eq(ctx.description),
            dsl::change_reason.eq(ctx.change_reason),
        ))
        .returning(Context::as_returning())
        .schema_name(schema_name)
        .get_result::<Context>(conn)?;
    Ok(update_resp.into())
}

pub fn replace_override_of_existing_ctx(
    conn: &mut DBConnection,
    ctx: Context,
    user: &User,
    schema_name: &SchemaName,
) -> superposition::Result<PutResp> {
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
) -> superposition::Result<PutResp> {
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
