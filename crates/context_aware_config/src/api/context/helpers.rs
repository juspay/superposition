use std::collections::{HashMap, HashSet};
use std::str;

use cac_client::utils::json_to_sorted_string;
use chrono::Utc;
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper};
use serde_json::{Map, Value};
#[cfg(feature = "jsonlogic")]
use service_utils::helpers::{extract_dimensions, get_variable_name_and_value};
use service_utils::service::types::SchemaName;
#[cfg(feature = "jsonlogic")]
use superposition_macros::bad_argument;
use superposition_macros::{unexpected_error, validation_error};
use superposition_types::{
    api::{
        context::PutRequest,
        functions::{FunctionExecutionRequest, FunctionExecutionResponse},
    },
    database::{
        models::{
            cac::{Context, DependencyGraph, FunctionCode, FunctionType},
            Description,
        },
        schema::{contexts, default_configs::dsl, dimensions},
    },
    result as superposition, Cac, Condition, DBConnection, Overrides, User,
};

use crate::helpers::DimensionData;
use crate::validation_functions::execute_fn;
use crate::{
    api::dimension::{get_dimension_data, get_dimension_data_map},
    helpers::calculate_context_weight,
};
use crate::{
    api::functions::helpers::get_published_functions_by_names, helpers::get_workspace,
};

use super::types::FunctionsInfo;
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

pub fn validate_condition_with_dependent_dimensions(
    conn: &mut DBConnection,
    context_map: &Map<String, Value>,
    schema_name: &SchemaName,
) -> superposition::Result<()> {
    use dimensions::dsl;
    let keys = context_map.keys();
    let dependency_lists: Vec<DependencyGraph> = dsl::dimensions
        .filter(dsl::dimension.eq_any(keys))
        .select(dsl::dependency_graph)
        .schema_name(schema_name)
        .load::<DependencyGraph>(conn)?;
    let dependency_list: HashSet<String> = dependency_lists
        .iter()
        .flat_map(|obj| obj.keys())
        .cloned()
        .collect();

    let all_dependencies_present = dependency_list
        .iter()
        .all(|dimension| context_map.contains_key(dimension));
    if !all_dependencies_present {
        return Err(validation_error!(
            "The context should contain all the dependent dimensions : {:?}.",
            dependency_list,
        ));
    }
    Ok(())
}

pub fn validate_condition_with_functions(
    conn: &mut DBConnection,
    context_map: &Map<String, Value>,
    schema_name: &SchemaName,
) -> superposition::Result<()> {
    use dimensions::dsl;
    let dimensions_list: Vec<String> = context_map.keys().cloned().collect();
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
    for (key, value) in context_map.iter() {
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

#[cfg(feature = "jsonlogic")]
pub fn validate_condition_with_strict_mode(
    context: &Condition,
    strict_mode: bool,
) -> superposition::Result<()> {
    if !strict_mode {
        return Ok(());
    }

    let conditions: Vec<Value> = match (*context).get("and") {
        Some(conditions_json) => conditions_json
            .as_array()
            .ok_or(bad_argument!("Error extracting dimensions, failed parsing conditions as an array. Ensure the context provided obeys the rules of JSON logic"))?
            .clone(),
        None => vec![Value::Object(context.to_owned().into())],
    };

    for condition in &conditions {
        let condition_obj =
            condition
                .as_object()
                .ok_or(bad_argument!(
                    "Failed to parse condition as an object. Ensure the context provided obeys the rules of JSON logic"
                ))?;
        let operators = condition_obj.keys();

        for operator in operators {
            let operands = condition_obj[operator].as_array().ok_or(bad_argument!(
                    "Failed to parse operands as an arrays. Ensure the context provided obeys the rules of JSON logic"
            ))?;

            let (dimension_name, _) = get_variable_name_and_value(operands)?;
            // variantIds use 'HAS', and since they are managed internally allow them to bypass strict mode
            if dimension_name == "variantIds" && operator != "==" && operator != "in" {
                return Err(bad_argument!(
                    "Strict mode is enabled, and variantIds is a system dimension that supports IS and IN. Found operator: {}",
                    operator
                ));
            }
            if dimension_name != "variantIds" && operator != "==" {
                return Err(bad_argument!(
                    "Strict mode is enabled, but the context contains unsupported operators. Please use IS operator only. Found operator: {}",
                    operator
                ));
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

    // primitives here either imply dimensions or default configs based on who is calling it
    let function_to_primitives_map: HashMap<String, FunctionsInfo> = keys_function_array
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
    Ok(function_to_primitives_map)
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
            FunctionType::Validation => {
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
            FunctionType::Autocomplete => Ok(()),
        },
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
    schema_name: &SchemaName,
) -> superposition::Result<Context> {
    let ctx_condition = req.context.to_owned().into_inner();
    let condition_val = Value::Object(ctx_condition.clone().into());
    let r_override = req.r#override.clone().into_inner();
    let ctx_override = Value::Object(r_override.clone().into());

    let dimension_data_map = validate_ctx(conn, schema_name, ctx_condition.clone())?;
    let change_reason = req.change_reason.clone();

    validate_override_with_default_configs(conn, &r_override, schema_name)?;
    validate_override_with_functions(conn, &r_override, schema_name)?;

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
    schema_name: &SchemaName,
    condition: Condition,
) -> superposition::Result<HashMap<String, DimensionData>> {
    let workspace_settings = get_workspace(schema_name, conn)?;

    cfg_if::cfg_if! {
        if #[cfg(feature = "jsonlogic")] {
            validate_condition_with_strict_mode(&condition, workspace_settings.strict_mode)?;

            let context_map = &extract_dimensions(&condition)?;
            let condition_val = Value::Object(condition.clone().into());
        } else {
            let context_map = &condition;
            let condition_val = condition.clone();
        }
    }

    validate_condition_with_mandatory_dimensions(
        context_map,
        &workspace_settings.mandatory_dimensions.unwrap_or_default(),
    )?;
    validate_condition_with_dependent_dimensions(conn, context_map, schema_name)?;
    validate_condition_with_functions(conn, context_map, schema_name)?;
    let dimension_data = get_dimension_data(conn, schema_name)?;
    let dimension_data_map = get_dimension_data_map(&dimension_data)?;
    validate_dimensions(
        #[cfg(feature = "jsonlogic")]
        "context",
        &condition_val,
        &dimension_data_map,
    )?;
    Ok(dimension_data_map)
}
