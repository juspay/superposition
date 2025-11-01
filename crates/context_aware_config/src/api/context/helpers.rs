use std::collections::HashMap;
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
        functions::{
            FunctionEnvironment, FunctionExecutionRequest, FunctionExecutionResponse,
        },
    },
    database::{
        models::{
            cac::{Context, DependencyGraph, DimensionType, FunctionCode},
            Description,
        },
        schema::{
            contexts, default_configs::dsl, dimensions, functions::dsl as functions,
        },
    },
    logic::{dimensions_to_start_from, evaluate_local_cohorts},
    result as superposition, Cac, Condition, DBConnection, DimensionInfo, Overrides,
    User,
};

use crate::{
    api::dimension::fetch_dimensions_info_map,
    helpers::{calculate_context_weight, get_workspace},
};
use crate::{
    api::functions::helpers::get_published_functions_by_names,
    validation_functions::execute_fn,
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
        .filter_map(|(key, f_name)| f_name.map(|func| (key, func)))
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
    fun_name: &str,
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
            let stdout = stdout.unwrap_or_default();
            log::error!(
                "function {fun_name} validation failed for {key} with error: {err}"
            );
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

fn compute_value_with_function(
    fun_name: &str,
    function: &FunctionCode,
    key: &str,
    context: Map<String, Value>,
    overrides: Map<String, Value>,
) -> superposition::Result<Value> {
    match execute_fn(
        function,
        &FunctionExecutionRequest::AutocompleteFunctionRequest {
            name: key.to_string(),
            prefix: String::new(),
            environment: FunctionEnvironment { context, overrides },
        },
    ) {
        Err((err, stdout)) => {
            let stdout = stdout.unwrap_or_default();
            log::error!(
                "function {fun_name} computation failed for {key} with error: {err}"
            );
            Err(validation_error!(
                "Function {fun_name} computation failed for {} with error {}. {}",
                key,
                err,
                stdout
            ))
        }
        Ok(FunctionExecutionResponse {
            fn_output, stdout, ..
        }) => {
            log::debug!("Function execution returned: {:?}", fn_output);
            match fn_output {
                Value::Array(arr) if arr.len() == 1 => Ok(arr[0].clone()),
                _ => {
                    log::error!(
                        "Computation function {fun_name} returned invalid output, logs are {stdout}"
                    );
                    Err(validation_error!(
                        "Computation function {fun_name} returned invalid output, please check your inputs",
                    ))
                }
            }
        }
    }
}

/// Evaluates dependencies of local cohort dimensions recursively using depth-first traversal
fn evaluate_remote_cohorts_dependency(
    dimension: &str,
    dependency_graph: &DependencyGraph,
    dimensions: &HashMap<String, DimensionInfo>,
    modified_context: &mut Map<String, Value>,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<()> {
    let mut stack = dependency_graph
        .get(dimension)
        .cloned()
        .unwrap_or_default()
        .into_iter()
        .map(|d| (d, dimension.to_string()))
        .collect::<Vec<_>>();

    // Depth-first traversal of dependencies
    while let Some((ref cohort_dimension, ref based_on)) = stack.pop() {
        if let Some(data) = dimensions.get(cohort_dimension) {
            if matches!(data.dimension_type, DimensionType::LocalCohort(_)) {
                continue;
            }

            let Some(ref autocomplete_fn) = data.autocomplete_function_name else {
                return Err(validation_error!(
                    "Value compute function not found for {cohort_dimension}",
                ));
            };

            let fn_code = functions::functions
                .filter(functions::function_name.eq(autocomplete_fn))
                .select(functions::published_code)
                .schema_name(schema_name)
                .first::<Option<FunctionCode>>(conn)?
                .ok_or_else(|| {
                    validation_error!(
                        "Published code not found for function {}",
                        autocomplete_fn
                    )
                })?;

            let value = compute_value_with_function(
                autocomplete_fn,
                &fn_code,
                based_on,
                modified_context.clone(),
                Map::new(),
            )?;

            modified_context.insert(cohort_dimension.clone(), value);

            stack.extend(
                dependency_graph
                    .get(cohort_dimension)
                    .cloned()
                    .unwrap_or_default()
                    .into_iter()
                    .map(|d| (d, cohort_dimension.clone()))
                    .collect::<Vec<_>>(),
            );
        }
    }
    Ok(())
}

/// Evaluates all remote cohort dimensions based on the provided query data and dimension definitions
/// First, all remote cohort dependents of regular and remote dimensions are evaluated, starting from
/// the dimensions present in query_data such that for each tree in the dependency graph,
/// the node closest to root from query_data is picked for each branch of the tree.
/// Next, local cohort dimensions from query_data are inserted into the modified context.
///
/// Values of regular and local cohort dimensions in query_data are not modified.
/// Returned value, might have a different value for remote cohort dimensions based on its based on dimensions,
/// if the value provided for the remote cohort was incorrect in the query data.
pub fn evaluate_remote_cohorts(
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: &Map<String, Value>,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<Map<String, Value>> {
    let mut modified_context = Map::new();

    // First, evaluate all remote cohort dimensions and their dependencies
    for dimension_key in dimensions_to_start_from(dimensions, query_data) {
        if let Some(value) = query_data.get(&dimension_key) {
            if let Some(data) = dimensions.get(&dimension_key) {
                match data.dimension_type {
                    DimensionType::LocalCohort(_) => continue,
                    DimensionType::Regular {} | DimensionType::RemoteCohort(_) => {
                        modified_context.insert(dimension_key.to_string(), value.clone());
                        evaluate_remote_cohorts_dependency(
                            &dimension_key,
                            &data.dependency_graph,
                            dimensions,
                            &mut modified_context,
                            conn,
                            schema_name,
                        )?;
                    }
                }
            }
        }
    }

    // Next, insert local cohort dimensions from query_data into modified_context
    for (dimension_key, value) in query_data {
        if let Some(data) = dimensions.get(dimension_key) {
            if matches!(data.dimension_type, DimensionType::LocalCohort(_)) {
                modified_context.insert(dimension_key.to_string(), value.clone());
            }
        }
    }

    Ok(modified_context)
}

fn validate_cohort_dimension_values(
    dimensions: &HashMap<String, DimensionInfo>,
    context_map: &Map<String, Value>,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<()> {
    let remote_evaluated_context =
        evaluate_remote_cohorts(dimensions, context_map, conn, schema_name)?;
    let evaluated_context = evaluate_local_cohorts(dimensions, &remote_evaluated_context);

    for (dimension_key, value) in context_map {
        if let Some(evaluated_value) = evaluated_context.get(dimension_key) {
            if evaluated_value != value {
                return Err(validation_error!(
                    "Context value mismatch for cohort dimension '{}': expected {}, found {}",
                    dimension_key,
                    evaluated_value,
                    value
                ));
            }
        }
    }

    Ok(())
}

pub fn validate_ctx(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    condition: Condition,
) -> superposition::Result<HashMap<String, DimensionInfo>> {
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
    let dimension_info_map = fetch_dimensions_info_map(conn, schema_name)?;
    validate_dimensions(
        #[cfg(feature = "jsonlogic")]
        "context",
        &condition_val,
        &dimension_info_map,
    )?;
    validate_cohort_dimension_values(
        &dimension_info_map,
        context_map,
        conn,
        schema_name,
    )?;
    validate_condition_with_functions(conn, context_map, schema_name)?;
    Ok(dimension_info_map)
}
