use std::collections::{HashMap, HashSet};

use actix_web::{
    http::header::{HeaderMap, HeaderName, HeaderValue},
    web::Data,
};
use bigdecimal::{BigDecimal, Num};
#[cfg(feature = "high-performance-mode")]
use chrono::DateTime;
use chrono::Utc;
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper};
#[cfg(feature = "high-performance-mode")]
use fred::interfaces::KeysInterface;
use jsonschema::{Draft, JSONSchema};
use num_bigint::BigUint;
use serde_json::{Map, Value, json};
use service_utils::{
    helpers::{fetch_dimensions_info_map, generate_snowflake_id},
    service::types::{AppState, EncryptionKey, SchemaName, WorkspaceContext},
};
use superposition_macros::{db_error, unexpected_error, validation_error};
#[cfg(feature = "high-performance-mode")]
use superposition_types::database::schema::event_log::dsl as event_log;
use superposition_types::{
    Cac, Condition, Config, Context, DBConnection, DimensionInfo, OverrideWithKeys,
    Overrides,
    api::functions::{
        CHANGE_REASON_VALIDATION_FN_NAME, FunctionEnvironment, FunctionExecutionRequest,
        FunctionExecutionResponse, KeyType,
    },
    database::{
        models::{
            ChangeReason, Description,
            cac::{
                ConfigVersion, DependencyGraph, DimensionType, FunctionCode,
                FunctionRuntimeVersion, FunctionType,
            },
        },
        schema::{
            config_versions,
            contexts::dsl::{self as ctxt},
            default_configs::dsl as def_conf,
        },
    },
    logic::dimensions_to_start_from,
    result as superposition,
};

#[cfg(feature = "high-performance-mode")]
use uuid::Uuid;

use crate::{
    api::{
        context::helpers::validation_function_executor,
        functions::{
            helpers::{get_first_function_by_type, get_published_function_code},
            types::FunctionInfo,
        },
    },
    validation_functions::execute_fn,
};

pub fn parse_headermap_safe(headermap: &HeaderMap) -> HashMap<String, String> {
    let mut req_headers = HashMap::new();
    let record_header = |(header_name, header_val): (&HeaderName, &HeaderValue)| {
        let header_val = match header_val.to_str() {
            Ok(s) => String::from(s),
            Err(e) => {
                log::error!(
                    "unable to parse value of header {}, error: {e}",
                    header_name
                );
                String::from("Error: non ASCII header value")
            }
        };
        req_headers.insert(header_name.to_string(), header_val);
    };
    headermap.iter().for_each(record_header);
    req_headers
}

pub fn get_meta_schema() -> JSONSchema {
    let my_schema = json!({
        "type": "object",
        "properties": {
            "type": {
                "enum": ["boolean", "number", "integer", "string", "array", "null"]
            },
        },
        "required": ["type"],
    });

    JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(&my_schema)
        .expect("Error encountered: Failed to compile 'context_dimension_schema_value'. Ensure it adheres to the correct format and data type.")
}

fn calculate_weight_from_index(index: u32) -> Result<BigDecimal, String> {
    let base = BigUint::from(2u32);
    let result = base.pow(index);
    let biguint_str = &result.to_str_radix(10);
    BigDecimal::from_str_radix(biguint_str, 10).map_err(|err| {
        log::error!("failed to parse bigdecimal with error: {}", err.to_string());
        String::from("failed to parse bigdecimal with error")
    })
}

pub fn calculate_context_weight(
    cond: &Value,
    dimension_position_map: &HashMap<String, DimensionInfo>,
) -> Result<BigDecimal, String> {
    let dimensions: HashSet<String> = cond
        .as_object()
        .map(|o| o.keys().cloned().collect())
        .unwrap_or_default();

    let mut weight = BigDecimal::from(0);
    for dimension in dimensions {
        let position = dimension_position_map
            .get(dimension.clone().as_str())
            .map(|x| x.position)
            .ok_or_else(|| {
                let msg =
                    format!("Dimension:{} not found in Dimension schema map", dimension);
                log::error!("{}", msg);
                msg
            })?;
        weight += calculate_weight_from_index(position as u32)?;
    }
    Ok(weight)
}
pub fn generate_cac(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<Config> {
    let contexts_vec: Vec<(String, Condition, String, Overrides)> = ctxt::contexts
        .select((ctxt::id, ctxt::value, ctxt::override_id, ctxt::override_))
        .order_by((ctxt::weight.asc(), ctxt::created_at.asc()))
        .schema_name(schema_name)
        .load::<(String, Condition, String, Overrides)>(conn)
        .map_err(|err| {
            log::error!("failed to fetch contexts with error: {}", err);
            db_error!(err)
        })?;
    let contexts_vec: Vec<(String, Condition, i32, String, Overrides)> = contexts_vec
        .iter()
        .enumerate()
        .map(|(index, (id, value, override_id, override_))| {
            (
                id.clone(),
                value.clone(),
                index as i32,
                override_id.clone(),
                override_.clone(),
            )
        })
        .collect();

    let mut contexts = Vec::new();
    let mut overrides: HashMap<String, Overrides> = HashMap::new();

    for (id, condition, weight, override_id, override_) in contexts_vec.iter() {
        let condition = Cac::<Condition>::validate_db_data(condition.clone().into())
            .map_err(|err| {
                log::error!("generate_cac : failed to decode context from db {}", err);
                unexpected_error!(err)
            })?
            .into_inner();

        let override_ = Cac::<Overrides>::validate_db_data(override_.clone().into())
            .map_err(|err| {
                log::error!("generate_cac : failed to decode overrides from db {}", err);
                unexpected_error!(err)
            })?
            .into_inner();
        let ctxt = Context {
            id: id.to_owned(),
            condition,
            priority: weight.to_owned(),
            weight: weight.to_owned(),
            override_with_keys: OverrideWithKeys::new(override_id.to_owned()),
        };
        contexts.push(ctxt);
        overrides.insert(override_id.to_owned(), override_);
    }

    let default_config_vec = def_conf::default_configs
        .select((def_conf::key, def_conf::value))
        .schema_name(schema_name)
        .load::<(String, Value)>(conn)
        .map_err(|err| {
            log::error!("failed to fetch default_configs with error: {}", err);
            db_error!(err)
        })?;

    let default_configs =
        default_config_vec
            .into_iter()
            .fold(Map::new(), |mut acc, item| {
                acc.insert(item.0, item.1);
                acc
            });

    let dimensions = fetch_dimensions_info_map(conn, schema_name)?;

    Ok(Config {
        contexts,
        overrides,
        default_configs,
        dimensions,
    })
}

pub fn add_config_version(
    state: &Data<AppState>,
    tags: Option<Vec<String>>,
    description: Description,
    db_conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<i64> {
    use config_versions::dsl::config_versions;
    let version_id = generate_snowflake_id(state)?;
    let config = generate_cac(db_conn, schema_name)?;
    let json_config = json!(config);
    let config_hash = blake3::hash(json_config.to_string().as_bytes()).to_string();
    let config_version = ConfigVersion {
        id: version_id,
        config: json_config,
        config_hash,
        tags,
        created_at: Utc::now(),
        description,
    };
    diesel::insert_into(config_versions)
        .values(&config_version)
        .returning(ConfigVersion::as_returning())
        .schema_name(schema_name)
        .execute(db_conn)?;
    Ok(version_id)
}

#[cfg(feature = "high-performance-mode")]
pub async fn put_config_in_redis(
    version_id: i64,
    state: Data<AppState>,
    schema_name: &SchemaName,
    db_conn: &mut DBConnection,
) -> superposition::Result<()> {
    let raw_config = generate_cac(db_conn, schema_name)?;
    let parsed_config = serde_json::to_string(&json!(raw_config)).map_err(|e| {
        log::error!("failed to convert cac config to string: {}", e);
        unexpected_error!("could not convert cac config to string")
    })?;
    let config_key = format!("{}::cac_config", **schema_name);
    let last_modified_at_key = format!("{}::cac_config::last_modified_at", **schema_name);
    let audit_id_key = format!("{}::cac_config::audit_id", **schema_name);
    let config_version_key = format!("{}::cac_config::config_version", **schema_name);
    let last_modified = DateTime::to_rfc2822(&Utc::now());
    let _ = state
        .redis
        .set::<(), String, String>(config_key, parsed_config, None, None, false)
        .await;
    let _ = state
        .redis
        .set::<(), String, String>(last_modified_at_key, last_modified, None, None, false)
        .await;
    if let Ok(uuid) = event_log::event_log
        .select(event_log::id)
        .filter(event_log::table_name.eq("contexts"))
        .order_by(event_log::timestamp.desc())
        .first::<Uuid>(db_conn)
    {
        let _ = state
            .redis
            .set::<(), String, String>(audit_id_key, uuid.to_string(), None, None, false)
            .await;
    }
    let _ = state
        .redis
        .set::<(), String, i64>(config_version_key, version_id, None, None, false)
        .await;
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compute_value_with_function(
    workspace_context: &WorkspaceContext,
    fun_name: &str,
    function: &FunctionCode,
    key: &str,
    context: Map<String, Value>,
    overrides: Map<String, Value>,
    runtime_version: FunctionRuntimeVersion,
    conn: &mut DBConnection,
    master_encryption_key: &Option<EncryptionKey>,
) -> superposition::Result<Value> {
    match execute_fn(
        workspace_context,
        function,
        &FunctionExecutionRequest::ValueComputeFunctionRequest {
            name: key.to_string(),
            prefix: String::new(),
            r#type: KeyType::Dimension,
            environment: FunctionEnvironment { context, overrides },
        },
        runtime_version,
        conn,
        master_encryption_key,
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
    workspace_context: &WorkspaceContext,
    master_encryption_key: &Option<EncryptionKey>,
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

            let Some(ref value_compute_function_name_) = data.value_compute_function_name
            else {
                return Err(validation_error!(
                    "Value compute function not found for {cohort_dimension}",
                ));
            };

            let FunctionInfo {
                published_code,
                published_runtime_version,
                ..
            } = get_published_function_code(
                conn,
                value_compute_function_name_,
                FunctionType::ValueCompute,
                &workspace_context.schema_name,
            )?;

            let fn_code = published_code.ok_or_else(|| {
                validation_error!(
                    "Published code not found for function {}",
                    value_compute_function_name_
                )
            })?;

            let published_runtime_version =
                published_runtime_version.ok_or_else(|| {
                    validation_error!(
                        "Published runtime version not found for function {}",
                        value_compute_function_name_
                    )
                })?;

            let value = compute_value_with_function(
                workspace_context,
                value_compute_function_name_,
                &fn_code,
                based_on,
                modified_context.clone(),
                Map::new(),
                published_runtime_version,
                conn,
                master_encryption_key,
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
    workspace_context: &WorkspaceContext,
    master_encryption_key: &Option<EncryptionKey>,
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
                            workspace_context,
                            master_encryption_key,
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

pub fn validate_change_reason(
    workspace_context: &WorkspaceContext,
    change_reason: &ChangeReason,
    conn: &mut DBConnection,
    master_encryption_key: &Option<EncryptionKey>,
) -> superposition::Result<()> {
    if !workspace_context.settings.enable_change_reason_validation {
        return Ok(());
    }

    let change_reason_validation_function = get_first_function_by_type(
        FunctionType::ChangeReasonValidation,
        conn,
        &workspace_context.schema_name,
    )?;
    if let (Some(function_code), Some(published_runtime_version)) = (
        change_reason_validation_function.published_code,
        change_reason_validation_function.published_runtime_version,
    ) {
        validation_function_executor(
            workspace_context,
            CHANGE_REASON_VALIDATION_FN_NAME,
            &function_code,
            &FunctionExecutionRequest::ChangeReasonValidationFunctionRequest {
                change_reason: change_reason.clone(),
            },
            published_runtime_version,
            conn,
            master_encryption_key,
        )?;
    }
    Ok(())
}

// ************ Tests *************

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;

    #[test]
    fn test_calculate_weight_from_index() {
        let number_2_100_str = "1267650600228229401496703205376";
        // test 2^100
        let big_decimal =
            BigDecimal::from_str(number_2_100_str).expect("Invalid string format");

        let number_2_200_str =
            "1606938044258990275541962092341162602522202993782792835301376";
        // test 2^100
        let big_decimal_200 =
            BigDecimal::from_str(number_2_200_str).expect("Invalid string format");

        assert_eq!(Some(big_decimal), calculate_weight_from_index(100).ok());
        assert_eq!(Some(big_decimal_200), calculate_weight_from_index(200).ok());
    }
}
