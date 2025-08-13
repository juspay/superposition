use std::collections::HashMap;
#[cfg(not(feature = "jsonlogic"))]
use std::collections::HashSet;

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
use jsonschema::{Draft, JSONSchema, ValidationError};
use num_bigint::BigUint;
use serde_json::{json, Map, Value};
use service_utils::{
    helpers::{generate_snowflake_id, validation_err_to_str},
    service::types::{AppState, SchemaName},
};
use superposition_macros::{bad_argument, db_error, unexpected_error, validation_error};
#[cfg(feature = "high-performance-mode")]
use superposition_types::database::schema::event_log::dsl as event_log;
use superposition_types::{
    database::{
        models::{cac::ConfigVersion, Description, Workspace},
        schema::{
            config_versions,
            contexts::dsl::{self as ctxt},
            default_configs::dsl as def_conf,
        },
        superposition_schema::superposition::workspaces,
    },
    result as superposition, Cac, Condition, Config, Context, DBConnection,
    OverrideWithKeys, Overrides,
};

#[cfg(feature = "high-performance-mode")]
use uuid::Uuid;

#[derive(Debug)]
pub struct DimensionData {
    pub schema: JSONSchema,
    pub position: i32,
}

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

pub fn validate_context_jsonschema(
    #[cfg(feature = "jsonlogic")] object_key: &str,
    dimension_value: &Value,
    dimension_schema: &JSONSchema,
) -> superposition::Result<()> {
    match dimension_value {
        #[cfg(feature = "jsonlogic")]
        Value::Array(val_arr) if object_key == "in" => {
            let mut verrors = Vec::new();
            val_arr.iter().for_each(|x| {
                dimension_schema
                    .validate(x)
                    .map_err(|e| {
                        verrors.append(&mut e.collect::<Vec<ValidationError>>());
                    })
                    .ok();
            });
            if verrors.is_empty() {
                Ok(())
            } else {
                // Check if the array as a whole validates, even with individual errors
                match dimension_schema.validate(dimension_value) {
                    Ok(()) => {
                        log::error!(
                            "Validation errors for individual dimensions, but array as a whole validates: {:?}",
                            verrors
                        );
                        Ok(())
                    }
                    Err(e) => {
                        verrors.append(&mut e.collect::<Vec<ValidationError>>());
                        log::error!(
                            "Validation errors for dimensions in array: {:?}",
                            verrors
                        );
                        Err(validation_error!(
                            "failed to validate dimension value {}: {}",
                            dimension_value.to_string(),
                            validation_err_to_str(verrors)
                                .first()
                                .unwrap_or(&String::new())
                        ))
                    }
                }
            }
        }
        _ => dimension_schema.validate(dimension_value).map_err(|e| {
            let verrors = e.collect::<Vec<ValidationError>>();
            log::error!(
                "failed to validate dimension value {}: {:?}",
                dimension_value.to_string(),
                verrors
            );
            validation_error!(
                "failed to validate dimension value {}: {}",
                dimension_value.to_string(),
                validation_err_to_str(verrors)
                    .first()
                    .unwrap_or(&String::new())
            )
        }),
    }
}

/*
  This step is required because an empty object
  is also a valid JSON schema. So added required
  validations for the input.
*/
// TODO: Recursive validation.

pub fn validate_jsonschema(
    validation_schema: &JSONSchema,
    schema: &Value,
) -> superposition::Result<()> {
    JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(schema)
        .map_err(|e| bad_argument!("Invalid JSON schema (failed to compile): {:?}", e))?;
    match validation_schema.validate(schema) {
        Ok(_) => Ok(()),
        Err(e) => {
            //TODO: Try & render as json.
            let verrors = e.collect::<Vec<ValidationError>>();
            Err(validation_error!(
                "schema validation failed: {}",
                validation_err_to_str(verrors)
                    .first()
                    .unwrap_or(&String::new())
            ))
        }
    }
}

#[cfg(not(feature = "jsonlogic"))]
pub fn allow_primitive_types(schema: &Value) -> superposition::Result<()> {
    match schema.as_object().and_then(|o| o.get("type")).cloned().unwrap_or_default() {
        Value::String(type_val) if type_val != "array" && type_val != "object" => {
            Ok(())
        }
        Value::Array(arr) if arr.iter().all(|v| v.as_str().is_some_and(|s| s != "array" && s != "object")) => {
            Ok(())
        }
        _ => {
            Err(validation_error!(
                "Invalid schema: expected a primitive type or an array of primitive types, found: {}",
                schema
            ))
        }
    }
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
    dimension_position_map: &HashMap<String, DimensionData>,
) -> Result<BigDecimal, String> {
    cfg_if::cfg_if! {
        if #[cfg(feature = "jsonlogic")] {
            let ast = jsonlogic::expression::Expression::from_json(cond).map_err(|msg| {
                log::error!("Condition validation error: {}", msg);
                msg
            })?;
            let dimensions =  ast.get_variable_names().map_err(|msg| {
                log::error!("Error while parsing variable names : {}", msg);
                msg
            })?;
        } else {
            let dimensions: HashSet<String> = cond
                .as_object()
                .map(|o| o.keys().cloned().collect())
                .unwrap_or_default();
        }
    }

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

    Ok(Config {
        contexts,
        overrides,
        default_configs,
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

pub fn get_workspace(
    workspace_schema_name: &String,
    db_conn: &mut DBConnection,
) -> superposition::Result<Workspace> {
    let workspace = workspaces::dsl::workspaces
        .filter(workspaces::workspace_schema_name.eq(workspace_schema_name))
        .get_result::<Workspace>(db_conn)?;
    Ok(workspace)
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

// ************ Tests *************

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;
    #[test]
    fn test_get_meta_schema() {
        let x = get_meta_schema();

        let ok_string_schema = json!({"type": "string", "pattern": ".*"});
        let ok_string_validation = x.validate(&ok_string_schema);
        assert!(ok_string_validation.is_ok());

        let error_object_schema = json!({"type": "object"});
        let error_object_validation = x.validate(&error_object_schema).map_err(|e| {
            let verrors = e.collect::<Vec<ValidationError>>();
            format!(
                "Error While validating object dataType, Bad schema: {:?}",
                verrors.as_slice()
            )
        });
        assert!(error_object_validation.is_err_and(|error| error.contains("Bad schema")));

        let ok_enum_schema = json!({"type": "string", "enum": ["ENUMVAL"]});
        let ok_enum_validation = x.validate(&ok_enum_schema);
        assert!(ok_enum_validation.is_ok());
    }

    #[test]
    fn test_validate_context_jsonschema() {
        let test_schema = json!({
            "type": "string",
            "pattern": ".*"
        });
        let test_jsonschema = JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(&test_schema)
        .expect("Error encountered: Failed to compile 'context_dimension_schema_value'. Ensure it adheres to the correct format and data type.");

        let str_dimension_val = json!("string1".to_owned());
        #[cfg(feature = "jsonlogic")]
        let arr_dimension_val = json!(["string1".to_owned(), "string2".to_owned()]);
        let ok_str_context = validate_context_jsonschema(
            #[cfg(feature = "jsonlogic")]
            "in",
            &str_dimension_val,
            &test_jsonschema,
        );
        #[cfg(feature = "jsonlogic")]
        let ok_arr_context =
            validate_context_jsonschema("in", &arr_dimension_val, &test_jsonschema);
        #[cfg(feature = "jsonlogic")]
        let err_arr_context =
            match validate_context_jsonschema("==", &arr_dimension_val, &test_jsonschema)
            {
                Err(superposition::AppError::ValidationError(err)) => {
                    log::info!("{:?}", err);
                    true
                }
                _ => false,
            };

        assert!(ok_str_context.is_ok());
        #[cfg(feature = "jsonlogic")]
        assert!(err_arr_context);
        #[cfg(feature = "jsonlogic")]
        assert!(ok_arr_context.is_ok());
    }
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
