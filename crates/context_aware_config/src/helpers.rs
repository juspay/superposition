use crate::{
    api::config::types::{Config, Context},
    db::{
        models::ConfigVersion,
        schema::{
            config_versions,
            contexts::dsl::{self as ctxt},
            default_configs::dsl as def_conf,
        },
    },
};

#[cfg(feature = "high-performance-mode")]
use crate::db::schema::event_log::dsl as event_log;

use actix_web::http::header::{HeaderMap, HeaderName, HeaderValue};
use actix_web::web::Data;
#[cfg(feature = "high-performance-mode")]
use chrono::DateTime;
use chrono::Utc;
use diesel::{
    r2d2::{ConnectionManager, PooledConnection},
    ExpressionMethods, PgConnection, QueryDsl, RunQueryDsl,
};
#[cfg(feature = "high-performance-mode")]
use fred::interfaces::KeysInterface;
use itertools::{self, Itertools};
use jsonschema::{Draft, JSONSchema, ValidationError};
use serde_json::{json, Map, Value};
#[cfg(feature = "high-performance-mode")]
use service_utils::service::types::Tenant;
use service_utils::{
    helpers::{generate_snowflake_id, validation_err_to_str},
    service::types::AppState,
};

use superposition_macros::{db_error, unexpected_error, validation_error};
use superposition_types::{result as superposition, Cac, Condition, Overrides};
#[cfg(feature = "high-performance-mode")]
use uuid::Uuid;

use std::collections::HashMap;

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
    object_key: &str,
    dimension_value: &Value,
    dimension_schema: &JSONSchema,
) -> superposition::Result<()> {
    match dimension_value {
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
    let res = match validation_schema.validate(schema) {
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
    };
    res
}

pub fn json_to_sorted_string(v: &Value) -> String {
    match v {
        Value::Object(m) => {
            let mut new_str: String = String::from("");
            for (i, val) in m.iter().sorted_by_key(|item| item.0) {
                let p: String = json_to_sorted_string(val);
                new_str.push_str(i);
                new_str.push_str(&String::from(":"));
                new_str.push_str(&p);
                new_str.push_str(&String::from("$"));
            }
            new_str
        }
        Value::String(m) => m.to_string(),
        Value::Number(m) => m.to_string(),
        Value::Bool(m) => m.to_string(),
        Value::Null => String::from("null"),
        Value::Array(m) => {
            let mut new_vec =
                m.iter().map(json_to_sorted_string).collect::<Vec<String>>();
            new_vec.sort();
            new_vec.join(",")
        }
    }
}

pub fn calculate_context_priority(
    _object_key: &str,
    cond: &Value,
    dimension_schema_map: &HashMap<String, (JSONSchema, i32)>,
) -> Result<i32, String> {
    let get_priority = |key: &str, val: &Value| -> Result<i32, String> {
        if key == "var" {
            let dimension_name =
                val.as_str().ok_or("failed to decode dimension as str")?;
            dimension_schema_map
                .get(dimension_name)
                .map(|(_, priority)| priority)
                .ok_or(String::from(
                    "No matching `dimension` found in dimension table",
                ))
                .copied()
        } else {
            calculate_context_priority(key, val, dimension_schema_map)
        }
    };

    match cond {
        Value::Object(x) => x.iter().try_fold(0, |acc, (key, val)| {
            get_priority(key, val).map(|res| res + acc)
        }),
        Value::Array(arr) => arr.iter().try_fold(0, |acc, item| {
            calculate_context_priority(_object_key, item, dimension_schema_map)
                .map(|res| res + acc)
        }),
        _ => Ok(0),
    }
}

pub fn generate_cac(
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<Config> {
    let contexts_vec = ctxt::contexts
        .select((
            ctxt::id,
            ctxt::value,
            ctxt::priority,
            ctxt::override_id,
            ctxt::override_,
        ))
        .order_by((ctxt::priority.asc(), ctxt::created_at.asc()))
        .load::<(String, Value, i32, String, Value)>(conn)
        .map_err(|err| {
            log::error!("failed to fetch contexts with error: {}", err);
            db_error!(err)
        })?;

    let mut contexts = Vec::new();
    let mut overrides: HashMap<String, Overrides> = HashMap::new();

    for (id, condition, priority_, override_id, override_) in contexts_vec.iter() {
        let condition = Cac::<Condition>::try_from_db(
            condition.as_object().unwrap_or(&Map::new()).clone(),
        )
        .map_err(|err| {
            log::error!("generate_cac : failed to decode context from db {}", err);
            unexpected_error!(err)
        })?
        .into_inner();

        let override_ = Cac::<Overrides>::try_from_db(
            override_.as_object().unwrap_or(&Map::new()).clone(),
        )
        .map_err(|err| {
            log::error!("generate_cac : failed to decode overrides from db {}", err);
            unexpected_error!(err)
        })?
        .into_inner();
        let ctxt = Context {
            id: id.to_owned(),
            condition,
            priority: priority_.to_owned(),
            override_with_keys: [override_id.to_owned()],
        };
        contexts.push(ctxt);
        overrides.insert(override_id.to_owned(), override_);
    }

    let default_config_vec = def_conf::default_configs
        .select((def_conf::key, def_conf::value))
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
    db_conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<i64> {
    use config_versions::dsl::config_versions;
    let version_id = generate_snowflake_id(state)?;
    let config = generate_cac(db_conn)?;
    let json_config = json!(config);
    let config_hash = blake3::hash(json_config.to_string().as_bytes()).to_string();
    let config_version = ConfigVersion {
        id: version_id,
        config: json_config,
        config_hash,
        tags,
        created_at: Utc::now().naive_utc(),
    };
    diesel::insert_into(config_versions)
        .values(&config_version)
        .execute(db_conn)?;
    Ok(version_id)
}

#[cfg(feature = "high-performance-mode")]
pub async fn put_config_in_redis(
    version_id: i64,
    state: Data<AppState>,
    tenant: Tenant,
    db_conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<()> {
    let config = generate_cac(db_conn)?;
    let redis_config = serde_json::to_string(&json!(config)).map_err(|e| {
        log::error!("failed to convert cac config to string: {}", e);
        unexpected_error!("could not convert cac config to string")
    })?;
    let config_key = format!("{}::cac_config", *tenant);
    let max_created_key = format!("{}::cac_config::max_created_at", *tenant);
    let audit_id_key = format!("{}::cac_config::audit_id", *tenant);
    let config_version_key = format!("{}::cac_config::config_version", *tenant);
    let last_modified = DateTime::to_rfc2822(&Utc::now());
    let _ = state
        .redis
        .set::<(), String, String>(config_key, redis_config, None, None, false)
        .await;
    let _ = state
        .redis
        .set::<(), String, String>(max_created_key, last_modified, None, None, false)
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
    fn test_json_to_sorted_string() {
        let first_condition: Value = json!({
            "and": [
                {
                    "==": [
                        {
                            "var": "os"
                        },
                        "android"
                    ]
                },
                {
                    "==": [
                        {
                            "var": "clientId"
                        },
                        "geddit"
                    ]
                }
            ]
        });

        let second_condition: Value = json!({
            "and": [
                {
                    "==": [
                        {
                            "var": "clientId"
                        },
                        "geddit"
                    ]
                },
                {
                    "==": [
                        {
                            "var": "os"
                        },
                        "android"
                    ]
                }
            ]
        });
        let expected_string: String =
            "and:==:android,var:os$$,==:geddit,var:clientId$$$".to_owned();
        assert_eq!(json_to_sorted_string(&first_condition), expected_string);
        assert_eq!(
            json_to_sorted_string(&first_condition),
            json_to_sorted_string(&second_condition)
        );
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
        let arr_dimension_val = json!(["string1".to_owned(), "string2".to_owned()]);
        let ok_str_context =
            validate_context_jsonschema("in", &str_dimension_val, &test_jsonschema);
        let ok_arr_context =
            validate_context_jsonschema("in", &arr_dimension_val, &test_jsonschema);
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
        assert!(err_arr_context);
        assert!(ok_arr_context.is_ok());
    }
}
