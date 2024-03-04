extern crate base64;
use base64::prelude::*;
use service_utils::helpers::extract_dimensions;
use std::str;

use crate::api::functions::helpers::get_published_functions_by_names;
use crate::validation_functions::execute_fn;
use crate::{
    api::context::types::FunctionsInfo,
    db::schema::{
        default_configs::dsl,
        dimensions::{self},
    },
};
use anyhow::anyhow;
use diesel::{
    r2d2::{ConnectionManager, PooledConnection},
    ExpressionMethods, PgConnection, QueryDsl, RunQueryDsl,
};
use serde_json::{json, Map, Value};
use std::collections::HashMap;
type DBConnection = PooledConnection<ConnectionManager<PgConnection>>;

pub fn validate_condition_with_functions(
    conn: &mut DBConnection,
    context: &Value,
) -> anyhow::Result<()> {
    use dimensions::dsl;
    let context = extract_dimensions(&context)?;
    let dimensions_list: Vec<String> = context.keys().cloned().collect();
    let keys_function_array: Vec<(String, Option<String>)> = dsl::dimensions
        .filter(dsl::dimension.eq_any(dimensions_list))
        .select((dsl::dimension, dsl::function_name))
        .load(conn)?;
    let new_keys_function_array: Vec<(String, String)> = keys_function_array
        .into_iter()
        .filter_map(|(key_, f_name)| f_name.map(|func| (key_, func)))
        .collect();

    let dimension_functions_map = get_functions_map(conn, new_keys_function_array)?;
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
) -> anyhow::Result<()> {
    let default_config_keys: Vec<String> = override_.keys().cloned().collect();
    let keys_function_array: Vec<(String, Option<String>)> = dsl::default_configs
        .filter(dsl::key.eq_any(default_config_keys))
        .select((dsl::key, dsl::function_name))
        .load(conn)?;
    let new_keys_function_array: Vec<(String, String)> = keys_function_array
        .into_iter()
        .filter_map(|(key_, f_name)| f_name.map(|func| (key_, func)))
        .collect();

    let default_config_functions_map = get_functions_map(conn, new_keys_function_array)?;
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
) -> anyhow::Result<HashMap<String, FunctionsInfo>> {
    let functions_map: HashMap<String, Option<String>> =
        get_published_functions_by_names(
            conn,
            keys_function_array
                .iter()
                .map(|(_, f_name)| f_name.clone())
                .collect(),
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

fn validate_value_with_function(
    fun_name: &str,
    function: &str,
    key: &String,
    value: &Value,
) -> anyhow::Result<()> {
    let base64_decoded = BASE64_STANDARD.decode(function.clone())?;
    let utf8_decoded = str::from_utf8(&base64_decoded)?;
    if let Err((e, stdout)) = execute_fn(&utf8_decoded, fun_name, value.to_owned()) {
        log::error!("function validation failed for {key} with error: {e}");
        return Err(anyhow!(json!({
            "message": format!("function validation failed for {}", key), "stdout": stdout
        })));
    }
    Ok(())
}
