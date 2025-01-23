extern crate base64;

use base64::prelude::*;
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use service_utils::service::types::SchemaName;
use std::str;
use superposition_macros::unexpected_error;
use superposition_types::{
    database::{
        models::cac::Function,
        schema::{self, functions::dsl::functions},
    },
    result as superposition, DBConnection,
};

pub fn fetch_function(
    f_name: &String,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<Function> {
    Ok(functions
        .filter(schema::functions::function_name.eq(f_name))
        .schema_name(schema_name)
        .get_result::<Function>(conn)?)
}

pub fn decode_function(function: &mut Function) -> superposition::Result<()> {
    function.draft_code = decode_base64_to_string(&function.draft_code)?;
    if let Some(code) = &function.published_code {
        function.published_code = Some(decode_base64_to_string(code)?);
    }
    Ok(())
}

pub fn decode_base64_to_string(code: &String) -> superposition::Result<String> {
    BASE64_STANDARD
        .decode(code)
        .map_err(|e| {
            log::info!("Error while decoding function: {}", e);
            unexpected_error!("Failed to decode function")
        })
        .and_then(|decoded_code| {
            str::from_utf8(&decoded_code)
                .map(|res| res.to_string())
                .map_err(|e| {
                    log::info!("Error while decoding function: {}", e);
                    unexpected_error!("Something went wrong, failed to decode function")
                })
        })
}

pub fn get_published_function_code(
    conn: &mut DBConnection,
    f_name: String,
    schema_name: &SchemaName,
) -> superposition::Result<Option<String>> {
    let function = functions
        .filter(schema::functions::function_name.eq(f_name))
        .select(schema::functions::published_code)
        .schema_name(schema_name)
        .first(conn)?;
    Ok(function)
}

pub fn get_published_functions_by_names(
    conn: &mut DBConnection,
    function_names: Vec<String>,
    schema_name: &SchemaName,
) -> superposition::Result<Vec<(String, Option<String>)>> {
    let function: Vec<(String, Option<String>)> = functions
        .filter(schema::functions::function_name.eq_any(function_names))
        .select((
            schema::functions::function_name,
            schema::functions::published_code,
        ))
        .schema_name(schema_name)
        .load(conn)?;
    Ok(function)
}
