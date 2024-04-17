extern crate base64;
use base64::prelude::*;
use diesel::{
    r2d2::{ConnectionManager, PooledConnection},
    ExpressionMethods, PgConnection, QueryDsl, RunQueryDsl,
};
use service_utils::{result as superposition, unexpected_error};
use std::str;

use crate::db::{self, models::Function, schema::functions::dsl::functions};

pub fn fetch_function(
    f_name: &String,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<Function> {
    Ok(functions
        .filter(db::schema::functions::function_name.eq(f_name))
        .get_result::<Function>(conn)?)
}

pub fn decode_function(function: &mut Function) -> superposition::Result<()> {
    function.draft_code = decode_base64_to_string(&function.draft_code)?;
    if let Some(code) = &function.published_code {
        function.published_code = Some(decode_base64_to_string(&code)?);
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
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    f_name: String,
) -> superposition::Result<Option<String>> {
    let function = functions
        .filter(db::schema::functions::function_name.eq(f_name))
        .select(db::schema::functions::published_code)
        .first(conn)?;
    Ok(function)
}

pub fn get_published_functions_by_names(
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    function_names: Vec<String>,
) -> superposition::Result<Vec<(String, Option<String>)>> {
    let function: Vec<(String, Option<String>)> = functions
        .filter(db::schema::functions::function_name.eq_any(function_names))
        .select((
            db::schema::functions::function_name,
            db::schema::functions::published_code,
        ))
        .load(conn)?;
    Ok(function)
}
