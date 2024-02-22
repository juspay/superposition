extern crate base64;
use actix_web::error::ErrorInternalServerError;
use base64::prelude::*;
use diesel::{
    r2d2::{ConnectionManager, PooledConnection},
    result::Error,
    ExpressionMethods, PgConnection, QueryDsl, RunQueryDsl,
};
use serde_json::json;
use std::str;

use crate::db::{self, models::Function, schema::functions::dsl::functions};

pub fn fetch_function(
    f_name: &String,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> actix_web::Result<Function, Error> {
    Ok(functions
        .filter(db::schema::functions::function_name.eq(f_name))
        .get_result::<Function>(conn)?)
}

pub fn decode_function(function: &mut Function) -> actix_web::Result<()> {
    function.draft_code = decode_base64_to_string(&function.draft_code)?;
    if let Some(code) = &function.published_code {
        function.published_code = Some(decode_base64_to_string(&code)?);
    }
    Ok(())
}

pub fn decode_base64_to_string(code: &String) -> actix_web::Result<String> {
    BASE64_STANDARD
        .decode(code)
        .map_err(|e| {
            log::info!("Error while decoding function: {}", e);
            ErrorInternalServerError(json!({"message": "Failed to decode function"}))
        })
        .and_then(|decoded_code| {
            str::from_utf8(&decoded_code)
                .map(|res| res.to_string())
                .map_err(|e| {
                    log::info!("Error while decoding function: {}", e);
                    ErrorInternalServerError(
                        json!({"message": "Failed to decode function"}),
                    )
                })
        })
}
