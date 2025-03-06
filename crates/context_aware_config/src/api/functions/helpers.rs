use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use service_utils::service::types::SchemaName;
use superposition_types::{
    database::{
        models::cac::{Function, FunctionCode},
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

pub fn get_published_function_code(
    conn: &mut DBConnection,
    f_name: String,
    schema_name: &SchemaName,
) -> superposition::Result<Option<FunctionCode>> {
    let function: Option<FunctionCode> = functions
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
) -> superposition::Result<Vec<(String, Option<FunctionCode>)>> {
    let function: Vec<(String, Option<FunctionCode>)> = functions
        .filter(schema::functions::function_name.eq_any(function_names))
        .select((
            schema::functions::function_name,
            schema::functions::published_code,
        ))
        .schema_name(schema_name)
        .load(conn)?;

    Ok(function)
}
