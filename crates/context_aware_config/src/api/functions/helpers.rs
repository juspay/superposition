use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use service_utils::service::types::{DbConnection, SchemaName};
use superposition_types::{
    database::{
        models::cac::{Function, FunctionCode, Variable as DbVariable},
        schema::{self, functions::dsl::functions, variables::dsl as variables},
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
    f_name: &String,
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

pub fn substitute_variables(
    code: &str,
    db_conn: &mut DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<FunctionCode> {
    let DbConnection(conn) = db_conn;
    let vars: Vec<DbVariable> =
        variables::variables.schema_name(&schema_name).load(conn)?;

    let mut processed_code = code.to_string();

    for var in vars {
        let placeholder = format!("{{{}}}", var.name);
        let value = format!("\"{}\"", var.value);
        processed_code = processed_code.replace(&placeholder, &value);
    }
    Ok(FunctionCode(processed_code))
}
