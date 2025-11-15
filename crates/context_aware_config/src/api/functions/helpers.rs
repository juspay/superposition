use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use service_utils::service::types::SchemaName;
use superposition_types::{
    database::{
        models::cac::{Function, FunctionCode},
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
    f_name: &str,
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

pub fn generate_vars_template(variables: &[(String, String)]) -> String {
    let vars = variables
        .iter()
        .map(|(k, v)| format!("\t{k}:{v}"))
        .collect::<Vec<_>>()
        .join(",\n");

    format!("const VARS = {{\n {} \n}};", vars)
}

pub fn inject_variables_into_code(
    code: &str,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<FunctionCode> {
    let vars: Vec<(String, String)> = variables::variables
        .select((variables::name, variables::value))
        .schema_name(schema_name)
        .load(conn)?;

    let vars_template = generate_vars_template(&vars);
    let processed_code = format!("{}\n\n{}", vars_template, code);

    Ok(FunctionCode(processed_code))
}
