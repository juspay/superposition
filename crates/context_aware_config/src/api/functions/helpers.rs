use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper};
use service_utils::service::types::SchemaName;
use superposition_macros::validation_error;
use superposition_types::{
    DBConnection,
    database::{
        models::cac::{Function, FunctionCode, FunctionType},
        schema::{self, functions::dsl::functions, variables::dsl as variables},
    },
    result as superposition,
};

use super::types::FunctionInfo;

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
    f_type: FunctionType,
    schema_name: &SchemaName,
) -> superposition::Result<FunctionInfo> {
    let function = functions
        .filter(schema::functions::function_name.eq(f_name))
        .select(FunctionInfo::as_select())
        .schema_name(schema_name)
        .first(conn)?;

    if function.function_type != f_type {
        return Err(validation_error!(
            "Function type mismatch for function: {}",
            f_name
        ));
    }

    Ok(function)
}

pub fn get_published_functions_by_names(
    conn: &mut DBConnection,
    function_names: Vec<String>,
    f_type: FunctionType,
    schema_name: &SchemaName,
) -> superposition::Result<Vec<FunctionInfo>> {
    let functions_data = functions
        .filter(schema::functions::function_name.eq_any(function_names))
        .select(FunctionInfo::as_select())
        .schema_name(schema_name)
        .load::<FunctionInfo>(conn)?;

    functions_data.iter().try_for_each(|f| {
        (f.function_type == f_type).then_some(()).ok_or_else(|| {
            validation_error!("Function type mismatch for function: {}", f.function_name)
        })
    })?;

    Ok(functions_data)
}

pub fn check_fn_published(
    fn_name: &str,
    fn_type: FunctionType,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<()> {
    let FunctionInfo { published_code, .. } =
        get_published_function_code(conn, fn_name, fn_type, schema_name)?;
    if published_code.is_some() {
        Ok(())
    } else {
        Err(validation_error!(
            "Function {}'s published code does not exist.",
            fn_name
        ))
    }
}

pub fn generate_vars_template(vars: &[(String, String)]) -> String {
    if vars.is_empty() {
        return String::new();
    }

    let json_entries: Vec<String> = vars
        .iter()
        .map(|(k, v)| format!("\"{}\":\"{}\"", k, v))
        .collect();

    format!("const VARS = {{\n{}\n}};", json_entries.join(",\n"))
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

pub fn get_first_function_by_type(
    function_type: FunctionType,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<FunctionInfo> {
    let function = functions
        .filter(schema::functions::function_type.eq(function_type))
        .select(FunctionInfo::as_select())
        .schema_name(schema_name)
        .first(conn)?;
    Ok(function)
}
