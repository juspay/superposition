use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use service_utils::{
    encryption::decrypt_with_fallback,
    service::types::SchemaName,
};
use superposition_types::{
    database::{
        models::{
            cac::{Function, FunctionCode},
            others::Secret,
            Workspace,
        },
        schema::{self, functions::dsl::functions, secrets::dsl as secrets_dsl, variables::dsl as variables},
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

pub fn generate_secrets_template(secrets: &[(String, String)]) -> String {
    if secrets.is_empty() {
        return String::new();
    }

    let json_entries: Vec<String> = secrets
        .iter()
        .map(|(k, v)| format!("\"{}\":\"{}\"", k, v))
        .collect();

    format!("const SECRETS = {{\n{}\n}};", json_entries.join(",\n"))
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

pub fn inject_secrets_and_variables_into_code(
    code: &str,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    workspace_id: &str,
    org_id: &str,
) -> superposition::Result<FunctionCode> {
    let vars: Vec<(String, String)> = variables::variables
        .select((variables::name, variables::value))
        .schema_name(schema_name)
        .load(conn)?;

    let all_secrets: Vec<Secret> = secrets_dsl::secrets
        .schema_name(schema_name)
        .load(conn)?;

    let decrypted_secrets: superposition::Result<Vec<(String, String)>> = if all_secrets.is_empty() {
        Ok(vec![])
    } else {
        use superposition_types::database::superposition_schema::superposition::workspaces::dsl as ws;
        
        let workspace: Workspace = ws::workspaces
            .filter(ws::workspace_name.eq(workspace_id))
            .filter(ws::organisation_id.eq(org_id))
            .first(conn)?;
        
        let encryption_key = workspace
            .encryption_key
            .ok_or_else(|| superposition::AppError::BadArgument("Workspace encryption key not found".to_string()))?;
        
        all_secrets
            .into_iter()
            .map(|secret| {
                let decrypted_value = decrypt_with_fallback(
                    &secret.encrypted_value,
                    &encryption_key,
                    workspace.previous_encryption_key.as_deref(),
                )
                .map_err(|e| {
                    superposition::AppError::BadArgument(format!(
                        "Failed to decrypt secret '{}': {}",
                        secret.name.0, e
                    ))
                })?;
                Ok((secret.name.0, decrypted_value))
            })
            .collect()
    };

    let decrypted_secrets = decrypted_secrets?;

    let vars_template = generate_vars_template(&vars);
    let secrets_template = generate_secrets_template(&decrypted_secrets);

    let mut injected_code = String::new();
    if !vars_template.is_empty() {
        injected_code.push_str(&vars_template);
        injected_code.push_str("\n\n");
    }
    if !secrets_template.is_empty() {
        injected_code.push_str(&secrets_template);
        injected_code.push_str("\n\n");
    }
    injected_code.push_str(code);

    Ok(FunctionCode(injected_code))
}
