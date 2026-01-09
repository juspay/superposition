use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper};

use service_utils::{
    encryption::{decrypt_with_fallback, decrypt_workspace_key},
    service::types::SchemaName,
};

use superposition_types::{
    database::{
        models::{
            cac::{Function, FunctionCode, FunctionType},
            others::Secret,
            Workspace,
        },
        schema::{
            self, functions::dsl::functions, secrets::dsl as secrets_dsl,
            variables::dsl as variables,
        },
    },
    result as superposition, DBConnection,
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
    schema_name: &SchemaName,
) -> superposition::Result<FunctionInfo> {
    let function = functions
        .filter(schema::functions::function_name.eq(f_name))
        .select(FunctionInfo::as_select())
        .schema_name(schema_name)
        .first(conn)?;
    Ok(function)
}

pub fn get_published_functions_by_names(
    conn: &mut DBConnection,
    function_names: Vec<String>,
    schema_name: &SchemaName,
) -> superposition::Result<Vec<FunctionInfo>> {
    let functions_data = functions
        .filter(schema::functions::function_name.eq_any(function_names))
        .select(FunctionInfo::as_select())
        .schema_name(schema_name)
        .load::<FunctionInfo>(conn)?;

    Ok(functions_data)
}

pub fn generate_vars_template(vars: &[(String, String)]) -> String {
    if vars.is_empty() {
        return String::new();
    }

    let map: serde_json::Map<String, serde_json::Value> = vars
        .iter()
        .map(|(k, v)| (k.clone(), serde_json::Value::String(v.clone())))
        .collect();

    let json = serde_json::to_string_pretty(&map).unwrap_or_default();

    format!("const VARS = {};", json)
}

pub fn generate_secrets_template(secrets: &[(String, String)]) -> String {
    if secrets.is_empty() {
        return String::new();
    }

    let map: serde_json::Map<String, serde_json::Value> = secrets
        .iter()
        .map(|(k, v)| (k.clone(), serde_json::Value::String(v.clone())))
        .collect();

    let json = serde_json::to_string_pretty(&map).unwrap_or_default();

    format!("const SECRETS = {};", json)
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
pub fn inject_secrets_and_variables_into_code(
    code: &str,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    master_key: &secrecy::SecretString,
) -> superposition::Result<FunctionCode> {
    let vars: Vec<(String, String)> = variables::variables
        .select((variables::name, variables::value))
        .schema_name(schema_name)
        .load(conn)?;

    let all_secrets: Vec<Secret> =
        secrets_dsl::secrets.schema_name(schema_name).load(conn)?;

    let decrypted_secrets: superposition::Result<Vec<(String, String)>> = if all_secrets
        .is_empty()
    {
        Ok(vec![])
    } else {
        use superposition_types::database::superposition_schema::superposition::workspaces::dsl as ws;

        // Get workspace from schema_name (extract workspace info from schema)
        let schema_parts: Vec<&str> = schema_name.as_str().split('_').collect();
        if schema_parts.len() < 2 {
            return Err(superposition::AppError::BadArgument(
                "Invalid schema name format".to_string(),
            ));
        }
        let org_id = schema_parts[0];
        let workspace_name = schema_parts[1..].join("_");

        let workspace: Workspace = ws::workspaces
            .filter(ws::workspace_name.eq(&workspace_name))
            .filter(ws::organisation_id.eq(org_id))
            .first(conn)?;

        let encrypted_workspace_key = workspace.encryption_key.ok_or_else(|| {
            superposition::AppError::BadArgument(
                "Workspace encryption key not found".to_string(),
            )
        })?;

        // Decrypt workspace key using master key
        let workspace_key = decrypt_workspace_key(&encrypted_workspace_key, master_key)
            .map_err(|e| {
            superposition::AppError::BadArgument(format!(
                "Failed to decrypt workspace key: {}",
                e
            ))
        })?;

        // Decrypt previous workspace key if exists
        let previous_workspace_key: Option<secrecy::SecretString> =
            if let Some(ref encrypted_prev) = workspace.previous_encryption_key {
                Some(
                    decrypt_workspace_key(encrypted_prev, master_key).map_err(|e| {
                        superposition::AppError::BadArgument(format!(
                            "Failed to decrypt previous workspace key: {}",
                            e
                        ))
                    })?.into(),
                )
            } else {
                None
            };

        all_secrets
            .into_iter()
            .map(|secret| {
                let decrypted_value = decrypt_with_fallback(
                    &secret.encrypted_value,
                    &workspace_key,
                    previous_workspace_key.as_ref(),
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
