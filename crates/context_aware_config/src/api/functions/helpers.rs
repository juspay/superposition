use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper};
use secrecy::ExposeSecret;
use service_utils::{
    encryption::{decrypt_secret, decrypt_workspace_key},
    service::types::{EncryptionKey, SchemaName, WorkspaceContext},
};
use superposition_macros::{unexpected_error, validation_error};
use superposition_types::{
    DBConnection,
    database::{
        models::cac::{Function, FunctionCode, FunctionType},
        schema::{
            self, functions::dsl::functions, secrets::dsl as secrets_dsl,
            variables::dsl as variables,
        },
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

pub fn generate_template(name: &str, vars: &[(String, String)]) -> String {
    if vars.is_empty() {
        return String::new();
    }
    let map: serde_json::Map<String, serde_json::Value> = vars
        .iter()
        .map(|(k, v)| (k.clone(), serde_json::Value::String(v.clone())))
        .collect();
    let json = serde_json::to_string_pretty(&map).unwrap_or_default();
    format!("const {} = {};", name, json)
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

    let vars_template = generate_template("VARS", &vars);
    let processed_code = format!("{}\n\n{}", vars_template, code);

    Ok(FunctionCode(processed_code))
}

pub fn inject_secrets_into_code(
    workspace_context: &WorkspaceContext,
    code: &str,
    conn: &mut DBConnection,
    master_encryption_key: &Option<EncryptionKey>,
) -> superposition::Result<FunctionCode> {
    let all_secrets: Vec<(String, String)> = secrets_dsl::secrets
        .select((secrets_dsl::name, secrets_dsl::encrypted_value))
        .schema_name(&workspace_context.schema_name)
        .load(conn)?;

    if all_secrets.is_empty() {
        let secrets_template = generate_template("SECRETS", &[]);
        let processed_code = format!("{}\n\n{}", secrets_template, code);
        return Ok(FunctionCode(processed_code));
    }

    // If master_encryption_key is not available, we cannot decrypt secrets
    let Some(master_encryption_key) = master_encryption_key else {
        log::warn!(
            "Master encryption key not configured, skipping secret injection in function code"
        );
        let secrets_template = generate_template("SECRETS", &[]);
        let processed_code = format!("{}\n\n{}", secrets_template, code);
        return Ok(FunctionCode(processed_code));
    };

    let workspace = &workspace_context.settings;

    let workspace_key =
        decrypt_workspace_key(&workspace.encryption_key, master_encryption_key)
            .map_err(|e| unexpected_error!("Failed to decrypt workspace key: {}", e))?;

    let decrypted_secrets: superposition::Result<Vec<(String, String)>> = all_secrets
        .into_iter()
        .map(|(name, encrypted_value)| {
            let decrypted_value = decrypt_secret(&encrypted_value, &workspace_key)
                .map_err(|e| {
                    unexpected_error!("Failed to decrypt secret '{}': {}", name, e)
                })?;
            Ok((name, decrypted_value.expose_secret().to_string()))
        })
        .collect();

    let secrets_template = generate_template("SECRETS", &decrypted_secrets?);
    let processed_code = format!("{}\n\n{}", secrets_template, code);

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
    workspace_context: &WorkspaceContext,
    master_encryption_key: &Option<EncryptionKey>,
) -> superposition::Result<FunctionCode> {
    let code_with_secrets =
        inject_secrets_into_code(workspace_context, code, conn, master_encryption_key)?;

    let final_code = inject_variables_into_code(
        &code_with_secrets,
        conn,
        &workspace_context.schema_name,
    )?;

    Ok(final_code)
}
