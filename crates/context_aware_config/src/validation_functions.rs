use std::time::Duration;

use rustyscript::{Module, Runtime, RuntimeOptions, json_args};
use serde::Serialize;
use superposition_macros::{bad_argument, validation_error};

use service_utils::service::types::{EncryptionKey, WorkspaceContext};
use superposition_types::{
    DBConnection,
    api::functions::{
        ContextValidationTrigger, FunctionEnvironment, FunctionExecutionRequest,
        FunctionExecutionResponse, KeyType,
    },
    database::models::{
        ChangeReason,
        cac::{FunctionCode, FunctionRuntimeVersion, FunctionType},
    },
    result as superposition,
};

use crate::api::functions::helpers::inject_secrets_and_variables_into_code;

#[derive(Serialize)]
struct FunctionPayload {
    version: FunctionRuntimeVersion,
    #[serde(flatten)]
    payload: FunctionExecutionRequest,
}

const FUNCTION_WRAPPER: &str = r#"
let logBuffer = [];
const originalConsole = console;
const customConsole = {
    log: (...args) => {
        logBuffer.push("[log] " + args.map(a => typeof a === 'object' ? JSON.stringify(a) : String(a)).join(' '));
    },
    info: (...args) => {
        logBuffer.push("[info] " + args.map(a => typeof a === 'object' ? JSON.stringify(a) : String(a)).join(' '));
    },
    warn: (...args) => {
        logBuffer.push("[warn] " + args.map(a => typeof a === 'object' ? JSON.stringify(a) : String(a)).join(' '));
    },
    error: (...args) => {
        logBuffer.push("[error] " + args.map(a => typeof a === 'object' ? JSON.stringify(a) : String(a)).join(' '));
    },
    debug: (...args) => {
        logBuffer.push("[debug] " + args.map(a => typeof a === 'object' ? JSON.stringify(a) : String(a)).join(' '));
    }
};

Object.defineProperty(globalThis, 'console', {
    value: customConsole,
    writable: false,
    configurable: false
});

function getLogBuffer() {
    return logBuffer;
}

function clearLogBuffer() {
    logBuffer = [];
}

{replaceme-with-code}

export { execute, getLogBuffer, clearLogBuffer };
"#;

fn generate_wrapped_code(code: &str) -> String {
    FUNCTION_WRAPPER.replace("{replaceme-with-code}", code)
}

/// Runs JS function code in a rustyscript runtime. This function is `Send`-safe
/// and does not require any DB connection, making it suitable for use with
/// `spawn_blocking`.
pub fn run_js_function(
    code: FunctionCode,
    args: FunctionExecutionRequest,
    runtime_version: FunctionRuntimeVersion,
) -> Result<FunctionExecutionResponse, (String, Option<String>)> {
    let wrapped_code = generate_wrapped_code(&code);
    let module = Module::new("function.js", &wrapped_code);

    let runtime_options = RuntimeOptions {
        timeout: Duration::from_millis(1500),
        // import_provider: Some(Box::new(DenyFileImportProvider::default())),
        ..Default::default()
    };

    let mut runtime = Runtime::new(runtime_options).map_err(|e| {
        let err_str = e.to_string();
        (format!("Failed to create runtime: {}", err_str), None)
    })?;

    let payload = FunctionPayload {
        version: runtime_version,
        payload: args.clone(),
    };
    let module_handle = runtime
        .load_module(&module)
        .map_err(|err| (err.to_string(), None))?;

    let tokio_runtime = runtime.tokio_runtime();

    let fn_output = tokio_runtime
        .block_on(async {
            runtime
                .call_function_async::<serde_json::Value>(
                    Some(&module_handle),
                    "execute",
                    json_args!(payload),
                )
                .await
        })
        .map_err(|err| (err.to_string(), None))?;

    let stdout = runtime
        .call_function::<Vec<String>>(Some(&module_handle), "getLogBuffer", json_args!())
        .map_err(|err| (err.to_string(), None))?
        .join("\n");
    runtime
        .call_function::<()>(Some(&module_handle), "clearLogBuffer", json_args!())
        .map_err(|err| (err.to_string(), None))?;
    let function_type = FunctionType::from(&args);
    log::trace!("Function output: {:?}", fn_output);
    log::trace!("Function logs: {}", stdout);
    Ok(FunctionExecutionResponse {
        fn_output,
        stdout,
        function_type,
    })
}

pub fn execute_fn(
    workspace_context: &WorkspaceContext,
    code_str: &FunctionCode,
    args: &FunctionExecutionRequest,
    runtime_version: FunctionRuntimeVersion,
    conn: &mut DBConnection,
    master_encryption_key: &Option<EncryptionKey>,
) -> Result<FunctionExecutionResponse, (String, Option<String>)> {
    let code = inject_secrets_and_variables_into_code(
        code_str,
        conn,
        workspace_context,
        master_encryption_key,
    )
    .map_err(|err| {
        log::error!("Failed to inject variables: {:?}", err);
        (err.to_string(), None)
    })?;

    run_js_function(code, args.clone(), runtime_version)
}

pub fn compile_fn(
    code_str: &FunctionCode,
    fn_type: &FunctionType,
) -> superposition::Result<()> {
    // syntax validation, so we do not need the actual secrets and vars
    let stubs = "const SECRETS = {};\nconst VARS = {};\n";
    let stubbed_code = FunctionCode(format!("{}\n{}", stubs, code_str));
    let type_check_code = generate_wrapped_code(&stubbed_code);

    rustyscript::validate(&type_check_code).map_err(|err| {
        log::error!("Invalid function syntax: {:?}", err);
        bad_argument!("Invalid function syntax: {}", err)
    })?;

    let module = Module::new("type_check.js", &type_check_code);
    let runtime_options = RuntimeOptions {
        timeout: Duration::from_millis(1500),
        ..Default::default()
    };

    let mut runtime = Runtime::new(runtime_options).map_err(|e| {
        let err_str = e.to_string();
        validation_error!("Failed to create runtime: {}", err_str)
    })?;

    let module_handle = runtime.load_module(&module).map_err(|e| {
        let err_str = e.to_string();
        validation_error!("Failed to load module: {}", err_str)
    })?;

    let tokio_runtime = runtime.tokio_runtime();
    tokio_runtime
        .block_on(async {
            let environment = FunctionEnvironment {
                context: serde_json::Map::new(),
                overrides: serde_json::Map::new(),
            };
            match fn_type {
                FunctionType::ValueCompute => {
                    let payload = FunctionExecutionRequest::ValueComputeFunctionRequest {
                        name: String::new(),
                        prefix: String::new(),
                        r#type: superposition_types::api::functions::KeyType::ConfigKey,
                        environment,
                    };
                    let serde_json::Value::Array(_) = runtime
                        .call_function_async::<serde_json::Value>(
                            Some(&module_handle),
                            "execute",
                            json_args!(payload),
                        )
                        .await? else {
                            return Err(rustyscript::Error::Runtime("The value compute function did not return an array".to_string()));
                        };
                    Ok(())
                }
                other => {
                    let payload = match other {
                        FunctionType::ValueValidation => FunctionExecutionRequest::ValueValidationFunctionRequest {
                            key: String::new(),
                            value: serde_json::Value::String(String::new()),
                            r#type:
                                KeyType::ConfigKey,
                            environment,
                        },
                        FunctionType::ContextValidation => FunctionExecutionRequest::ContextValidationFunctionRequest {
                            environment,
                            trigger_reason: ContextValidationTrigger::Context,
                        },
                        FunctionType::ChangeReasonValidation => FunctionExecutionRequest::ChangeReasonValidationFunctionRequest {
                            change_reason: ChangeReason::default(),
                        },
                        _ => {
                            return Err(rustyscript::Error::Runtime("An invalid situation occurred in the runtime".to_string()))
                        }
                    };
                    let serde_json::Value::Bool(_) = runtime
                        .call_function_async::<serde_json::Value>(
                            Some(&module_handle),
                            "execute",
                            json_args!(payload),
                        )
                        .await? else {
                            return Err(rustyscript::Error::Runtime("The function did not return a boolean".to_string()))
                        };
                    Ok(())
                }

            }
        })
        .map_err(|e| {
            let err_str = e.to_string();
            validation_error!("Function validation failed: {}", err_str)
        })?;

    Ok(())
}
