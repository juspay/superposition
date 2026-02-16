use std::time::Duration;

use rustyscript::{Module, Runtime, RuntimeOptions, json_args, static_runtime};
use serde::Serialize;
use superposition_macros::{bad_argument, validation_error};

use service_utils::service::types::{EncryptionKey, WorkspaceContext};
use superposition_types::{
    DBConnection,
    api::functions::{FunctionExecutionRequest, FunctionExecutionResponse},
    database::models::cac::{FunctionCode, FunctionRuntimeVersion, FunctionType},
    result as superposition,
};

use crate::api::functions::helpers::inject_secrets_and_variables_into_code;

static_runtime!(S11N_RUNTIME, {
    let timeout = std::time::Duration::from_secs(30);
    RuntimeOptions {
        timeout,
        ..Default::default()
    }
});

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
"#;

fn generate_wrapped_code(code: &str) -> String {
    FUNCTION_WRAPPER.replace("{replaceme-with-code}", code)
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

    let wrapped_code = generate_wrapped_code(&code);
    log::trace!("Running function code: {:?}", wrapped_code);

    let module = Module::new("function.js", &wrapped_code);

    let payload = FunctionPayload {
        version: runtime_version,
        payload: args.clone(),
    };
    let execution_response = S11N_RUNTIME::with(|runtime| {
        let module_handle = runtime.load_module(&module)?;

        let tokio_runtime = runtime.tokio_runtime();

        tokio_runtime.block_on(async {
            let v = runtime
                .call_function_async::<serde_json::Value>(
                    Some(&module_handle),
                    "execute",
                    json_args!(payload),
                )
                .await?;
            let stdout = runtime
                .call_function::<Vec<String>>(
                    Some(&module_handle),
                    "getLogBuffer",
                    json_args!(),
                )?
                .join("\n");
            runtime.call_function::<()>(
                Some(&module_handle),
                "clearLogBuffer",
                json_args!(),
            )?;
            let function_type = FunctionType::from(args);
            log::trace!("Function output: {:?}", v);
            log::trace!("Function logs: {}", stdout);
            Ok(FunctionExecutionResponse {
                fn_output: v,
                stdout,
                function_type,
            })
        })
    });

    execution_response.map_err(|err| {
        log::error!("Could not execute function: {:?}", err);
        (format!("js_eval error: {}", err), None)
    })
}

pub fn compile_fn(code_str: &FunctionCode) -> superposition::Result<()> {
    let type_check_code = format!(
        r#"
        {}

        if (typeof execute !== "function") {{
            throw new Error("execute is not of function type");
        }}
        "#,
        code_str
    );

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
            runtime
                .call_function_async::<()>(Some(&module_handle), "default", json_args!())
                .await
        })
        .map_err(|e| {
            let err_str = e.to_string();
            validation_error!("Function validation failed: {}", err_str)
        })?;

    Ok(())
}
