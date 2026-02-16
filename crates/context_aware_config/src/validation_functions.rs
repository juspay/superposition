use std::time::Duration;

use rustyscript::{Module, Runtime, RuntimeOptions, json_args};
use superposition_macros::validation_error;

use service_utils::service::types::{EncryptionKey, WorkspaceContext};
use superposition_types::{
    DBConnection,
    api::functions::{FunctionExecutionRequest, FunctionExecutionResponse},
    database::models::cac::{FunctionCode, FunctionRuntimeVersion, FunctionType},
    result as superposition,
};

use crate::api::functions::helpers::inject_secrets_and_variables_into_code;

// #[derive(Clone)]
// struct LogCapture {
//     logs: Arc<Mutex<Vec<String>>>,
// }

// impl LogCapture {
//     fn new() -> Self {
//         Self {
//             logs: Arc::new(Mutex::new(Vec::new())),
//         }
//     }

//     fn capture(&self, level: &str, msg: &str) {
//         let mut logs = self.logs.lock().unwrap();
//         logs.push(format!("[{}] {}", level, msg));
//     }

//     fn get_logs(&self) -> Vec<String> {
//         let logs = self.logs.lock().unwrap();
//         logs.clone()
//     }
// }

fn generate_wrapped_code(
    code: &str,
    args: &FunctionExecutionRequest,
    runtime_version: FunctionRuntimeVersion,
) -> String {
    let args_value = serde_json::to_value(args).unwrap_or(serde_json::Value::Null);
    let payload = serde_json::json!({
        "version": runtime_version,
        "value_validate": args_value.get("value_validate"),
        "value_compute": args_value.get("value_compute"),
        "context_validate": args_value.get("context_validate"),
        "change_reason_validate": args_value.get("change_reason_validate")
    });

    let output_check = match args {
        FunctionExecutionRequest::ValueValidationFunctionRequest { .. } => "output!=true",
        FunctionExecutionRequest::ValueComputeFunctionRequest { .. } => {
            "!(Array.isArray(output))"
        }
        FunctionExecutionRequest::ContextValidationFunctionRequest { .. } => {
            "output!=true"
        }
        FunctionExecutionRequest::ChangeReasonValidationFunctionRequest { .. } => {
            "output!=true"
        }
    };

    format!(
        r#"
        {}

        const logBuffer = [];
        const originalConsole = console;
        const customConsole = {{
            log: (...args) => {{
                logBuffer.push("[info] " + args.map(a => typeof a === 'object' ? JSON.stringify(a) : String(a)).join(' '));
                originalConsole.log(...args);
            }},
            info: (...args) => {{
                logBuffer.push("[info] " + args.map(a => typeof a === 'object' ? JSON.stringify(a) : String(a)).join(' '));
                originalConsole.info(...args);
            }},
            warn: (...args) => {{
                logBuffer.push("[warn] " + args.map(a => typeof a === 'object' ? JSON.stringify(a) : String(a)).join(' '));
                originalConsole.warn(...args);
            }},
            error: (...args) => {{
                logBuffer.push("[error] " + args.map(a => typeof a === 'object' ? JSON.stringify(a) : String(a)).join(' '));
                originalConsole.error(...args);
            }},
            debug: (...args) => {{
                logBuffer.push("[debug] " + args.map(a => typeof a === 'object' ? JSON.stringify(a) : String(a)).join(' '));
                originalConsole.debug(...args);
            }}
        }};

        Object.defineProperty(globalThis, 'console', {{
            value: customConsole,
            writable: false,
            configurable: false
        }});

        (async () => {{
            try {{
                const payload = {};
                const output = await execute(payload);

                if ({}) {{
                    throw new Error("The function did not return a value that was expected. Check the return type and logic of the function");
                }}

                return {{ output, logs: logBuffer }};
            }} catch (err) {{
                throw {{ error: err.message || String(err), logs: logBuffer }};
            }}
        }})();
        "#,
        code,
        serde_json::to_string(&payload).unwrap_or_else(|_| "Invalid Payload".to_string()),
        output_check
    )
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

    let wrapped_code = generate_wrapped_code(&code, args, runtime_version);
    log::trace!("Running function code: {:?}", wrapped_code);

    let module = Module::new("function.js", &wrapped_code);

    let mut runtime_options = RuntimeOptions {
        timeout: Duration::from_secs(10),
        ..Default::default()
    };

    let mut runtime = Runtime::new(runtime_options)
        .map_err(|e| (format!("Failed to create runtime: {}", e), None))?;

    let module_handle = runtime
        .load_module(&module)
        .map_err(|e| (format!("Failed to load module: {}", e), None))?;

    let tokio_runtime = runtime.tokio_runtime();

    let result: serde_json::Value = tokio_runtime
        .block_on(async {
            runtime
                .call_function_async::<serde_json::Value>(
                    Some(&module_handle),
                    "default",
                    json_args!(),
                )
                .await
        })
        .map_err(|e| (format!("Execution error: {}", e), None))?;

    let (output, logs) = if let Some(obj) = result.as_object() {
        let logs_value = obj.get("logs").and_then(|v| v.as_array());
        let logs_vec: Vec<String> = logs_value
            .map(|arr| {
                arr.iter()
                    .filter_map(|v| v.as_str().map(String::from))
                    .collect()
            })
            .unwrap_or_default();

        if obj.contains_key("error") {
            let error_msg = obj
                .get("error")
                .and_then(|v| v.as_str())
                .unwrap_or("Unknown error")
                .to_string();
            return Err((error_msg, Some(logs_vec.join("\\n"))));
        }

        let output_val = obj
            .get("output")
            .cloned()
            .unwrap_or(serde_json::Value::Null);
        (output_val, logs_vec)
    } else {
        (result, vec![])
    };

    let stdout = logs.join("\n");
    let function_type = FunctionType::from(args);

    log::trace!("Function output: {:?}", output);
    log::trace!("Function logs: {}", stdout);

    Ok(FunctionExecutionResponse {
        fn_output: output,
        stdout,
        function_type,
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
