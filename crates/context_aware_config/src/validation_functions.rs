use std::{process::Command, str};

use service_utils::service::types::SchemaName;
use superposition_macros::{unexpected_error, validation_error};
use superposition_types::{
    api::functions::{FunctionExecutionRequest, FunctionExecutionResponse},
    database::models::cac::{FunctionCode, FunctionType},
    result as superposition, DBConnection,
};

use crate::api::functions::helpers::inject_variables_into_code;

static FUNCTION_ENV_VARIABLES: &str =
    "HTTP_PROXY,HTTPS_PROXY,HTTP_PROXY_HOST,HTTP_PROXY_PORT,NO_PROXY";

const CODE_TOKEN: &str = "{replaceme-with-code}";

const FUNCTION_ENV_TOKEN: &str = "{function-envs}";

const FUNCTION_NAME_TOKEN: &str = "{function-name}";

const FUNCTION_TYPE_CHECK_SNIPPET: &str = r#"const vm = require("node:vm")
        const axios = require("./target/node_modules/axios/dist/node/axios.cjs")
        const script = new vm.Script(\`

        {replaceme-with-code}

        if(typeof({function-name})!="function")
        {
            throw Error("{function-name} is not of function type")
        }\`);

        script.runInNewContext({axios,console}, { timeout: 1500 });
        "#;

const FUNCTION_EXECUTION_SNIPPET: &str = r#"
        const vm = require("node:vm")
        const axios = require("./target/node_modules/axios/dist/node/axios.cjs")
        const { parentPort } = require("node:worker_threads")
        const script = new vm.Script(\`

        {replaceme-with-code}
        Promise.resolve({function-invocation}).then((output) => {
            if({condition}) {
                throw new Error("The function did not return a value that was expected. Check the return type and logic of the function")
            }
            parentPort.postMessage({tag: "result", value: output});
            return output;
        }).catch((err)=> {
            throw new Error(err)
        });\`);

        script.runInNewContext({ axios, console, parentPort }, { timeout: 1500 });
        "#;

const CODE_GENERATION_SNIPPET: &str = r#"
    const { Worker, isMainThread, threadId } =  require("node:worker_threads");
    if (isMainThread) {
        let function_env_variables = "{function-envs}"
        let variablesToKeep = []
        variablesToKeep = function_env_variables.split(',').map(variable => variable.trim());
        for (const key in process.env) {
            if (!variablesToKeep.includes(key)) {
                delete process.env[key];
            }
        }


    // starting worker thread , making separated from the main thread
    function runService() {
        return new Promise((resolve, reject) => {
        let result = null;
        const worker = new Worker(
            `{replaceme-with-code}`,{ eval:true }
        );
        worker.on("message", (msg) => {
            if (typeof msg === 'object' && 'tag' in msg) {
                result = msg;
            } else {
                console.log(msg);
            }
        });
        worker.on("error", (err) => {
            clearTimeout(tl);
            console.error(err.message);
            process.exit(1);
        });
        worker.on("exit", (code) => {
            clearTimeout(tl);
            if (code != 0) {
                console.error(`Script stopped with exit code ${code}`);
                worker.terminate();
                throw new Error(code);
            } else {
                resolve(result);
            }
        });

        function timelimit() {
            worker.terminate();
            throw new Error("time limit exceeded");
        }

        // terminate worker thread if execution time exceed 10 secs
        var tl = setTimeout(timelimit, 10000);
        return result;
        });
    }

    runService()
        .then((v) => console.log("|", v.value))
        .catch((err) => console.error(err));
    }
    "#;

fn type_check(code_str: &FunctionCode, function_name: &str) -> String {
    FUNCTION_TYPE_CHECK_SNIPPET
        .replace(FUNCTION_NAME_TOKEN, function_name)
        .replace(CODE_TOKEN, code_str)
}

fn generate_fn_code(
    code_str: &FunctionCode,
    function_args: &FunctionExecutionRequest,
) -> String {
    let (function_invocation, output_check) = match function_args {
        FunctionExecutionRequest::ValueValidationFunctionRequest {
            key,
            value,
            r#type,
            environment,
        } => (
            FunctionType::ValueValidation
                .get_fn_signature()
                .replace("{key}", format!("\"{}\"", &key).as_str())
                .replace("{value}", &value.to_string())
                .replace("{type}", &format!("\"{}\"", &r#type.to_string()))
                .replace(
                    "{environment}",
                    &serde_json::to_string(&environment).unwrap_or_default(),
                ),
            "output!=true",
        ),
        FunctionExecutionRequest::ValueComputeFunctionRequest {
            name,
            prefix,
            r#type,
            environment,
        } => (
            FunctionType::ValueCompute
                .get_fn_signature()
                .replace("{name}", format!("\"{}\"", &name).as_str())
                .replace("{prefix}", format!("\"{}\"", &prefix).as_str())
                .replace("{type}", &format!("\"{}\"", &r#type.to_string()))
                .replace(
                    "{environment}",
                    &serde_json::to_string(&environment).unwrap_or_default(),
                ),
            "!(Array.isArray(output))",
        ),
        FunctionExecutionRequest::ContextValidationFunctionRequest { environment } => (
            FunctionType::ContextValidation.get_fn_signature().replace(
                "{environment}",
                &serde_json::to_string(&environment).unwrap_or_default(),
            ),
            "output!=true",
        ),
        FunctionExecutionRequest::ChangeReasonValidationFunctionRequest {
            change_reason,
        } => (
            FunctionType::ChangeReasonValidation
                .get_fn_signature()
                .replace(
                    "{change_reason}",
                    &serde_json::to_string(&change_reason).unwrap_or_default(),
                ),
            "output!=true",
        ),
    };
    FUNCTION_EXECUTION_SNIPPET
        .replace("{condition}", output_check)
        .replace("{function-invocation}", &function_invocation)
        .replace(CODE_TOKEN, code_str)
}

fn generate_wrapper_runtime(code_str: &str) -> String {
    CODE_GENERATION_SNIPPET
        .replace(FUNCTION_ENV_TOKEN, FUNCTION_ENV_VARIABLES)
        .replace(CODE_TOKEN, code_str)
}

pub fn execute_fn(
    code_str: &FunctionCode,
    args: &FunctionExecutionRequest,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> Result<FunctionExecutionResponse, (String, Option<String>)> {
    let code =
        inject_variables_into_code(code_str, conn, schema_name).map_err(|err| {
            let err_msg = format!("Failed to inject variables: {:?}", err);
            log::error!("{}", err_msg);
            (err_msg, None)
        })?;

    let exec_code = generate_fn_code(&code, args);
    log::trace!("{}", format!("Running function code: {:?}", exec_code));
    let output = Command::new("node")
        .arg("-e")
        .arg(generate_wrapper_runtime(&exec_code))
        .output();
    log::trace!("{}", format!("Running function output : {:?}", output));
    match output {
        Ok(val) => {
            let stdout = str::from_utf8(&val.stdout)
                .unwrap_or("[Invalid UTF-8 in stdout]")
                .to_owned();
            if !(val.status.success()) {
                let stderr = str::from_utf8(&val.stderr)
                    .unwrap_or("[Invalid UTF-8 in stderr]")
                    .to_owned();
                log::error!(
                    "{}",
                    format!("validation function output error: {:?}", stderr)
                );
                Err((stderr, Some(stdout)))
            } else {
                let function_type = FunctionType::from(args);
                let stdout_vec = stdout.trim().split('|').collect::<Vec<_>>();
                let fn_output = stdout_vec
                    .last()
                    .map(|i| i.to_string())
                    .unwrap_or_default()
                    .replace('\'', "\"");

                log::trace!("Function output in rust {:?}", fn_output);
                let fn_output = serde_json::from_str::<serde_json::Value>(&fn_output)
                    .unwrap_or_default();
                Ok(FunctionExecutionResponse {
                    fn_output,
                    stdout: stdout_vec[0..stdout_vec.len() - 1].join("\n"),
                    function_type,
                })
            }
        }
        Err(e) => {
            log::error!("js_eval error: {}", e);
            Err((format!("js_eval error: {}", e), None))
        }
    }
}

pub fn compile_fn(
    function_name: &str,
    code_str: &FunctionCode,
) -> superposition::Result<()> {
    let type_check_code = type_check(code_str, function_name);
    log::trace!(
        "{}",
        format!(
            "validation function code : {:?}",
            generate_wrapper_runtime(&type_check_code)
        )
    );
    let output = Command::new("node")
        .arg("-e")
        .arg(generate_wrapper_runtime(&type_check_code))
        .output();

    log::trace!("{}", format!("validation function output : {:?}", output));
    match output {
        Ok(val) => {
            if !(val.status.success()) {
                let stderr = str::from_utf8(&val.stderr)
                    .unwrap_or("[Invalid UTF-8 in stderr]")
                    .to_owned();
                log::error!("{}", format!("eslint check output error: {:?}", stderr));
                Err(validation_error!(stderr))
            } else {
                Ok(())
            }
        }
        Err(e) => {
            log::error!("eslint check error: {}", e);
            Err(unexpected_error!("js_eval error: {}", e))
        }
    }
}
