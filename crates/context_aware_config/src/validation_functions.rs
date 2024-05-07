use serde_json::{json, Value};
use service_utils::result as superposition;
use service_utils::unexpected_error;
use service_utils::validation_error;
use std::process::Command;
use std::str;

fn type_check_validate(code_str: &str) -> String {
    format!(
        r#"const vm = require("node:vm")
        const axios = require("./target/node_modules/axios")
        const script = new vm.Script(\`

        {}

        if(typeof(validate)!="function")
        {{
            throw Error("validate is not of function type")
        }}\`);

        script.runInNewContext({{axios,console}}, {{ timeout: 1500}});
        "#,
        code_str
    )
}

fn execute_validate_fun(code_str: &str, value: Value, key: String) -> String {
    format!(
        r#"
        const vm = require("node:vm")
        const axios = require("./target/node_modules/axios")
        const script = new vm.Script(\`

        {}
        Promise.resolve(validate({}, {})).then((output) => {{

            if(output!=true){{
                throw new Error("The function did not return true as expected. Check the conditions or logic inside the function.")
            }}
            return output;
        }}).catch((err)=> {{
            throw new Error(err)
        }});\`);

        script.runInNewContext({{axios,console,process}}, {{ timeout: 1500}});
        "#,
        code_str, value, key
    )
}

fn generate_code(code_str: &str) -> String {
    format!(
        r#"
    const {{ Worker, isMainThread, threadId }} =  require("node:worker_threads");

    if (isMainThread) {{

    // starting worker thread , making separated from the main thread
    function runService() {{
        return new Promise((resolve, reject) => {{
        const worker = new Worker(
            `{}`,{{eval:true}}
        );
        worker.on("message", (msg) => {{
            console.log(msg);
        }});
        worker.on("error", (err) => {{
            clearTimeout(tl);
            console.error(err.message);
            process.exit(1);
        }});
        worker.on("exit", (code) => {{
            clearTimeout(tl);
            if (code !== 0) {{
                console.error(`Script stopped with exit code ${{code}}`);
                process.exit(code);
            }} else {{
                worker.terminate();
            }}
        }});

        function timelimit() {{
            worker.terminate();
            throw new Error("time limit exceeded");
        }}

        // terminate worker thread if execution time exceed 2 secs

        var tl = setTimeout(timelimit, 2000);
        }});
    }}

    async function run() {{
        const result = await runService();
        console.log("result output: ", result);
    }}
    run().catch((err) => console.error(err));
    }}

    "#,
        code_str
    )
}

pub fn execute_fn(
    code_str: &str,
    key: &str,
    value: Value,
) -> Result<String, (String, Option<String>)> {
    let exec_code =
        execute_validate_fun(code_str, value, format!("\"{}\"", key.to_string()));
    let output = Command::new("node")
        .arg("-e")
        .arg(generate_code(&exec_code))
        .output();
    log::trace!("{}", format!("validation function output : {:?}", output));
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
                Ok(stdout)
            }
        }
        Err(e) => {
            log::error!("js_eval error: {}", e);
            Err((format!("js_eval error: {}", e), None))
        }
    }
}

pub fn compile_fn(code_str: &str) -> superposition::Result<()> {
    let type_check_code = type_check_validate(code_str);
    let output = Command::new("node")
        .arg("-e")
        .arg(generate_code(&type_check_code))
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
