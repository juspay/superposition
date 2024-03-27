use serde_json::{json, Value};
use std::process::Command;
use std::str;

const IMPORT_CODE: &str = r#"
    /*eslint no-unused-vars: "off"*/
    /*eslint no-extra-semi: "off"*/
    const axios =  require("./target/node_modules/axios");
    "#;

const EXIT_LOGIC_CODE: &str = r#"
    if (fun_value != true) {
        console.error(fun_value)
        process.exit(1);
    };
    "#;

const ES_LINT_CODE: &str = r#"
    const {ESLint} = require("./target/node_modules/eslint");
    const linter = new ESLint({
        useEslintrc: false,
        overrideConfig: {
            extends: ["eslint:recommended"],
            parserOptions: {
                sourceType: "module",
                ecmaVersion: "latest",
            },
            env: {
                browser: true,
                commonjs: true,
            }
        },
    });

    linter.lintText(codeToLint).then((results) => {
        var err_count = 0;
        var err_msgs = [];
        for (var err_obj of results) {
            console.log(err_obj.messages);
            err_count = err_count + err_obj.errorCount;
            err_msgs.push(err_obj.messages);
        }
        
        if (err_count > 0) {
            console.error(err_msgs);
            process.exit(1);
        } 
        }).catch((error) => {
            console.error(error);
            process.exit(1);
        });

    "#;

fn runtime_wrapper(function_name: &str, key: &str, value: Value) -> String {
    let fun_call: String = format!(
        "const fun_value = {}({});",
        function_name,
        json!({
            "key": key,
            "value": value
        })
    );
    fun_call + EXIT_LOGIC_CODE
}

pub fn execute_fn(
    code_str: &str,
    fun_name: &str,
    key: &str,
    value: Value,
) -> Result<String, (String, Option<String>)> {
    let output = Command::new("node")
        .arg("-e")
        .arg(IMPORT_CODE.to_string() + code_str + &runtime_wrapper(fun_name, key, value))
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

fn eslint_logic(code_str: &str, fun_name: &str) -> String {
    let code =
        IMPORT_CODE.to_string() + code_str + &format!("console.log({});", fun_name);
    let fun_call: String = format!("\nconst codeToLint = {};", json!(code));
    fun_call + ES_LINT_CODE
}

pub fn compile_fn(code_str: &str, fun_name: &str) -> Result<(), String> {
    let output = Command::new("node")
        .arg("-e")
        .arg(eslint_logic(code_str, fun_name))
        .output();
    log::trace!("{}", format!("validation function output : {:?}", output));
    match output {
        Ok(val) => {
            if !(val.status.success()) {
                let stderr = str::from_utf8(&val.stderr)
                    .unwrap_or("[Invalid UTF-8 in stderr]")
                    .to_owned();
                log::error!("{}", format!("eslint check output error: {:?}", stderr));
                Err(stderr)
            } else {
                Ok(())
            }
        }
        Err(e) => {
            log::error!("eslint check error: {}", e);
            Err(format!("js_eval error: {}", e))
        }
    }
}
