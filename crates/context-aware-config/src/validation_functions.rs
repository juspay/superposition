use serde_json::{json, Value};
use std::process::Command;
use std::str;

const IMPORT_CODE: &str = r#"
    /*eslint no-unused-vars: "off"*/
    /*eslint no-extra-semi: "off"*/
    const axios =  require("/target/node_modules/axios");
    "#;

const EXIT_LOGIC_CODE: &str = r#"
    if (fun_value != true) {
        process.exit(1);
    };
    "#;

const ES_LINT_CODE: &str = r#"
    const eslint = require("eslint");
    const linter = new eslint.ESLint({
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
        for (var err_obj of results) {
            err_count = err_count + err_obj.errorCount;
        }
        
        if (err_count > 0) {
            process.exit(1);
        } 
        }).catch((error) => {
            process.exit(1);
        
        });

    "#;

fn runtime_wrapper(function_name: &str, value: Value) -> String {
    let fun_call: String = format!("const fun_value = {}({});", function_name, value);
    fun_call + EXIT_LOGIC_CODE
}

pub fn execute_fn(code_str: &str, fun_name: &str, value: Value) -> bool {
    let output = Command::new("node")
        .arg("-e")
        .arg(IMPORT_CODE.to_string() + code_str + &runtime_wrapper(fun_name, value))
        .output();
    log::trace!("{}", format!("validation function output : {:?}", output));
    match output {
        Ok(val) => {
            if !(val.status.success()) {
                let stderr = str::from_utf8(&val.stderr)
                    .unwrap_or("[Invalid UTF-8 in stderr]")
                    .to_owned();
                log::error!("{}", format!("validation function output : {:?}", stderr));
            }
            val.status.success()
        }
        Err(e) => {
            log::error!("js_eval error: {}", e);
            false
        }
    }
}

fn eslint_logic(code_str: &str) -> String {
    let code = IMPORT_CODE.to_string() + code_str;
    let fun_call: String = format!("\nconst codeToLint = {};", json!(code));
    fun_call + ES_LINT_CODE
}

pub fn compile_fn(code_str: &str) -> bool {
    let output = Command::new("node")
        .arg("-e")
        .arg(eslint_logic(code_str))
        .output();
    match output {
        Ok(val) => {
            if !(val.status.success()) {
                let stderr = str::from_utf8(&val.stderr)
                    .unwrap_or("[Invalid UTF-8 in stderr]")
                    .to_owned();
                log::error!("{}", format!("eslint check output : {:?}", stderr));
            }
            val.status.success()
        }
        Err(e) => {
            log::error!("eslint check error: {}", e);
            false
        }
    }
}
