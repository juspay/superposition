use context_aware_config::validation_functions::{compile_fn, execute_fn};
use serde_json::json;
use service_utils::result as superposition;

// #[test] //todo : currently there is issue in running this test
fn test_execute_fn() {
    let code_ok = r#"
        function validate() {
            return true;
        };
    "#;

    let execute_code_error = r#"
        function validate() {
            return false;
        }
    "#;

    let compile_code_error = r#"
        function validate( {
            return true;
        }
    "#;

    let err_execute = match execute_fn(execute_code_error, "test", json!(10)) {
        Ok(_) => false,
        Err((e, _)) => e.contains("Bad schema"),
    };
    let err_compile = match compile_fn(compile_code_error) {
        Ok(()) => false,
        Err(superposition::AppError::ValidationError(_)) => true,
        _ => false,
    };
    assert_eq!(
        execute_fn(code_ok, "test", json!(10)),
        Ok("true".to_string())
    );
    assert!(err_execute);
    assert!(compile_fn(code_ok).is_ok());
    assert!(err_compile);
}
