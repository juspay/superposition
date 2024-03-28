use context_aware_config::validation_functions::{compile_fn, execute_fn};
use serde_json::json;

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

    let err_execute =
        match execute_fn(&(execute_code_error.to_owned()), "test", json!(10)) {
            Ok(_) => false,
            Err((e, stdout)) => e.contains("Bad schema"),
        };
    let err_compile = match compile_fn(&(compile_code_error.to_owned())) {
        Ok(()) => false,
        Err(e) => e.contains("Bad schema"),
    };
    assert_eq!(
        execute_fn(&(code_ok.to_owned()), "test", json!(10)),
        Ok("true".to_string())
    );
    assert_eq!(err_execute, true);
    assert_eq!(compile_fn(&(code_ok.to_owned())), Ok(()));
    assert_eq!(err_compile, true);
}
