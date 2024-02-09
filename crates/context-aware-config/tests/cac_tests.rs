use context_aware_config::validation_functions::{compile_fn, execute_fn};
use serde_json::json;

// #[test] //todo : currently there is issue in running this test
fn test_execute_fn() {
    let code_ok = r#"
        function test_fun() {
            return true;
        };
    "#;

    let execute_code_error = r#"
        function test_fun() {
            return false;
        }
    "#;

    let compile_code_error = r#"
        function test_fun( {
            return true;
        }
    "#;

    assert_eq!(
        execute_fn(&(code_ok.to_owned()), &"test_fun".to_owned(), json!(10)),
        true
    );
    assert_eq!(
        execute_fn(
            &(execute_code_error.to_owned()),
            &"test_fun".to_owned(),
            json!(10)
        ),
        false
    );
    assert_eq!(compile_fn(&(code_ok.to_owned())), true);
    assert_eq!(compile_fn(&(compile_code_error.to_owned())), true);
}
