use serde_json::Value;

use crate::config::Defaults;

/// If the params is a JSON object and is missing `workspace_id` / `org_id`,
/// inject defaults from the configuration. Other shapes (non-object) are
/// passed through unchanged — the SDK call will surface a typed deserialize
/// error verbatim.
pub fn inject_defaults(params: Value, defaults: &Defaults) -> Value {
    let Value::Object(mut obj) = params else {
        return params;
    };
    if !obj.contains_key("workspace_id") {
        if let Some(w) = defaults.workspace_id.as_ref() {
            obj.insert("workspace_id".to_string(), Value::String(w.clone()));
        }
    }
    if !obj.contains_key("org_id") {
        if let Some(o) = defaults.org_id.as_ref() {
            obj.insert("org_id".to_string(), Value::String(o.clone()));
        }
    }
    Value::Object(obj)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn defaults(w: Option<&str>, o: Option<&str>) -> Defaults {
        Defaults {
            workspace_id: w.map(String::from),
            org_id: o.map(String::from),
        }
    }

    #[test]
    fn injects_both_when_absent() {
        let params = json!({"key": "v"});
        let out = inject_defaults(params, &defaults(Some("w"), Some("o")));
        assert_eq!(out, json!({"key": "v", "workspace_id": "w", "org_id": "o"}));
    }

    #[test]
    fn param_value_wins_over_default() {
        let params = json!({"workspace_id": "explicit"});
        let out = inject_defaults(params, &defaults(Some("default"), None));
        assert_eq!(out, json!({"workspace_id": "explicit"}));
    }

    #[test]
    fn injects_org_only_when_only_org_defaulted() {
        let params = json!({});
        let out = inject_defaults(params, &defaults(None, Some("o")));
        assert_eq!(out, json!({"org_id": "o"}));
    }

    #[test]
    fn no_change_when_no_defaults() {
        let params = json!({"x": 1});
        let out = inject_defaults(params.clone(), &defaults(None, None));
        assert_eq!(out, params);
    }

    #[test]
    fn passes_through_non_object() {
        let params = json!(42);
        let out = inject_defaults(params.clone(), &defaults(Some("w"), Some("o")));
        assert_eq!(out, params);
    }
}
