use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Condition {
    pub variable: String,
    pub value: Value,
}

#[derive(
    Clone,
    Debug,
    PartialEq,
    derive_more::Deref,
    derive_more::DerefMut,
    Serialize,
    Deserialize,
    Default,
)]
pub struct Conditions(pub Vec<Condition>);

impl Conditions {
    pub fn includes(&self, variable: &str) -> bool {
        self.iter().any(|c| c.variable == variable)
    }

    pub fn try_from_resolve_context_str(context: &str) -> Result<Self, &'static str> {
        let parsed: Value = serde_json::from_str(context)
            .map_err(|_| "failed to parse context string as JSON")?;
        Ok(Self::from_iter(
            parsed
                .as_object()
                .ok_or("not a valid context JSON")?
                .clone(),
        ))
    }
}

impl FromIterator<Condition> for Conditions {
    fn from_iter<T: IntoIterator<Item = Condition>>(iter: T) -> Self {
        Conditions(iter.into_iter().collect::<Vec<_>>())
    }
}

impl FromIterator<(String, Value)> for Conditions {
    fn from_iter<T: IntoIterator<Item = (String, Value)>>(iter: T) -> Self {
        Self(
            iter.into_iter()
                .map(|(variable, value)| Condition { variable, value })
                .collect::<Vec<_>>(),
        )
    }
}

impl From<Conditions> for Map<String, Value> {
    fn from(value: Conditions) -> Self {
        value.0.into_iter().map(|c| (c.variable, c.value)).collect()
    }
}

pub mod jsonlogic {
    use super::*;

    pub mod condition {
        use super::*;

        fn try_var_from_condition_map(
            source: &Map<String, serde_json::Value>,
        ) -> Result<String, &'static str> {
            if let Some(operator) = source.keys().next() {
                return source[operator]
                    .as_array()
                    .ok_or("Invalid operands list for context")?
                    .iter()
                    .find_map(|o| {
                        o.as_object()
                            .and_then(|v| v.get("var"))
                            .and_then(|s| s.as_str())
                            .map(|s| s.to_owned())
                    })
                    .ok_or("variable doesn't exist in operands");
            }
            Err("not a valid condition map")
        }

        pub fn try_from_condition_map(
            source: &Map<String, serde_json::Value>,
        ) -> Result<Condition, &'static str> {
            let variable = try_var_from_condition_map(source)?;
            let value = expression::try_from_condition_map(source)?;

            Ok(Condition { variable, value })
        }

        pub fn try_from_condition_json(
            source: &serde_json::Value,
        ) -> Result<Condition, &'static str> {
            let obj = source
                .as_object()
                .ok_or("not a valid condition value, should be an object")?;
            try_from_condition_map(obj)
        }

        pub fn to_condition_json(value: &Condition) -> serde_json::Value {
            expression::to_condition_json(&value.value, &value.variable)
        }
    }

    pub mod expression {
        use serde_json::json;

        use super::*;

        fn is_variable(v: &serde_json::Value) -> bool {
            v.is_object()
                && v.as_object()
                    .map(|v| v.contains_key("var"))
                    .unwrap_or_default()
        }

        fn is_constant(v: &serde_json::Value) -> bool {
            !is_variable(v)
        }

        pub(super) fn try_from_condition_map(
            source: &Map<String, serde_json::Value>,
        ) -> Result<Value, &'static str> {
            if let Some(operator) = source.keys().next() {
                let operands = source[operator]
                    .as_array()
                    .cloned()
                    .ok_or("Invalid operands list for context")?;

                let operand_0 = operands.first();
                let operand_1 = operands.get(1);
                let operand_2 = operands.get(2);

                match (operator.as_str(), operand_0, operand_1, operand_2) {
                    ("==", Some(o1), Some(o2), None)
                        if is_variable(o1) && is_constant(o2) =>
                    {
                        return Ok(o2.clone());
                    }
                    _ => return Err("unsupported operator"),
                }
            }

            Err("not a valid expression map")
        }

        pub(crate) fn to_condition_json(value: &Value, var: &str) -> Value {
            json!({
                "==": [
                    {"var": var},
                    value
                ]
            })
        }
    }
}
