use std::fmt::Display;

use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};

use crate::schema::{HtmlDisplay, SchemaType};
use superposition_types::Context;

/// The `Expression` enum represents a JSONLogic expression and ensures
/// the correct construction of expressions by tying operators and operands together.
///
/// This enum provides a way to enforce the proper order and number of operands
/// for each operator, ensuring the resulting JSONLogic expression is well-formed.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Expression {
    Is(serde_json::Value),
    In(serde_json::Value),
    Has(serde_json::Value),
    Between(serde_json::Value, serde_json::Value),
    Other(String, Vec<serde_json::Value>),
}

pub fn is_variable(v: &serde_json::Value) -> bool {
    v.is_object()
        && v.as_object()
            .map(|v| v.contains_key("var"))
            .unwrap_or_default()
}

pub fn is_constant(v: &serde_json::Value) -> bool {
    !is_variable(v)
}

impl Expression {
    fn try_from_expression_map(
        source: &Map<String, serde_json::Value>,
    ) -> Result<Self, &'static str> {
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
                    return Ok(Expression::Is(o2.clone()));
                }
                ("<=", Some(o1), Some(o2), Some(o3))
                    if is_variable(o2) && is_constant(o1) && is_constant(o3) =>
                {
                    return Ok(Expression::Between(o1.clone(), o3.clone()));
                }
                ("in", Some(o1), Some(o2), None)
                    if is_variable(o1) && is_constant(o2) =>
                {
                    return Ok(Expression::In(o2.clone()));
                }
                ("in", Some(o1), Some(o2), None)
                    if is_variable(o2) && is_constant(o1) =>
                {
                    return Ok(Expression::Has(o1.clone()));
                }
                _ => {
                    return Ok(Expression::Other(operator.clone(), operands.clone()));
                }
            }
        }

        Err("not a valid expression map")
    }
    fn to_expression_json(&self, var: &str) -> serde_json::Value {
        match self {
            Expression::Is(c) => {
                json!({
                    "==": [
                        {"var": var},
                        c
                    ]
                })
            }
            Expression::In(c) => {
                json!({
                    "in": [
                        {"var": var},
                        c
                    ]
                })
            }
            Expression::Has(c) => {
                json!({
                    "in": [
                        c,
                        {"var": var}
                    ]
                })
            }
            Expression::Between(c1, c2) => {
                json!({
                    "<=": [
                        c1,
                        {"var": var},
                        c2
                    ]
                })
            }
            Expression::Other(operator, operands) => {
                json!({
                    operator: operands.clone()
                })
            }
        }
    }
    fn to_expression_query_str(&self, var: &str) -> Option<String> {
        match self {
            Expression::Is(c) => Some(format!("{var}={value}", value = c.html_display())),
            _ => None,
        }
    }
    pub fn to_constants_vec(&self) -> Vec<serde_json::Value> {
        match self {
            Expression::Is(c) | Expression::In(c) | Expression::Has(c) => {
                vec![c.clone()]
            }
            Expression::Between(c1, c2) => {
                vec![c1.clone(), c2.clone()]
            }
            Expression::Other(_, operands) => operands
                .iter()
                .filter_map(|operand| {
                    if is_variable(operand) {
                        return None;
                    }
                    Some(operand.clone())
                })
                .collect::<Vec<serde_json::Value>>(),
        }
    }
    pub fn to_operator(&self) -> Operator {
        Operator::from(self)
    }
}

impl From<(SchemaType, Operator)> for Expression {
    fn from((r#type, operator): (SchemaType, Operator)) -> Self {
        match operator {
            Operator::Is => Expression::Is(r#type.default_value()),
            Operator::In => Expression::In(Value::Array(vec![])),
            Operator::Has => Expression::Has(r#type.default_value()),
            Operator::Between => {
                Expression::Between(r#type.default_value(), r#type.default_value())
            }
            Operator::Other(o) => Expression::Other(o, vec![]),
        }
    }
}

/// This a copy of `Expression` enum, to prevent copying/moving of data
/// where just the operator information is needed.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Operator {
    Is,
    In,
    Has,
    Between,
    Other(String),
}

impl From<&Expression> for Operator {
    fn from(value: &Expression) -> Self {
        match value {
            Expression::Is(_) => Operator::Is,
            Expression::In(_) => Operator::In,
            Expression::Has(_) => Operator::Has,
            Expression::Between(_, _) => Operator::Between,
            Expression::Other(o, _) => Operator::Other(o.clone()),
        }
    }
}

impl From<&Condition> for Operator {
    fn from(value: &Condition) -> Self {
        (&value.expression).into()
    }
}

impl Operator {
    pub fn from_operator_input(source: String) -> Self {
        match source.as_str() {
            "==" => Operator::Is,
            "<=" => Operator::Between,
            "in" => Operator::In,
            "has" => Operator::Has,
            other => Operator::Other(other.to_string()),
        }
    }

    pub fn to_condition_json_operator(self) -> String {
        match self {
            Self::Has => "in".to_owned(),
            Self::Is => "==".to_owned(),
            Self::In => "in".to_owned(),
            Self::Between => "<=".to_owned(),
            Self::Other(o) => o,
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Has => f.write_str("has"),
            Self::Is => f.write_str("is"),
            Self::In => f.write_str("in"),
            Self::Between => f.write_str("between"),
            Self::Other(o) => f.write_str(o),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Condition {
    pub variable: String,
    pub expression: Expression,
}

impl Condition {
    pub fn new_with_default_expression(var: String, r#type: SchemaType) -> Self {
        Condition {
            variable: var,
            expression: Expression::Is(r#type.default_value()),
        }
    }

    fn try_var_from_condition_map(
        source: &Map<String, serde_json::Value>,
    ) -> Result<String, &'static str> {
        if let Some(operator) = source.keys().next() {
            return source[operator]
                .as_array()
                .ok_or("Invalid operands list for context")?
                .iter()
                .find_map(|o| {
                    o.as_object().and_then(|v| {
                        v.get("var").and_then(|s| s.as_str().map(|s| s.to_owned()))
                    })
                })
                .ok_or("variable doesn't exist in operands");
        }
        Err("not a valid condition map")
    }

    pub fn try_from_condition_map(
        source: &Map<String, serde_json::Value>,
    ) -> Result<Self, &'static str> {
        let variable = Self::try_var_from_condition_map(source)?;
        let expression = Expression::try_from_expression_map(source)?;

        Ok(Condition {
            variable,
            expression,
        })
    }

    pub fn try_from_condition_json(
        source: &serde_json::Value,
    ) -> Result<Self, &'static str> {
        let obj = source
            .as_object()
            .ok_or("not a valid condition value, should be an object")?;
        Self::try_from_condition_map(obj)
    }

    pub fn to_condition_json(&self) -> serde_json::Value {
        self.expression.to_expression_json(&self.variable)
    }

    pub fn to_condition_query_str(&self) -> Option<String> {
        self.expression.to_expression_query_str(&self.variable)
    }
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
    pub fn includes(&self, variable: &String) -> bool {
        self.iter().any(|c| c.variable == *variable)
    }

    pub fn from_context_json(
        context: &Map<String, serde_json::Value>,
    ) -> Result<Self, &'static str> {
        Ok(Conditions(match context.get("and") {
            Some(v) => v
                .as_array()
                .ok_or("failed to parse value of and as array")
                .and_then(|arr| {
                    arr.iter()
                        .map(Condition::try_from_condition_json)
                        .collect::<Result<Vec<Condition>, &'static str>>()
                })?,
            None => Condition::try_from_condition_map(context).map(|v| vec![v])?,
        }))
    }
    pub fn as_context_json(&self) -> Map<String, Value> {
        if self.is_empty() {
            return Map::new();
        }
        Map::from_iter([(
            String::from("and"),
            Value::Array(
                self.iter()
                    .map(|v| v.to_condition_json())
                    .collect::<Vec<serde_json::Value>>(),
            ),
        )])
    }
    pub fn as_query_string(&self) -> String {
        self.iter()
            .filter_map(|v| v.to_condition_query_str())
            .collect::<Vec<String>>()
            .join("&")
    }
}

impl FromIterator<Condition> for Conditions {
    fn from_iter<T: IntoIterator<Item = Condition>>(iter: T) -> Self {
        Conditions(iter.into_iter().collect::<Vec<Condition>>())
    }
}

impl TryFrom<&Context> for Conditions {
    type Error = &'static str;
    fn try_from(context: &Context) -> Result<Self, Self::Error> {
        Self::from_context_json(context.condition.as_ref())
    }
}
