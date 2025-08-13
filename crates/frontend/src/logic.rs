use std::fmt::Display;

use serde::{Deserialize, Serialize};
#[cfg(feature = "jsonlogic")]
use serde_json::json;
use serde_json::{Map, Value};
use superposition_types::{database::models::cac::Context, Context as ConfigContext};

use crate::schema::{HtmlDisplay, SchemaType};

/// The `Expression` enum represents a JSONLogic expression and ensures
/// the correct construction of expressions by tying operators and operands together.
///
/// This enum provides a way to enforce the proper order and number of operands
/// for each operator, ensuring the resulting JSONLogic expression is well-formed.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Expression {
    Is(serde_json::Value),
    #[cfg(feature = "jsonlogic")]
    In(serde_json::Value),
    Has(serde_json::Value),
    #[cfg(feature = "jsonlogic")]
    Between(serde_json::Value, serde_json::Value),
    #[cfg(feature = "jsonlogic")]
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
    #[cfg(feature = "jsonlogic")]
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

    #[cfg(feature = "jsonlogic")]
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
            Expression::Is(c) | Expression::Has(c) => {
                vec![c.clone()]
            }
            #[cfg(feature = "jsonlogic")]
            Expression::In(c) => {
                vec![c.clone()]
            }
            #[cfg(feature = "jsonlogic")]
            Expression::Between(c1, c2) => {
                vec![c1.clone(), c2.clone()]
            }
            #[cfg(feature = "jsonlogic")]
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

    pub fn to_value(&self) -> Value {
        match self {
            Expression::Is(c) | Expression::Has(c) => c.clone(),
            #[cfg(feature = "jsonlogic")]
            Expression::In(c) => c.clone(),
            #[cfg(feature = "jsonlogic")]
            Expression::Between(c1, c2) => json!([c1, c2]),
            #[cfg(feature = "jsonlogic")]
            Expression::Other(_, operands) => Value::Array(operands.clone()),
        }
    }
}

impl From<(SchemaType, Operator)> for Expression {
    fn from((r#type, operator): (SchemaType, Operator)) -> Self {
        match operator {
            Operator::Is => Expression::Is(r#type.default_value()),
            #[cfg(feature = "jsonlogic")]
            Operator::In => Expression::In(Value::Array(vec![])),
            Operator::Has => Expression::Has(r#type.default_value()),
            #[cfg(feature = "jsonlogic")]
            Operator::Between => {
                Expression::Between(r#type.default_value(), r#type.default_value())
            }
            #[cfg(feature = "jsonlogic")]
            Operator::Other(o) => Expression::Other(o, vec![]),
        }
    }
}

/// This a copy of `Expression` enum, to prevent copying/moving of data
/// where just the operator information is needed.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Operator {
    Is,
    #[cfg(feature = "jsonlogic")]
    In,
    Has,
    #[cfg(feature = "jsonlogic")]
    Between,
    #[cfg(feature = "jsonlogic")]
    Other(String),
}

impl From<&Expression> for Operator {
    fn from(value: &Expression) -> Self {
        match value {
            Expression::Is(_) => Operator::Is,
            #[cfg(feature = "jsonlogic")]
            Expression::In(_) => Operator::In,
            Expression::Has(_) => Operator::Has,
            #[cfg(feature = "jsonlogic")]
            Expression::Between(_, _) => Operator::Between,
            #[cfg(feature = "jsonlogic")]
            Expression::Other(o, _) => Operator::Other(o.clone()),
        }
    }
}

impl From<&Condition> for Operator {
    fn from(value: &Condition) -> Self {
        Self::from(&value.expression)
    }
}

impl Operator {
    #[cfg(feature = "jsonlogic")]
    pub fn from_operator_input(source: String) -> Self {
        match source.as_str() {
            "==" => Operator::Is,
            "<=" => Operator::Between,
            "in" => Operator::In,
            "has" => Operator::Has,
            other => Operator::Other(other.to_string()),
        }
    }

    #[cfg(feature = "jsonlogic")]
    pub fn to_condition_json_operator(&self) -> String {
        match self {
            Self::Has => "in",
            Self::Is => "==",
            Self::In => "in",
            Self::Between => "<=",
            Self::Other(o) => o,
        }
        .to_owned()
    }

    pub fn strict_mode_display(&self) -> String {
        match self {
            Self::Is => "==".to_string(),
            _ => self.to_string(),
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Has => f.write_str("has"),
            Self::Is => f.write_str("is"),
            #[cfg(feature = "jsonlogic")]
            Self::In => f.write_str("in"),
            #[cfg(feature = "jsonlogic")]
            Self::Between => f.write_str("between"),
            #[cfg(feature = "jsonlogic")]
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

    #[cfg(feature = "jsonlogic")]
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

    #[cfg(feature = "jsonlogic")]
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

    #[cfg(feature = "jsonlogic")]
    pub fn try_from_condition_json(
        source: &serde_json::Value,
    ) -> Result<Self, &'static str> {
        let obj = source
            .as_object()
            .ok_or("not a valid condition value, should be an object")?;
        Self::try_from_condition_map(obj)
    }

    #[cfg(feature = "jsonlogic")]
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

    #[cfg(feature = "jsonlogic")]
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

    #[cfg(not(feature = "jsonlogic"))]
    pub fn from_context_json(
        context: &Map<String, serde_json::Value>,
    ) -> Result<Self, &'static str> {
        Ok(Conditions(
            context
                .iter()
                .map(|(k, v)| Condition {
                    variable: k.clone(),
                    expression: if k == "variantIds" {
                        Expression::Has(v.clone())
                    } else {
                        Expression::Is(v.clone())
                    },
                })
                .collect::<Vec<_>>(),
        ))
    }

    pub fn from_resolve_context(context: &Map<String, serde_json::Value>) -> Self {
        Self(
            context
                .iter()
                .map(|(k, v)| Condition {
                    variable: k.clone(),
                    expression: Expression::Is(v.clone()),
                })
                .collect::<Vec<_>>(),
        )
    }

    pub fn try_from_resolve_context_str(context: &str) -> Result<Self, &'static str> {
        let parsed: Value = serde_json::from_str(context)
            .map_err(|_| "failed to parse context string as JSON")?;
        Ok(Self::from_resolve_context(
            parsed.as_object().ok_or("not a valid context JSON")?,
        ))
    }

    #[cfg(feature = "jsonlogic")]
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

    #[cfg(not(feature = "jsonlogic"))]
    pub fn as_context_json(&self) -> Map<String, Value> {
        self.iter()
            .map(|c| (c.variable.clone(), c.expression.to_value()))
            .collect()
    }

    pub fn as_query_string(&self) -> String {
        self.iter()
            .filter_map(|v| v.to_condition_query_str())
            .collect::<Vec<String>>()
            .join("&")
    }

    pub fn as_resolve_context(&self) -> Map<String, Value> {
        self.iter()
            .map(|c| (c.variable.clone(), c.expression.to_value()))
            .collect()
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
        Self::from_context_json(&context.value)
    }
}

impl TryFrom<&ConfigContext> for Conditions {
    type Error = &'static str;
    fn try_from(context: &ConfigContext) -> Result<Self, Self::Error> {
        Self::from_context_json(&context.condition)
    }
}
