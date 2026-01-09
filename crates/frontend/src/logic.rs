use std::fmt::Display;

use serde::{Deserialize, Serialize};
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
    Has(serde_json::Value),
}

impl Expression {
    fn to_expression_query_str(&self, var: &str) -> Option<String> {
        match self {
            Expression::Is(c) => Some(format!("{var}={value}", value = c.html_display())),
            _ => None,
        }
    }

    pub fn to_operator(&self) -> Operator {
        Operator::from(self)
    }

    pub fn to_value(&self) -> &Value {
        match self {
            Expression::Is(c) | Expression::Has(c) => c,
        }
    }
}

impl From<(SchemaType, Operator)> for Expression {
    fn from((r#type, operator): (SchemaType, Operator)) -> Self {
        match operator {
            Operator::Is => Expression::Is(r#type.default_value()),
            Operator::Has => Expression::Has(r#type.default_value()),
        }
    }
}

/// This a copy of `Expression` enum, to prevent copying/moving of data
/// where just the operator information is needed.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Operator {
    Is,
    Has,
}

impl From<&Expression> for Operator {
    fn from(value: &Expression) -> Self {
        match value {
            Expression::Is(_) => Operator::Is,
            Expression::Has(_) => Operator::Has,
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Has => f.write_str("has"),
            Self::Is => f.write_str("=="),
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

    pub fn as_context_json(&self) -> Map<String, Value> {
        self.iter()
            .map(|c| (c.variable.clone(), c.expression.to_value().clone()))
            .collect()
    }

    pub fn as_resolve_context(&self) -> Map<String, Value> {
        self.iter()
            .map(|c| (c.variable.clone(), c.expression.to_value().clone()))
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
        ) -> Result<Condition, &'static str> {
            let variable = try_var_from_condition_map(source)?;
            let expression = expression::try_from_expression_map(source)?;

            Ok(Condition {
                variable,
                expression,
            })
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
            expression::to_expression_json(&value.expression, &value.variable)
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

        pub(super) fn try_from_expression_map(
            source: &Map<String, serde_json::Value>,
        ) -> Result<Expression, &'static str> {
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
                    ("in", Some(o1), Some(o2), None)
                        if is_variable(o2) && is_constant(o1) =>
                    {
                        return Ok(Expression::Has(o1.clone()));
                    }
                    _ => return Err("unsupported operator"),
                }
            }

            Err("not a valid expression map")
        }

        pub(crate) fn to_expression_json(
            expr: &Expression,
            var: &str,
        ) -> serde_json::Value {
            match expr {
                Expression::Is(c) => {
                    json!({
                        "==": [
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
            }
        }
    }
}
