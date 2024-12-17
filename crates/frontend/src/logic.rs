use std::fmt::Display;

use serde::{Deserialize, Serialize};
use serde_json::{json, Map};

use crate::schema::{HtmlDisplay, SchemaType};
use superposition_types::Context;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Variable(pub String);
impl Variable {
    pub fn is_variable(v: &serde_json::Value) -> bool {
        v.is_object() && v.as_object().unwrap().contains_key("var")
    }

    pub fn variable_name_from_json(v: &serde_json::Value) -> Option<String> {
        v.as_object()
            .map(|o| {
                o.get("var")
                    .map(|s| s.as_str().map(|s| s.to_owned()))
                    .flatten()
            })
            .flatten()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Constant(pub serde_json::Value);
impl Constant {
    pub fn is_constant(v: &serde_json::Value) -> bool {
        !Variable::is_variable(v)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Expression {
    Is(Variable, Constant),
    In(Variable, Constant),
    Has(Constant, Variable),
    Between(Constant, Variable, Constant),
    Other(String, Vec<serde_json::Value>),
}

impl Expression {
    pub fn is(dimension_name: &str, r#type: SchemaType) -> Self {
        Expression::Is(
            Variable(dimension_name.to_string()),
            Constant(r#type.default_value()),
        )
    }
    pub fn r#in(dimension_name: &str, r#type: SchemaType) -> Self {
        Expression::In(
            Variable(dimension_name.to_string()),
            Constant(r#type.default_value()),
        )
    }
    pub fn has(dimension_name: &str, r#type: SchemaType) -> Self {
        Expression::Has(
            Constant(r#type.default_value()),
            Variable(dimension_name.to_string()),
        )
    }
    pub fn between(dimension_name: &str, r#type: SchemaType) -> Self {
        Expression::Between(
            Constant(r#type.default_value()),
            Variable(dimension_name.to_string()),
            Constant(r#type.default_value()),
        )
    }
    pub fn variable_name(&self) -> String {
        match self {
            Expression::Is(Variable(v), _) => v.to_owned(),
            Expression::In(Variable(v), _) => v.to_owned(),
            Expression::Has(_, Variable(v)) => v.to_owned(),
            Expression::Between(_, Variable(v), _) => v.to_owned(),
            Expression::Other(_, operands) => {
                let mut var = String::new();
                for operand in operands.iter() {
                    if let serde_json::Value::Object(v) = operand {
                        if let Some(name) = v.get("var") {
                            var = name
                                .as_str()
                                .map(|v| v.to_owned())
                                .ok_or(format!(
                                    "failed to get dimension name from expression {:?}",
                                    self
                                ))
                                .unwrap();
                        }
                    }
                }
                var
            }
        }
    }
    pub fn try_from_expression_map(
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
                    if Variable::is_variable(o1) && Constant::is_constant(o2) =>
                {
                    let var = Variable::variable_name_from_json(o1).unwrap();
                    return Ok(Expression::Is(Variable(var), Constant(o2.clone())));
                }
                ("<=", Some(o1), Some(o2), Some(o3))
                    if Variable::is_variable(o2)
                        && Constant::is_constant(o1)
                        && Constant::is_constant(o3) =>
                {
                    let var = Variable::variable_name_from_json(o2).unwrap();
                    return Ok(Expression::Between(
                        Constant(o1.clone()),
                        Variable(var),
                        Constant(o3.clone()),
                    ));
                }
                ("in", Some(o1), Some(o2), None)
                    if Variable::is_variable(o1) && Constant::is_constant(o2) =>
                {
                    let var = Variable::variable_name_from_json(o1).unwrap();
                    return Ok(Expression::In(Variable(var), Constant(o2.clone())));
                }
                ("in", Some(o1), Some(o2), None)
                    if Variable::is_variable(o2) && Constant::is_constant(o1) =>
                {
                    let var = Variable::variable_name_from_json(o2).unwrap();
                    return Ok(Expression::Has(Constant(o1.clone()), Variable(var)));
                }
                _ => {
                    return Ok(Expression::Other(operator.clone(), operands.clone()));
                }
            }
        }

        Err("not a valid expression map")
    }
    pub fn try_from_expression_json(
        source: &serde_json::Value,
    ) -> Result<Self, &'static str> {
        let obj = source
            .as_object()
            .ok_or("not a valid expression value, should be an object")?;
        Expression::try_from_expression_map(obj)
    }
    pub fn to_expression_json(&self) -> serde_json::Value {
        match self {
            Expression::Is(Variable(v), Constant(c)) => {
                json!({
                    "==": [
                        {"var": v},
                        c
                    ]
                })
            }
            Expression::In(Variable(v), Constant(c)) => {
                json!({
                    "in": [
                        {"var": v},
                        c
                    ]
                })
            }
            Expression::Has(Constant(c), Variable(v)) => {
                json!({
                    "in": [
                        c,
                        {"var": v}
                    ]
                })
            }
            Expression::Between(Constant(c1), Variable(v), Constant(c2)) => {
                json!({
                    "<=": [
                        c1,
                        {"var": v},
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
    pub fn to_expression_query_str(&self) -> String {
        match self {
            Expression::Is(Variable(v), Constant(c)) => {
                format!("{}{}{}", v, "==", c.html_display())
            }
            Expression::In(Variable(v), Constant(c)) => {
                format!("{}{}{}", v, "in", c.html_display())
            }
            Expression::Has(Constant(c), Variable(v)) => {
                format!("{}{}{}", v, "in", c.html_display())
            }
            Expression::Between(Constant(c1), Variable(v), Constant(c2)) => {
                let c_str = format!("{},{}", c1.html_display(), c2.html_display());
                format!("{}{}{}", v, "<=", c_str)
            }
            Expression::Other(operator, operands) => {
                let name = self.variable_name();
                let c_str = operands
                    .iter()
                    .filter_map(|operand| {
                        if Variable::is_variable(operand) {
                            return None;
                        }
                        Some(operand.html_display())
                    })
                    .collect::<Vec<String>>()
                    .join(",");
                format!("{}{}{}", name, operator, c_str)
            }
        }
    }
    pub fn to_constants_vec(&self) -> Vec<serde_json::Value> {
        match self {
            Expression::Is(_, Constant(c))
            | Expression::In(_, Constant(c))
            | Expression::Has(Constant(c), _) => {
                vec![c.clone()]
            }
            Expression::Between(Constant(c1), _, Constant(c2)) => {
                vec![c1.clone(), c2.clone()]
            }
            Expression::Other(_, operands) => operands
                .iter()
                .filter_map(|operand| {
                    if Variable::is_variable(operand) {
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

impl From<(&str, SchemaType, Operator)> for Expression {
    fn from((dimension_name, r#type, operator): (&str, SchemaType, Operator)) -> Self {
        match operator {
            Operator::Is => Expression::is(dimension_name, r#type),
            Operator::In => Expression::r#in(dimension_name, r#type),
            Operator::Has => Expression::has(dimension_name, r#type),
            Operator::Between => Expression::between(dimension_name, r#type),
            Operator::Other(o) => Expression::Other(o, vec![]),
        }
    }
}

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
            Expression::Is(_, _) => Operator::Is,
            Expression::In(_, _) => Operator::In,
            Expression::Has(_, _) => Operator::Has,
            Expression::Between(_, _, _) => Operator::Between,
            Expression::Other(o, _) => Operator::Other(o.clone()),
        }
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

#[derive(
    Clone,
    Debug,
    derive_more::Deref,
    derive_more::DerefMut,
    Serialize,
    Deserialize,
    Default,
)]
pub struct Conditions(pub Vec<Expression>);

impl Conditions {
    pub fn from_context_json(
        context: &Map<String, serde_json::Value>,
    ) -> Result<Self, &'static str> {
        Ok(Conditions(match context.get("and") {
            Some(v) => v
                .as_array()
                .ok_or("failed to parse value of and as array")
                .and_then(|arr| {
                    arr.iter()
                        .map(Expression::try_from_expression_json)
                        .collect::<Result<Vec<Expression>, &'static str>>()
                })?,
            None => Expression::try_from_expression_map(&context).map(|v| vec![v])?,
        }))
    }
    pub fn to_context_json(self) -> serde_json::Value {
        json!({
            "and": self.iter().map(|v| v.to_expression_json()).collect::<Vec<serde_json::Value>>()
        })
    }
    pub fn to_query_string(self) -> String {
        self.iter()
            .map(|v| v.to_expression_query_str())
            .collect::<Vec<String>>()
            .join("&")
    }
}

impl FromIterator<Expression> for Conditions {
    fn from_iter<T: IntoIterator<Item = Expression>>(iter: T) -> Self {
        Conditions(iter.into_iter().collect::<Vec<Expression>>())
    }
}

impl TryFrom<&Context> for Conditions {
    type Error = &'static str;
    fn try_from(context: &Context) -> Result<Self, Self::Error> {
        Self::from_context_json(&context.condition.as_ref())
    }
}
