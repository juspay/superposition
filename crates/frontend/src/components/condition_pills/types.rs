use std::fmt::Display;

use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use superposition_types::Context;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ConditionOperator {
    Is,
    In,
    Has,
    Between,
    Other(String),
}

impl Display for ConditionOperator {
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

impl From<String> for ConditionOperator {
    fn from(op: String) -> Self {
        match op.as_str() {
            "==" => ConditionOperator::Is,
            "<=" => ConditionOperator::Between,
            "in" => ConditionOperator::In,
            "has" => ConditionOperator::Has,
            other => ConditionOperator::Other(other.to_string()),
        }
    }
}

impl From<(String, &Vec<Value>)> for ConditionOperator {
    fn from(value: (String, &Vec<Value>)) -> Self {
        let (operator, operands) = value;
        let operand_0 = operands.first();
        let operand_1 = operands.get(1);
        let operand_2 = operands.get(2);
        match (operator.as_str(), operand_0, operand_1, operand_2) {
            // assuming there will be only two operands, one with the dimension name and other with the value
            ("==", _, _, None) => ConditionOperator::Is,
            ("<=", Some(_), Some(Value::Object(a)), Some(_)) if a.contains_key("var") => {
                ConditionOperator::Between
            }
            // assuming there will be only two operands, one with the dimension name and other with the value
            ("in", Some(Value::Object(a)), Some(_), None) if a.contains_key("var") => {
                ConditionOperator::In
            }
            // assuming there will be only two operands, one with the dimension name and other with the value
            ("in", Some(_), Some(Value::Object(a)), None) if a.contains_key("var") => {
                ConditionOperator::Has
            }
            _ => ConditionOperator::Other(operator),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Condition {
    pub left_operand: String,
    pub operator: ConditionOperator,
    pub right_operand: Vec<Value>,
}

#[derive(Default, Clone)]
pub struct Conditions(pub Vec<Condition>);

impl TryFrom<&Map<String, Value>> for Condition {
    type Error = &'static str;
    fn try_from(source: &Map<String, Value>) -> Result<Self, Self::Error> {
        if let Some(operator) = source.keys().next() {
            let emty_vec = vec![];
            let operands = source[operator].as_array().unwrap_or(&emty_vec);

            let operator = ConditionOperator::from((operator.to_owned(), operands));

            let dimension_name = operands
                .iter()
                .find_map(|item| match item.as_object() {
                    Some(o) if o.contains_key("var") => {
                        Some(o["var"].as_str().unwrap_or(""))
                    }
                    _ => None,
                })
                .unwrap_or("");

            return Ok(Condition {
                operator,
                left_operand: dimension_name.to_owned(),
                right_operand: operands
                    .to_vec()
                    .into_iter()
                    .filter(|operand| {
                        !(operand.is_object() && operand.get("var").is_some())
                    })
                    .collect(),
            });
        }

        Err("not a valid condition map")
    }
}

impl TryFrom<&Value> for Condition {
    type Error = &'static str;
    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        let obj = value
            .as_object()
            .ok_or("not a valid condition value, should be an object")?;
        Condition::try_from(obj)
    }
}

impl TryFrom<&Context> for Conditions {
    type Error = &'static str;
    fn try_from(context: &Context) -> Result<Self, Self::Error> {
        let obj: Map<String, Value> = context.condition.clone().into();
        match obj.get("and") {
            Some(v) => v
                .as_array()
                .ok_or("failed to parse value of and as array")
                .and_then(|arr| {
                    arr.iter()
                        .map(Condition::try_from)
                        .collect::<Result<Vec<Condition>, &'static str>>()
                }),
            None => Condition::try_from(&obj).map(|v| vec![v]),
        }
        .map(Self)
    }
}
