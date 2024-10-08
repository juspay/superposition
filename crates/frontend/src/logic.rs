use std::fmt::Display;

use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};

use crate::{
    schema::{HtmlDisplay, SchemaType},
    types::Context,
};
use derive_more::{Deref, DerefMut};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Operand {
    Value(serde_json::Value),
    Dimension(serde_json::Value),
}

impl Operand {
    pub fn from_operand_json(value: Value) -> Self {
        match value {
            Value::Object(ref o) if o.contains_key("var") => Operand::Dimension(value),
            v => Operand::Value(v),
        }
    }
}

#[derive(Debug, Clone, Deref, DerefMut, Serialize, Deserialize)]
pub struct Operands(pub Vec<Operand>);

impl FromIterator<Value> for Operands {
    fn from_iter<T: IntoIterator<Item = Value>>(iter: T) -> Self {
        Operands(
            iter.into_iter()
                .map(Operand::from_operand_json)
                .collect::<Vec<Operand>>(),
        )
    }
}

impl TryFrom<(&Operator, &str, &SchemaType)> for Operands {
    type Error = String;
    fn try_from(
        (operator, d_name, r#type): (&Operator, &str, &SchemaType),
    ) -> Result<Self, Self::Error> {
        match operator {
            Operator::Is => Ok(Operands(vec![
                Operand::Dimension(json!({ "var": d_name })),
                Operand::Value(r#type.default_value()),
            ])),
            Operator::In => Ok(Operands(vec![
                Operand::Dimension(json!({ "var": d_name })),
                Operand::Value(r#type.default_value()),
            ])),
            Operator::Has => Ok(Operands(vec![
                Operand::Value(r#type.default_value()),
                Operand::Dimension(json!({ "var": d_name })),
            ])),
            Operator::Between => Ok(Operands(vec![
                Operand::Value(r#type.default_value()),
                Operand::Dimension(json!({ "var": d_name })),
                Operand::Value(r#type.default_value()),
            ])),
            // TODO fix this as there will be cases of unsupported operators
            _ => Err(String::from("unsupported operator")),
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
            Self::Has => "has".to_owned(),
            Self::Is => "is".to_owned(),
            Self::In => "in".to_owned(),
            Self::Between => "between".to_owned(),
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

impl From<(String, &Operands)> for Operator {
    fn from((operator, operands): (String, &Operands)) -> Self {
        let operand_0 = operands.first();
        let operand_1 = operands.get(1);
        let operand_2 = operands.get(2);
        match (operator.as_str(), operand_0, operand_1, operand_2) {
            // assuming there will be only two operands, one with the dimension name and other with the value
            ("==", _, _, None) => Operator::Is,
            ("<=", Some(_), Some(Operand::Dimension(_)), Some(_)) => Operator::Between,
            // assuming there will be only two operands, one with the dimension name and other with the value
            ("in", Some(Operand::Dimension(_)), Some(_), None) => Operator::In,
            // assuming there will be only two operands, one with the dimension name and other with the value
            ("in", Some(_), Some(Operand::Dimension(_)), None) => Operator::Has,
            _ => Operator::Other(operator),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Condition {
    pub dimension: String,
    pub operator: Operator,
    pub operands: Operands,
}

impl Condition {
    fn try_dimension_name_from_operands(
        operands: &Operands,
    ) -> Result<String, &'static str> {
        for operand in operands.iter() {
            if let Operand::Dimension(Value::Object(var)) = operand {
                if let Some(d_val) = var.get("var") {
                    return d_val
                        .as_str()
                        .map(|v| v.to_owned())
                        .ok_or("Not a valid dimension name string");
                }
            }
        }
        Err("Dimension doesn't exist in operands list")
    }
    fn try_from_condition_map(source: &Map<String, Value>) -> Result<Self, &'static str> {
        if let Some(operator) = source.keys().next() {
            let operands = Operands::from_iter(
                source[operator]
                    .as_array()
                    .cloned()
                    .ok_or("Invalid operands list for context")?,
            );

            let operator = Operator::from((operator.to_owned(), &operands));

            let dimension_name = Self::try_dimension_name_from_operands(&operands)?;

            return Ok(Condition {
                operator,
                dimension: dimension_name,
                operands: operands.to_owned(),
            });
        }

        Err("not a valid condition map")
    }

    pub fn try_from_condition_json(source: &Value) -> Result<Self, &'static str> {
        let obj = source
            .as_object()
            .ok_or("not a valid condition value, should be an object")?;
        Condition::try_from_condition_map(obj)
    }

    pub fn to_condition_json(self) -> Value {
        let operator = self.operator.to_condition_json_operator();

        let operands = self
            .operands
            .iter()
            .map(|v| match v {
                Operand::Dimension(d) => d.clone(),
                Operand::Value(v) => v.clone(),
            })
            .collect::<Vec<Value>>();

        json!({ operator: operands })
    }

    pub fn to_condition_query_str(self) -> String {
        let operator = self.operator.to_condition_json_operator();
        let dimension = self.dimension;

        let value = self
            .operands
            .iter()
            .filter_map(|operand| {
                if let Operand::Value(v) = operand {
                    return Some(v.to_owned().html_display());
                }
                None
            })
            .collect::<Vec<String>>()
            .join(",");

        format!("{}{}{}", dimension, operator, value)
    }
}

impl TryFrom<(Operator, String, SchemaType)> for Condition {
    type Error = String;
    fn try_from(
        (operator, d_name, r#type): (Operator, String, SchemaType),
    ) -> Result<Self, Self::Error> {
        Ok(Condition {
            dimension: d_name.clone(),
            operator: operator.clone(),
            operands: Operands::try_from((&operator, d_name.as_str(), &r#type))?,
        })
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
pub struct Conditions(pub Vec<Condition>);

impl Conditions {
    pub fn from_context_json(context: Value) -> Result<Self, &'static str> {
        Ok(Conditions(
            context
                .as_object()
                .ok_or("failed to parse context.condition as an object")
                .and_then(|obj| match obj.get("and") {
                    Some(v) => v
                        .as_array()
                        .ok_or("failed to parse value of and as array")
                        .and_then(|arr| {
                            arr.iter()
                                .map(Condition::try_from_condition_json)
                                .collect::<Result<Vec<Condition>, &'static str>>()
                        }),
                    None => Condition::try_from_condition_map(obj).map(|v| vec![v]),
                })?,
        ))
    }
    pub fn to_context_json(self) -> Value {
        json!({
            "and": self.iter().map(|v| Condition::to_condition_json(v.clone())).collect::<Vec<Value>>()
        })
    }
    pub fn to_query_string(self) -> String {
        self.iter()
            .map(|condition| condition.clone().to_condition_query_str())
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
        Self::from_context_json(context.condition.clone())
    }
}
