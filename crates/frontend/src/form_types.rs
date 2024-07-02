use std::str::FromStr;

use crate::{
    types::{Context, Dimension, VariantType},
    utils::get_variable_name_and_value,
};
use derive_more::{Deref, DerefMut};
use serde::{Deserialize, Serialize};

trait Form {
    type Error;
    type Source;
    type ParseOutput;

    fn parse(&self, source: &[Self::Source]) -> Result<Self::ParseOutput, Self::Error>;
    fn validate(&self) -> bool;
}

#[derive(Debug, Clone, strum_macros::EnumString)]
#[strum(serialize_all = "lowercase")]
pub enum SchemaType {
    Boolean,
    Number,
    String,
    Integer,
    Array,
    Object,
    Null,
}

#[derive(Debug, Clone, Deref, DerefMut, Deserialize, Serialize)]
pub struct ContextForm(pub Vec<(String, String, String)>);

#[derive(Debug, Clone, Deref, DerefMut, Deserialize, Serialize)]
pub struct OverrideForm(pub Vec<(String, serde_json::Value)>);

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct VariantForm {
    pub overrides: OverrideForm,
    pub variant_type: VariantType,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ExperimentForm {
    pub name: String,
    pub context: ContextForm,
    pub variants: Vec<VariantForm>,
}

pub trait FormParseSource {
    fn get_schema(&self) -> Option<serde_json::Value>;
    fn get_possible_type(&self) -> Option<Vec<SchemaType>> {
        // TODO: give a default implementation for the function
        use serde_json::Value;

        let t = self
            .get_schema()
            .map(|s| s.get("type").cloned())
            .flatten()?;
        match t {
            Value::Array(types) => types
                .iter()
                .map(|item: &Value| {
                    item.as_str()
                        .map(|s| SchemaType::from_str(s).ok())
                        .flatten()
                })
                .collect::<Option<Vec<SchemaType>>>(),
            Value::String(type_str) => SchemaType::from_str(type_str.as_str())
                .ok()
                .map(|v| vec![v]),
            _ => None,
        }
    }
}

impl TryFrom<Context> for ContextForm {
    type Error = String;

    fn try_from(value: Context) -> Result<Self, Self::Error> {
        let context = value.condition;

        let conditions = match context.get("and") {
            Some(conditions_json) => conditions_json
                .as_array()
                .ok_or("An error occurred while extracting dimensions: failed parsing conditions as an array".to_string())?
                .clone(),
            None => vec![context.clone()],
        };

        let mut condition_tuples = Vec::new();
        for condition in &conditions {
            let condition_obj = condition
                .as_object()
                .ok_or("failed to parse condition as an object".to_string())?;
            let operators = condition_obj.keys();

            for operator in operators {
                let operands = condition_obj[operator]
                    .as_array()
                    .ok_or("failed to parse operands as an arrays".to_string())?;

                let (variable_name, variable_value) =
                    get_variable_name_and_value(operands)?;

                condition_tuples.push((
                    String::from(variable_name),
                    operator.to_owned(),
                    variable_value.to_owned(),
                ));
            }
        }
        Ok(ContextForm(condition_tuples))
    }
}

impl Form for ContextForm {
    type Error = String;
    type Source = Dimension;
    type ParseOutput = Vec<(String, String, serde_json::Value)>;

    fn parse(&self, source: &[Self::Source]) -> Result<Self::ParseOutput, Self::Error> {
        // TODO: implement this function
        // self.iter().map(|(dimension_name, opeator, value)| {
        //     let dimension_type = source.iter().find_map(|d| {
        //         if d.dimension.as_str() == dimension_name {
        //             return d.get_possible_type();
        //         }
        //         None
        //     });
        //     match (dimension_type, dimension_name, opeator.as_str(), value) {
        //         (None, _, _, _) => None,
        //         (Some(possible_type), n, "==", v) => None,
        //         (Some(possible_type), n, "<=", v) => None,
        //         (Some(possible_type), n, "in", v) => None,
        //     }
        // });
        Ok(vec![(
            String::new(),
            String::new(),
            serde_json::json!("{}"),
        )])
    }
    fn validate(&self) -> bool {
        // TODO: implement this function
        true
    }
}
