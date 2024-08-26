use crate::{
    types::{Context, VariantType},
    utils::get_variable_name_and_value_2,
};
use derive_more::{Deref, DerefMut};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, strum_macros::EnumString, PartialEq)]
#[strum(serialize_all = "lowercase")]
pub enum SchemaType {
    Boolean,
    Number,
    Pattern,
    Enum(Vec<String>),
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

                let (vname, vvalue) = get_variable_name_and_value_2(operands)?;

                let value = match operator.as_str() {
                    "==" => vvalue
                        .get(0)
                        .map(|v| format!("{v}"))
                        .unwrap_or(String::new()),
                    "<=" => vvalue
                        .iter()
                        .map(|v| format!("{v}"))
                        .collect::<Vec<String>>()
                        .join(","),
                    "in" => vvalue
                        .get(0)
                        .map(|v| format!("{v}"))
                        .unwrap_or(String::new()),
                    _ => String::new(),
                };

                condition_tuples.push((String::from(vname), operator.to_owned(), value));
            }
        }
        Ok(ContextForm(condition_tuples))
    }
}
