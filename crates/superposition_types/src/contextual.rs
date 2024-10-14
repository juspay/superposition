use jsonlogic::{expression::Expression, partial_apply, PartialApplyOutcome};
use serde_json::{Map, Value};

use crate::Condition;

pub trait Contextual: Clone {
    fn get_condition(&self) -> Condition;

    fn filter_by_eval(
        contexts: Vec<Self>,
        dimension_data: &Map<String, Value>,
    ) -> Vec<Self> {
        contexts
            .into_iter()
            .filter(|context| {
                matches!(
                    partial_apply(
                        &Value::Object(context.get_condition().into()),
                        &Value::Object(dimension_data.clone()),
                    ),
                    Ok(PartialApplyOutcome::Resolved(Value::Bool(true)))
                        | Ok(PartialApplyOutcome::Ambiguous)
                )
            })
            .collect()
    }

    fn filter_by_dimension(contexts: Vec<Self>, dimension_keys: &[String]) -> Vec<Self> {
        contexts
            .into_iter()
            .filter(|context| {
                Expression::from_json(&Value::Object(
                    context.clone().get_condition().into(),
                ))
                .and_then(|ast| ast.get_variable_names())
                .map_or(false, |variables| {
                    dimension_keys
                        .iter()
                        .all(|dimension| variables.contains(dimension))
                })
            })
            .collect()
    }
}
