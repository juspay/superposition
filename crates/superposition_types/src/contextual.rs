use serde_json::{Map, Value};

use crate::config::Condition;
#[cfg(not(feature = "jsonlogic"))]
use crate::logic;

pub trait Contextual: Clone {
    fn get_condition(&self) -> Condition;

    #[cfg(feature = "jsonlogic")]
    fn filter_by_eval(
        contexts: Vec<Self>,
        dimension_data: &Map<String, Value>,
    ) -> Vec<Self> {
        contexts
            .into_iter()
            .filter(|context| {
                matches!(
                    jsonlogic::partial_apply(
                        &Value::Object(context.get_condition().into()),
                        &Value::Object(dimension_data.clone()),
                    ),
                    Ok(jsonlogic::PartialApplyOutcome::Resolved(Value::Bool(true)))
                        | Ok(jsonlogic::PartialApplyOutcome::Ambiguous)
                )
            })
            .collect()
    }

    #[cfg(not(feature = "jsonlogic"))]
    fn filter_by_eval(
        contexts: Vec<Self>,
        dimension_data: &Map<String, Value>,
    ) -> Vec<Self> {
        contexts
            .into_iter()
            .filter(|context| {
                crate::partial_apply(&context.get_condition().into(), dimension_data)
            })
            .collect()
    }

    #[cfg(feature = "jsonlogic")]
    fn filter_exact_match(
        contexts: Vec<Self>,
        dimension_data: &Map<String, Value>,
    ) -> Vec<Self> {
        contexts
            .into_iter()
            .filter(|context| {
                let condition: Map<String, Value> = context.get_condition().into();
                jsonlogic::apply(
                    &Value::Object(condition.clone()),
                    &Value::Object(dimension_data.clone()),
                ) == Ok(Value::Bool(true))
                    && {
                        let condition_dimensions =
                            jsonlogic::expression::Expression::from_json(&Value::Object(
                                condition,
                            ))
                            .and_then(|ast| ast.get_variable_names())
                            .map(|hs| hs.into_iter().collect::<Vec<_>>())
                            .unwrap_or_default();
                        condition_dimensions.len() == dimension_data.len()
                    }
            })
            .collect()
    }

    #[cfg(not(feature = "jsonlogic"))]
    fn filter_exact_match(
        contexts: Vec<Self>,
        dimension_data: &Map<String, Value>,
    ) -> Vec<Self> {
        contexts
            .into_iter()
            .filter(|context| {
                let condition = context.get_condition().into();
                logic::apply(&condition, dimension_data)
                    && condition.len() == dimension_data.len()
            })
            .collect()
    }

    #[cfg(feature = "jsonlogic")]
    fn filter_by_dimension(contexts: Vec<Self>, dimension_keys: &[String]) -> Vec<Self> {
        contexts
            .into_iter()
            .filter(|context| {
                jsonlogic::expression::Expression::from_json(&Value::Object(
                    context.clone().get_condition().into(),
                ))
                .and_then(|ast| ast.get_variable_names())
                .is_ok_and(|variables| {
                    dimension_keys
                        .iter()
                        .all(|dimension| variables.contains(dimension))
                })
            })
            .collect()
    }

    #[cfg(not(feature = "jsonlogic"))]
    fn filter_by_dimension(contexts: Vec<Self>, dimension_keys: &[String]) -> Vec<Self> {
        contexts
            .into_iter()
            .filter(|context| {
                let variables: Map<String, Value> = context.get_condition().into();
                dimension_keys
                    .iter()
                    .all(|dimension| variables.contains_key(dimension))
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {

    #[cfg(feature = "jsonlogic")]
    use crate::config::tests::jsonlogic::{
        get_dimension_filtered_contexts1, get_dimension_filtered_contexts2,
        with_dimensions::{
            get_config, get_dimension_filtered_config1, get_dimension_filtered_config2,
        },
    };
    #[cfg(not(feature = "jsonlogic"))]
    use crate::config::tests::map::{
        get_dimension_filtered_contexts1, get_dimension_filtered_contexts2,
        with_dimensions::{
            get_config, get_dimension_filtered_config1, get_dimension_filtered_config2,
        },
    };
    use crate::config::tests::{
        get_dimension_data1, get_dimension_data2, get_dimension_data3,
        get_dimension_filtered_config3_with_dimension,
    };

    use super::Contextual;

    #[test]
    fn filter_by_eval() {
        let config = get_config();

        assert_eq!(
            Contextual::filter_by_eval(config.contexts.clone(), &get_dimension_data1()),
            get_dimension_filtered_config1().contexts
        );

        assert_eq!(
            Contextual::filter_by_eval(config.contexts.clone(), &get_dimension_data2()),
            get_dimension_filtered_config2().contexts
        );

        assert_eq!(
            Contextual::filter_by_eval(config.contexts, &get_dimension_data3()),
            get_dimension_filtered_config3_with_dimension().contexts
        );
    }

    #[test]
    fn filter_by_dimension() {
        let config = get_config();

        assert_eq!(
            Contextual::filter_by_dimension(
                config.contexts.clone(),
                &get_dimension_data1().keys().cloned().collect::<Vec<_>>()
            ),
            get_dimension_filtered_contexts1()
        );

        assert_eq!(
            Contextual::filter_by_dimension(
                config.contexts.clone(),
                &get_dimension_data2().keys().cloned().collect::<Vec<_>>()
            ),
            get_dimension_filtered_contexts2()
        );

        assert_eq!(
            Contextual::filter_by_dimension(
                config.contexts,
                &get_dimension_data3().keys().cloned().collect::<Vec<_>>()
            ),
            Vec::new()
        );
    }
}
