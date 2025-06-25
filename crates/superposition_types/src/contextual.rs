use jsonlogic::{expression::Expression, partial_apply, PartialApplyOutcome};
use serde_json::{Map, Value};

use crate::config::Condition;

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
                .is_ok_and(|variables| {
                    dimension_keys
                        .iter()
                        .all(|dimension| variables.contains(dimension))
                })
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use serde_json::{from_value, json};

    use crate::{
        config::tests::{
            get_config, get_dimension_data1, get_dimension_data2, get_dimension_data3,
            get_dimension_filtered_config1, get_dimension_filtered_config2,
            get_dimension_filtered_config3,
        },
        Context,
    };

    use super::Contextual;

    fn get_dimension_filtered_contexts1() -> Vec<Context> {
        let contexts = json!([
            {
                "id": "40c2564c114e1a2036bc6ce0e730289d05e117b051f2d286d6e7c68960f3bc7d",
                "condition": {
                    "==": [
                        {
                            "var": "test3"
                        },
                        true
                    ]
                },
                "priority": 0,
                "weight": 0,
                "override_with_keys": [
                    "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef"
                ]
            },
            {
                "id": "9fbf3b9fa10caaaf31f6003cbd20ed36d40efe73b5c6b238288c0a96e6933500",
                "condition": {
                    "and": [
                        {
                            "==": [
                                {
                                    "var": "test3"
                                },
                                false
                            ]
                        },
                        {
                            "==": [
                                {
                                    "var": "test"
                                },
                                "test"
                            ]
                        }
                    ]
                },
                "priority": 2,
                "weight": 2,
                "override_with_keys": [
                    "e2fa5b38c3a1448cf0e27f9d555fdb8964a686d8ae41b70b55e6ee30359b87c8"
                ]
            }
        ]);

        from_value(contexts).unwrap()
    }

    fn get_dimension_filtered_contexts2() -> Vec<Context> {
        let contexts = json!([{
            "id": "9fbf3b9fa10caaaf31f6003cbd20ed36d40efe73b5c6b238288c0a96e6933500",
            "condition": {
                "and": [
                    {
                        "==": [
                            {
                                "var": "test3"
                            },
                            false
                        ]
                    },
                    {
                        "==": [
                            {
                                "var": "test"
                            },
                            "test"
                        ]
                    }
                ]
            },
            "priority": 2,
            "weight": 2,
            "override_with_keys": [
                "e2fa5b38c3a1448cf0e27f9d555fdb8964a686d8ae41b70b55e6ee30359b87c8"
            ]
        }]);

        from_value(contexts).unwrap()
    }

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
            get_dimension_filtered_config3().contexts
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
