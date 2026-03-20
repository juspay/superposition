use std::collections::HashMap;

use serde_json::{Map, Value};

use crate::config::Condition;
use crate::{logic, DimensionInfo};

pub trait Contextual: Clone {
    fn get_condition(&self) -> &Condition;

    fn filter_by_eval(
        contexts: Vec<Self>,
        dimension_data: &Map<String, Value>,
    ) -> Vec<Self> {
        contexts
            .into_iter()
            .filter(|context| {
                crate::partial_apply(context.get_condition(), dimension_data)
            })
            .collect()
    }

    fn filter_exact_match(
        contexts: Vec<Self>,
        dimension_data: &Map<String, Value>,
    ) -> Vec<Self> {
        contexts
            .into_iter()
            .filter(|context| {
                let condition = context.get_condition();
                logic::apply(condition, dimension_data)
                    && condition.len() == dimension_data.len()
            })
            .collect()
    }

    fn filter_by_dimension(
        contexts: Vec<Self>,
        original_dimension_keys: &[&String],
        dimensions_info: &HashMap<String, DimensionInfo>,
    ) -> Vec<Self> {
        contexts
            .into_iter()
            .filter(|context| {
                let variables = context.get_condition();
                original_dimension_keys.iter().all(|dimension| {
                    variables.contains_key(*dimension)
                        || dimensions_info
                            .get(*dimension)
                            .map(|info| {
                                info.dependency_graph
                                    .keys()
                                    .any(|k| variables.contains_key(k))
                            })
                            .unwrap_or_default()
                })
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
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
                &get_dimension_data1().keys().collect::<Vec<_>>(),
                &config.dimensions
            ),
            get_dimension_filtered_contexts1()
        );

        assert_eq!(
            Contextual::filter_by_dimension(
                config.contexts.clone(),
                &get_dimension_data2().keys().collect::<Vec<_>>(),
                &config.dimensions
            ),
            get_dimension_filtered_contexts2()
        );

        assert_eq!(
            Contextual::filter_by_dimension(
                config.contexts,
                &get_dimension_data3().keys().collect::<Vec<_>>(),
                &config.dimensions
            ),
            Vec::new()
        );
    }
}
