use std::collections::{HashMap, HashSet};

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
                logic::partial_apply(context.get_condition(), dimension_data)
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

    fn filter_by_context_any_match(
        contexts: Vec<Self>,
        query_keys: &HashSet<String>,
        dimensions_info: &HashMap<String, DimensionInfo>,
    ) -> Vec<Self> {
        contexts
            .into_iter()
            .filter(|context| {
                context.get_condition().keys().any(|context_key| {
                    query_keys.contains(context_key)
                        || query_keys.iter().any(|query_key| {
                            dimensions_info
                                .get(query_key)
                                .map(|info| {
                                    info.dependency_graph.contains_key(context_key)
                                })
                                .unwrap_or_default()
                        })
                })
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};

    use serde_json::{Map, Value};

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
    use crate::config::{Context, OverrideWithKeys};
    use crate::database::models::cac::{DependencyGraph, DimensionType};
    use crate::{Cac, Condition, DimensionInfo};

    use super::Contextual;

    fn value_map(entries: &[(&str, &str)]) -> Map<String, Value> {
        entries
            .iter()
            .map(|(key, value)| ((*key).to_string(), Value::String((*value).to_string())))
            .collect()
    }

    fn condition(entries: &[(&str, &str)]) -> Condition {
        Cac::<Condition>::try_from(value_map(entries))
            .unwrap()
            .into_inner()
    }

    fn context(context_id: &str, entries: &[(&str, &str)]) -> Context {
        Context {
            id: context_id.to_string(),
            condition: condition(entries),
            priority: 0,
            weight: 0,
            override_with_keys: OverrideWithKeys::new(format!("override_{context_id}")),
        }
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

    #[test]
    fn filter_by_context_any_match_keeps_contexts_with_any_matching_query_dimension() {
        let contexts = vec![
            context("ctx_omp", &[("O", "1"), ("M", "1"), ("P", "1")]),
            context("ctx_o", &[("O", "1")]),
            context("ctx_q", &[("Q", "1")]),
            context("ctx_ox", &[("O", "1"), ("X", "1")]),
            context("ctx_mp", &[("M", "1"), ("P", "1")]),
            context("ctx_x", &[("X", "1")]),
        ];
        let query_keys = ["O".to_string(), "M".to_string(), "P".to_string()]
            .into_iter()
            .collect::<HashSet<_>>();

        let filtered = Contextual::filter_by_context_any_match(
            contexts,
            &query_keys,
            &HashMap::new(),
        );
        let ids = filtered
            .into_iter()
            .map(|context| context.id)
            .collect::<Vec<_>>();

        assert_eq!(ids, vec!["ctx_omp", "ctx_o", "ctx_ox", "ctx_mp"]);
    }

    #[test]
    fn filter_by_context_any_match_returns_empty_for_empty_query_keys() {
        let contexts = vec![context("ctx_o", &[("O", "1")])];

        let filtered = Contextual::filter_by_context_any_match(
            contexts,
            &HashSet::new(),
            &HashMap::new(),
        );

        assert!(filtered.is_empty());
    }

    #[test]
    fn filter_by_context_any_match_uses_dependency_graph() {
        let contexts = vec![
            context("ctx_cohort", &[("merchant_cohort", "food")]),
            context("ctx_theme", &[("theme", "dark")]),
        ];
        let query_keys = ["cid".to_string()].into_iter().collect::<HashSet<_>>();
        let dimensions_info = HashMap::from([(
            "cid".to_string(),
            DimensionInfo {
                dependency_graph: DependencyGraph(HashMap::from([(
                    "merchant_cohort".to_string(),
                    vec!["cid".to_string()],
                )])),
                dimension_type: DimensionType::Regular {},
                ..Default::default()
            },
        )]);

        let filtered = Contextual::filter_by_context_any_match(
            contexts,
            &query_keys,
            &dimensions_info,
        );
        let ids = filtered
            .into_iter()
            .map(|context| context.id)
            .collect::<Vec<_>>();

        assert_eq!(ids, vec!["ctx_cohort"]);
    }
}
