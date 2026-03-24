use std::collections::HashSet;

use serde_json::{Map, Value};

use crate::{database::models::experimentation::Variant, logic, Condition, Overridden};

pub trait Experimental: Clone {
    fn get_condition(&self) -> &Condition;

    fn filter_by_eval(
        experiments: Vec<Self>,
        dimension_data: &Map<String, Value>,
    ) -> Vec<Self> {
        experiments
            .into_iter()
            .filter(|exp| {
                let condition = exp.get_condition();
                if condition.is_empty() {
                    true
                } else {
                    logic::partial_apply(condition, dimension_data)
                }
            })
            .collect()
    }

    fn get_satisfied(
        experiments: Vec<Self>,
        dimension_data: &Map<String, Value>,
    ) -> Vec<Self> {
        experiments
            .into_iter()
            .filter(|exp| logic::apply(exp.get_condition(), dimension_data))
            .collect()
    }
}

pub trait ExperimentalVariants: Experimental {
    fn get_variants_mut(&mut self) -> &mut Vec<Variant>;

    fn filter_keys_by_prefix(
        experiments: Vec<Self>,
        prefix_list: &HashSet<String>,
    ) -> Vec<Self> {
        experiments
            .into_iter()
            .filter_map(|mut experiment| {
                experiment.get_variants_mut().retain_mut(|variant| {
                    Variant::filter_keys_by_prefix(variant, prefix_list)
                        .map(|filtered_overrides_map| {
                            variant.overrides = filtered_overrides_map;
                            true
                        })
                        .unwrap_or(false)
                });

                if !experiment.get_variants_mut().is_empty() {
                    Some(experiment)
                } else {
                    None // Skip this experiment
                }
            })
            .collect()
    }
}
