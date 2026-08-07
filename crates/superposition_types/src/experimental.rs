use serde_json::{Map, Value};

use crate::{
    database::models::experimentation::Variant, logic, Condition, Overridden, PrefixList,
};

pub trait Experimental: Clone {
    fn get_condition(&self) -> &Condition;

    fn filter_by_eval(
        mut experiments: Vec<Self>,
        dimension_data: &Map<String, Value>,
    ) -> Vec<Self> {
        experiments.retain(|exp| {
            let condition = exp.get_condition();
            if condition.is_empty() {
                true
            } else {
                logic::partial_apply(condition, dimension_data)
            }
        });
        experiments
    }

    fn get_satisfied(
        mut experiments: Vec<Self>,
        dimension_data: &Map<String, Value>,
    ) -> Vec<Self> {
        experiments.retain(|exp| logic::apply(exp.get_condition(), dimension_data));
        experiments
    }
}

pub trait ExperimentalVariants: Experimental {
    fn get_variants_mut(&mut self) -> &mut Vec<Variant>;

    fn filter_keys_by_prefix(
        mut experiments: Vec<Self>,
        prefix_list: &PrefixList,
        exclude_prefix_list: &PrefixList,
    ) -> Vec<Self> {
        experiments.retain_mut(|experiment| {
            experiment.get_variants_mut().retain_mut(|variant| {
                Variant::filter_keys_by_prefix(variant, prefix_list, exclude_prefix_list)
                    .is_ok()
            });

            !experiment.get_variants_mut().is_empty()
        });
        experiments
    }
}
