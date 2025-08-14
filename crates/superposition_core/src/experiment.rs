use std::collections::HashSet;

use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use superposition_types::database::models::experimentation::{
    Variant, VariantType, Variants,
};
use superposition_types::{Condition, Overridden};

use std::fmt;

pub trait MapError<T> {
    fn map_err_to_string(self) -> Result<T, String>;
}

impl<T, E> MapError<T> for Result<T, E>
where
    E: fmt::Display,
{
    fn map_err_to_string(self) -> Result<T, String> {
        self.map_err(|e| e.to_string())
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, uniffi::Record)]
pub struct FfiExperiment {
    pub id: String,
    pub traffic_percentage: u8,
    pub variants: Variants,
    pub context: Condition,
}

#[derive(Serialize, Deserialize, Debug, uniffi::Record)]
pub struct ExperimentationArgs {
    pub experiments: Vec<FfiExperiment>,
    // Named as per OpenFeature verbiage.
    pub targeting_key: String,
}

pub type Experiments = Vec<FfiExperiment>;

pub fn get_applicable_variants(
    experiments: &Experiments,
    query_data: &Map<String, Value>,
    toss: i8,
    prefix: Option<Vec<String>>,
) -> Result<Vec<String>, String> {
    let experiments: Vec<FfiExperiment> =
        get_satisfied_experiments(experiments, query_data, prefix)?;
    let mut variants: Vec<String> = Vec::new();
    for exp in experiments {
        if let Some(v) = decide_variant(exp.traffic_percentage, exp.variants, toss)? {
            variants.push(v.id)
        }
    }
    Ok(variants)
}

pub fn get_satisfied_experiments(
    experiments: &Experiments,
    context: &Map<String, Value>,
    filter_prefixes: Option<Vec<String>>,
) -> Result<Experiments, String> {
    let running_experiments = experiments
        .iter()
        .filter(|exp| {
            cfg_if::cfg_if! {
                if #[cfg(feature = "jsonlogic")] {
                    exp.context.is_empty()
                        || jsonlogic::apply(
                            &Value::Object(exp.context.clone().into()),
                            &Value::Object(context.clone()),
                        ) == Ok(Value::Bool(true))
                } else {
                    superposition_types::partial_apply(&exp.context, context)
                }
            }
        })
        .cloned()
        .collect();

    if let Some(prefix_list) = filter_prefixes {
        return Ok(filter_experiments_by_prefix(
            running_experiments,
            prefix_list,
        ));
    }

    Ok(running_experiments)
}

fn filter_experiments_by_prefix(
    experiments: Experiments,
    filter_prefixes: Vec<String>,
) -> Experiments {
    let prefix_list: HashSet<String> = HashSet::from_iter(filter_prefixes);
    experiments
        .into_iter()
        .filter_map(|experiment| {
            let variants: Vec<_> = experiment
                .variants
                .into_inner()
                .into_iter()
                .filter_map(|mut variant| {
                    Variant::filter_keys_by_prefix(&variant, &prefix_list)
                        .map(|filtered_overrides_map| {
                            variant.overrides = filtered_overrides_map;
                            variant
                        })
                        .ok()
                })
                .collect();

            if !variants.is_empty() {
                Some(FfiExperiment {
                    variants: Variants::new(variants),
                    ..experiment
                })
            } else {
                None // Skip this experiment
            }
        })
        .collect()
}

fn decide_variant(
    traffic: u8,
    applicable_variants: Variants,
    toss: i8,
) -> Result<Option<Variant>, String> {
    if toss < 0 {
        for variant in applicable_variants.iter() {
            if variant.variant_type == VariantType::EXPERIMENTAL {
                return Ok(Some(variant.clone()));
            }
        }
    }
    let variant_count = applicable_variants.len() as u8;
    let range = (traffic * variant_count) as i32;
    if (toss as i32) >= range {
        return Ok(None);
    }
    let buckets = (1..=variant_count)
        .map(|i| (traffic * i) as i8)
        .collect::<Vec<i8>>();
    let index = buckets
        .into_iter()
        .position(|x| toss < x)
        .ok_or_else(|| "Unable to fetch variant's index".to_string())
        .map_err_to_string()?;
    Ok(applicable_variants.get(index).cloned())
}
