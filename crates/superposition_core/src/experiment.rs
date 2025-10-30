use std::collections::{HashMap, HashSet};
use std::hash::{DefaultHasher, Hash, Hasher};

use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use superposition_types::database::models::experimentation::{
    Bucket, Buckets, GroupType, Variant, Variants,
};
use superposition_types::{logic::evaluate_cohort, Condition, DimensionInfo, Overridden};

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

#[derive(Serialize, Deserialize, Clone, Debug, uniffi::Record)]
pub struct FfiExperimentGroup {
    pub id: String,
    pub context: Condition,
    pub traffic_percentage: u8,
    pub member_experiment_ids: Vec<String>,
    pub group_type: GroupType,
    pub buckets: Buckets,
}

#[derive(Serialize, Deserialize, Debug, uniffi::Record)]
pub struct ExperimentationArgs {
    pub experiments: Vec<FfiExperiment>,
    pub experiment_groups: Vec<FfiExperimentGroup>,
    // Named as per OpenFeature verbiage.
    pub targeting_key: String,
}

pub type Experiments = Vec<FfiExperiment>;

pub type ExperimentGroups = Vec<FfiExperimentGroup>;

pub fn get_applicable_variants(
    dimensions_info: &HashMap<String, DimensionInfo>,
    experiments: &Experiments,
    experiment_groups: &ExperimentGroups,
    query_data: &Map<String, Value>,
    identifier: &str,
    prefix: Option<Vec<String>>,
) -> Result<Vec<String>, String> {
    let context = Value::Object(evaluate_cohort(dimensions_info, query_data));

    let buckets =
        get_applicable_buckets_from_group(experiment_groups, &context, identifier);

    let experiments: HashMap<String, FfiExperiment> = get_satisfied_experiments(
        experiments,
        &context.as_object().cloned().unwrap_or_default(),
        prefix,
    )?
    .into_iter()
    .map(|exp| (exp.id.clone(), exp.clone()))
    .collect();

    let applicable_variants =
        get_applicable_variants_from_group_response(&experiments, &context, &buckets);

    Ok(applicable_variants)
}

pub fn get_applicable_buckets_from_group(
    experiment_groups: &ExperimentGroups,
    context: &Value,
    identifier: &str,
) -> Vec<(usize, Bucket)> {
    if identifier.is_empty() {
        return vec![];
    }

    experiment_groups
        .iter()
        .filter_map(|exp_group| {
            let hashed_percentage = calculate_bucket_index(identifier, &exp_group.id);
            log::info!(
                "Identifier: {}, Experiment Group ID: {}, Hashed Percentage: {}",
                identifier,
                exp_group.id,
                hashed_percentage
            );
            let exp_context = &exp_group.context;

            cfg_if::cfg_if! {
                if #[cfg(feature = "jsonlogic")] {
                    let valid_context = exp_context.is_empty()
                        || jsonlogic::apply(&Value::Object(exp_context.clone().into()), context)
                            == Ok(Value::Bool(true));
                } else {
                    let valid_context = superposition_types::apply(
                        exp_context,
                        &context.as_object().cloned().unwrap_or_default(),
                    );
                }
            }

            let res =
                valid_context && exp_group.traffic_percentage >= hashed_percentage as u8;

            res.then_some(
                exp_group
                    .buckets
                    .get(hashed_percentage)
                    .and_then(Clone::clone),
            )
            .flatten()
            .and_then(|b| {
                if exp_group.group_type == GroupType::SystemGenerated {
                    Some((hashed_percentage, b))
                } else if exp_group.traffic_percentage > 0 {
                    Some((
                        (hashed_percentage * 100) / exp_group.traffic_percentage as usize,
                        b,
                    ))
                } else {
                    None
                }
            })
        })
        .collect()
}

pub fn get_applicable_variants_from_group_response(
    experiments: &HashMap<String, FfiExperiment>,
    context: &Value,
    bucket_response: &[(usize, Bucket)],
) -> Vec<String> {
    bucket_response
        .iter()
        .filter_map(|(toss, bucket)| {
            experiments.get(&bucket.experiment_id).and_then(|exp| {
                cfg_if::cfg_if! {
                    if #[cfg(feature = "jsonlogic")] {
                        let valid_context = exp.context.is_empty()
                            || jsonlogic::apply(
                                &Value::Object(exp.context.clone().into()),
                                context,
                            ) == Ok(Value::Bool(true));
                    } else {
                        let valid_context = superposition_types::apply(
                            &exp.context,
                            &context.as_object().cloned().unwrap_or_default(),
                        );
                    }
                }

                let res = valid_context
                    && (exp.traffic_percentage as usize * exp.variants.len()) >= *toss;

                res.then_some(bucket.variant_id.clone())
            })
        })
        .collect()
}

#[inline]
pub fn calculate_bucket_index(identifier: &str, group_id: &str) -> usize {
    let mut hasher = DefaultHasher::new();
    (identifier, group_id).hash(&mut hasher);
    (hasher.finish() % 100) as usize
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
