use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use std::collections::HashMap;
use superposition_types::{
    api::experiments::ExperimentResponse,
    database::models::experimentation::{Variant, VariantType},
    Overrides,
};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ContextEvaluation {
    Full,
    Partial,
}

impl Default for ContextEvaluation {
    fn default() -> Self {
        Self::Full
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum VariantSelection {
    Simple,
    Bucketed,
    Experimental,
}

impl Default for VariantSelection {
    fn default() -> Self {
        Self::Simple
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, Default)]
pub struct EvaluationOptions {
    pub context_eval: ContextEvaluation,
    pub variant_selection: VariantSelection,
    pub include_reasoning: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ExperimentReasoning {
    pub experiment_id: String,
    pub experiment_name: String,
    pub context_matched: bool,
    pub traffic_passed: bool,
    pub variants_evaluated: Vec<VariantReasoning>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct VariantReasoning {
    pub variant_id: String,
    pub variant_type: VariantType,
    pub context_matched: bool,
    pub selected: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ExperimentResult {
    pub variants: Vec<Variant>,
    pub overrides: Map<String, Value>,
    pub experiments_evaluated: usize,
    pub reasoning: Option<Vec<ExperimentReasoning>>, // Only present if include_reasoning = true
}

// Main evaluation function with optional advanced features
pub fn eval_experiments(
    experiments: &[ExperimentResponse],
    variants: &[Variant],
    overrides: &HashMap<String, Overrides>,
    user_context: &Map<String, Value>,
    toss: i32,
    filter_prefixes: &[String],
    options: Option<EvaluationOptions>, // Optional advanced features
) -> Result<ExperimentResult, String> {
    let opts = options.unwrap_or_default();
    let mut result_variants = Vec::new();
    let mut result_overrides = Map::new();
    let mut experiments_evaluated = 0;
    let mut reasoning_data = if opts.include_reasoning {
        Some(Vec::new())
    } else {
        None
    };

    let user_context_value = Value::Object(user_context.clone());

    for experiment in experiments {
        if !experiment.status.active() {
            continue;
        }
        let mut experiment_reasoning = if opts.include_reasoning {
            Some(ExperimentReasoning {
                experiment_id: experiment.id.clone(),
                experiment_name: experiment.name.clone(),
                context_matched: false,
                traffic_passed: false,
                variants_evaluated: Vec::new(),
            })
        } else {
            None
        };

        // Apply prefix filtering
        if !filter_prefixes.is_empty() {
            let matches_prefix = filter_prefixes.iter().any(|prefix| {
                experiment.name.starts_with(prefix) || experiment.id.starts_with(prefix)
            });
            if !matches_prefix {
                continue;
            }
        }

        // Context evaluation based on strategy
        let context_matches = match opts.context_eval {
            ContextEvaluation::Full => {
                jsonlogic::apply(
                    &Value::Object(experiment.context.clone().into()),
                    &user_context_value,
                ) == Ok(Value::Bool(true))
            }
            ContextEvaluation::Partial => {
                matches!(
                    jsonlogic::partial_apply(
                        &Value::Object(experiment.context.clone().into()),
                        &user_context_value,
                    ),
                    Ok(jsonlogic::PartialApplyOutcome::Resolved(Value::Bool(true)))
                        | Ok(jsonlogic::PartialApplyOutcome::Ambiguous)
                )
            }
        };

        if let Some(ref mut reasoning) = experiment_reasoning {
            reasoning.context_matched = context_matches;
        }

        if !context_matches {
            if let Some(reasoning) = experiment_reasoning {
                if let Some(ref mut reasoning_data) = reasoning_data {
                    reasoning_data.push(reasoning);
                }
            }
            continue;
        }

        // Traffic evaluation
        let user_toss = if toss == -1 { 0 } else { toss };
        let traffic_threshold = (*experiment.traffic_percentage as f64 * 100.0) as i32;
        let traffic_passes = user_toss < traffic_threshold;

        if let Some(ref mut reasoning) = experiment_reasoning {
            reasoning.traffic_passed = traffic_passes;
        }

        if traffic_passes {
            // Find applicable variants for this experiment
            let experiment_variants: Vec<&Variant> = variants
                .iter()
                .filter(|v| v.id == experiment.id) // <-- Remove .to_string()
                .collect();

            let mut applicable_variants = Vec::new();

            // Evaluate each variant's context
            for variant in experiment_variants {
                // For variants, we need to check if they have context
                // Since Variant has context_id instead of context, we'll assume
                // variants without context_id match all contexts
                let variant_context_matches = variant.context_id.is_none() || {
                    // If there's a context_id, we'd need to resolve it
                    // For now, assume it matches (this would need context resolution logic)
                    true
                };

                let variant_reasoning = if opts.include_reasoning {
                    Some(VariantReasoning {
                        variant_id: variant.id.clone(),
                        variant_type: variant.variant_type.clone(),
                        context_matched: variant_context_matches,
                        selected: false,
                    })
                } else {
                    None
                };

                if variant_context_matches {
                    applicable_variants.push(variant);
                }

                if let Some(reasoning) = variant_reasoning {
                    if let Some(ref mut exp_reasoning) = experiment_reasoning {
                        exp_reasoning.variants_evaluated.push(reasoning);
                    }
                }
            }

            // Variant selection based on strategy
            let selected_variants = match opts.variant_selection {
                VariantSelection::Simple => {
                    select_variants_simple(&applicable_variants, toss)
                }
                VariantSelection::Bucketed => select_variants_bucketed(
                    &applicable_variants,
                    *experiment.traffic_percentage,
                    toss,
                ),
                VariantSelection::Experimental => {
                    select_variants_experimental(&applicable_variants, toss)
                }
            };

            // Update reasoning for selected variants
            if let Some(ref mut exp_reasoning) = experiment_reasoning {
                for &selected_variant in &selected_variants {
                    if let Some(variant_reasoning) = exp_reasoning
                        .variants_evaluated
                        .iter_mut()
                        .find(|vr| vr.variant_id == selected_variant.id)
                    {
                        variant_reasoning.selected = true;
                    }
                }
            }

            // Collect selected variants and their overrides
            for &variant in &selected_variants {
                result_variants.push(variant.clone());

                // Collect variant overrides
                let variant_overrides: Map<String, Value> =
                    variant.overrides.clone().into_inner().into();
                for (key, value) in variant_overrides {
                    result_overrides.insert(key, value);
                }
            }

            // Collect experiment-level overrides
            for override_key in &experiment.override_keys {
                if let Some(override_config) = overrides.get(override_key) {
                    let override_map: Map<String, Value> = override_config.clone().into();
                    for (key, value) in override_map {
                        result_overrides.insert(key, value);
                    }
                }
            }

            experiments_evaluated += 1;
        }

        // Add experiment reasoning if enabled
        if let Some(reasoning) = experiment_reasoning {
            if let Some(ref mut reasoning_data) = reasoning_data {
                reasoning_data.push(reasoning);
            }
        }
    }

    Ok(ExperimentResult {
        variants: result_variants,
        overrides: result_overrides,
        experiments_evaluated,
        reasoning: reasoning_data,
    })
}

// Simple variant selection: return all applicable variants
fn select_variants_simple<'a>(
    variants: &'a [&'a Variant],
    _toss: i32,
) -> Vec<&'a Variant> {
    variants.to_vec()
}

// Bucketed variant selection: complex distribution logic from existing client
fn select_variants_bucketed<'a>(
    variants: &'a [&'a Variant],
    traffic_percentage: u8,
    toss: i32,
) -> Vec<&'a Variant> {
    let variant_count = variants.len() as u8;
    let range = (traffic_percentage * variant_count) as i32;

    if toss >= range {
        return Vec::new();
    }

    let buckets: Vec<i32> = (1..=variant_count)
        .map(|i| (traffic_percentage * i) as i32)
        .collect();

    if let Some(index) = buckets.iter().position(|&x| toss < x) {
        variants.get(index).map(|&v| vec![v]).unwrap_or_default()
    } else {
        Vec::new()
    }
}

fn select_variants_experimental<'a>(
    variants: &'a [&'a Variant],
    toss: i32,
) -> Vec<&'a Variant> {
    if toss < 0 {
        // For negative toss, prefer experimental variants
        let experimental_variants: Vec<&Variant> = variants
            .iter()
            .filter(|v| v.variant_type == VariantType::EXPERIMENTAL)
            .copied()
            .collect();

        if !experimental_variants.is_empty() {
            experimental_variants
        } else {
            variants.to_vec()
        }
    } else {
        variants.to_vec()
    }
}

// Specialized query methods for different use cases

/// Get only applicable variant IDs (like the existing client's get_applicable_variant)
pub fn get_applicable_variants(
    experiments: &[ExperimentResponse],
    variants: &[Variant],
    overrides: &HashMap<String, Overrides>,
    user_context: &Map<String, Value>,
    toss: i32,
    filter_prefixes: &[String],
) -> Result<Vec<String>, String> {
    let result = eval_experiments(
        experiments,
        variants,
        overrides,
        user_context,
        toss,
        filter_prefixes,
        None, // Use default options
    )?;

    Ok(result.variants.into_iter().map(|v| v.id).collect())
}

/// Get satisfied experiments with full evaluation
pub fn get_satisfied_experiments(
    experiments: &[ExperimentResponse],
    user_context: &Map<String, Value>,
    filter_prefixes: &[String],
) -> Result<Vec<ExperimentResponse>, String> {
    let mut satisfied = Vec::new();
    let user_context_value = Value::Object(user_context.clone());

    for experiment in experiments {
        // Apply prefix filtering
        if !filter_prefixes.is_empty() {
            let matches_prefix = filter_prefixes.iter().any(|prefix| {
                experiment.name.starts_with(prefix) || experiment.id.starts_with(prefix)
            });
            if !matches_prefix {
                continue;
            }
        }

        // Check if experiment context matches
        if jsonlogic::apply(
            &Value::Object(experiment.context.clone().into()),
            &user_context_value,
        ) == Ok(Value::Bool(true))
        {
            satisfied.push(experiment.clone());
        }
    }

    Ok(satisfied)
}

/// Get satisfied experiments with partial evaluation (includes ambiguous contexts)
pub fn get_filtered_satisfied_experiments(
    experiments: &[ExperimentResponse],
    variants: &[Variant],
    overrides: &HashMap<String, Overrides>,
    user_context: &Map<String, Value>,
    filter_prefixes: &[String],
) -> Result<Vec<ExperimentResponse>, String> {
    let result = eval_experiments(
        experiments,
        variants,
        overrides,
        user_context,
        -1, // Use -1 to bypass traffic filtering
        filter_prefixes,
        Some(EvaluationOptions {
            context_eval: ContextEvaluation::Partial,
            variant_selection: VariantSelection::Simple,
            include_reasoning: false,
        }),
    )?;

    // Extract unique experiments from the result
    let mut experiment_ids: std::collections::HashSet<String> =
        std::collections::HashSet::new();
    let mut satisfied = Vec::new();

    for variant in result.variants {
        if experiment_ids.insert(variant.id.clone()) {
            // Find the corresponding experiment
            if let Some(experiment) = experiments.iter().find(|exp| exp.id == variant.id)
            {
                satisfied.push(experiment.clone());
            }
        }
    }

    Ok(satisfied)
}
