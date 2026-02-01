//! Resolution benchmark for the config evaluation hot path.
//!
//! This benchmark reproduces the shape of the workload reported from
//! production-scale data (hundreds of thousands of context override rules,
//! a handful of default configs, ~18 regular dimensions, and derived local
//! cohorts) and measures a single "resolve all config" call — the same
//! operation the OpenFeature provider performs on every flag evaluation via
//! `superposition_core::eval_config`.
//!
//! Two variants are compared:
//!   * `optimized_borrowed`     — the current implementation, which resolves
//!     directly against the borrowed context/override set.
//!   * `pre_optimization_cloned` — reproduces the pre-optimization overhead,
//!     where every resolve deep-cloned the entire context set (once to read
//!     the dimensions, once more to build a throwaway `Config`).
//!
//! Run with:
//!     cargo bench -p superposition_core --bench resolve
//!
//! The dataset size defaults to the production figure (468,006 contexts) and
//! can be overridden for quicker local runs:
//!     BENCH_CONTEXTS=50000 cargo bench -p superposition_core --bench resolve

use std::cell::Cell;
use std::collections::HashMap;
use std::time::{Duration, Instant};

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use serde_json::{json, Map, Value};
use superposition_core::{eval_config, Config, MergeStrategy};
use superposition_types::{
    database::models::cac::{DependencyGraph, DimensionType},
    Cac, Condition, Context, DimensionInfo, ExtendedMap, OverrideWithKeys, Overrides,
};

/// Number of regular dimensions in the synthetic dataset (mirrors the reported data).
const NUM_DIMENSIONS: usize = 18;
/// Default number of contexts / override rules (reported production figure).
const DEFAULT_NUM_CONTEXTS: usize = 468_006;
/// Number of sampled query contexts to resolve against.
const NUM_QUERIES: usize = 100;
/// Per-dimension value cardinality; controls how many contexts a query matches.
const DIMENSION_CARDINALITY: usize = 12;
/// The (small) set of default config keys.
const DEFAULT_CONFIG_KEYS: [&str; 3] = ["config.alpha", "config.beta", "config.gamma"];

/// Tiny deterministic xorshift64 PRNG so the dataset is fully reproducible
/// without pulling in an external `rand` dependency.
struct Rng(u64);

impl Rng {
    fn new(seed: u64) -> Self {
        Rng(seed)
    }

    fn next_u64(&mut self) -> u64 {
        let mut x = self.0;
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        self.0 = x;
        x
    }

    fn below(&mut self, n: usize) -> usize {
        (self.next_u64() % n as u64) as usize
    }
}

fn root_dimension(dim: usize) -> String {
    format!("d{dim}")
}

fn cohort_dimension(dim: usize) -> String {
    format!("lc{dim}")
}

fn root_value(val: usize) -> String {
    format!("v{val}")
}

fn cohort_value(val: usize) -> String {
    format!("c{val}")
}

fn build_dimensions() -> HashMap<String, DimensionInfo> {
    let mut dimensions = HashMap::with_capacity(NUM_DIMENSIONS * 2);

    for dim in 0..NUM_DIMENSIONS {
        let root = root_dimension(dim);
        let cohort = cohort_dimension(dim);

        dimensions.insert(
            root.clone(),
            DimensionInfo {
                schema: ExtendedMap::from(Map::new()),
                position: dim as i32,
                dimension_type: DimensionType::Regular {},
                dependency_graph: DependencyGraph(HashMap::from([
                    (root.clone(), vec![cohort.clone()]),
                    (cohort.clone(), Vec::new()),
                ])),
                value_compute_function_name: None,
                description: String::new(),
            },
        );

        let mut definitions = Map::new();
        let mut enum_values = Vec::with_capacity(DIMENSION_CARDINALITY + 1);
        for val in 0..DIMENSION_CARDINALITY {
            let cohort_val = cohort_value(val);
            enum_values.push(Value::String(cohort_val.clone()));
            definitions.insert(
                cohort_val,
                json!({
                    "==": [
                        { "var": root },
                        root_value(val)
                    ]
                }),
            );
        }
        enum_values.push(Value::String("otherwise".to_string()));

        let mut schema = Map::new();
        schema.insert("type".to_string(), json!("string"));
        schema.insert("enum".to_string(), Value::Array(enum_values));
        schema.insert("definitions".to_string(), Value::Object(definitions));

        dimensions.insert(
            cohort.clone(),
            DimensionInfo {
                schema: ExtendedMap::from(schema),
                position: (NUM_DIMENSIONS + dim) as i32,
                dimension_type: DimensionType::LocalCohort(root),
                dependency_graph: DependencyGraph(HashMap::from([(cohort, Vec::new())])),
                value_compute_function_name: None,
                description: String::new(),
            },
        );
    }

    dimensions
}

fn query_from_condition(condition: &Condition) -> Map<String, Value> {
    condition
        .iter()
        .map(|(key, value)| {
            if let Some(dim) = key.strip_prefix("lc") {
                let root_key = format!("d{dim}");
                let root_val = value
                    .as_str()
                    .and_then(|v| v.strip_prefix('c'))
                    .map(|v| Value::String(format!("v{v}")))
                    .unwrap_or_else(|| value.clone());
                (root_key, root_val)
            } else {
                (key.clone(), value.clone())
            }
        })
        .collect()
}

#[derive(Clone)]
struct Dataset {
    default_config: ExtendedMap,
    contexts: Vec<Context>,
    overrides: HashMap<String, Overrides>,
    dimensions: HashMap<String, DimensionInfo>,
    queries: Vec<Map<String, Value>>,
}

fn build_dataset(num_contexts: usize) -> Dataset {
    let mut rng = Rng::new(0x9E37_79B9_7F4A_7C15);

    let default_config: ExtendedMap = DEFAULT_CONFIG_KEYS
        .iter()
        .map(|k| (k.to_string(), json!("default")))
        .collect();

    let mut contexts = Vec::with_capacity(num_contexts);
    let mut overrides = HashMap::with_capacity(num_contexts);

    for i in 0..num_contexts {
        // Each context conditions on 1..=3 dimensions with concrete values.
        let num_dims = 1 + rng.below(3);
        let start = rng.below(NUM_DIMENSIONS);
        let mut cond = Map::new();
        for j in 0..num_dims {
            let dim = (start + j) % NUM_DIMENSIONS;
            let val = rng.below(DIMENSION_CARDINALITY);
            let (key, value) = if (i + j) % 3 == 0 {
                (cohort_dimension(dim), cohort_value(val))
            } else {
                (root_dimension(dim), root_value(val))
            };
            cond.insert(key, json!(value));
        }
        let condition = Cac::<Condition>::try_from(cond)
            .expect("non-empty condition")
            .into_inner();

        let override_key = format!("o{i}");
        let target = DEFAULT_CONFIG_KEYS[i % DEFAULT_CONFIG_KEYS.len()];
        let mut override_map = Map::new();
        override_map.insert(target.to_string(), json!(format!("override_{i}")));
        let override_value = Cac::<Overrides>::try_from(override_map)
            .expect("non-empty override")
            .into_inner();
        overrides.insert(override_key.clone(), override_value);

        contexts.push(Context {
            id: format!("ctx_{i}"),
            condition,
            priority: (i % 100) as i32,
            weight: 1,
            override_with_keys: OverrideWithKeys::new(override_key),
        });
    }

    // Sample query contexts directly from real context conditions, matching
    // the "100 actual context conditions sampled from data" methodology.
    let mut queries = Vec::with_capacity(NUM_QUERIES);
    for _ in 0..NUM_QUERIES {
        let idx = rng.below(num_contexts);
        queries.push(query_from_condition(&contexts[idx].condition));
    }

    Dataset {
        default_config,
        contexts,
        overrides,
        dimensions: build_dimensions(),
        queries,
    }
}

/// Current implementation: `eval_config` consumes its inputs, so a caller that
/// keeps its dataset around has to hand over its own copy on every resolve.
fn resolve_optimized(ds: Dataset, query: Map<String, Value>) -> Map<String, Value> {
    eval_config(
        ds.default_config,
        ds.contexts,
        ds.overrides,
        ds.dimensions,
        query,
        MergeStrategy::MERGE,
        None,
        None,
    )
    .expect("resolve")
}

/// Pre-optimization behavior: every resolve deep-cloned the full context set
/// twice — once while extracting dimensions, once while building a throwaway
/// `Config` for (unused) prefix filtering.
fn resolve_pre_optimization(
    ds: &Dataset,
    query: &Map<String, Value>,
) -> Map<String, Value> {
    // (1) Old `get_dimensions_info`: cloned the whole Config just to read
    //     `.dimensions`.
    let full = Config {
        contexts: ds.contexts.clone(),
        overrides: ds.overrides.clone(),
        default_configs: ds.default_config.clone(),
        dimensions: ds.dimensions.clone(),
    };
    let _dimensions = full.dimensions.clone();

    // (2) Old `eval_config`: cloned contexts + overrides into a temporary
    //     Config before resolving.
    let contexts = ds.contexts.clone();
    let overrides = ds.overrides.clone();
    pre_optimization::eval_config(
        full.default_configs.into_inner(),
        &contexts,
        &overrides,
        &ds.dimensions,
        query,
        MergeStrategy::MERGE,
        None,
        None,
    )
    .expect("resolve")
}

fn bench_resolve(c: &mut Criterion) {
    let num_contexts = std::env::var("BENCH_CONTEXTS")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(DEFAULT_NUM_CONTEXTS);

    let start = Instant::now();
    let ds = build_dataset(num_contexts);
    eprintln!(
        "Built dataset: {} contexts, {} regular dimensions, {} local cohorts, {} queries in {:.2?}",
        num_contexts,
        NUM_DIMENSIONS,
        NUM_DIMENSIONS,
        ds.queries.len(),
        start.elapsed()
    );

    let mut group = c.benchmark_group("resolve_all_config");
    // One iteration == one full resolve of one query context.
    group.sample_size(10);
    group.warm_up_time(Duration::from_secs(1));
    group.measurement_time(Duration::from_secs(15));

    group.bench_function("optimized_borrowed", |b| {
        let counter = Cell::new(0usize);
        b.iter(|| {
            let q = &ds.queries[counter.get() % ds.queries.len()];
            counter.set(counter.get() + 1);
            black_box(resolve_optimized(ds.clone(), q.clone()))
        })
    });

    group.bench_function("pre_optimization_cloned", |b| {
        let counter = Cell::new(0usize);
        b.iter(|| {
            let q = &ds.queries[counter.get() % ds.queries.len()];
            counter.set(counter.get() + 1);
            black_box(resolve_pre_optimization(&ds, q))
        })
    });

    group.finish();
}

criterion_group!(benches, bench_resolve);
criterion_main!(benches);

mod pre_optimization {
    use std::collections::HashMap;

    use serde_json::{Map, Value};
    pub use superposition_types::api::config::MergeStrategy;
    use superposition_types::{
        database::models::cac::{DependencyGraph, DimensionType},
        Config, Context, DimensionInfo, Overrides, PrefixList,
    };

    #[inline]
    fn apply_logic(
        condition: &Map<String, Value>,
        context: &Map<String, Value>,
        partial: bool,
    ) -> bool {
        for (dimension, value) in condition {
            if let Some(context_value) = context.get(dimension) {
                if dimension == "variantIds" {
                    if let Value::Array(ref context_values) = context_value {
                        if !context_values.contains(value) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                } else if *context_value != *value {
                    return false;
                }
            } else if partial {
                continue;
            } else {
                return false;
            }
        }
        true
    }

    /// Core context application logic - checks if all dimensions in condition are satisfied by context
    /// Only exact matches are considered valid, except for "variantIds" dimension where containment is checked
    /// Returns true if condition is satisfied by context, false otherwise
    pub fn apply(condition: &Map<String, Value>, context: &Map<String, Value>) -> bool {
        apply_logic(condition, context, false)
    }

    fn _evaluate_local_cohort_dimension(
        cohort_based_on: &str,
        cohort_based_on_value: &Value,
        schema: &Map<String, Value>,
    ) -> Option<String> {
        let definitions_object = schema.get("definitions")?.as_object()?;

        // Get the array of cohort names from the "enum" field and remove "otherwise"
        let cohort_enums = schema
            .get("enum")?
            .as_array()?
            .iter()
            .filter_map(|v| v.as_str())
            .filter(|s| *s != "otherwise")
            .collect::<Vec<_>>();

        for cohort_option in cohort_enums {
            let jsonlogic = definitions_object.get(cohort_option)?;
            // Find the first matching cohort definition
            let evaluation_data =
                serde_json::json!({cohort_based_on: cohort_based_on_value});
            if jsonlogic::apply(jsonlogic, &evaluation_data) == Ok(Value::Bool(true)) {
                return Some(cohort_option.to_string());
            }
        }

        None
    }

    fn evaluate_local_cohort_dimension(
        cohort_based_on: &str,
        cohort_based_on_value: &Value,
        schema: &Map<String, Value>,
    ) -> String {
        _evaluate_local_cohort_dimension(cohort_based_on, cohort_based_on_value, schema)
            .unwrap_or_else(|| "otherwise".to_string())
    }

    /// Evaluates local cohort dependencies in a depth-first manner
    fn evaluate_local_cohorts_dependency(
        dimension: &str,
        value: &Value,
        dependency_graph: &DependencyGraph,
        dimensions: &HashMap<String, DimensionInfo>,
        modified_context: &mut Map<String, Value>,
        query_data: &Map<String, Value>,
    ) {
        let mut stack = dependency_graph
            .get(dimension)
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .map(|d| (d, dimension.to_string(), value.clone()))
            .collect::<Vec<_>>();

        // Depth-first traversal of dependencies
        while let Some((cohort_dimension, based_on, based_on_val)) = stack.pop() {
            if let Some(dimension_info) = dimensions.get(&cohort_dimension) {
                let mut cohort_val = None;
                match &dimension_info.dimension_type {
                    DimensionType::LocalCohort(_) => {
                        let cohort_value =
                            Value::String(evaluate_local_cohort_dimension(
                                &based_on,
                                &based_on_val,
                                &dimension_info.schema,
                            ));
                        modified_context
                            .insert(cohort_dimension.clone(), cohort_value.clone());
                        cohort_val = Some(cohort_value);
                    }
                    _ => {
                        if let Some(value) = query_data.get(&cohort_dimension) {
                            modified_context
                                .insert(cohort_dimension.clone(), value.clone());
                            cohort_val = Some(value.clone());
                        }
                    }
                }

                if let Some(cohort_val) = cohort_val {
                    stack.extend(
                        dimension_info
                            .dependency_graph
                            .get(&cohort_dimension)
                            .cloned()
                            .unwrap_or_default()
                            .into_iter()
                            .map(|d| (d, cohort_dimension.clone(), cohort_val.clone()))
                            .collect::<Vec<_>>(),
                    );
                }
            }
        }
    }

    fn _evaluate_local_cohorts(
        dimensions: &HashMap<String, DimensionInfo>,
        query_data: &Map<String, Value>,
        skip_unresolved: bool,
    ) -> Map<String, Value> {
        if dimensions.is_empty() {
            return query_data.clone();
        }

        let mut modified_context = Map::new();

        // Start from dimensions that are closest to root in each tree
        for dimension_key in dimensions_to_start_from(dimensions, query_data) {
            if let Some(value) = query_data.get(&dimension_key) {
                if let Some(dimension_info) = dimensions.get(&dimension_key) {
                    modified_context.insert(dimension_key.to_string(), value.clone());
                    evaluate_local_cohorts_dependency(
                        &dimension_key,
                        value,
                        &dimension_info.dependency_graph,
                        dimensions,
                        &mut modified_context,
                        query_data,
                    );
                }
            }
        }

        if skip_unresolved {
            return modified_context;
        }

        // For any local cohort dimension not yet set, set it to "otherwise"
        for dimension_key in dimensions.keys() {
            if let Some(dimension_info) = dimensions.get(dimension_key) {
                if matches!(dimension_info.dimension_type, DimensionType::LocalCohort(_))
                    && !modified_context.contains_key(dimension_key)
                {
                    modified_context.insert(
                        dimension_key.to_string(),
                        Value::String("otherwise".to_string()),
                    );
                }
            }
        }

        modified_context
    }

    /// Evaluates all local cohort dimensions based on the provided query data and dimension definitions
    /// First all local cohorts which are computable from the query data are evaluated, then any remaining local cohorts are set to "otherwise"
    /// Computation starts from such a point, such that dependencies can be resolved in a depth-first manner
    ///
    /// Values of regular and remote cohort dimensions in query_data are retained as is.
    /// Returned value, might have a different value for local cohort dimensions based on its based on dimensions,
    /// if the value provided for the local cohort was incorrect in the query data.
    pub fn evaluate_local_cohorts(
        dimensions: &HashMap<String, DimensionInfo>,
        query_data: &Map<String, Value>,
    ) -> Map<String, Value> {
        _evaluate_local_cohorts(dimensions, query_data, false)
    }

    /// Identifies starting dimensions for evaluation based on query data and dimension definitions
    /// For each tree in the dependency graph, picks the node closest to root from query_data for each branch of the tree.
    /// If nothing is found and a local cohort is encountered, picks that local cohort as start point from that branch.
    pub fn dimensions_to_start_from(
        dimensions: &HashMap<String, DimensionInfo>,
        query_data: &Map<String, Value>,
    ) -> Vec<String> {
        let mut start_dimensions = Vec::new();

        let regular_dimensions = dimensions
            .iter()
            .filter(|(_, data)| matches!(data.dimension_type, DimensionType::Regular {}))
            .map(|(dim_name, _)| dim_name.clone())
            .collect::<Vec<String>>();

        for root_dimension in regular_dimensions {
            let dependency_graph = &dimensions
                .get(&root_dimension)
                .map(|data| data.dependency_graph.clone())
                .unwrap_or_default();

            let mut stack = vec![root_dimension];

            while let Some(current_dimension) = stack.pop() {
                if query_data.contains_key(&current_dimension) {
                    start_dimensions.push(current_dimension);
                    continue;
                }

                if let Some(data) = dimensions.get(&current_dimension) {
                    if matches!(data.dimension_type, DimensionType::LocalCohort(_)) {
                        start_dimensions.push(current_dimension);
                        continue;
                    }
                }

                stack.extend(
                    dependency_graph
                        .get(&current_dimension)
                        .cloned()
                        .unwrap_or_default(),
                );
            }
        }

        start_dimensions
    }

    #[allow(clippy::too_many_arguments)]
    pub fn eval_config(
        default_config: Map<String, Value>,
        contexts: &[Context],
        overrides: &HashMap<String, Overrides>,
        dimensions: &HashMap<String, DimensionInfo>,
        query_data: &Map<String, Value>,
        merge_strategy: MergeStrategy,
        filter_prefixes: Option<Vec<String>>,
        filter_exclude_prefixes: Option<Vec<String>>,
    ) -> Result<Map<String, Value>, String> {
        // Local cohort evaluation only reads `dimensions`, which prefix filtering
        // leaves untouched, so it is safe to compute once regardless of the path.
        let modified_query_data = evaluate_local_cohorts(dimensions, query_data);

        let filter_prefixes = filter_prefixes.filter(|p| !p.is_empty());
        let filter_exclude_prefixes = filter_exclude_prefixes.filter(|p| !p.is_empty());

        // Fast path: no prefix filtering. Resolve directly against the borrowed
        // contexts/overrides instead of deep-cloning the entire context set (which
        // can be hundreds of thousands of entries) into a temporary `Config`.
        if filter_prefixes.is_none() && filter_exclude_prefixes.is_none() {
            let overrides_map = get_overrides(
                &modified_query_data,
                contexts,
                overrides,
                &merge_strategy,
                None,
            )?;

            let mut result_config = default_config;
            merge_overrides_on_default_config(
                &mut result_config,
                overrides_map,
                &merge_strategy,
            );
            return Ok(result_config);
        }

        // Slow path: prefix filtering needs an owned, filtered `Config`.
        let config = Config {
            default_configs: default_config.into(),
            contexts: contexts.to_vec(),
            overrides: overrides.clone(),
            dimensions: dimensions.clone(),
        }
        .filter_by_prefix(
            &PrefixList::from(filter_prefixes),
            &PrefixList::from(filter_exclude_prefixes),
        );

        let overrides_map: Map<String, Value> = get_overrides(
            &modified_query_data,
            &config.contexts,
            &config.overrides,
            &merge_strategy,
            None,
        )?;

        // Apply overrides to default config
        let mut result_config = config.default_configs;
        merge_overrides_on_default_config(
            &mut result_config,
            overrides_map,
            &merge_strategy,
        );

        Ok(result_config.into_inner())
    }

    pub fn merge(doc: &mut Value, patch: &Value) {
        if !patch.is_object() {
            *doc = patch.clone();
            return;
        }

        if !doc.is_object() {
            *doc = Value::Object(Map::new());
        }

        let map = doc.as_object_mut().unwrap();
        for (key, value) in patch.as_object().unwrap() {
            merge(map.entry(key.as_str()).or_insert(Value::Null), value);
        }
    }

    fn get_overrides(
        query_data: &Map<String, Value>,
        contexts: &[Context],
        overrides: &HashMap<String, Overrides>,
        merge_strategy: &MergeStrategy,
        mut on_override_select: Option<&mut dyn FnMut(Context)>,
    ) -> Result<Map<String, Value>, String> {
        let mut required_overrides = Map::new();

        for context in contexts {
            if !apply(&context.condition, query_data) {
                continue;
            }

            let override_key = context.override_with_keys.get_key();
            let Some(overriden_value) = overrides.get(override_key) else {
                continue;
            };

            match merge_strategy {
                MergeStrategy::REPLACE => {
                    for (key, value) in overriden_value.iter() {
                        required_overrides.insert(key.clone(), value.clone());
                    }
                }
                MergeStrategy::MERGE => {
                    for (key, value) in overriden_value.iter() {
                        merge(
                            required_overrides
                                .entry(key.as_str())
                                .or_insert(Value::Null),
                            value,
                        );
                    }
                }
            }
            // Only pay for a `Context` clone when a caller actually consumes it; the
            // borrow keeps the common (callback-less) resolution path allocation-free.
            if let Some(ref mut func) = on_override_select {
                func(context.clone());
            }
        }

        Ok(required_overrides)
    }

    fn merge_overrides_on_default_config(
        default_config: &mut Map<String, Value>,
        overrides: Map<String, Value>,
        merge_strategy: &MergeStrategy,
    ) {
        overrides.into_iter().for_each(|(key, val)| {
            if let Some(og_val) = default_config.get_mut(&key) {
                match merge_strategy {
                    MergeStrategy::REPLACE => {
                        let _ = default_config.insert(key.clone(), val.clone());
                    }
                    MergeStrategy::MERGE => merge(og_val, &val),
                }
            } else {
                log::error!("Config: found non-default_config key: {key} in overrides");
            }
        })
    }
}
