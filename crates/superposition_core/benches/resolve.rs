//! Resolution benchmark for the config evaluation hot path.
//!
//! This benchmark reproduces the shape of the workload reported from
//! production-scale data (hundreds of thousands of context override rules,
//! a handful of default configs, ~18 dimensions) and measures a single
//! "resolve all config" call — the same operation the OpenFeature provider
//! performs on every flag evaluation via `superposition_core::eval_config`.
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
    Cac, Condition, Context, DimensionInfo, OverrideWithKeys, Overrides,
};

/// Number of dimensions in the synthetic dataset (mirrors the reported data).
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

struct Dataset {
    default_config: Map<String, Value>,
    contexts: Vec<Context>,
    overrides: HashMap<String, Overrides>,
    dimensions: HashMap<String, DimensionInfo>,
    queries: Vec<Map<String, Value>>,
}

fn build_dataset(num_contexts: usize) -> Dataset {
    let mut rng = Rng::new(0x9E37_79B9_7F4A_7C15);

    let default_config: Map<String, Value> = DEFAULT_CONFIG_KEYS
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
            cond.insert(format!("d{dim}"), json!(format!("v{val}")));
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
        let query: Map<String, Value> = contexts[idx]
            .condition
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        queries.push(query);
    }

    Dataset {
        default_config,
        contexts,
        overrides,
        // Empty dimensions => local-cohort evaluation short-circuits, isolating
        // the context-matching + override-merge cost we care about.
        dimensions: HashMap::new(),
        queries,
    }
}

/// Current implementation: resolve directly against the borrowed data.
fn resolve_optimized(ds: &Dataset, query: &Map<String, Value>) -> Map<String, Value> {
    eval_config(
        ds.default_config.clone(),
        &ds.contexts,
        &ds.overrides,
        &ds.dimensions,
        query,
        MergeStrategy::MERGE,
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
        default_configs: ds.default_config.clone().into(),
        dimensions: ds.dimensions.clone(),
    };
    let _dimensions = full.dimensions.clone();

    // (2) Old `eval_config`: cloned contexts + overrides into a temporary
    //     Config before resolving.
    let contexts = ds.contexts.clone();
    let overrides = ds.overrides.clone();
    eval_config(
        ds.default_config.clone(),
        &contexts,
        &overrides,
        &ds.dimensions,
        query,
        MergeStrategy::MERGE,
        None,
    )
    .expect("resolve")
}

/// Resolve with a prefix filter set. This exercises the (unchanged) slow path
/// that builds an owned `Config` and runs `filter_by_prefix` over every
/// override and context.
///
/// NOTE: the OpenFeature `resolve_*_value` methods always pass `None`, so this
/// path is *not* on the per-flag resolve hot path — it is reachable via
/// `evaluate_config` / `get_cached_config` with an explicit prefix. It is
/// benchmarked for coverage and regression-guarding, not to demonstrate the
/// clone optimization (which only applies to the no-prefix path).
fn resolve_with_prefix(ds: &Dataset, query: &Map<String, Value>) -> Map<String, Value> {
    eval_config(
        ds.default_config.clone(),
        &ds.contexts,
        &ds.overrides,
        &ds.dimensions,
        query,
        MergeStrategy::MERGE,
        Some(vec![DEFAULT_CONFIG_KEYS[0].to_string()]),
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
        "Built dataset: {} contexts, {} dimensions, {} queries in {:.2?}",
        num_contexts,
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
            black_box(resolve_optimized(&ds, q))
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

    // Off-hot-path: prefix filtering (clone + filter_by_prefix), for coverage.
    group.bench_function("prefix_filter_slow_path", |b| {
        let counter = Cell::new(0usize);
        b.iter(|| {
            let q = &ds.queries[counter.get() % ds.queries.len()];
            counter.set(counter.get() + 1);
            black_box(resolve_with_prefix(&ds, q))
        })
    });

    group.finish();
}

criterion_group!(benches, bench_resolve);
criterion_main!(benches);
