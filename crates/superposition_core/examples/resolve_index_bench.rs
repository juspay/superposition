//! Standalone benchmark comparing the linear-scan resolve path
//! (`eval_config`) against the index-accelerated path (`eval_config_with_index`)
//! on a production-scale dataset.
//!
//! It reproduces the workload from the resolution perf analysis:
//! **468,006 contexts, 18 dimensions, and 100 query contexts sampled from the
//! data**. The dataset is generated deterministically with a tiny built-in
//! xorshift PRNG, so runs are reproducible and no external `rand`/`criterion`
//! dependency is needed.
//!
//! Run it (release mode matters — debug is ~20x slower and not representative):
//!
//! ```bash
//! cargo run --release -p superposition_core --example resolve_index_bench
//!
//! # Smaller / quicker local run:
//! BENCH_CONTEXTS=60000 cargo run --release -p superposition_core --example resolve_index_bench
//! ```

use std::collections::HashMap;
use std::time::{Duration, Instant};

use serde_json::{json, Map, Value};
use superposition_core::{
    eval_config, eval_config_with_index, merge, ContextIndex, MergeStrategy,
};
use superposition_types::logic::evaluate_local_cohorts;
use superposition_types::{
    Cac, Condition, Context, DimensionInfo, OverrideWithKeys, Overrides,
};

/// A borrowed linear scan (no per-resolve clone of the context set), i.e. the
/// baseline *after* the clone-removal optimization. Isolating this from the
/// current cloning `eval_config` lets the benchmark attribute the index's
/// speedup to the index itself, not to avoiding clones. MERGE strategy only.
fn resolve_linear_borrowed(
    default_config: &Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: &Map<String, Value>,
) -> Map<String, Value> {
    let query = evaluate_local_cohorts(dimensions, query_data);
    let mut required: Value = json!({});
    for context in contexts {
        if superposition_types::apply(&context.condition, &query) {
            if let Some(ov) = overrides.get(context.override_with_keys.get_key()) {
                merge(&mut required, &Value::Object(ov.clone().into()));
            }
        }
    }
    let mut result = default_config.clone();
    if let Value::Object(map) = required {
        for (key, val) in map {
            if let Some(og) = result.get_mut(&key) {
                merge(og, &val);
            }
        }
    }
    result
}

/// Minimal deterministic PRNG (xorshift64). No external dependency.
struct XorShift(u64);

impl XorShift {
    fn new(seed: u64) -> Self {
        // State must be non-zero.
        Self(seed | 1)
    }

    fn next_u64(&mut self) -> u64 {
        let mut x = self.0;
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        self.0 = x;
        x
    }

    fn below(&mut self, n: u64) -> usize {
        (self.next_u64() % n) as usize
    }
}

fn condition(pairs: Vec<(String, Value)>) -> Condition {
    let map: Map<String, Value> = pairs.into_iter().collect();
    Cac::<Condition>::try_from(map).unwrap().into_inner()
}

fn overrides(pairs: Vec<(String, Value)>) -> Overrides {
    let map: Map<String, Value> = pairs.into_iter().collect();
    Cac::<Overrides>::try_from(map).unwrap().into_inner()
}

fn env_usize(key: &str, default: usize) -> usize {
    std::env::var(key)
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(default)
}

struct Stats {
    avg: Duration,
    p50: Duration,
    p90: Duration,
    p99: Duration,
}

fn stats(mut samples: Vec<Duration>) -> Stats {
    samples.sort_unstable();
    let n = samples.len();
    let total: Duration = samples.iter().sum();
    let pct = |p: f64| samples[((n as f64 * p) as usize).min(n - 1)];
    Stats {
        avg: total / n as u32,
        p50: pct(0.50),
        p90: pct(0.90),
        p99: pct(0.99),
    }
}

fn main() {
    let num_contexts = env_usize("BENCH_CONTEXTS", 468_006);
    let num_queries = env_usize("BENCH_QUERIES", 100);
    let num_dimensions = 18usize;
    let num_config_keys = 10usize;

    println!("Generating dataset: {num_contexts} contexts, {num_dimensions} dimensions, {num_queries} queries...");

    // Per-dimension cardinality — a realistic mix of low- and high-cardinality
    // dimensions. High-cardinality dimensions are what make the pivot index
    // selective.
    let dim_names: Vec<String> =
        (0..num_dimensions).map(|i| format!("dim_{i}")).collect();
    let cardinality = |d: usize| -> u64 {
        match d % 3 {
            0 => 5,     // low  (e.g. country-ish)
            1 => 50,    // mid  (e.g. city-ish)
            _ => 2_000, // high (e.g. device/user-segment-ish)
        }
    };

    let default_config: Map<String, Value> = (0..num_config_keys)
        .map(|k| (format!("key_{k}"), json!(0)))
        .collect();

    let mut rng = XorShift::new(0x9E37_79B9_7F4A_7C15);
    let mut contexts: Vec<Context> = Vec::with_capacity(num_contexts);
    let mut overrides_map: HashMap<String, Overrides> =
        HashMap::with_capacity(num_contexts);
    let mut total_constraints = 0usize;

    for i in 0..num_contexts {
        let k = 1 + rng.below(4); // 1..=4 dimensions per condition
        let mut pairs = Vec::with_capacity(k);
        for _ in 0..k {
            let d = rng.below(num_dimensions as u64);
            let v = rng.below(cardinality(d));
            pairs.push((dim_names[d].clone(), json!(format!("v{v}"))));
        }
        total_constraints += pairs.len();

        let override_key = format!("o{i}");
        overrides_map.insert(
            override_key.clone(),
            overrides(vec![(
                format!("key_{}", rng.below(num_config_keys as u64)),
                json!(i),
            )]),
        );
        contexts.push(Context {
            id: format!("c{i}"),
            condition: condition(pairs),
            priority: i as i32,
            weight: 0,
            override_with_keys: OverrideWithKeys::new(override_key),
        });
    }

    // Sample query contexts from actual context conditions (as the perf test did).
    let dimensions = HashMap::new();
    let queries: Vec<Map<String, Value>> = (0..num_queries)
        .map(|_| {
            let src = &contexts[rng.below(num_contexts as u64)];
            let mut q: Map<String, Value> = src
                .condition
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            // The OpenFeature provider always injects variantIds.
            q.insert("variantIds".to_string(), Value::Array(vec![]));
            q
        })
        .collect();

    // Build the index (this cost is paid once at config-load, not per resolve).
    let t = Instant::now();
    let index = ContextIndex::build(&contexts);
    let build_time = t.elapsed();

    let s = index.stats();
    println!(
        "avg constraints/context: {:.2}",
        total_constraints as f64 / num_contexts as f64,
    );
    println!(
        "index: {} buckets | {} postings | {} unindexed | max bucket {} | ~{:.2} MB heap | {:.1} bytes/context",
        s.buckets,
        s.postings,
        s.unindexed,
        s.max_bucket,
        s.approx_heap_bytes as f64 / (1024.0 * 1024.0),
        s.approx_heap_bytes as f64 / num_contexts as f64,
    );
    println!("ContextIndex::build (one-time): {build_time:?}\n");

    // Correctness cross-check: both paths must agree on every query.
    for (i, q) in queries.iter().enumerate() {
        let a = eval_config(
            default_config.clone(),
            &contexts,
            &overrides_map,
            &dimensions,
            q,
            MergeStrategy::MERGE,
            None,
        )
        .unwrap();
        let b = eval_config_with_index(
            default_config.clone(),
            &contexts,
            &index,
            &overrides_map,
            &dimensions,
            q,
            MergeStrategy::MERGE,
        )
        .unwrap();
        assert_eq!(a, b, "linear vs indexed mismatch on query {i}");
    }
    println!("Correctness: linear and indexed agree on all {num_queries} queries.\n");

    // Warm up.
    for q in &queries {
        let _ = eval_config(
            default_config.clone(),
            &contexts,
            &overrides_map,
            &dimensions,
            q,
            MergeStrategy::MERGE,
            None,
        );
        let _ = eval_config_with_index(
            default_config.clone(),
            &contexts,
            &index,
            &overrides_map,
            &dimensions,
            q,
            MergeStrategy::MERGE,
        );
    }

    let linear: Vec<Duration> = queries
        .iter()
        .map(|q| {
            let t = Instant::now();
            let _ = eval_config(
                default_config.clone(),
                &contexts,
                &overrides_map,
                &dimensions,
                q,
                MergeStrategy::MERGE,
                None,
            )
            .unwrap();
            t.elapsed()
        })
        .collect();

    let borrowed: Vec<Duration> = queries
        .iter()
        .map(|q| {
            let t = Instant::now();
            let _ = resolve_linear_borrowed(
                &default_config,
                &contexts,
                &overrides_map,
                &dimensions,
                q,
            );
            t.elapsed()
        })
        .collect();

    let indexed: Vec<Duration> = queries
        .iter()
        .map(|q| {
            let t = Instant::now();
            let _ = eval_config_with_index(
                default_config.clone(),
                &contexts,
                &index,
                &overrides_map,
                &dimensions,
                q,
                MergeStrategy::MERGE,
            )
            .unwrap();
            t.elapsed()
        })
        .collect();

    let cs = stats(linear);
    let bs = stats(borrowed);
    let is = stats(indexed);

    println!("Per-resolve latency over {num_queries} sampled queries:\n");
    println!(
        "{:<18} {:>12} {:>12} {:>12} {:>12}",
        "path", "avg", "p50", "p90", "p99"
    );
    println!(
        "{:<18} {:>12?} {:>12?} {:>12?} {:>12?}",
        "linear (clones)", cs.avg, cs.p50, cs.p90, cs.p99
    );
    println!(
        "{:<18} {:>12?} {:>12?} {:>12?} {:>12?}",
        "linear (borrowed)", bs.avg, bs.p50, bs.p90, bs.p99
    );
    println!(
        "{:<18} {:>12?} {:>12?} {:>12?} {:>12?}",
        "indexed", is.avg, is.p50, is.p90, is.p99
    );

    let vs_clone = cs.avg.as_secs_f64() / is.avg.as_secs_f64();
    let vs_borrowed = bs.avg.as_secs_f64() / is.avg.as_secs_f64();
    println!("\nSpeedup vs linear (clones)   : {vs_clone:.1}x  (index + clone removal)");
    println!("Speedup vs linear (borrowed) : {vs_borrowed:.1}x  (index alone)");
}
