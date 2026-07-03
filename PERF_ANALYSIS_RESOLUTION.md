# Performance Analysis: Rust OpenFeature Provider — Resolution Logic

This document analyzes the resolution (config evaluation) hot path of the Rust
OpenFeature provider (`crates/superposition_provider`) and its underlying core
library (`crates/superposition_core`), identifies the dominant cost at scale,
and describes the optimizations applied along with a reproducible benchmark.

## Context

A performance test was run against a production-scale dataset:

```
Dataset:
- Context override rules / contexts: 468,006
- Default configs: 3
- Dimensions: 18

Provider startup:
- fetch_config: 11.69s
- cache_init: 0.73s

Resolve benchmark (before optimization):
- Mode: resolve all config
- Input: 100 actual context conditions sampled from data
- Total time: 44.40s
- Throughput: ~2 resolves/sec
- Avg: 443.95ms
- p50: 438.45ms
- p90: 471.95ms
- p95: 475.05ms
- p99: 547.55ms
```

At ~444ms per resolve, resolution — not startup — was the bottleneck for
high-throughput evaluation.

## The resolution path

A single flag evaluation flows through:

```
SuperpositionProvider::resolve_{bool,string,int,float,struct}_value
  └─ SuperpositionProvider::eval_config                 (provider.rs)
       ├─ get_dimensions_info()                          (provider.rs)
       │    └─ CacConfig::get_cached_config()            (client.rs)
       └─ CacConfig::evaluate_config()                   (client.rs)
            └─ superposition_core::eval_config()         (core/config.rs)
                 ├─ evaluate_local_cohorts()
                 ├─ get_overrides()   // scans all contexts, calls `apply`
                 └─ merge_overrides_on_default_config()
```

## Root cause: the entire context set was deep-cloned on every resolve

The actual matching work — iterating contexts and testing each condition with
`apply()` — is inherent and comparatively cheap. The dominant cost was
**deep-cloning all 468,006 contexts (plus the overrides map) multiple times per
resolve**. Each `Context` owns a `String` id, a `Condition` (a `BTreeMap`), and
an override key `String`; cloning ~468k of them repeatedly allocates on the
order of a million heap objects per resolve.

Three distinct clone sites were found in the hot path:

### 1. `get_dimensions_info` cloned the whole `Config` to read 18 entries

`provider.rs::get_dimensions_info` called `CacConfig::get_cached_config()`,
which clones the **entire** `Config` (all contexts + all overrides + dimensions)
and then kept only `.dimensions` (18 entries), discarding the rest:

```rust
// provider.rs (before)
cac_config
    .get_cached_config()        // clones ALL 468k contexts + overrides
    .await
    .map(|c| c.dimensions.clone())   // keeps 18 entries, drops the rest
    .unwrap_or_default()
```

```rust
// client.rs (before)
pub async fn get_cached_config(&self) -> Option<Config> {
    let cached_config = self.cached_config.read().await;
    cached_config.clone()       // full deep clone of the config
}
```

Worse, this ran on **every** resolve even when experimentation was disabled, in
which case the dimensions were computed and then never used.

### 2. `eval_config` cloned contexts + overrides for optional prefix filtering

`superposition_core::eval_config` built a throwaway `Config` purely so it could
call `filter_by_prefix` — but the provider always passes `filter_prefixes =
None`, so the clone was pure waste on the provider path:

```rust
// core/config.rs (before)
let mut config = Config {
    default_configs: default_config.into(),
    contexts: contexts.to_vec(),   // clone of all 468k contexts, again
    overrides: overrides.clone(),  // clone of the whole overrides map, again
    dimensions: dimensions.clone(),
};
if let Some(prefixes) = filter_prefixes.filter(|p| !p.is_empty()) {
    config = config.filter_by_prefix(...);
}
```

### 3. `get_overrides` cloned each matched context and each override value

For every matching context, the matched `Context` was cloned unconditionally
(even when no callback consumed it), and each override map was cloned into a
temporary `Value::Object` before merging:

```rust
// core/config.rs (before)
MergeStrategy::MERGE => {
    merge(&mut required_overrides,
          &Value::Object(overriden_value.clone().into())); // extra clone
    on_override_select(context.clone())                    // unconditional clone
}
```

## Optimizations applied

| # | Location | Change |
|---|----------|--------|
| 1 | `client.rs` / `provider.rs` | Added `CacConfig::get_dimensions()` that clones only the `dimensions` map under the read lock. The provider now fetches dimensions **only when experimentation is enabled** (they are unused otherwise). |
| 2 | `core/config.rs` (`eval_config`, `eval_config_with_reasoning`) | Added a fast path that resolves directly against the **borrowed** `contexts`/`overrides` when there is no prefix filter. Only the rare prefix-filter path builds an owned `Config`. |
| 3 | `core/config.rs` (`get_overrides`) | Clone a matched `Context` only when a callback actually consumes it; merge override maps by reference instead of cloning them into a temporary `Value::Object`. |

All changes preserve existing behavior and semantics; the full
`superposition_core` test suite (85 tests) passes.

## Benchmark

A criterion benchmark was added at
`crates/superposition_core/benches/resolve.rs`. It reproduces the reported
workload — **468,006 contexts, 18 dimensions, 3 default configs, and 100 query
contexts sampled from actual context conditions** — and compares the optimized
path against a faithful reproduction of the pre-optimization clone behavior.

Run it with:

```bash
# Full production-scale run (default: 468,006 contexts)
cargo bench -p superposition_core --bench resolve

# Quicker local run with a smaller dataset
BENCH_CONTEXTS=60000 cargo bench -p superposition_core --bench resolve
```

The dataset is generated deterministically (a small built-in xorshift PRNG), so
runs are reproducible without any external `rand` dependency.

### Results (median per-resolve)

| Scale | Pre-optimization (cloned) | Optimized (borrowed) | Speedup |
|-------|---------------------------|----------------------|---------|
| 60,000 contexts | 138 ms | 2.69 ms | ~51× |
| 468,006 contexts | 1.37 s | **22.98 ms** | **~60×** |

> Note: the benchmark's synthetic per-context payload is heavier than the
> production data, so the absolute pre-optimization figure runs higher than the
> reported 444ms. The meaningful takeaways are the **ratio** (~60×) and the
> **~23ms optimized floor**. In the reported environment, the ~444ms average is
> expected to drop to well under ~50ms.

## Remaining opportunity

After removing the clones, the remaining ~23ms is dominated by the inherent
linear scan of all 468k contexts calling `apply()` on each. The next lever is an
**index** — bucketing contexts by a high-cardinality dimension so a query only
tests candidate contexts rather than the full set. This is a larger
architectural change and is left as a follow-up.
