# Performance Analysis: Indexed In-Memory Representation for Resolution

This is a follow-up to [`PERF_ANALYSIS_RESOLUTION.md`](./PERF_ANALYSIS_RESOLUTION.md),
which removed the per-resolve deep-clones of the context set and left one lever
on the table:

> **Remaining opportunity.** After removing the clones, the remaining tens of
> milliseconds are dominated by the inherent linear scan of all 468k contexts
> calling `apply()` on each. The next lever is an **index** — bucketing contexts
> by a high-cardinality dimension so a query only tests candidate contexts
> rather than the full set.

This document picks up exactly there: it introduces a **different in-memory
representation of the contexts/overrides** — an inverted *pivot index* — that
replaces the `O(N)` scan on the resolve hot path with a lookup over a small
candidate set, and benchmarks the result at production scale (468,006 contexts).

## The shape of the problem

Resolution matching is a **purely conjunctive equality predicate**. Look at the
core of `superposition_types::apply`:

```rust
for (dimension, value) in condition {
    if let Some(context_value) = context.get(dimension) {
        // exact equality, except `variantIds` which is containment
        if *context_value != *value { return false; }
    } else {
        return false; // a required dimension is absent from the query
    }
}
true
```

A context matches a query **iff every `(dimension, value)` pair in its condition
is satisfied by the query.** Nothing about the query lets a context match
*more* — extra constraints can only make it match *less*. That monotonic,
all-of structure is what makes the full linear scan wasteful: the overwhelming
majority of the 468k contexts fail on their very first constraint, yet every one
of them is still visited on every resolve.

## The idea: a pivot inverted index

For each context, pick a single **pivot** — the *rarest* (most selective) scalar
constraint in its condition — and bucket the context under that pivot
`(dimension, value)`. To resolve a query, we only look at the buckets the
query's own `(dimension, value)` pairs unlock, and verify each candidate with
the exact same `apply` check.

```
build (once, at config load):
  freq[(dim,val)]  = how many conditions contain this constraint
  for each context:
      pivot = argmin_{(dim,val) in condition} freq[(dim,val)]   // rarest constraint
      postings[pivot].push(context_index)

resolve (per query):
  candidates = ∅
  for (dim, val) in query:              // ~18 dimensions, not 468k contexts
      candidates ∪= postings[(dim, val)]
  for ctx in sort(candidates):          // small set, original order preserved
      if apply(ctx.condition, query): merge ctx's override
```

### Why a *pivot*, and why the *rarest* one

A context matches only if **all** its constraints hold, so it is enough to index
it under **any single** one of them and re-check the rest — every true match is
still discovered, because a matching query necessarily satisfies whichever
constraint we chose. Choosing the **rarest** constraint minimizes bucket sizes:
each context lands in exactly one posting list, and that list is as short as the
data allows. This adapts automatically to the data — no need to hand-pick a
"good" dimension to bucket on; a high-cardinality dimension naturally wins
because its `(dim, val)` pairs are rare.

Contrast with the alternatives considered:

| Structure | Why not |
|-----------|---------|
| **Full inverted index + counting** (index every constraint, count hits per context, keep those whose count equals their constraint count) | Correct, but a context appears in *every* one of its constraints' posting lists, and a common `(dim,val)` (e.g. a boolean) yields a posting list of ~N, so a query touches `O(N)` postings anyway. Also needs an `O(N)` counter array per resolve. |
| **Match/decision tree over dimensions** (à la pub/sub matching, SIENA/Gryphon) | Strong pruning, but contexts constrain arbitrary *subsets* of dimensions, forcing "any" edges and multi-path traversal — much more complex to build and keep correct for marginal gain over the pivot index at this cardinality. |
| **Pivot inverted index** (chosen) | Each context in exactly one bucket → index is `O(N)` memory with tiny overhead; per-query work is `Σ` over the query's pivot buckets, which the rarest-pivot choice keeps small; correctness falls out of an `apply` re-check. |

### Correctness

The index only ever **narrows the candidate set**; the actual decision is still
made by the unchanged `apply`. Formally, if the linear scan matches a context
`C`, then `C`'s pivot `(d, v)` is one of `C`'s own constraints, so the query
satisfies it — meaning the query contains `(d, v)` (or, for `variantIds`, an
array containing `v`). Hence `C` sits in a bucket this query unlocks, so `C` is
enumerated and then confirmed by `apply`. Conversely, every enumerated candidate
is re-checked with `apply`, so nothing spurious survives. The two paths produce
identical match sets.

Two subtleties handled explicitly:

- **Merge order.** Under `MergeStrategy::MERGE`/`REPLACE`, when several matched
  contexts touch the same key the *order* of merging changes the result. The
  linear scan merges in slice order, so the index de-duplicates candidate
  indices and merges them in **ascending (original slice) order** — byte-for-byte
  identical output.
- **Only scalars are indexed.** Two structurally-equal JSON objects/arrays can
  serialize to different strings, which would risk a *missed* bucket. So only
  JSON scalars (string/number/bool/null) are used as pivot keys; a context whose
  condition has no scalar constraint (rare — conditions are validated non-empty)
  falls into a small always-scanned `unindexed` list. Correctness therefore
  never depends on a value being indexable. `variantIds` is matched by
  containment: a context constrains a single id (a scalar, usable as a pivot),
  and the query carries the array of active ids, so at query time each id in the
  array unlocks its bucket.

## What changed

| # | Location | Change |
|---|----------|--------|
| 1 | `superposition_core::config` | New `ContextIndex` type: `build(&[Context])` constructs the pivot index (`postings` + `unindexed`); a private `candidates(query)` returns the sorted, de-duplicated candidate indices. |
| 2 | `superposition_core::config` | New `eval_config_with_index(...)`: same semantics as `eval_config` with no prefix filter, but resolves against a pre-built `ContextIndex`. The linear and indexed paths share a single `apply_context_override` merge helper, guaranteeing identical merge behavior. |
| 3 | `superposition_provider::client` (`CacConfig`) | The cache now stores a `CachedConfig { config, index }`; the index is built **once** whenever the config is (re)loaded (initial fetch, polling, on-demand, fallback) and reused on every resolve. `evaluate_config` takes the indexed path when there is no prefix filter (the OpenFeature `resolve_*_value` hot path) and falls back to the linear `eval_config` for the rare prefix-filter case. |

The index build (`O(total constraints)`) is paid at config-load time — off the
per-flag hot path — and amortizes to zero across the many resolves between config
refreshes. No public behavior changes; the full `superposition_core` test suite
passes, plus two new equivalence tests asserting the indexed path matches the
linear scan (including overlapping/partial/`variantIds` conditions and
merge-order sensitivity).

## Benchmark

A self-contained benchmark lives at
`crates/superposition_core/examples/resolve_index_bench.rs`. It reproduces the
reported workload — **468,006 contexts, 18 dimensions, and 100 query contexts
sampled from actual context conditions** — generated deterministically with a
tiny built-in xorshift PRNG (no external `rand`/`criterion` dependency). It
cross-checks that the linear and indexed paths agree on every query before
timing them.

```bash
# Full production-scale run (default: 468,006 contexts)
cargo run --release -p superposition_core --example resolve_index_bench

# Quicker local run with a smaller dataset
BENCH_CONTEXTS=60000 cargo run --release -p superposition_core --example resolve_index_bench
```

It reports three paths:

- **`linear (clones)`** — the current `eval_config` on this branch, which still
  deep-clones the context set per resolve (i.e. pre-`PERF_ANALYSIS_RESOLUTION`
  behavior). Included so the numbers stand on their own regardless of which
  branch is checked out.
- **`linear (borrowed)`** — a borrowed linear scan with no per-resolve clone.
  This is the faithful equivalent of the clone-removal optimization's
  `optimized_borrowed` path, and the honest baseline for judging the **index**
  in isolation.
- **`indexed`** — the new `eval_config_with_index` path.

### Results (median per-resolve)

Measured on a shared container, `avg` per resolve over 100 sampled queries:

| Scale | `linear (clones)` | `linear (borrowed)` | `indexed` | index build (one-time) | Speedup: index alone | Speedup: index + clone removal |
|-------|-------------------|---------------------|-----------|------------------------|----------------------|--------------------------------|
| 60,000 contexts  | 49.4 ms  | 3.14 ms  | 0.111 ms | 43 ms  | **~28×** | ~447× |
| 468,006 contexts | 626 ms   | 28.1 ms  | 1.30 ms  | 431 ms | **~22×** | ~480× |

The `linear (borrowed)` column reproduces the clone-removal optimization's
`optimized_borrowed` path (28.1 ms at 468k here vs the ~24–60 ms range reported
in `PERF_ANALYSIS_RESOLUTION.md`), so the **index-alone** column is the honest
apples-to-apples gain on top of that work. Note the indexed path barely grows
from 60k → 468k relative to the scan: the linear scan is `O(N)` (3.14 ms →
28.1 ms, ~9×, tracking the ~7.8× context growth), while the indexed lookup is
governed by candidate-set size, not `N`.

> **On the numbers.** These were measured on a shared container and are noisy in
> absolute terms; the defensible conclusion is the **order-of-magnitude** gap
> within a single run. Against the borrowed scan — the fair, clone-free baseline
> — the pivot index turns a full-context scan into a small candidate lookup,
> cutting per-resolve latency by **~one to two orders of magnitude**, with the
> gap widening as the context count grows (the scan is `O(N)`; the indexed
> lookup is roughly `O(candidates)` and largely independent of `N`). The index
> build is a one-time, config-load cost, not a per-resolve cost.

## Maintenance cost

### Memory

The index owns two things: one `u32` posting entry per context (each context
lands in exactly one bucket), plus a `String` key and a `Vec` header per
distinct pivot. So its footprint is `O(N + K)` where `K` is the number of
distinct pivots (`K ≤ N`). Measured at production scale (`ContextIndex::stats()`):

| Metric | 468,006 contexts |
|--------|------------------|
| pivot buckets (`K`) | 12,330 |
| posting entries | 468,006 |
| unindexed contexts | 0 |
| largest bucket | 2,541 |
| approx. heap owned by index | **~2.5 MB (~5.6 bytes/context)** |

That is roughly **1% of the `Config` it indexes** (the 468k contexts + overrides
themselves are on the order of hundreds of MB), so the representation is
effectively free memory-wise. The estimate is a lower bound (it ignores
allocator rounding and `HashMap` control bytes); real RSS is a small multiple of
it, still single-digit MB. The `largest bucket` (2,541) is the worst-case number
of `apply` checks a single pivot can trigger before verification — three orders
of magnitude below the full 468k scan, and the metric to watch for pathological
low-selectivity data.

### Updates

The index is **immutable and rebuilt from scratch** on each config (re)load;
there is no incremental/delta update. A rebuild is `O(total constraints)` —
measured **~430 ms at 468k** — and happens only on the events that already
replace the whole cached `Config`: initial fetch, polling refresh, on-demand TTL
refresh, and fallback. It is entirely **off the per-resolve hot path**.

Two properties keep the update cost cheap in practice:

- **Amortization.** The ~430 ms build is paid once per refresh and reused across
  every resolve until the next refresh. With a polling interval of seconds to
  minutes, its per-resolve share is negligible.
- **Built off the lock.** The rebuild runs *before* the cache's write lock is
  taken; only the pointer swap happens under the lock. So a refresh does not
  stall in-flight resolves for the build duration — readers see either the old
  or the new `(config, index)` pair, never a half-built one. (Peak memory during
  a swap is briefly ~2× the index — a few MB — as the old one is dropped after
  the new one is installed.)

An incremental update (patching the index for a handful of changed contexts
instead of a full rebuild) is possible but unnecessary here: the config arrives
as a full snapshot, and a full rebuild at 430 ms is already dominated by the
config fetch/deserialize it accompanies.

## Scope and follow-ups

- The index is wired into the **remote `CacConfig`** provider path measured in
  the original analysis. `local_provider.rs` (file/HTTP data sources) still uses
  the linear `eval_config`; it can adopt the same `CachedConfig { config, index }`
  pattern with `eval_config_with_index` — the core API is already public.
- The **prefix-filter** path is intentionally left on the linear scan: a prefix
  filter changes the effective context/override set and would invalidate an
  index built over the full set. It is not on the per-flag resolve hot path.
- Pathological data (a dimension whose every value is low-cardinality, so even
  the rarest pivot is common) degrades gracefully toward the linear scan rather
  than breaking — `unindexed_len()` and per-bucket sizes are the health metrics
  to watch if that is ever a concern.
