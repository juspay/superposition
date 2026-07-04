use std::collections::{HashMap, HashSet};

use serde_json::{json, Map, Value};
pub use superposition_types::api::config::MergeStrategy;
use superposition_types::{
    logic::evaluate_local_cohorts, Config, Context, DimensionInfo, Overrides,
};

pub fn eval_config(
    default_config: Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
) -> Result<Map<String, Value>, String> {
    // Create Config struct to use existing filtering logic
    let mut config = Config {
        default_configs: default_config.into(),
        contexts: contexts.to_vec(),
        overrides: overrides.clone(),
        dimensions: dimensions.clone(),
    };

    // Apply prefix filtering if keys are provided (using existing superposition_types logic)
    if let Some(prefixes) = filter_prefixes.filter(|p| !p.is_empty()) {
        config = config.filter_by_prefix(&HashSet::from_iter(prefixes.iter().cloned()));
    }

    let modified_query_data = evaluate_local_cohorts(&config.dimensions, query_data);

    let overrides_map: Map<String, Value> = get_overrides(
        &modified_query_data,
        &config.contexts,
        &config.overrides,
        &merge_strategy,
        None,
    )?;

    // Apply overrides to default config
    let mut result_config = config.default_configs;
    merge_overrides_on_default_config(&mut result_config, overrides_map, &merge_strategy);

    Ok(result_config.into_inner())
}

pub fn eval_config_with_reasoning(
    default_config: Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>, // Optional prefix filtering
) -> Result<Map<String, Value>, String> {
    let mut config = Config {
        default_configs: default_config.into(),
        contexts: contexts.to_vec(),
        overrides: overrides.clone(),
        dimensions: dimensions.clone(),
    };

    if let Some(prefixes) = filter_prefixes.filter(|p| !p.is_empty()) {
        config = config.filter_by_prefix(&HashSet::from_iter(prefixes.iter().cloned()));
    }

    let modified_query_data = evaluate_local_cohorts(&config.dimensions, query_data);

    let overrides_map = get_overrides(
        &modified_query_data,
        &config.contexts,
        &config.overrides,
        &merge_strategy,
        None,
    )?;

    let mut result_config = config.default_configs;
    merge_overrides_on_default_config(&mut result_config, overrides_map, &merge_strategy);

    Ok(result_config.into_inner())
}

/// Index-accelerated `eval_config` for the hot resolve path.
///
/// Identical semantics to [`eval_config`] with `filter_prefixes = None`, but
/// uses a pre-built [`ContextIndex`] to skip the linear scan over all contexts.
/// The index is built once when the config is loaded and reused across every
/// resolve, so its build cost is amortized to zero on the per-flag path.
///
/// Prefix filtering is intentionally not supported here: it changes which
/// contexts/overrides are in play and would invalidate the index. Callers that
/// need a prefix filter should fall back to [`eval_config`].
pub fn eval_config_with_index(
    mut default_config: Map<String, Value>,
    contexts: &[Context],
    index: &ContextIndex,
    overrides: &HashMap<String, Overrides>,
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
) -> Result<Map<String, Value>, String> {
    let modified_query_data = evaluate_local_cohorts(dimensions, query_data);

    let overrides_map = get_overrides_indexed(
        &modified_query_data,
        contexts,
        index,
        overrides,
        &merge_strategy,
        None,
    )?;

    merge_overrides_on_default_config(
        &mut default_config,
        overrides_map,
        &merge_strategy,
    );

    Ok(default_config)
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

fn replace_top_level(
    doc: &mut Map<String, Value>,
    patch: &Value,
    mut on_override: impl FnMut(),
    override_key: &String,
) {
    match patch.as_object() {
        Some(patch_map) => {
            for (key, value) in patch_map {
                doc.insert(key.clone(), value.clone());
            }
            on_override();
        }
        None => {
            log::error!(
                "Config: found non-object override key: {override_key} in overrides"
            );
        }
    }
}

/// Merges a single matched context's override into `required_overrides`,
/// honoring the merge strategy and invoking the (optional) selection callback.
///
/// Shared by the linear ([`get_overrides`]) and indexed
/// ([`get_overrides_indexed`]) resolution paths so both produce byte-for-byte
/// identical results for the same set of matched contexts.
fn apply_context_override(
    context: &Context,
    overrides: &HashMap<String, Overrides>,
    merge_strategy: &MergeStrategy,
    required_overrides: &mut Value,
    on_override_select: &mut impl FnMut(Context),
) {
    let override_key = context.override_with_keys.get_key();
    if let Some(overriden_value) = overrides.get(override_key) {
        match merge_strategy {
            MergeStrategy::REPLACE => replace_top_level(
                required_overrides.as_object_mut().unwrap(),
                &Value::Object(overriden_value.clone().into()),
                || on_override_select(context.clone()),
                override_key,
            ),
            MergeStrategy::MERGE => {
                merge(
                    required_overrides,
                    &Value::Object(overriden_value.clone().into()),
                );
                on_override_select(context.clone())
            }
        }
    }
}

fn get_overrides(
    query_data: &Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    merge_strategy: &MergeStrategy,
    mut on_override_select: Option<&mut dyn FnMut(Context)>,
) -> Result<Map<String, Value>, String> {
    let mut required_overrides: Value = json!({});
    let mut on_override_select = |context: Context| {
        if let Some(ref mut func) = on_override_select {
            func(context)
        }
    };

    for context in contexts {
        if superposition_types::apply(&context.condition, query_data) {
            apply_context_override(
                context,
                overrides,
                merge_strategy,
                &mut required_overrides,
                &mut on_override_select,
            );
        }
    }

    match required_overrides {
        Value::Object(map) => Ok(map),
        _ => Err("Failed to create overrides map".to_string()),
    }
}

/// Resolution semantics: a context matches the query only when **every**
/// `(dimension, value)` pair in its condition is satisfied. This is a purely
/// conjunctive equality predicate, which lets us avoid testing every context on
/// every resolve.
///
/// [`ContextIndex`] assigns each context a single *pivot* — the rarest
/// (most selective) scalar constraint in its condition — and buckets contexts
/// by that pivot. Because a matching context necessarily satisfies its pivot,
/// a query only has to look at the buckets its own `(dimension, value)` pairs
/// unlock, then verify each candidate with the exact same [`superposition_types::apply`]
/// check. The linear scan is `O(N * conditions)`; the indexed lookup touches
/// only candidates whose pivot the query actually hits.
///
/// ### Correctness
/// Let `C` be a context that the linear scan would match. Its pivot `(d, v)` is
/// one of its own constraints, so the query satisfies it, i.e. the query
/// contains `(d, v)` (or, for `variantIds`, an array containing `v`). Hence `C`
/// lives in a bucket that this query looks up, so `C` is enumerated and then
/// confirmed by `apply`. Conversely every enumerated candidate is re-checked
/// with `apply`, so no non-match slips through. The candidate indices are
/// de-duplicated and returned in ascending (original slice) order, so the merge
/// order — which is significant for `MergeStrategy::MERGE` — is identical to the
/// linear scan.
///
/// Only JSON scalars are used as pivot keys: two structurally-equal objects or
/// arrays can serialize to different strings, which would risk a missed bucket.
/// Contexts whose condition has no scalar constraint (rare — conditions are
/// validated non-empty) go into an always-scanned `unindexed` list, so
/// correctness never depends on a value being indexable.
#[derive(Debug, Clone, Default)]
pub struct ContextIndex {
    /// pivot key (`dimension \0 value`) -> ascending indices into the slice
    postings: HashMap<String, Vec<u32>>,
    /// contexts with no indexable scalar constraint; always scanned.
    unindexed: Vec<u32>,
}

/// Structural diagnostics for a [`ContextIndex`]; see [`ContextIndex::stats`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ContextIndexStats {
    /// Number of distinct pivot buckets.
    pub buckets: usize,
    /// Total posting entries across all buckets (equals the number of indexed
    /// contexts).
    pub postings: usize,
    /// Contexts in the always-scanned fallback list.
    pub unindexed: usize,
    /// Size of the largest bucket — the worst-case candidates a single pivot can
    /// contribute before `apply` verification.
    pub max_bucket: usize,
    /// Approximate heap bytes owned by the index (lower bound; excludes the
    /// contexts themselves).
    pub approx_heap_bytes: usize,
}

/// JSON scalars have a stable, order-independent `Display`, so they are safe to
/// use as index keys. Objects/arrays are excluded.
#[inline]
fn is_indexable_scalar(value: &Value) -> bool {
    matches!(
        value,
        Value::String(_) | Value::Number(_) | Value::Bool(_) | Value::Null
    )
}

/// Builds a `dimension \0 value` key. The NUL separator cannot occur in a
/// dimension name or in serde_json's compact scalar rendering, so keys are
/// unambiguous.
#[inline]
fn index_key(dimension: &str, value: &Value) -> String {
    use std::fmt::Write;
    let mut key = String::with_capacity(dimension.len() + 8);
    key.push_str(dimension);
    key.push('\0');
    let _ = write!(key, "{value}");
    key
}

impl ContextIndex {
    /// Builds the pivot index for a set of contexts. `O(total constraints)`.
    /// Build this once when the config is (re)loaded and reuse it across every
    /// resolve.
    pub fn build(contexts: &[Context]) -> Self {
        // Pass 1: frequency of every indexable (dimension, value) constraint,
        // so each context can pick its rarest constraint as its pivot.
        let mut freq: HashMap<String, u32> = HashMap::new();
        for context in contexts {
            for (dimension, value) in context.condition.iter() {
                if is_indexable_scalar(value) {
                    *freq.entry(index_key(dimension, value)).or_insert(0) += 1;
                }
            }
        }

        // Pass 2: bucket each context under its rarest scalar constraint.
        let mut postings: HashMap<String, Vec<u32>> = HashMap::new();
        let mut unindexed = Vec::new();
        for (idx, context) in contexts.iter().enumerate() {
            let idx = idx as u32;
            let mut best: Option<(u32, String)> = None;
            for (dimension, value) in context.condition.iter() {
                if !is_indexable_scalar(value) {
                    continue;
                }
                let key = index_key(dimension, value);
                let f = freq.get(&key).copied().unwrap_or(u32::MAX);
                if best.as_ref().is_none_or(|(bf, _)| f < *bf) {
                    best = Some((f, key));
                }
            }
            match best {
                // Contexts are visited in slice order, so each posting list is
                // naturally ascending — no sort needed here.
                Some((_, key)) => postings.entry(key).or_default().push(idx),
                None => unindexed.push(idx),
            }
        }

        ContextIndex {
            postings,
            unindexed,
        }
    }

    /// Number of contexts that fell back to the always-scanned list.
    /// Exposed for diagnostics / tests; ideally zero.
    pub fn unindexed_len(&self) -> usize {
        self.unindexed.len()
    }

    /// Structural diagnostics: number of pivot buckets, the largest bucket, and
    /// an estimate of the heap bytes the index itself owns (excluding the
    /// contexts it points into). Useful for capacity planning and for spotting
    /// pathological low-selectivity data.
    pub fn stats(&self) -> ContextIndexStats {
        let buckets = self.postings.len();
        let mut postings = 0usize;
        let mut max_bucket = 0usize;
        let mut key_bytes = 0usize;
        for (key, list) in &self.postings {
            postings += list.len();
            max_bucket = max_bucket.max(list.len());
            key_bytes += key.len();
        }
        // Rough heap estimate: posting entries (u32) + per-Vec headers +
        // per-key String headers + the key bytes themselves. Ignores allocator
        // rounding and HashMap control bytes, so treat as a lower bound.
        let posting_bytes = (postings + self.unindexed.len()) * 4;
        let vec_headers = buckets * std::mem::size_of::<Vec<u32>>();
        let key_headers = buckets * std::mem::size_of::<String>();
        ContextIndexStats {
            buckets,
            postings,
            unindexed: self.unindexed.len(),
            max_bucket,
            approx_heap_bytes: posting_bytes + vec_headers + key_headers + key_bytes,
        }
    }

    /// Collects the (sorted, de-duplicated) indices of contexts that could
    /// match the query — those whose pivot bucket the query unlocks, plus all
    /// unindexed contexts. These are candidates only; the caller must still run
    /// `apply` on each.
    fn candidates(&self, query_data: &Map<String, Value>) -> Vec<u32> {
        let mut out: Vec<u32> = self.unindexed.clone();
        let mut push_bucket = |key: &str| {
            if let Some(list) = self.postings.get(key) {
                out.extend_from_slice(list);
            }
        };
        for (dimension, value) in query_data {
            match value {
                // `variantIds` matches by containment: a context constrains a
                // single id, the query carries the array of active ids. Unlock
                // the bucket for each id in the query array.
                Value::Array(elems) if dimension == "variantIds" => {
                    for elem in elems {
                        if is_indexable_scalar(elem) {
                            push_bucket(&index_key(dimension, elem));
                        }
                    }
                }
                scalar if is_indexable_scalar(scalar) => {
                    push_bucket(&index_key(dimension, scalar));
                }
                // Non-scalar, non-variantIds query values can never equal a
                // scalar pivot, so they unlock nothing.
                _ => {}
            }
        }
        out.sort_unstable();
        out.dedup();
        out
    }
}

/// Index-accelerated equivalent of [`get_overrides`]. Produces identical output
/// for the same `contexts`/`overrides`/`query_data`; only the candidate
/// enumeration differs.
fn get_overrides_indexed(
    query_data: &Map<String, Value>,
    contexts: &[Context],
    index: &ContextIndex,
    overrides: &HashMap<String, Overrides>,
    merge_strategy: &MergeStrategy,
    mut on_override_select: Option<&mut dyn FnMut(Context)>,
) -> Result<Map<String, Value>, String> {
    let mut required_overrides: Value = json!({});
    let mut on_override_select = |context: Context| {
        if let Some(ref mut func) = on_override_select {
            func(context)
        }
    };

    for idx in index.candidates(query_data) {
        let context = &contexts[idx as usize];
        if superposition_types::apply(&context.condition, query_data) {
            apply_context_override(
                context,
                overrides,
                merge_strategy,
                &mut required_overrides,
                &mut on_override_select,
            );
        }
    }

    match required_overrides {
        Value::Object(map) => Ok(map),
        _ => Err("Failed to create overrides map".to_string()),
    }
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

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use serde_json::{json, Map, Value};
    use superposition_types::{Cac, Condition, Context, OverrideWithKeys, Overrides};

    use super::*;

    fn value_map(values: Vec<(&str, Value)>) -> Map<String, Value> {
        values
            .into_iter()
            .map(|(key, value)| (key.to_string(), value))
            .collect()
    }

    fn condition(values: Vec<(&str, Value)>) -> Condition {
        Cac::<Condition>::try_from(value_map(values))
            .unwrap()
            .into_inner()
    }

    fn overrides(values: Vec<(&str, Value)>) -> Overrides {
        Cac::<Overrides>::try_from(value_map(values))
            .unwrap()
            .into_inner()
    }

    #[test]
    fn eval_config_with_reasoning_does_not_add_metadata_key() {
        let default_config = value_map(vec![("checkout.enabled", json!(false))]);
        let context = Context {
            id: "c0".to_string(),
            condition: condition(vec![("country", json!("IN"))]),
            priority: 0,
            weight: 0,
            override_with_keys: OverrideWithKeys::new("o0".to_string()),
        };
        let overrides = HashMap::from([(
            "o0".to_string(),
            overrides(vec![("checkout.enabled", json!(true))]),
        )]);
        let query_data = value_map(vec![("country", json!("IN"))]);

        let resolved = eval_config_with_reasoning(
            default_config,
            &[context],
            &overrides,
            &HashMap::new(),
            &query_data,
            MergeStrategy::MERGE,
            None,
        )
        .unwrap();

        assert_eq!(resolved.get("checkout.enabled"), Some(&json!(true)));
        assert!(!resolved.contains_key("metadata"));
    }

    fn ctx(
        id: &str,
        cond: Vec<(&str, Value)>,
        priority: i32,
        override_key: &str,
    ) -> Context {
        Context {
            id: id.to_string(),
            condition: condition(cond),
            priority,
            weight: 0,
            override_with_keys: OverrideWithKeys::new(override_key.to_string()),
        }
    }

    /// The indexed path must produce byte-for-byte identical results to the
    /// linear scan across a spread of overlapping/partial/variantIds contexts.
    #[test]
    fn indexed_matches_linear_scan() {
        let default_config = value_map(vec![
            ("theme", json!("light")),
            ("banner", json!("none")),
            ("limit", json!(10)),
        ]);

        let contexts = vec![
            ctx("c0", vec![("country", json!("IN"))], 0, "o0"),
            ctx(
                "c1",
                vec![("country", json!("IN")), ("tier", json!("gold"))],
                1,
                "o1",
            ),
            ctx("c2", vec![("country", json!("US"))], 2, "o2"),
            ctx("c3", vec![("tier", json!("gold"))], 3, "o3"),
            ctx("c4", vec![("os", json!("android"))], 4, "o4"),
            // variantIds is matched by containment against the query array.
            ctx("c5", vec![("variantIds", json!("v1"))], 5, "o5"),
        ];

        let overrides = HashMap::from([
            ("o0".to_string(), overrides(vec![("theme", json!("dark"))])),
            (
                "o1".to_string(),
                overrides(vec![("banner", json!("promo"))]),
            ),
            ("o2".to_string(), overrides(vec![("theme", json!("usa"))])),
            ("o3".to_string(), overrides(vec![("limit", json!(50))])),
            ("o4".to_string(), overrides(vec![("theme", json!("droid"))])),
            ("o5".to_string(), overrides(vec![("banner", json!("beta"))])),
        ]);

        let dimensions = HashMap::new();
        let index = ContextIndex::build(&contexts);

        let queries = vec![
            value_map(vec![("country", json!("IN"))]),
            value_map(vec![("country", json!("IN")), ("tier", json!("gold"))]),
            value_map(vec![("country", json!("US")), ("tier", json!("gold"))]),
            value_map(vec![("os", json!("android"))]),
            value_map(vec![("variantIds", json!(["v1", "v9"]))]),
            value_map(vec![
                ("country", json!("IN")),
                ("tier", json!("gold")),
                ("os", json!("android")),
                ("variantIds", json!(["v1"])),
            ]),
            value_map(vec![("country", json!("ZZ"))]), // matches nothing
            value_map(vec![]),                         // empty query
        ];

        for query in queries {
            let linear = eval_config(
                default_config.clone(),
                &contexts,
                &overrides,
                &dimensions,
                &query,
                MergeStrategy::MERGE,
                None,
            )
            .unwrap();

            let indexed = eval_config_with_index(
                default_config.clone(),
                &contexts,
                &index,
                &overrides,
                &dimensions,
                &query,
                MergeStrategy::MERGE,
            )
            .unwrap();

            assert_eq!(indexed, linear, "mismatch for query {query:?}");
        }
    }

    /// MERGE order is significant when multiple matched contexts touch the same
    /// key; the indexed path must merge in original slice order just like the
    /// linear scan.
    #[test]
    fn indexed_preserves_merge_order() {
        let default_config = value_map(vec![("k", json!("base"))]);
        let contexts = vec![
            ctx("c0", vec![("a", json!(1))], 0, "o0"),
            ctx("c1", vec![("b", json!(2))], 1, "o1"),
            ctx("c2", vec![("a", json!(1)), ("b", json!(2))], 2, "o2"),
        ];
        let overrides = HashMap::from([
            ("o0".to_string(), overrides(vec![("k", json!("first"))])),
            ("o1".to_string(), overrides(vec![("k", json!("second"))])),
            ("o2".to_string(), overrides(vec![("k", json!("third"))])),
        ]);
        let dimensions = HashMap::new();
        let index = ContextIndex::build(&contexts);
        let query = value_map(vec![("a", json!(1)), ("b", json!(2))]);

        let linear = eval_config(
            default_config.clone(),
            &contexts,
            &overrides,
            &dimensions,
            &query,
            MergeStrategy::REPLACE,
            None,
        )
        .unwrap();
        let indexed = eval_config_with_index(
            default_config,
            &contexts,
            &index,
            &overrides,
            &dimensions,
            &query,
            MergeStrategy::REPLACE,
        )
        .unwrap();

        // Last matched context in slice order (c2 -> "third") wins under REPLACE.
        assert_eq!(indexed.get("k"), Some(&json!("third")));
        assert_eq!(indexed, linear);
        assert_eq!(index.unindexed_len(), 0);
    }
}
