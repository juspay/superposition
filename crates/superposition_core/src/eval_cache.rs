//! A memory-bounded LRU cache for repeated evaluation (`eval_config`) queries.
//!
//! Callers only choose the budget (in megabytes). Entries are keyed by a
//! [`blake3`] digest of every input that influences resolution. Data freshness
//! is up to the caller: the FFI [`ProviderCache`][crate::ffi::ProviderCache]
//! clears its cache whenever new config or experiment data is loaded, while
//! other embedders (e.g. the OpenFeature providers) can instead version their
//! data and pass the versions through [`EvaluationCache::key`], which folds
//! them into the digest so refreshed data keys differently.

use std::collections::HashMap;

use blake3::Hasher;
use lru::LruCache;
use serde_json::{Map, Value};
use superposition_types::api::config::MergeStrategy;

/// Domain/version prefix so a future key-format change cannot collide with
/// digests produced by older builds.
const KEY_PREFIX: &[u8] = b"superposition-eval-cache/v1";

/// Rough bookkeeping cost per cached entry (LRU node + digest + map header).
/// Exact allocator sizes are unknowable; the budget is an approximation, so a
/// constant overhead keeps thousands of tiny entries honest.
const ENTRY_OVERHEAD: usize = 96;

/// Rough cost of one key/value pair held by a cached result map.
const PAIR_OVERHEAD: usize = 48;

fn json_value_size(v: &Value) -> usize {
    match v {
        Value::Null | Value::Bool(_) => 8,
        Value::Number(_) => 16,
        Value::String(s) => 24 + s.len(),
        Value::Array(items) => 24 + items.iter().map(json_value_size).sum::<usize>(),
        Value::Object(map) => {
            24 + map
                .iter()
                .map(|(k, v)| k.len() + PAIR_OVERHEAD + json_value_size(v))
                .sum::<usize>()
        }
    }
}

fn result_size(result: &Map<String, Value>) -> usize {
    ENTRY_OVERHEAD
        + result
            .iter()
            .map(|(k, v)| k.len() + PAIR_OVERHEAD + json_value_size(v))
            .sum::<usize>()
}

fn feed_str(hasher: &mut Hasher, s: &str) {
    hasher.update(&(s.len() as u64).to_le_bytes());
    hasher.update(s.as_bytes());
}

fn feed_opt_list(hasher: &mut Hasher, list: Option<&[String]>) {
    match list {
        Some(items) => {
            let mut sorted: Vec<&String> = items.iter().collect();
            sorted.sort();
            hasher.update(&(sorted.len() as u64).to_le_bytes());
            for s in sorted {
                feed_str(hasher, s);
            }
        }
        // u64::MAX: cannot be confused with a real length.
        None => {
            hasher.update(&u64::MAX.to_le_bytes());
        }
    };
}

/// Query-argument pairs canonicalized for keying: sorted so that two semantically
/// identical queries presented in different orders map to the same digest.
pub fn pairs_from_string_map(
    query_data: &HashMap<String, String>,
) -> Vec<(String, String)> {
    let mut pairs: Vec<(String, String)> = query_data
        .iter()
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect();
    pairs.sort();
    pairs
}

/// Same as [`pairs_from_string_map`] for raw JSON query data. Values are
/// re-serialized, matching the JSON-string form a string-based caller supplies.
pub fn pairs_from_json_map(query_data: &Map<String, Value>) -> Vec<(String, String)> {
    let mut pairs: Vec<(String, String)> = query_data
        .iter()
        .map(|(k, v)| (k.clone(), serde_json::to_string(v).unwrap_or_default()))
        .collect();
    pairs.sort();
    pairs
}

/// Memory-bounded LRU cache of evaluation results.
///
/// `Default` and `EvaluationCache::new(0)` both leave the cache disabled; all
/// operations are then cheap no-ops.
pub struct EvaluationCache {
    inner: LruCache<blake3::Hash, Map<String, Value>>,
    bytes_used: usize,
    max_bytes: usize,
}

impl std::fmt::Debug for EvaluationCache {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EvaluationCache")
            .field("entries", &self.inner.len())
            .field("bytes_used", &self.bytes_used)
            .field("max_bytes", &self.max_bytes)
            .finish()
    }
}

impl Default for EvaluationCache {
    fn default() -> Self {
        Self::new(0)
    }
}

impl EvaluationCache {
    /// Create a cache with the given memory budget in megabytes. `0` disables
    /// the cache.
    pub fn new(max_size_mb: u64) -> Self {
        let mb = usize::try_from(max_size_mb).unwrap_or(usize::MAX);
        Self {
            inner: LruCache::unbounded(),
            bytes_used: 0,
            max_bytes: mb.saturating_mul(1024 * 1024),
        }
    }

    pub fn is_enabled(&self) -> bool {
        self.max_bytes > 0
    }

    /// Digest every input that can change the outcome of a resolution. Callers
    /// pass the query as pre-sorted pairs via [`pairs_from_string_map`] or
    /// [`pairs_from_json_map`]; prefix lists are sorted here.
    ///
    /// `data_version` is an opaque pair of generation counters (config data,
    /// experiment data). Callers that clear the cache themselves on every data
    /// reload pass `(0, 0)`; callers that let refresh bump versions pass their
    /// current values so post-refresh entries key differently.
    pub fn key(
        data_version: (u64, u64),
        query_pairs: &[(String, String)],
        merge_strategy: MergeStrategy,
        filter_prefixes: Option<&[String]>,
        filter_exclude_prefixes: Option<&[String]>,
        targeting_key: Option<&str>,
    ) -> blake3::Hash {
        let mut hasher = Hasher::new();
        hasher.update(KEY_PREFIX);
        hasher.update(&data_version.0.to_le_bytes());
        hasher.update(&data_version.1.to_le_bytes());
        hasher.update(&[match merge_strategy {
            MergeStrategy::MERGE => 1,
            MergeStrategy::REPLACE => 2,
        }]);
        match targeting_key {
            Some(key) => {
                hasher.update(&[1]);
                feed_str(&mut hasher, key);
            }
            None => {
                hasher.update(&[0]);
            }
        };
        feed_opt_list(&mut hasher, filter_prefixes);
        feed_opt_list(&mut hasher, filter_exclude_prefixes);
        hasher.update(&(query_pairs.len() as u64).to_le_bytes());
        for (k, v) in query_pairs {
            feed_str(&mut hasher, k);
            feed_str(&mut hasher, v);
        }
        hasher.finalize()
    }

    /// Look up a cached result; a hit also promotes the entry. Returns a clone
    /// because the cache must keep ownership of the stored value.
    pub fn get(&mut self, key: &blake3::Hash) -> Option<Map<String, Value>> {
        self.inner.get(key).cloned()
    }

    /// Store a result, evicting least-recently-used entries until the total
    /// estimated size is within budget. Results that alone exceed the budget
    /// are never cached.
    pub fn insert(&mut self, key: blake3::Hash, result: Map<String, Value>) {
        if !self.is_enabled() {
            return;
        }
        let entry_size = result_size(&result);
        if entry_size > self.max_bytes {
            return;
        }

        while self.bytes_used + entry_size > self.max_bytes {
            match self.inner.pop_lru() {
                Some((_, evicted)) => {
                    self.bytes_used =
                        self.bytes_used.saturating_sub(result_size(&evicted));
                }
                None => break,
            }
        }

        // Defensive: `insert` normally runs only after `get` missed, but if the
        // key already existed the displaced value must leave the accounting.
        if let Some((_, old)) = self.inner.push(key, result) {
            self.bytes_used = self.bytes_used.saturating_sub(result_size(&old));
        }
        self.bytes_used += entry_size;
    }

    /// Drop all entries, e.g. when new config or experiment data is loaded.
    pub fn clear(&mut self) {
        self.inner.clear();
        self.bytes_used = 0;
    }

    /// Number of entries currently held (excluding bookkeeping for byte usage).
    pub fn cached_entry_count(&self) -> usize {
        self.inner.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn result(pairs: &[(&str, u64)]) -> Map<String, Value> {
        pairs
            .iter()
            .map(|(k, v)| (k.to_string(), Value::from(*v)))
            .collect()
    }

    fn query(pairs: &[(&str, &str)]) -> HashMap<String, String> {
        pairs
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect()
    }

    #[test]
    fn key_is_order_independent() {
        let a = pairs_from_string_map(&query(&[("os", "linux"), ("ver", "1")]));
        let b = pairs_from_string_map(&query(&[("ver", "1"), ("os", "linux")]));
        let prefixes = Some(vec!["a".to_string(), "b".to_string()]);
        let prefixes_rev = Some(vec!["b".to_string(), "a".to_string()]);

        let ka = EvaluationCache::key(
            (0, 0),
            &a,
            MergeStrategy::MERGE,
            prefixes.as_deref(),
            None,
            Some("user-1"),
        );
        let kb = EvaluationCache::key(
            (0, 0),
            &b,
            MergeStrategy::MERGE,
            prefixes_rev.as_deref(),
            None,
            Some("user-1"),
        );
        assert_eq!(ka, kb);
    }

    #[test]
    fn key_changes_with_any_input() {
        let pairs = pairs_from_string_map(&query(&[("os", "linux")]));
        let base =
            EvaluationCache::key((0, 0), &pairs, MergeStrategy::MERGE, None, None, None);

        let other_query = pairs_from_string_map(&query(&[("os", "macos")]));
        assert_ne!(
            base,
            EvaluationCache::key(
                (0, 0),
                &other_query,
                MergeStrategy::MERGE,
                None,
                None,
                None
            )
        );
        assert_ne!(
            base,
            EvaluationCache::key(
                (0, 0),
                &pairs,
                MergeStrategy::REPLACE,
                None,
                None,
                None
            )
        );
        assert_ne!(
            base,
            EvaluationCache::key(
                (0, 0),
                &pairs,
                MergeStrategy::MERGE,
                None,
                None,
                Some("k")
            )
        );
        let prefixes = Some(vec!["p".to_string()]);
        assert_ne!(
            base,
            EvaluationCache::key(
                (0, 0),
                &pairs,
                MergeStrategy::MERGE,
                prefixes.as_deref(),
                None,
                None
            )
        );
    }

    #[test]
    fn key_changes_with_data_version() {
        let pairs = pairs_from_string_map(&query(&[("os", "linux")]));
        assert_ne!(
            EvaluationCache::key((0, 0), &pairs, MergeStrategy::MERGE, None, None, None),
            EvaluationCache::key((1, 0), &pairs, MergeStrategy::MERGE, None, None, None)
        );
        assert_ne!(
            EvaluationCache::key((0, 0), &pairs, MergeStrategy::MERGE, None, None, None),
            EvaluationCache::key((0, 1), &pairs, MergeStrategy::MERGE, None, None, None)
        );
    }

    #[test]
    fn hit_returns_inserted_result() {
        let mut cache = EvaluationCache::new(1);
        let pairs = pairs_from_string_map(&query(&[("os", "linux")]));
        let key =
            EvaluationCache::key((0, 0), &pairs, MergeStrategy::MERGE, None, None, None);
        let value = result(&[("timeout", 30)]);

        cache.insert(key, value.clone());
        assert_eq!(cache.get(&key), Some(value));
    }

    #[test]
    fn evicts_lru_entries_to_stay_within_budget() {
        // 1 MB budget; each entry is ~100 bytes of content, so all fit; force
        // eviction by using a tiny manual budget instead.
        let mut cache = EvaluationCache::new(1);
        cache.max_bytes = result_size(&result(&[("a", 1)])) * 2 + ENTRY_OVERHEAD;

        let key_of = |q: &str| {
            EvaluationCache::key(
                (0, 0),
                &pairs_from_string_map(&query(&[("q", q)])),
                MergeStrategy::MERGE,
                None,
                None,
                None,
            )
        };
        let k1 = key_of("1");
        let k2 = key_of("2");
        let k3 = key_of("3");

        cache.insert(k1, result(&[("a", 1)]));
        cache.insert(k2, result(&[("a", 2)]));
        cache.get(&k1); // promote k1: k2 is now least-recently-used
        cache.insert(k3, result(&[("a", 3)])); // must evict k2, not k1

        assert_eq!(cache.cached_entry_count(), 2);
        assert!(cache.get(&k1).is_some());
        assert!(cache.get(&k2).is_none());
        assert!(cache.get(&k3).is_some());
    }

    #[test]
    fn result_larger_than_budget_is_never_cached() {
        let mut cache = EvaluationCache::new(1);
        cache.max_bytes = 1;
        let pairs = pairs_from_string_map(&query(&[("os", "linux")]));
        let key =
            EvaluationCache::key((0, 0), &pairs, MergeStrategy::MERGE, None, None, None);

        cache.insert(key, result(&[("timeout", 30)]));
        assert_eq!(cache.cached_entry_count(), 0);
        assert!(cache.get(&key).is_none());
    }

    #[test]
    fn disabled_cache_is_a_noop() {
        let mut cache = EvaluationCache::default();
        assert!(!cache.is_enabled());
        let pairs = pairs_from_string_map(&query(&[("os", "linux")]));
        let key =
            EvaluationCache::key((0, 0), &pairs, MergeStrategy::MERGE, None, None, None);

        cache.insert(key, result(&[("timeout", 30)]));
        assert!(cache.get(&key).is_none());
    }

    #[test]
    fn clear_empties_the_cache() {
        let mut cache = EvaluationCache::new(1);
        let pairs = pairs_from_string_map(&query(&[("os", "linux")]));
        let key =
            EvaluationCache::key((0, 0), &pairs, MergeStrategy::MERGE, None, None, None);

        cache.insert(key, result(&[("timeout", 30)]));
        assert!(cache.bytes_used > 0);
        cache.clear();
        assert_eq!(cache.cached_entry_count(), 0);
        assert_eq!(cache.bytes_used, 0);
    }
}
