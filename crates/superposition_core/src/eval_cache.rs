//! Key derivation for the evaluation cache: a blake3 digest of every input that discriminates a resolution.

use std::num::NonZeroUsize;

use lru::LruCache;
use serde_json::{Map, Value};
use superposition_types::api::config::MergeStrategy;

/// Store for memoized resolutions, keyed by [`key`] digests.
pub type EvalCache = LruCache<blake3::Hash, Map<String, Value>>;

/// A bounded LRU holding up to `max_entries` resolutions; `0` disables
/// caching (`None`).
pub fn new(max_entries: u64) -> Option<EvalCache> {
    NonZeroUsize::new(usize::try_from(max_entries).unwrap_or(usize::MAX))
        .map(LruCache::new)
}

fn sorted(list: Option<&[String]>) -> Option<Vec<&String>> {
    list.map(|items| {
        let mut sorted: Vec<&String> = items.iter().collect();
        sorted.sort();
        sorted
    })
}

/// Blake3 digest of the context query plus the inputs that discriminate
/// resolution (merge strategy, prefix filters, targeting key). Query pairs and
/// prefix lists are sorted before serialization, so semantically identical
/// requests presented in different orders map to the same digest — regardless
/// of whether `serde_json` preserves insertion order (`preserve_order`
/// feature can flip under feature unification).
pub fn key(
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<&[String]>,
    filter_exclude_prefixes: Option<&[String]>,
    targeting_key: Option<&str>,
) -> blake3::Hash {
    let mut pairs: Vec<(&String, &Value)> = query_data.iter().collect();
    pairs.sort_by(|a, b| a.0.cmp(b.0));

    let canonical = serde_json::to_string(&(
        merge_strategy.to_string(),
        sorted(filter_prefixes),
        sorted(filter_exclude_prefixes),
        targeting_key,
        pairs,
    ))
    .unwrap_or_default();
    blake3::hash(canonical.as_bytes())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn query(pairs: &[(&str, &str)]) -> Map<String, Value> {
        pairs
            .iter()
            .map(|(k, v)| (k.to_string(), Value::from(*v)))
            .collect()
    }

    fn key_of(query_data: &Map<String, Value>) -> blake3::Hash {
        key(query_data, MergeStrategy::MERGE, None, None, None)
    }

    #[test]
    fn key_is_order_independent() {
        let a = query(&[("os", "linux"), ("ver", "1")]);
        let b = query(&[("ver", "1"), ("os", "linux")]);
        let prefixes = Some(vec!["a".to_string(), "b".to_string()]);
        let prefixes_rev = Some(vec!["b".to_string(), "a".to_string()]);

        let ka = key(
            &a,
            MergeStrategy::MERGE,
            prefixes.as_deref(),
            None,
            Some("user-1"),
        );
        let kb = key(
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
        let q = query(&[("os", "linux")]);
        let base = key_of(&q);

        let other_query = query(&[("os", "macos")]);
        assert_ne!(base, key_of(&other_query));
        assert_ne!(base, key(&q, MergeStrategy::REPLACE, None, None, None));
        assert_ne!(base, key(&q, MergeStrategy::MERGE, None, None, Some("k")));
        let prefixes = Some(vec!["p".to_string()]);
        assert_ne!(
            base,
            key(&q, MergeStrategy::MERGE, prefixes.as_deref(), None, None)
        );
        let exclude = Some(vec!["e".to_string()]);
        assert_ne!(
            base,
            key(&q, MergeStrategy::MERGE, None, exclude.as_deref(), None)
        );
    }
}
