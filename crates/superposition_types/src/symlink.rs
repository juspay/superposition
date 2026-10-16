//! How a default-config symlink is represented on disk.
//!
//! A symlink is an ordinary `default_configs` row: its `value` holds the target
//! key's name and its `schema` carries [`SYMLINK_KEYWORD`] set to `true`. The row
//! is therefore self-validating — the schema genuinely describes the value — so the
//! existing create/update validation gate does real work on link rows.

use serde_json::{Map, Value};

use crate::RegexEnum;

/// Schema keyword marking a row as a symlink to another default-config key.
///
/// Draft-7 ignores unknown keywords, so a schema carrying this compiles and
/// validates through the existing `try_into_jsonschema` path with no resolver.
pub const SYMLINK_KEYWORD: &str = "x-superposition-symlink";

/// Keywords a symlink schema may carry besides the marker itself.
const ALLOWED_KEYWORDS: [&str; 2] = ["type", "pattern"];

/// A symlink row, as the read path needs it.
#[derive(Debug, Clone, PartialEq)]
pub struct SymlinkRow {
    pub key: String,
    pub target: String,
    /// The link's own description, not the target's.
    pub description: String,
}

/// Whether this schema marks its row as a symlink.
///
/// The boolean marker is the *only* discriminator: a row with
/// `{"type": "string"}` whose value happens to look like a key name is an
/// ordinary config.
///
/// This is the read-side definition, and it must agree exactly with the SQL
/// predicates in `context_aware_config::symlinks` — see
/// [`carries_symlink_marker`] for why.
pub fn is_symlink_schema(schema: &Map<String, Value>) -> bool {
    schema.get(SYMLINK_KEYWORD) == Some(&Value::Bool(true))
}

/// Whether this schema mentions the symlink marker *at all*, whatever its value.
///
/// The write path must branch on this rather than on [`is_symlink_schema`], so
/// that a marker which is present but not the boolean `true` reaches
/// [`normalize_symlink_write`] and is rejected, instead of being waved through as
/// an ordinary key. A waved-through row is how a caller could once store
/// `{"x-superposition-symlink": "true"}` — a JSON *string* — which the read path's
/// `->>` unquoting treated as a link while this Rust side treated it as an
/// ordinary key, skipping the symlink target's authorization check entirely.
///
/// The invariant the two functions buy together: a stored row either carries no
/// marker, or carries the boolean `true`. Nothing else can be written.
pub fn carries_symlink_marker(schema: &Map<String, Value>) -> bool {
    schema.contains_key(SYMLINK_KEYWORD)
}

/// The target key a symlink row points at, or `None` for an ordinary row.
pub fn symlink_target<'a>(
    schema: &Map<String, Value>,
    value: &'a Value,
) -> Option<&'a str> {
    if is_symlink_schema(schema) {
        value.as_str()
    } else {
        None
    }
}

/// The canonical schema stored on every symlink row.
pub fn canonical_symlink_schema() -> Map<String, Value> {
    let mut schema = Map::new();
    schema.insert("type".to_string(), Value::String("string".to_string()));
    schema.insert(
        "pattern".to_string(),
        Value::String(RegexEnum::DefaultConfigKey.to_string()),
    );
    schema.insert(SYMLINK_KEYWORD.to_string(), Value::Bool(true));
    schema
}

/// Validates a write carrying the symlink marker.
///
/// Returns the target key and the canonical schema to store, so a caller may send
/// the short form `{"x-superposition-symlink": true}` and never duplicate the key
/// regex.
pub fn normalize_symlink_write(
    schema: &Map<String, Value>,
    value: &Value,
) -> Result<(String, Map<String, Value>), String> {
    match schema.get(SYMLINK_KEYWORD) {
        Some(Value::Bool(true)) => {}
        Some(other) => {
            return Err(format!(
                "{SYMLINK_KEYWORD} must be the boolean true, got {other}"
            ))
        }
        None => return Err(format!("{SYMLINK_KEYWORD} is absent")),
    }

    if let Some(unexpected) = schema.keys().find(|key| {
        key.as_str() != SYMLINK_KEYWORD && !ALLOWED_KEYWORDS.contains(&key.as_str())
    }) {
        return Err(format!(
            "a symlink schema cannot carry {unexpected}; it may only carry \
             {SYMLINK_KEYWORD}, type and pattern"
        ));
    }

    if let Some(declared) = schema.get("type") {
        if declared != &Value::String("string".to_string()) {
            return Err("a symlink schema's type must be \"string\"".to_string());
        }
    }

    let canonical = canonical_symlink_schema();
    if let Some(declared) = schema.get("pattern") {
        if Some(declared) != canonical.get("pattern") {
            return Err(
                "a symlink schema's pattern must be the default-config key pattern"
                    .to_string(),
            );
        }
    }

    let target = value
        .as_str()
        .ok_or("a symlink's value must be the target key, as a string")?;

    RegexEnum::DefaultConfigKey.match_regex(target)?;

    Ok((target.to_string(), canonical))
}

#[cfg(test)]
mod tests {
    use serde_json::{json, Map, Value};

    use super::*;

    fn map(value: Value) -> Map<String, Value> {
        value
            .as_object()
            .expect("fixture must be an object")
            .clone()
    }

    #[test]
    fn short_form_normalizes_to_canonical_schema() {
        let schema = map(json!({ SYMLINK_KEYWORD: true }));
        let value = json!("payments.retry.count");

        let (target, stored) = normalize_symlink_write(&schema, &value)
            .expect("short form should be accepted");

        assert_eq!(target, "payments.retry.count");
        assert_eq!(stored.get("type"), Some(&json!("string")));
        assert_eq!(stored.get(SYMLINK_KEYWORD), Some(&json!(true)));
        assert!(stored.contains_key("pattern"));
    }

    #[test]
    fn canonical_form_round_trips() {
        let schema = canonical_symlink_schema();
        let value = json!("payments.retry.count");

        let (target, stored) = normalize_symlink_write(&schema, &value)
            .expect("canonical form should be accepted");

        assert_eq!(target, "payments.retry.count");
        assert_eq!(stored, canonical_symlink_schema());
    }

    #[test]
    fn a_key_shaped_string_value_is_not_a_symlink() {
        // Review Focus 2: an ordinary string config whose value happens to look
        // like a key name must never be mistaken for a link.
        let schema = map(json!({ "type": "string" }));
        let value = json!("payments.retry.count");

        assert!(!is_symlink_schema(&schema));
        assert_eq!(symlink_target(&schema, &value), None);
    }

    #[test]
    fn marker_must_be_boolean_true() {
        for marker in [json!("true"), json!(1), json!(false), json!(null)] {
            let schema = map(json!({ SYMLINK_KEYWORD: marker }));
            assert!(!is_symlink_schema(&schema));
            assert!(normalize_symlink_write(&schema, &json!("a.b")).is_err());
        }
    }

    #[test]
    fn a_string_true_marker_is_routed_to_the_write_gate_and_rejected() {
        // The authorization bypass this closes: `->>` unquotes, so the JSON
        // string "true" matched the read path's SQL predicate while
        // `is_symlink_schema` said "ordinary key" — and the write path, gated on
        // `is_symlink_schema`, skipped the target's authorization check. The
        // write gate is `carries_symlink_marker`, so the rejection below is the
        // one that actually fires.
        let schema = map(json!({ "type": "string", SYMLINK_KEYWORD: "true" }));

        assert!(
            carries_symlink_marker(&schema),
            "the write path must route a present-but-not-boolean marker into the \
             symlink branch, or its rejection is dead code"
        );
        assert!(!is_symlink_schema(&schema));

        let err = normalize_symlink_write(&schema, &json!("restricted.key"))
            .expect_err("a string marker must be refused");
        assert!(
            err.contains("must be the boolean true"),
            "error should name the requirement: {err}"
        );
    }

    #[test]
    fn carries_the_marker_is_wider_than_is_a_symlink() {
        for marker in [json!(true), json!(false), json!("true"), json!(null)] {
            let schema = map(json!({ SYMLINK_KEYWORD: marker }));
            assert!(carries_symlink_marker(&schema));
        }
        assert!(!carries_symlink_marker(&map(json!({ "type": "string" }))));
    }

    #[test]
    fn extra_keywords_are_rejected() {
        let schema = map(json!({ SYMLINK_KEYWORD: true, "minLength": 3 }));

        let err = normalize_symlink_write(&schema, &json!("a.b"))
            .expect_err("a half-schema half-link row must be rejected");

        assert!(
            err.contains("minLength"),
            "error should name the keyword: {err}"
        );
    }

    #[test]
    fn declared_type_must_be_string() {
        let schema = map(json!({ SYMLINK_KEYWORD: true, "type": "integer" }));
        assert!(normalize_symlink_write(&schema, &json!("a.b")).is_err());
    }

    #[test]
    fn value_must_be_a_string() {
        let schema = map(json!({ SYMLINK_KEYWORD: true }));
        for value in [json!(3), json!(null), json!({"key": "a.b"}), json!(["a.b"])] {
            assert!(normalize_symlink_write(&schema, &value).is_err());
        }
    }

    #[test]
    fn over_long_target_is_rejected() {
        // Review Focus 5: the pattern caps a key at 256 characters, so a longer
        // pointer must fail at write time rather than store unresolvable.
        let schema = map(json!({ SYMLINK_KEYWORD: true }));
        let long_key = "a".repeat(257);

        let err = normalize_symlink_write(&schema, &json!(long_key))
            .expect_err("an over-long target must be rejected");

        assert!(!err.is_empty());
    }

    #[test]
    fn symlink_target_reads_the_value() {
        let schema = canonical_symlink_schema();
        let value = json!("payments.retry.count");
        assert_eq!(
            symlink_target(&schema, &value),
            Some("payments.retry.count")
        );
    }
}
