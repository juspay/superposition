# Default-Config Symlinks Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let a default-config key be a symlink to another default-config key, so both names always resolve to the same value, with no client changes and no database migration.

**Architecture:** A symlink is an ordinary `default_configs` row whose `value` is the target key name and whose `schema` carries `"x-superposition-symlink": true`. Writes that name a link are redirected to its target before hashing and before authorization. Reads expand the link into a real entry — in `default_configs` **and** in every override map that mentions the target — at the three points where a config becomes persisted or served, and never in `reduce_handler`, which writes contexts back.

**Tech Stack:** Rust (actix-web, Diesel, Postgres 16), `jsonschema ~0.17` (Draft-7), Leptos frontend (SSR + wasm hydrate), Smithy-generated SDKs, Bun/TypeScript integration tests.

**Spec:** `docs/superpowers/specs/2026-10-06-default-config-symlinks-design.md`

## Global Constraints

- **No DDL.** No migration file, no new table, no new column. The representation uses `default_configs.value` and `default_configs.schema`, which already exist, because this repo has no automated path for rolling DDL to existing per-workspace schemas.
- **Keyword:** `x-superposition-symlink`, and its value must be the boolean `true` — never a string, never the target name.
- **Canonical stored schema**, exactly: `{"type": "string", "pattern": "^[a-zA-Z0-9-_]([a-zA-Z0-9-_.]{0,254}[a-zA-Z0-9-_])?$", "x-superposition-symlink": true}`. The pattern is produced by `RegexEnum::DefaultConfigKey.to_string()` — never hand-copied.
- **Normalize before hashing.** Override ids are content hashes (`context/operations.rs:124-135`), so redirecting after hashing yields two ids for one semantic override.
- **Normalize before authorizing.** Authorization is keyed by config key name (`context/handlers.rs:84-91`, `default_config/handlers.rs:81`). The other order makes a symlink an authority-widening device.
- **`reduce_handler` must never receive an expanded config.** It recomputes override ids by hashing override contents (`config/handlers.rs:412`) and persists the result.
- **`superposition_types::Config` keeps its shape.** No field is added or removed, so no client, SDK or uniffi binding changes.
- **A link's response is resolved:** `value`, `schema`, and both function names come from the target; `key`, `description`, `change_reason` and audit fields are the link's own.
- Run `make check` (fmt + leptosfmt + clippy with `-Dwarnings`) before every commit.
- Rust unit tests: `cargo test -p <crate> <filter>`. Integration tests: `cd tests && bun test src/<file>`.

## Review Focus

Input classes the spec implies but which no task's happy path exercises. Each has its test added to the task that owns the code.

1. **An override map naming both a link and its target.** `{"old.key": 5, "new.key": 7}` normalizes to one key — a silent value loss. Must be rejected with a 400 naming both. (Task 5)
2. **An ordinary row whose value happens to look like a key name.** A legitimate `{"type": "string"}` config holding `"payments.retry.count"` must never be treated as a link; the boolean marker is the only discriminator. (Task 1)
3. **A dangling target** — a row hand-edited or deleted by direct SQL. Expansion must omit the key and log ERROR, and the rest of the workspace's config must still assemble and serve. (Task 2)
4. **MERGE strategy with object values.** Both names must merge identically, not just under REPLACE. (Task 2)
5. **A target name at the length boundary.** The 256-character pattern must reject an over-long pointer at write time with a readable message, rather than storing a row that can never resolve. (Task 1)

## File Structure

**Created:**
- `crates/superposition_types/src/symlink.rs` — the representation: keyword, predicates, canonical schema, write validation, `SymlinkRow`. Pure; no DB, no actix.
- `crates/context_aware_config/src/symlinks.rs` — the server-side layer: `RawConfig`, the link queries, and the write-path normalization helpers.
- `tests/src/default_config_symlink.test.ts` — end-to-end behaviour over the generated JS SDK.
- `docs/docs/features/default-config-symlinks.md` — user-facing documentation.

**Modified:**
- `crates/superposition_types/src/lib.rs` — register `pub mod symlink;`
- `crates/superposition_types/src/config.rs` — `Config::expand_symlinks`, `DetailedConfig::expand_symlinks`
- `crates/cac_client/src/eval.rs` — the eval invariant tests
- `crates/context_aware_config/src/lib.rs` — register `pub mod symlinks;`
- `crates/context_aware_config/src/helpers.rs` — `generate_cac`, `generate_detailed_cac`, `add_config_version`, `put_config_in_redis`
- `crates/context_aware_config/src/api/config/helpers.rs` — `generate_config_from_version`
- `crates/context_aware_config/src/api/config/handlers.rs` — `reduce_handler` call sites
- `crates/context_aware_config/src/api/default_config/handlers.rs` — create, get, update, delete, list
- `crates/context_aware_config/src/api/context/handlers.rs` — create, update, move, bulk, validate
- `crates/experimentation_platform/src/api/experiments/handlers.rs` — create, update, conclude
- `smithy/models/default-config.smithy` — `symlink_to` response property
- `crates/frontend/src/pages/default_config_list.rs`, `crates/frontend/src/components/default_config_form.rs`, `crates/frontend/src/components/override_form.rs`

---

### Task 1: Symlink representation

The one place that knows what a symlink looks like on disk. Everything else asks this module.

**Files:**
- Create: `crates/superposition_types/src/symlink.rs`
- Modify: `crates/superposition_types/src/lib.rs` (add `pub mod symlink;` beside `pub mod logic;` on line 13)
- Test: same file, `#[cfg(test)] mod tests`

**Interfaces:**
- Consumes: `crate::RegexEnum` (already public in `lib.rs:183`)
- Produces:
  - `pub const SYMLINK_KEYWORD: &str`
  - `pub struct SymlinkRow { pub key: String, pub target: String, pub description: String }`
  - `pub fn is_symlink_schema(schema: &Map<String, Value>) -> bool`
  - `pub fn symlink_target<'a>(schema: &Map<String, Value>, value: &'a Value) -> Option<&'a str>`
  - `pub fn canonical_symlink_schema() -> Map<String, Value>`
  - `pub fn normalize_symlink_write(schema: &Map<String, Value>, value: &Value) -> Result<(String, Map<String, Value>), String>`

- [ ] **Step 1: Write the failing tests**

Create `crates/superposition_types/src/symlink.rs` with only this test module at first:

```rust
#[cfg(test)]
mod tests {
    use serde_json::{json, Map, Value};

    use super::*;

    fn map(value: Value) -> Map<String, Value> {
        value.as_object().expect("fixture must be an object").clone()
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
    fn extra_keywords_are_rejected() {
        let schema = map(json!({ SYMLINK_KEYWORD: true, "minLength": 3 }));

        let err = normalize_symlink_write(&schema, &json!("a.b"))
            .expect_err("a half-schema half-link row must be rejected");

        assert!(err.contains("minLength"), "error should name the keyword: {err}");
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
        assert_eq!(symlink_target(&schema, &value), Some("payments.retry.count"));
    }
}
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p superposition_types symlink`
Expected: FAIL — `cannot find function normalize_symlink_write` and friends, plus `file not found for module symlink` until `lib.rs` is updated.

- [ ] **Step 3: Write the implementation**

Add `pub mod symlink;` to `crates/superposition_types/src/lib.rs` next to `pub mod logic;` (line 13), then put this above the test module in `symlink.rs`:

```rust
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
pub fn is_symlink_schema(schema: &Map<String, Value>) -> bool {
    schema.get(SYMLINK_KEYWORD) == Some(&Value::Bool(true))
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

    if let Some(unexpected) = schema
        .keys()
        .find(|key| key.as_str() != SYMLINK_KEYWORD && !ALLOWED_KEYWORDS.contains(&key.as_str()))
    {
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
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p superposition_types symlink`
Expected: PASS, 9 tests.

- [ ] **Step 5: Pin down why this is a vendor keyword and not `$ref`**

The spec's rationale rests on how `jsonschema ~0.17` treats an unresolvable external ref. Record the real behaviour instead of trusting the claim. Add to `crates/superposition_core/src/validations.rs`:

```rust
#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::try_into_jsonschema;

    #[test]
    fn an_unknown_keyword_compiles_and_validates_anything() {
        // Why the symlink marker can live in a schema at all: Draft-7 ignores
        // keywords it does not know, so no resolver is needed.
        let schema = json!({ "x-superposition-symlink": true });
        let compiled = try_into_jsonschema(&schema).expect("unknown keywords are ignored");
        assert!(compiled.validate(&json!("payments.retry.count")).is_ok());
    }

    #[test]
    fn an_external_ref_cannot_resolve_here() {
        // Why the marker is not spelled `$ref`: this compile registers no
        // resolver, so a superposition:// scheme has no handler.
        let schema = json!({ "$ref": "superposition://default-config/payments.retry.count" });
        assert!(try_into_jsonschema(&schema).is_err());
    }
}
```

Run: `cargo test -p superposition_core validations`
Expected: PASS. **If `an_external_ref_cannot_resolve_here` fails**, 0.17 is more tolerant than the spec assumed — keep the vendor keyword (the semantic argument stands on its own), change that test to assert the behaviour you actually observed, and correct the "Why not `$ref`" section of the spec so it stops claiming a mechanical failure that does not happen.

- [ ] **Step 6: Commit**

```bash
make check
git add crates/superposition_types/src/symlink.rs crates/superposition_types/src/lib.rs crates/superposition_core/src/validations.rs
git commit -m "feat(types): add default-config symlink representation"
```

---

### Task 2: Expansion, and the eval invariant

Expansion is the whole feature from a reader's point of view. The second loop — over override maps — is the half that is easy to omit, and omitting it is the silent-freeze bug.

**Files:**
- Modify: `crates/superposition_types/src/config.rs` (add `impl` blocks after `Config`'s existing impl, which starts at line 304)
- Modify: `crates/cac_client/src/eval.rs` (append a `#[cfg(test)] mod tests`)
- Test: both files

**Interfaces:**
- Consumes: `superposition_types::symlink::SymlinkRow` (Task 1)
- Produces:
  - `impl Config { pub fn expand_symlinks(&mut self, links: &[SymlinkRow]) }`
  - `impl DetailedConfig { pub fn expand_symlinks(&mut self, links: &[SymlinkRow]) }`

- [ ] **Step 1: Write the failing unit tests for expansion**

Append to `crates/superposition_types/src/config/tests.rs`:

```rust
#[cfg(test)]
mod symlink_expansion {
    use serde_json::{from_value, json};

    use crate::{config::DetailedConfig, symlink::SymlinkRow, Config};

    fn link(key: &str, target: &str) -> SymlinkRow {
        SymlinkRow {
            key: key.to_string(),
            target: target.to_string(),
            description: format!("alias of {target}"),
        }
    }

    fn config_with_one_override() -> Config {
        from_value(json!({
            "contexts": [{
                "id": "ctx1",
                "condition": { "city": "bangalore" },
                "priority": 0,
                "weight": 0,
                "override_with_keys": ["ovr1"]
            }],
            "overrides": { "ovr1": { "payments.retry.count": 5 } },
            "default_configs": { "payments.retry.count": 3 },
            "dimensions": {}
        }))
        .expect("fixture should deserialize")
    }

    #[test]
    fn expansion_copies_the_default_value() {
        let mut config = config_with_one_override();
        config.expand_symlinks(&[link("payments.retry_count", "payments.retry.count")]);

        assert_eq!(
            config.default_configs.get("payments.retry_count"),
            Some(&json!(3))
        );
    }

    #[test]
    fn expansion_copies_into_every_override_map() {
        let mut config = config_with_one_override();
        config.expand_symlinks(&[link("payments.retry_count", "payments.retry.count")]);

        let overrides = config.overrides.get("ovr1").expect("override should exist");
        assert_eq!(overrides.get("payments.retry_count"), Some(&json!(5)));
        assert_eq!(overrides.get("payments.retry.count"), Some(&json!(5)));
    }

    #[test]
    fn two_links_to_one_target_both_resolve() {
        let mut config = config_with_one_override();
        config.expand_symlinks(&[
            link("payments.retry_count", "payments.retry.count"),
            link("checkout.retries", "payments.retry.count"),
        ]);

        assert_eq!(config.default_configs.get("payments.retry_count"), Some(&json!(3)));
        assert_eq!(config.default_configs.get("checkout.retries"), Some(&json!(3)));
    }

    #[test]
    fn a_dangling_target_omits_the_key_and_leaves_the_rest_intact() {
        // Review Focus 3: a hand-edited or directly deleted target must not take
        // the workspace's config assembly down with it.
        let mut config = config_with_one_override();
        config.expand_symlinks(&[link("payments.retry_count", "does.not.exist")]);

        assert!(!config.default_configs.contains_key("payments.retry_count"));
        assert_eq!(
            config.default_configs.get("payments.retry.count"),
            Some(&json!(3)),
            "the rest of the config must survive"
        );
    }

    #[test]
    fn an_override_not_mentioning_the_target_is_untouched() {
        let mut config: Config = from_value(json!({
            "contexts": [],
            "overrides": { "ovr1": { "something.else": true } },
            "default_configs": { "payments.retry.count": 3, "something.else": false },
            "dimensions": {}
        }))
        .expect("fixture should deserialize");

        config.expand_symlinks(&[link("payments.retry_count", "payments.retry.count")]);

        let overrides = config.overrides.get("ovr1").expect("override should exist");
        assert_eq!(overrides.len(), 1, "unrelated override maps must not grow");
    }

    #[test]
    fn expansion_leaves_override_ids_untouched() {
        // A documented invariant: ids are content hashes, and reduce recomputes them.
        // Expansion mutates override *contents* and must not re-key the map.
        let mut config = config_with_one_override();
        let before: Vec<String> = config.overrides.keys().cloned().collect();

        config.expand_symlinks(&[link("payments.retry_count", "payments.retry.count")]);

        let after: Vec<String> = config.overrides.keys().cloned().collect();
        assert_eq!(before, after);
    }

    #[test]
    fn detailed_expansion_takes_the_targets_schema_and_the_links_description() {
        let mut detailed: DetailedConfig = from_value(json!({
            "contexts": [],
            "overrides": {},
            "default_configs": {
                "payments.retry.count": {
                    "value": 3,
                    "schema": { "type": "integer" },
                    "description": "retries before failure"
                }
            },
            "dimensions": {}
        }))
        .expect("fixture should deserialize");

        detailed.expand_symlinks(&[link("payments.retry_count", "payments.retry.count")]);

        let entry = detailed
            .default_configs
            .get("payments.retry_count")
            .expect("link should be present");
        assert_eq!(entry.value, json!(3));
        assert_eq!(entry.schema, json!({ "type": "integer" }));
        assert_eq!(
            entry.description, "alias of payments.retry.count",
            "a link keeps its own description"
        );
    }
}
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p superposition_types symlink_expansion`
Expected: FAIL — `no method named expand_symlinks`.

- [ ] **Step 3: Implement expansion**

Add to `crates/superposition_types/src/config.rs`. Import `crate::symlink::SymlinkRow` at the top, then:

```rust
impl Config {
    /// Materialises symlinked keys, so a link and its target resolve identically
    /// under the defaults *and* under every context.
    ///
    /// `links` is already flattened to depth 1 by the write path, so this makes a
    /// single pass. A link whose target is missing is skipped with an ERROR rather
    /// than failing assembly for the whole workspace.
    pub fn expand_symlinks(&mut self, links: &[SymlinkRow]) {
        for SymlinkRow { key, target, .. } in links {
            let Some(value) = self.default_configs.get(target).cloned() else {
                log::error!(
                    "symlink {key} -> {target}: target missing from default configs, key omitted"
                );
                continue;
            };
            self.default_configs.insert(key.clone(), value);

            // The half that is easy to forget: without this, the link freezes at
            // its default while the target moves under a matching context.
            for overrides in self.overrides.values_mut() {
                if let Some(overridden) = overrides.get(target).cloned() {
                    overrides.insert(key.clone(), overridden);
                }
            }
        }
    }
}

impl DetailedConfig {
    /// As [`Config::expand_symlinks`], but a link's entry takes the target's value
    /// and schema while keeping the link's own description — which is what makes the
    /// TOML and JSON dumps, `resolve_detailed` and `explain` show a real type.
    pub fn expand_symlinks(&mut self, links: &[SymlinkRow]) {
        for SymlinkRow { key, target, description } in links {
            let Some(mut info) = self.default_configs.get(target).cloned() else {
                log::error!(
                    "symlink {key} -> {target}: target missing from default configs, key omitted"
                );
                continue;
            };
            info.description = description.clone();
            self.default_configs.insert(key.clone(), info);

            for overrides in self.overrides.values_mut() {
                if let Some(overridden) = overrides.get(target).cloned() {
                    overrides.insert(key.clone(), overridden);
                }
            }
        }
    }
}
```

If `DefaultConfigInfo` (line 502) does not already derive `Clone`, add it to its derive list.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p superposition_types symlink_expansion`
Expected: PASS, 7 tests.

- [ ] **Step 5: Write the failing eval invariant tests**

This is the feature stated as a test. Append to `crates/cac_client/src/eval.rs`:

```rust
#[cfg(test)]
mod symlink_invariant {
    use serde_json::{from_value, json, Map, Value};
    use superposition_types::{symlink::SymlinkRow, Config};

    use crate::{eval_cac, MergeStrategy};

    const LINK: &str = "payments.retry_count";
    const TARGET: &str = "payments.retry.count";

    fn link() -> Vec<SymlinkRow> {
        vec![SymlinkRow {
            key: LINK.to_string(),
            target: TARGET.to_string(),
            description: "renamed".to_string(),
        }]
    }

    fn query(city: &str) -> Map<String, Value> {
        let mut data = Map::new();
        data.insert("city".to_string(), json!(city));
        data
    }

    fn scalar_config() -> Config {
        let mut config: Config = from_value(json!({
            "contexts": [{
                "id": "ctx1",
                "condition": { "city": "bangalore" },
                "priority": 0,
                "weight": 0,
                "override_with_keys": ["ovr1"]
            }],
            "overrides": { "ovr1": { TARGET: 5 } },
            "default_configs": { TARGET: 3 },
            "dimensions": {}
        }))
        .expect("fixture should deserialize");
        config.expand_symlinks(&link());
        config
    }

    #[test]
    fn link_equals_target_on_the_default() {
        let resolved = eval_cac(scalar_config(), query("chennai"), MergeStrategy::REPLACE);

        assert_eq!(resolved.get(LINK), Some(&json!(3)));
        assert_eq!(resolved.get(LINK), resolved.get(TARGET));
    }

    #[test]
    fn link_equals_target_under_a_matching_context() {
        let resolved = eval_cac(scalar_config(), query("bangalore"), MergeStrategy::REPLACE);

        assert_eq!(
            resolved.get(LINK),
            Some(&json!(5)),
            "the link must move with the target, not freeze at the default"
        );
        assert_eq!(resolved.get(LINK), resolved.get(TARGET));
    }

    #[test]
    fn link_equals_target_under_merge_strategy() {
        // Review Focus 4: object values under MERGE must agree too, not just scalars
        // under REPLACE.
        let mut config: Config = from_value(json!({
            "contexts": [{
                "id": "ctx1",
                "condition": { "city": "bangalore" },
                "priority": 0,
                "weight": 0,
                "override_with_keys": ["ovr1"]
            }],
            "overrides": { "ovr1": { TARGET: { "attempts": 5 } } },
            "default_configs": { TARGET: { "attempts": 3, "backoff": "linear" } },
            "dimensions": {}
        }))
        .expect("fixture should deserialize");
        config.expand_symlinks(&link());

        let resolved = eval_cac(config, query("bangalore"), MergeStrategy::MERGE);

        assert_eq!(
            resolved.get(LINK),
            Some(&json!({ "attempts": 5, "backoff": "linear" }))
        );
        assert_eq!(resolved.get(LINK), resolved.get(TARGET));
    }

    #[test]
    fn link_survives_a_prefix_filter_that_excludes_the_target() {
        use superposition_types::PrefixList;

        let config = scalar_config();
        let allow = PrefixList::from_iter(vec!["payments.retry_".to_string()]);
        let exclude = PrefixList::default();
        let filtered = config.filter_default_by_prefix(&allow, &exclude);

        assert_eq!(
            filtered.get(LINK),
            Some(&json!(3)),
            "an old client pinned to the old prefix keeps working through a rename"
        );
        assert!(!filtered.contains_key(TARGET));
    }
}
```

- [ ] **Step 6: Run the invariant tests**

Run: `cargo test -p cac_client symlink_invariant`
Expected: PASS, 4 tests — expansion already exists, so these confirm the invariant rather than drive new code. If `link_equals_target_under_a_matching_context` fails while the default-only test passes, the override loop in Step 3 is missing or wrong.

- [ ] **Step 7: Commit**

```bash
make check
git add crates/superposition_types/src/config.rs crates/superposition_types/src/config/tests.rs crates/cac_client/src/eval.rs
git commit -m "feat(types): expand default-config symlinks into served config"
```

---

### Task 3: Server read path — `RawConfig` and the link queries

Four call sites share one function today. Three must expand; the fourth must never. This task makes that distinction nameable.

**Files:**
- Create: `crates/context_aware_config/src/symlinks.rs`
- Modify: `crates/context_aware_config/src/lib.rs` (register `pub mod symlinks;`)
- Modify: `crates/context_aware_config/src/helpers.rs` (`generate_cac:130`, `generate_detailed_cac:161`, `add_config_version:219`, `put_config_in_redis:256`)
- Modify: `crates/context_aware_config/src/api/config/helpers.rs` (`generate_config_from_version:109`)
- Modify: `crates/context_aware_config/src/api/config/handlers.rs` (`reduce_handler:465,485`)
- Test: `crates/context_aware_config/src/symlinks.rs`

**Interfaces:**
- Consumes: `superposition_types::symlink::{SymlinkRow, SYMLINK_KEYWORD, is_symlink_schema, symlink_target}` (Task 1), `Config::expand_symlinks` / `DetailedConfig::expand_symlinks` (Task 2)
- Produces:
  - `pub struct RawConfig(Config)` with `pub fn new(Config) -> Self`, `pub fn expand(self, &[SymlinkRow]) -> Config`, `pub fn into_unexpanded(self) -> Config`
  - `pub fn fetch_symlinks(conn: &mut DBConnection, schema_name: &SchemaName) -> superposition::Result<Vec<SymlinkRow>>`
  - `pub fn symlink_dependents(conn: &mut DBConnection, schema_name: &SchemaName, key: &str) -> superposition::Result<Vec<String>>`
  - `pub fn symlink_map_for_keys<'a>(conn: &mut DBConnection, schema_name: &SchemaName, keys: impl Iterator<Item = &'a String>) -> superposition::Result<HashMap<String, String>>`
  - `pub fn flatten_target(conn: &mut DBConnection, schema_name: &SchemaName, requested: &str) -> superposition::Result<String>`
  - `pub const NOT_A_SYMLINK_SQL: &str` and `pub const IS_A_SYMLINK_SQL: &str`
- `generate_cac` changes its return type from `Config` to `RawConfig`. `generate_detailed_cac` keeps returning `DetailedConfig` but gains the exclusion filter and expands internally, since its only callers serve.

- [ ] **Step 1: Write the failing test for `RawConfig`**

Create `crates/context_aware_config/src/symlinks.rs` with this test module first:

```rust
#[cfg(test)]
mod tests {
    use serde_json::{from_value, json};
    use superposition_types::{symlink::SymlinkRow, Config};

    use super::RawConfig;

    #[test]
    fn expand_adds_the_link_and_into_unexpanded_does_not() {
        let fixture: Config = from_value(json!({
            "contexts": [],
            "overrides": {},
            "default_configs": { "a.b": 1 },
            "dimensions": {}
        }))
        .expect("fixture should deserialize");

        let links = vec![SymlinkRow {
            key: "a_b".to_string(),
            target: "a.b".to_string(),
            description: "alias".to_string(),
        }];

        let expanded = RawConfig::new(fixture.clone()).expand(&links);
        assert!(expanded.default_configs.contains_key("a_b"));

        let raw = RawConfig::new(fixture).into_unexpanded();
        assert!(
            !raw.default_configs.contains_key("a_b"),
            "the unexpanded config is what reduce must see"
        );
    }

    #[test]
    fn sql_predicates_are_total_text_comparisons() {
        // A cast such as (schema->>'...')::boolean raises on a hand-edited
        // non-boolean marker and would fail the whole query, so both predicates
        // must compare text.
        assert!(!super::IS_A_SYMLINK_SQL.contains("::boolean"));
        assert!(!super::NOT_A_SYMLINK_SQL.contains("::boolean"));
    }
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p context_aware_config symlinks`
Expected: FAIL — `file not found for module symlinks` / `cannot find struct RawConfig`.

- [ ] **Step 3: Implement the module**

Add `pub mod symlinks;` to `crates/context_aware_config/src/lib.rs`, then write `symlinks.rs` above its test module:

```rust
//! Server-side symlink handling: the queries that find links, and the type that
//! keeps an unexpanded config away from the paths that serve or persist one.

use std::collections::HashMap;

use diesel::{dsl::sql, sql_types::Bool, ExpressionMethods, QueryDsl, RunQueryDsl};
use serde_json::Value;
use service_utils::service::types::SchemaName;
use superposition_macros::bad_argument;
use superposition_types::{
    custom_query::CustomQuery,
    database::{models::cac::DefaultConfig, schema::default_configs::dsl},
    result as superposition,
    symlink::{is_symlink_schema, symlink_target, SymlinkRow, SYMLINK_KEYWORD},
    Config, DBConnection,
};

/// Rows that are symlinks. Compares text rather than casting, so a hand-edited
/// non-boolean marker cannot fail the query.
pub const IS_A_SYMLINK_SQL: &str = "schema->>'x-superposition-symlink' = 'true'";

/// Rows that are ordinary default configs.
pub const NOT_A_SYMLINK_SQL: &str =
    "coalesce(schema->>'x-superposition-symlink', '') <> 'true'";

/// A config as the database holds it, with no symlinked keys.
pub struct RawConfig(Config);

impl RawConfig {
    pub fn new(config: Config) -> Self {
        Self(config)
    }

    /// Adds the symlinked keys. The result is what may be persisted or served.
    pub fn expand(self, links: &[SymlinkRow]) -> Config {
        let mut config = self.0;
        config.expand_symlinks(links);
        config
    }

    /// The config without symlinks.
    ///
    /// Only for maintenance paths that write contexts back. `reduce_config_key`
    /// recomputes override ids by hashing override contents, so an expanded config
    /// would persist ids that disagree with what the write path produces for the
    /// same semantic override. Never serve or snapshot this.
    pub fn into_unexpanded(self) -> Config {
        self.0
    }
}

/// Every symlink row in the workspace.
pub fn fetch_symlinks(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<Vec<SymlinkRow>> {
    let rows = dsl::default_configs
        .filter(sql::<Bool>(IS_A_SYMLINK_SQL))
        .select((dsl::key, dsl::value, dsl::description))
        .schema_name(schema_name)
        .load::<(String, Value, String)>(conn)?;

    Ok(rows
        .into_iter()
        .filter_map(|(key, value, description)| {
            let target = value.as_str()?.to_string();
            Some(SymlinkRow {
                key,
                target,
                description,
            })
        })
        .collect())
}

/// The symlinks pointing at `key`, so a delete can refuse with a readable message.
pub fn symlink_dependents(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    key: &str,
) -> superposition::Result<Vec<String>> {
    Ok(fetch_symlinks(conn, schema_name)?
        .into_iter()
        .filter(|link| link.target == key)
        .map(|link| link.key)
        .collect())
}

/// For the given keys, which are symlinks and where do they point.
///
/// Returns only the entries that *are* links, so an empty map means there is
/// nothing to rewrite.
pub fn symlink_map_for_keys<'a>(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    keys: impl Iterator<Item = &'a String>,
) -> superposition::Result<HashMap<String, String>> {
    let keys: Vec<&'a String> = keys.collect();
    if keys.is_empty() {
        return Ok(HashMap::new());
    }

    let rows = dsl::default_configs
        .filter(dsl::key.eq_any(keys))
        .filter(sql::<Bool>(IS_A_SYMLINK_SQL))
        .select((dsl::key, dsl::value))
        .schema_name(schema_name)
        .load::<(String, Value)>(conn)?;

    Ok(rows
        .into_iter()
        .filter_map(|(key, value)| Some((key, value.as_str()?.to_string())))
        .collect())
}

/// Resolves a requested target through any existing symlink, so stored links are
/// always depth 1 and cycles are impossible by construction.
///
/// A serial rename therefore cannot leave a chain: `old -> new` followed by
/// `new -> newer` repoints `old` straight at `newer`.
pub fn flatten_target(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    requested: &str,
) -> superposition::Result<String> {
    let row = dsl::default_configs
        .filter(dsl::key.eq(requested))
        .select(DefaultConfig::as_select())
        .schema_name(schema_name)
        .first::<DefaultConfig>(conn)
        .optional()?
        .ok_or_else(|| {
            bad_argument!("symlink target `{requested}` does not exist")
        })?;

    if is_symlink_schema(row.schema.inner()) {
        let target = symlink_target(row.schema.inner(), &row.value).ok_or_else(|| {
            bad_argument!("symlink `{requested}` is malformed; its value is not a key name")
        })?;
        return Ok(target.to_string());
    }

    Ok(requested.to_string())
}
```

Add `use diesel::{OptionalExtension, SelectableHelper};` if the compiler asks for `.optional()` or `as_select()`. `SYMLINK_KEYWORD` is imported for use in error messages; if clippy reports it unused, drop it from the import list.

- [ ] **Step 4: Run the test to verify it passes**

Run: `cargo test -p context_aware_config symlinks`
Expected: PASS, 2 tests.

- [ ] **Step 5: Exclude links from `generate_cac` and return `RawConfig`**

In `crates/context_aware_config/src/helpers.rs`, change `generate_cac` (line 130) to return `superposition::Result<RawConfig>`, add the filter to its query, and wrap the result:

```rust
    let default_config_vec = def_conf::default_configs
        .filter(diesel::dsl::sql::<diesel::sql_types::Bool>(
            crate::symlinks::NOT_A_SYMLINK_SQL,
        ))
        .select((def_conf::key, def_conf::value))
        .schema_name(schema_name)
        .load::<(String, Value)>(conn)
```

and at the end:

```rust
    Ok(RawConfig::new(Config {
        contexts,
        overrides,
        default_configs: default_configs.into(),
        dimensions,
    }))
```

Apply the same filter to `generate_detailed_cac`'s query (line ~168), and expand before returning, since both of its callers serve:

```rust
    let links = crate::symlinks::fetch_symlinks(conn, schema_name)?;
    let mut detailed = DetailedConfig { contexts, overrides, default_configs, dimensions };
    detailed.expand_symlinks(&links);
    Ok(detailed)
```

- [ ] **Step 6: Expand at the three serving and persisting sites**

In `helpers.rs`, `add_config_version` (line 219):

```rust
    let links = crate::symlinks::fetch_symlinks(db_conn, schema_name)?;
    let config = generate_cac(db_conn, schema_name)?.expand(&links);
```

In `helpers.rs`, `put_config_in_redis` (line 256):

```rust
    let links = crate::symlinks::fetch_symlinks(db_conn, schema_name)?;
    let raw_config = generate_cac(db_conn, schema_name)?.expand(&links);
```

In `crates/context_aware_config/src/api/config/helpers.rs`, both `generate_cac` fallbacks inside `generate_config_from_version` (lines 139 and 144) become:

```rust
                    generate_cac(conn, schema_name).and_then(|raw| {
                        let links = crate::symlinks::fetch_symlinks(conn, schema_name)?;
                        Ok(raw.expand(&links))
                    })
```

- [ ] **Step 7: Keep `reduce_handler` unexpanded**

In `crates/context_aware_config/src/api/config/handlers.rs`, lines 465 and 485 become:

```rust
    let mut config = generate_cac(conn, &workspace_context.schema_name)?.into_unexpanded();
```

- [ ] **Step 8: Resolve the per-key metadata used by resolve and explain**

`fetch_default_config_metadata` (`api/config/helpers.rs:301`) selects `(key, schema, description)` straight from the table, so without this step `resolve_detailed` and `explain` would report a link's pointer schema instead of a real type. Add the exclusion filter to its query and re-add the links with their target's schema:

```rust
    let links = crate::symlinks::fetch_symlinks(conn, schema_name)?;
    for SymlinkRow { key, target, description } in &links {
        if let Some(target_schema) = metadata.get(target).map(|m| m.schema.clone()) {
            metadata.insert(
                key.clone(),
                DefaultConfigMetadata {
                    schema: target_schema,
                    description: description.clone(),
                },
            );
        }
    }
```

Match the local struct and map names in that function — read it first; it builds its rows inline rather than through a named type.

- [ ] **Step 9: Verify the guard holds**

Run: `grep -rn "into_unexpanded" crates/`
Expected: exactly two hits, both in `api/config/handlers.rs` inside `reduce_handler`. Any other caller is a bug — a serving path must use `.expand(..)`.

Run: `cargo build -p context_aware_config`
Expected: compiles. Any other `generate_cac` caller that fails to compile is a site that was silently serving a raw config; fix it with `.expand(&fetch_symlinks(..)?)`.

- [ ] **Step 10: Commit**

```bash
make check
git add crates/context_aware_config/src/symlinks.rs crates/context_aware_config/src/lib.rs crates/context_aware_config/src/helpers.rs crates/context_aware_config/src/api/config/
git commit -m "feat(cac): expand symlinks when persisting and serving config"
```

---

### Task 4: Default-config write path and resolved responses

**Files:**
- Modify: `crates/superposition_types/src/api/default_config.rs` (add the response type and `symlink_to` to the update request)
- Modify: `crates/context_aware_config/src/api/default_config/handlers.rs` (create:72, get:213, update:228, list:440, delete:512)
- Test: `tests/src/default_config_symlink.test.ts` is Task 8; this task's own tests are the pure-logic ones below

**Interfaces:**
- Consumes: Task 1's representation helpers, Task 3's `flatten_target` / `symlink_dependents` / `fetch_symlinks`
- Produces: `superposition_types::api::default_config::DefaultConfigResponse { config: DefaultConfig, symlink_to: Option<String> }`, and, in `symlinks.rs`, `merge_target_into_link(link: DefaultConfig, target: &DefaultConfig) -> DefaultConfig` plus `resolve_for_response(conn, schema_name, DefaultConfig) -> superposition::Result<DefaultConfigResponse>`

- [ ] **Step 1: Add the response type**

In `crates/superposition_types/src/api/default_config.rs`:

```rust
/// A default config as the API returns it.
///
/// For a symlink, `value`, `schema` and both function names are the **target's**,
/// so a consumer renders a real type without knowing symlinks exist; `symlink_to`
/// is the only field a link has and an ordinary key does not.
#[derive(Debug, Serialize, Deserialize)]
pub struct DefaultConfigResponse {
    #[serde(flatten)]
    pub config: crate::database::models::cac::DefaultConfig,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub symlink_to: Option<String>,
}
```

- [ ] **Step 2: Write the failing resolution test**

Append to `crates/context_aware_config/src/symlinks.rs`'s test module:

```rust
    /// A `DefaultConfig` fixture. Every field is spelled out because the model's
    /// newtypes (`Description`, `ChangeReason`) validate on construction.
    fn row(key: &str, value: Value, schema: Value) -> DefaultConfig {
        use chrono::Utc;
        use superposition_types::database::models::{ChangeReason, Description};

        DefaultConfig {
            key: key.to_string(),
            value,
            schema: schema.as_object().expect("schema fixture").clone().into(),
            created_at: Utc::now(),
            created_by: "test@example.com".to_string(),
            last_modified_at: Utc::now(),
            last_modified_by: "test@example.com".to_string(),
            description: Description::try_from(format!("description of {key}"))
                .expect("description fixture"),
            change_reason: ChangeReason::try_from("test".to_string())
                .expect("change reason fixture"),
            value_validation_function_name: None,
            value_compute_function_name: None,
        }
    }

    #[test]
    fn a_resolved_link_takes_the_targets_value_and_keeps_its_own_identity() {
        use superposition_types::symlink::canonical_symlink_schema;

        let link = row(
            "payments.retry_count",
            json!("payments.retry.count"),
            Value::Object(canonical_symlink_schema()),
        );
        let mut target = row("payments.retry.count", json!(3), json!({"type": "integer"}));
        target.value_validation_function_name = Some("validate_retries".to_string());

        let resolved = super::merge_target_into_link(link, &target);

        // From the target
        assert_eq!(resolved.value, json!(3));
        assert_eq!(
            resolved.schema.inner().get("type"),
            Some(&json!("integer")),
            "the stored pointer schema must never reach a response"
        );
        assert!(!resolved.schema.inner().contains_key(SYMLINK_KEYWORD));
        assert_eq!(
            resolved.value_validation_function_name.as_deref(),
            Some("validate_retries")
        );

        // The link's own
        assert_eq!(resolved.key, "payments.retry_count");
        assert_eq!(
            resolved.description.to_string(),
            "description of payments.retry_count"
        );
    }
```

- [ ] **Step 3: Run it to verify it fails**

Run: `cargo test -p context_aware_config symlinks`
Expected: FAIL — `cannot find function merge_target_into_link`.

- [ ] **Step 4: Implement resolution**

Add to `crates/context_aware_config/src/symlinks.rs`:

```rust
/// Builds the row a response should carry for a symlink: everything that says what
/// the value *is* comes from the target; everything that says what this *name* is
/// stays the link's own.
pub fn merge_target_into_link(
    mut link: DefaultConfig,
    target: &DefaultConfig,
) -> DefaultConfig {
    link.value = target.value.clone();
    link.schema = target.schema.clone();
    link.value_validation_function_name = target.value_validation_function_name.clone();
    link.value_compute_function_name = target.value_compute_function_name.clone();
    link
}

/// Resolves a row for a response. Ordinary rows pass through untouched.
pub fn resolve_for_response(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    row: DefaultConfig,
) -> superposition::Result<DefaultConfigResponse> {
    let Some(target_key) = symlink_target(row.schema.inner(), &row.value).map(str::to_string)
    else {
        return Ok(DefaultConfigResponse {
            config: row,
            symlink_to: None,
        });
    };

    let target = dsl::default_configs
        .filter(dsl::key.eq(&target_key))
        .select(DefaultConfig::as_select())
        .schema_name(schema_name)
        .first::<DefaultConfig>(conn)
        .optional()?;

    let Some(target) = target else {
        log::error!(
            "symlink {} -> {target_key}: target missing, returning the pointer unresolved",
            row.key
        );
        return Ok(DefaultConfigResponse {
            config: row,
            symlink_to: Some(target_key),
        });
    };

    let config = merge_target_into_link(row, &target);

    Ok(DefaultConfigResponse {
        config,
        symlink_to: Some(target_key),
    })
}
```

- [ ] **Step 5: Run it to verify it passes**

Run: `cargo test -p context_aware_config symlinks`
Expected: PASS, 3 tests.

- [ ] **Step 6: Wire the create handler**

In `default_config/handlers.rs::create_handler`, move `let conn = write_permit.connection();` **above** the authorization call, then replace the authorization line (line 81) with:

```rust
    let conn = write_permit.connection();

    let symlink = if is_symlink_schema(req.schema.inner()) {
        let (requested, canonical) = normalize_symlink_write(req.schema.inner(), &req.value)
            .map_err(|e| bad_argument!("{e}"))?;

        if req.value_validation_function_name.is_some()
            || req.value_compute_function_name.is_some()
        {
            return Err(bad_argument!(
                "a symlink cannot carry validation or compute functions; \
                 they belong to its target `{requested}`"
            ));
        }

        let target = flatten_target(conn, &workspace_context.schema_name, &requested)?;
        if target == *req.key {
            return Err(bad_argument!("a symlink cannot point at itself"));
        }
        Some((target, canonical))
    } else {
        None
    };

    // Creating a link needs authority over the target too: otherwise a principal
    // could surface a restricted key's value under a name a prefix-scoped reader
    // is allowed to see.
    match &symlink {
        Some((target, _)) => {
            _auth_z.authorized(&[req.key.deref(), target.as_str()]).await?
        }
        None => _auth_z.authorized(&[req.key.deref()]).await?,
    };
```

Then, where `default_config` is built (line ~101), use the canonical schema and the flattened target when this is a link:

```rust
    let (value, schema) = match &symlink {
        Some((target, canonical)) => (
            Value::String(target.clone()),
            ExtendedMap::from(canonical.clone()),
        ),
        None => (req.value.clone(), req.schema.clone()),
    };
```

and return a resolved response instead of the raw row, replacing `Ok(http_resp.json(default_config))`:

```rust
    let response = resolve_for_response(conn, &workspace_context.schema_name, default_config)?;
    Ok(http_resp.json(response))
```

- [ ] **Step 7: Wire get, list, update and delete**

`get_handler` (line 213) — return the resolved response:

```rust
async fn get_handler(
    workspace_context: WorkspaceContext,
    key: Path<DefaultConfigKey>,
    db_conn: DbConnection,
) -> superposition::Result<Json<DefaultConfigResponse>> {
    let DbConnection(mut conn) = db_conn;
    let res = fetch_default_key(&key, &mut conn, &workspace_context.schema_name)?;
    let resolved = resolve_for_response(&mut conn, &workspace_context.schema_name, res)?;
    Ok(Json(resolved))
}
```

`list_handler` (line 440) — map each row through `resolve_for_response` before building the `PaginatedResponse`, changing its item type to `DefaultConfigResponse`.

`update_handler` (line 228) — insert the discriminator immediately after `existing` is fetched (line ~246):

```rust
    let repoint = req
        .schema
        .as_ref()
        .map(|schema| is_symlink_schema(schema.inner()))
        .unwrap_or(false);

    let existing_target =
        symlink_target(existing.schema.inner(), &existing.value).map(str::to_string);

    let addressed_key = match (repoint, &existing_target) {
        // A write carrying the marker addresses the link itself.
        (true, _) => key_str.clone(),
        // A write without it addresses the value, and so redirects to the target.
        (false, Some(target)) => target.clone(),
        (false, None) => key_str.clone(),
    };

    if repoint {
        let value = req
            .value
            .clone()
            .ok_or_else(|| bad_argument!("repointing a symlink requires its new target as value"))?;
        let (requested, _) = normalize_symlink_write(
            req.schema.as_ref().expect("checked above").inner(),
            &value,
        )
        .map_err(|e| bad_argument!("{e}"))?;
        let target = flatten_target(conn, &workspace_context.schema_name, &requested)?;
        if target == key_str {
            return Err(bad_argument!("a symlink cannot point at itself"));
        }
    }

    // A description-only update stays on the link; everything else follows
    // `addressed_key`.
    _auth_z.authorized(&[addressed_key.as_str()]).await?;
```

Replace the existing authorization on line 238 with the block above, and use `addressed_key` in place of `key_str` for the `diesel::update(...)` filter and for `validate_default_config_with_function`. Return `resolve_for_response(..)` instead of the raw row.

`delete_handler` (line 512) — add the dependency check beside the existing context-usage check:

```rust
    let dependents = symlink_dependents(conn, &workspace_context.schema_name, &key)?;
    if !dependents.is_empty() {
        return Err(bad_argument!(
            "cannot delete `{key}`: it is the target of symlink(s) {}. \
             Delete or repoint them first.",
            dependents.join(", ")
        ));
    }
```

- [ ] **Step 8: Build and commit**

Run: `cargo build -p context_aware_config && make check`
Expected: compiles clean. Behaviour is verified end-to-end in Task 8; there is no Rust DB test harness in this repo, which is why that task exists.

```bash
git add crates/superposition_types/src/api/default_config.rs crates/context_aware_config/src/api/default_config/handlers.rs crates/context_aware_config/src/symlinks.rs
git commit -m "feat(cac): create, resolve, repoint and guard default-config symlinks"
```

---

### Task 5: Context write path — normalize, then authorize

Override ids are content hashes, so this rewrite must land before hashing; authorization is keyed by config key name, so it must land before that too. One helper, applied at the top of each handler.

**Files:**
- Modify: `crates/context_aware_config/src/symlinks.rs` (add the two functions below)
- Modify: `crates/context_aware_config/src/api/context/handlers.rs` (create:97, update:235, bulk:841, validate:1276)
- Test: `crates/context_aware_config/src/symlinks.rs`

**Interfaces:**
- Consumes: `symlink_map_for_keys` (Task 3)
- Produces:
  - `pub fn apply_symlink_map(overrides: Map<String, Value>, links: &HashMap<String, String>) -> Result<Map<String, Value>, String>`
  - `pub fn normalize_override_keys(conn: &mut DBConnection, schema_name: &SchemaName, overrides: Overrides) -> superposition::Result<Overrides>`

- [ ] **Step 1: Write the failing tests**

Append to the test module in `crates/context_aware_config/src/symlinks.rs`:

```rust
    #[test]
    fn apply_rewrites_a_link_to_its_target() {
        use std::collections::HashMap;

        let mut links = HashMap::new();
        links.insert("payments.retry_count".to_string(), "payments.retry.count".to_string());

        let overrides = json!({ "payments.retry_count": 5 })
            .as_object()
            .expect("fixture")
            .clone();

        let normalized = super::apply_symlink_map(overrides, &links).expect("should rewrite");

        assert_eq!(normalized.get("payments.retry.count"), Some(&json!(5)));
        assert!(!normalized.contains_key("payments.retry_count"));
    }

    #[test]
    fn apply_leaves_ordinary_keys_alone() {
        use std::collections::HashMap;

        let overrides = json!({ "a.b": 1, "c.d": 2 }).as_object().expect("fixture").clone();
        let normalized =
            super::apply_symlink_map(overrides.clone(), &HashMap::new()).expect("no links");

        assert_eq!(normalized, overrides);
    }

    #[test]
    fn a_link_and_its_target_with_different_values_is_rejected() {
        // Review Focus 1: normalizing would otherwise collapse two entries into one
        // and silently lose a value the caller asked for.
        use std::collections::HashMap;

        let mut links = HashMap::new();
        links.insert("payments.retry_count".to_string(), "payments.retry.count".to_string());

        let overrides = json!({ "payments.retry_count": 5, "payments.retry.count": 7 })
            .as_object()
            .expect("fixture")
            .clone();

        let err = super::apply_symlink_map(overrides, &links)
            .expect_err("a colliding override must be refused");

        assert!(err.contains("payments.retry_count"), "error names the link: {err}");
        assert!(err.contains("payments.retry.count"), "error names the target: {err}");
    }

    #[test]
    fn a_link_and_its_target_with_the_same_value_collapses_quietly() {
        use std::collections::HashMap;

        let mut links = HashMap::new();
        links.insert("payments.retry_count".to_string(), "payments.retry.count".to_string());

        let overrides = json!({ "payments.retry_count": 5, "payments.retry.count": 5 })
            .as_object()
            .expect("fixture")
            .clone();

        let normalized = super::apply_symlink_map(overrides, &links).expect("agreeing values");

        assert_eq!(normalized.len(), 1);
        assert_eq!(normalized.get("payments.retry.count"), Some(&json!(5)));
    }
```

- [ ] **Step 2: Run them to verify they fail**

Run: `cargo test -p context_aware_config symlinks`
Expected: FAIL — `cannot find function apply_symlink_map`.

- [ ] **Step 3: Implement normalization**

Add to `crates/context_aware_config/src/symlinks.rs`:

```rust
/// Rewrites symlinked keys in an override map to their targets.
///
/// Refuses a map that names both a link and its target with different values: the
/// rewrite would collapse them into one entry and silently drop a value the caller
/// asked for.
pub fn apply_symlink_map(
    overrides: serde_json::Map<String, Value>,
    links: &HashMap<String, String>,
) -> Result<serde_json::Map<String, Value>, String> {
    if links.is_empty() {
        return Ok(overrides);
    }

    let mut normalized = serde_json::Map::new();
    let mut origin: HashMap<String, String> = HashMap::new();

    for (key, value) in overrides {
        let target = links.get(&key).cloned().unwrap_or_else(|| key.clone());

        if let Some(existing) = normalized.get(&target) {
            if *existing != value {
                let first = origin.get(&target).cloned().unwrap_or_else(|| target.clone());
                return Err(format!(
                    "override names both `{first}` and `{key}`, which resolve to the \
                     same config key `{target}`, with different values; keep one of them"
                ));
            }
        }

        origin.insert(target.clone(), key);
        normalized.insert(target, value);
    }

    Ok(normalized)
}

/// Looks up which of these override keys are symlinks and rewrites them.
pub fn normalize_override_keys(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    overrides: Overrides,
) -> superposition::Result<Overrides> {
    let links = symlink_map_for_keys(conn, schema_name, overrides.keys())?;
    if links.is_empty() {
        return Ok(overrides);
    }

    let normalized = apply_symlink_map(overrides.into_inner(), &links)
        .map_err(|err| bad_argument!("{err}"))?;

    Cac::<Overrides>::try_from(normalized)
        .map(|cac| cac.into_inner())
        .map_err(|err| bad_argument!("{err}"))
}
```

Add `Cac` and `Overrides` to the module's `superposition_types` import list.

- [ ] **Step 4: Run them to verify they pass**

Run: `cargo test -p context_aware_config symlinks`
Expected: PASS, 7 tests.

- [ ] **Step 5: Apply it at the four context handlers**

`create_handler` — before `create_authorized` on line 107:

```rust
    let conn = write_permit.connection();
    let mut req = req.into_inner();
    req.r#override =
        normalize_override_keys(conn, &workspace_context.schema_name, req.r#override)?
            .try_into()
            .map_err(|e| bad_argument!("{e}"))?;

    create_authorized(&_auth_z, &req.r#override).await?;
```

The `try_into` is needed only if `req.r#override` is a wrapped type such as `Exp<Overrides>`; check the field's declared type and convert to match, or assign directly if it is plain `Overrides`.

`update_handler` — before `update_authorized` on line 248, normalize `req.override_` the same way. The connection is already in scope on line 243.

`bulk_operations_handler` — before `bulk_authorized` on line 858, normalize each operation's override map:

```rust
    for op in ops.iter_mut() {
        match op {
            ContextAction::Put(put_req) => {
                put_req.r#override = normalize_override_keys(
                    conn,
                    &workspace_context.schema_name,
                    put_req.r#override.clone(),
                )?;
            }
            ContextAction::Replace(update_req) => {
                update_req.override_ = normalize_override_keys(
                    conn,
                    &workspace_context.schema_name,
                    update_req.override_.clone(),
                )?;
            }
            _ => {}
        }
    }
```

Match the exact variant names and field types from `ContextAction`; `ops` must be `mut`.

`validate_handler` (line 1276) — normalize before validating, so the validation result describes what would actually be stored.

- [ ] **Step 6: Confirm `move` carries no override map**

Run: `grep -n "fn r#move" -A 30 crates/context_aware_config/src/api/context/operations.rs | grep -n "override"`
Expected: no match on a *request* override — `move` changes a context's condition and reuses the stored (already normalized) override. If this grep does show a request-supplied override map, normalize it in `move_handler` before `move_authorized` on line 378.

- [ ] **Step 7: Commit**

```bash
cargo build -p context_aware_config && make check
git add crates/context_aware_config/src/symlinks.rs crates/context_aware_config/src/api/context/handlers.rs
git commit -m "feat(cac): redirect symlinked override keys before hashing and authz"
```

---

### Task 6: Experiment write path

Experiments store variant overrides in their own table *and* create CAC contexts. Both must name the target, or the two views of one experiment disagree.

**Files:**
- Modify: `crates/experimentation_platform/src/api/experiments/handlers.rs` (create:160, update:~1413, conclude)
- Test: `tests/src/default_config_symlink.test.ts` (Task 8)

**Interfaces:**
- Consumes: `context_aware_config::symlinks::normalize_override_keys` (Task 5)
- Produces: no new public interface

- [ ] **Step 1: Reorder so the connection precedes authorization**

In `create_handler`, line 162 authorizes `&req.variants` *before* the connection exists on line 164. Normalization needs the connection, and must precede authorization. Swap them:

```rust
    let req = req.into_inner();
    let DbConnection(mut conn) = db_conn;

    let mut variants = req.variants;
    for variant in variants.iter_mut() {
        variant.overrides = normalize_override_keys(
            &mut conn,
            &workspace_context.schema_name,
            variant.overrides.clone().into_inner(),
        )?
        .into();
    }

    create_authorized(_auth_z, &variants).await?;
```

Then delete the later `let mut variants = req.variants;` on line 178, since `variants` now exists. Match `variant.overrides`'s wrapper type when converting — it is an `Exp<Overrides>` in the request, so `.into_inner()` on the way in and `.into()` on the way out.

- [ ] **Step 2: Normalize the declared override keys**

`unique_override_keys` is derived from `variants[0].overrides` (line ~197), so once Step 1 normalizes the variants it is already target-keyed. Confirm that nothing reads `req.override_keys` separately:

Run: `grep -n "override_keys" crates/experimentation_platform/src/api/experiments/handlers.rs`
Expected: every read derives from the variants. If a request-supplied `override_keys` field is read directly, map it through `symlink_map_for_keys` the same way.

- [ ] **Step 3: Normalize the update path**

In `update_handler` (around line 1413), the variant overrides arrive the same way. Apply the same loop before `validate_control_overrides` and before any authorization call, using the connection already in scope.

- [ ] **Step 4: Confirm conclude needs nothing**

Run: `grep -n "fn conclude_handler" -A 60 crates/experimentation_platform/src/api/experiments/handlers.rs | grep -n "overrides"`
Expected: conclude reads the *stored* variant overrides, which Steps 1 and 3 already normalized, and writes them into CAC through the context operations. If it accepts a request-supplied override map, normalize it there too.

- [ ] **Step 5: Build and commit**

```bash
cargo build -p experimentation_platform && make check
git add crates/experimentation_platform/src/api/experiments/handlers.rs
git commit -m "feat(experiments): redirect symlinked variant override keys to targets"
```

---

### Task 7: Smithy `symlink_to` and SDK regeneration

**Files:**
- Modify: `smithy/models/default-config.smithy`
- Generated: `crates/superposition_sdk/**`, `clients/java/sdk/**`, `clients/python/sdk/**`, `clients/haskell/sdk/**`, `clients/javascript/sdk/**`

**Interfaces:**
- Consumes: the response shape from Task 4
- Produces: `symlink_to` on `DefaultConfigResponse` in all five generated SDKs

- [ ] **Step 1: Add the property**

In `smithy/models/default-config.smithy`, add to the `DefaultConfig` resource's `properties` block (after `value_compute_function_name` on line 21):

```smithy
        symlink_to: String
```

- [ ] **Step 2: Add it to the response only**

In `structure DefaultConfigResponse` (line 56), add it alongside the other response-only members, with no `@required`:

```smithy
    @documentation("Present only when this key is a symlink; the key whose value and schema this entry resolves to.")
    $symlink_to
```

Leave `DefaultConfigMixin` untouched, so `CreateDefaultConfig`'s input keeps `@required` on `$value` and `$schema` — a symlink create sends the target as `value` and the marker in `schema`, satisfying both.

- [ ] **Step 3: Verify the model builds**

Run: `make smithy-build`
Expected: success. A failure naming `symlink_to` means the property was not declared on the resource.

- [ ] **Step 4: Regenerate the clients**

Run: `make smithy-clients`
Expected: regenerated sources under `crates/superposition_sdk` and `clients/*/sdk`.

Run: `git status --short clients crates/superposition_sdk`
Expected: modified generated files mentioning `symlink_to`. Confirm with:
`grep -rl "symlink_to" crates/superposition_sdk/src clients/javascript/sdk | head`

- [ ] **Step 5: Build and commit**

```bash
cargo build && make check
git add smithy/models/default-config.smithy crates/superposition_sdk clients
git commit -m "feat(api): expose symlink_to on default-config responses"
```

---

### Task 8: End-to-end integration tests

The behaviour that matters lives in handlers talking to Postgres, and this repo verifies that layer with Bun tests against the generated JS SDK. This is where the feature is actually proven.

**Files:**
- Create: `tests/src/default_config_symlink.test.ts`

**Interfaces:**
- Consumes: the regenerated JS SDK from Task 7 (`CreateDefaultConfigCommand`, `UpdateDefaultConfigCommand`, `DeleteDefaultConfigCommand`, `GetConfigCommand`), and `superpositionClient` / `ENV` from `tests/env.ts`
- Produces: no code interface

- [ ] **Step 1: Write the test file**

```typescript
import {
    CreateDefaultConfigCommand,
    UpdateDefaultConfigCommand,
    DeleteDefaultConfigCommand,
    GetDefaultConfigCommand,
    CreateContextCommand,
    GetConfigCommand,
} from "@juspay/superposition-sdk";
import { superpositionClient, ENV } from "../env.ts";
import { describe, afterAll, test, expect } from "bun:test";

const TARGET = "symlink.target.count";
const LINK = "symlink_target_count";
const SECOND_LINK = "symlink.alias.count";

const base = { workspace_id: ENV.workspace_id, org_id: ENV.org_id };
const created: string[] = [];

async function createTarget() {
    await superpositionClient.send(
        new CreateDefaultConfigCommand({
            ...base,
            key: TARGET,
            value: 3,
            schema: { type: "integer", minimum: 0, maximum: 10 },
            description: "symlink test target",
            change_reason: "test setup",
        }),
    );
    created.push(TARGET);
}

async function createLink(key: string, target: string) {
    const response = await superpositionClient.send(
        new CreateDefaultConfigCommand({
            ...base,
            key,
            value: target,
            schema: { "x-superposition-symlink": true },
            description: `alias of ${target}`,
            change_reason: "test setup",
        }),
    );
    created.push(key);
    return response;
}

describe("Default Config Symlinks", () => {
    afterAll(async () => {
        // Links first: a target cannot be deleted while a link points at it.
        for (const key of [...created].reverse()) {
            try {
                await superpositionClient.send(
                    new DeleteDefaultConfigCommand({ ...base, key }),
                );
            } catch (error) {
                console.log(`cleanup failed for ${key}:`, error);
            }
        }
    });

    test("a link resolves to the target's value and schema, and reports symlink_to", async () => {
        await createTarget();
        await createLink(LINK, TARGET);

        const got = await superpositionClient.send(
            new GetDefaultConfigCommand({ ...base, key: LINK }),
        );

        expect(got.symlink_to).toBe(TARGET);
        expect(got.value).toBe(3);
        expect(got.schema).toMatchObject({ type: "integer" });
        expect(got.schema["x-superposition-symlink"]).toBeUndefined();
    });

    test("both names appear in the evaluated config", async () => {
        const config = await superpositionClient.send(new GetConfigCommand({ ...base }));

        expect(config.default_configs?.[TARGET]).toBe(3);
        expect(config.default_configs?.[LINK]).toBe(3);
    });

    test("an override written against the link lands on the target", async () => {
        await superpositionClient.send(
            new CreateContextCommand({
                ...base,
                context: { "symlink.test.dimension": "on" },
                override: { [LINK]: 7 },
                description: "override via the link",
                change_reason: "test",
            }),
        );

        const config = await superpositionClient.send(new GetConfigCommand({ ...base }));
        const overrides = Object.values(config.overrides ?? {});
        const stored = overrides.find((o: any) => o[TARGET] === 7) as any;

        expect(stored).toBeDefined();
        expect(stored[TARGET]).toBe(7);
        expect(stored[LINK]).toBe(7);
    });

    test("a value update on the link moves the target", async () => {
        await superpositionClient.send(
            new UpdateDefaultConfigCommand({
                ...base,
                key: LINK,
                value: 8,
                change_reason: "write through the link",
            }),
        );

        const target = await superpositionClient.send(
            new GetDefaultConfigCommand({ ...base, key: TARGET }),
        );
        expect(target.value).toBe(8);
        expect(target.symlink_to).toBeUndefined();
    });

    test("deleting the target is refused while a link points at it", async () => {
        await expect(
            superpositionClient.send(new DeleteDefaultConfigCommand({ ...base, key: TARGET })),
        ).rejects.toThrow(new RegExp(LINK));
    });

    test("a link to a link is flattened to the concrete key", async () => {
        const response = await createLink(SECOND_LINK, LINK);
        expect(response.symlink_to).toBe(TARGET);
    });

    test("a symlink cannot carry a validation function", async () => {
        await expect(
            superpositionClient.send(
                new CreateDefaultConfigCommand({
                    ...base,
                    key: "symlink.rejected",
                    value: TARGET,
                    schema: { "x-superposition-symlink": true },
                    value_validation_function_name: "any_function",
                    description: "should be refused",
                    change_reason: "test",
                }),
            ),
        ).rejects.toThrow();
    });

    test("an override naming both a link and its target is refused", async () => {
        await expect(
            superpositionClient.send(
                new CreateContextCommand({
                    ...base,
                    context: { "symlink.test.dimension": "collide" },
                    override: { [LINK]: 1, [TARGET]: 2 },
                    description: "colliding override",
                    change_reason: "test",
                }),
            ),
        ).rejects.toThrow();
    });
});
```

- [ ] **Step 2: Run the tests**

Run: `cd tests && bun test src/default_config_symlink.test.ts`
Expected: PASS. The dimension `symlink.test.dimension` must exist in the test workspace — if the override tests fail on an unknown dimension, create it in a `beforeAll` with `CreateDimensionCommand`, following `tests/src/context.test.ts`.

- [ ] **Step 3: Run the neighbouring suites for regressions**

Run: `cd tests && bun test src/default_config.test.ts src/config.test.ts src/context.test.ts src/experiments.test.ts`
Expected: PASS — a workspace with no symlinks must behave exactly as before.

- [ ] **Step 4: Commit**

```bash
git add tests/src/default_config_symlink.test.ts
git commit -m "test: end-to-end coverage for default-config symlinks"
```

---

### Task 9: Frontend

Additive only: the server resolves schemas, so the existing schema-driven form needs no change to render a link's type.

**Files:**
- Modify: `crates/frontend/src/pages/default_config_list.rs`, `crates/frontend/src/pages/default_config_list/types.rs`
- Modify: `crates/frontend/src/components/default_config_form.rs`
- Modify: `crates/frontend/src/components/override_form.rs`

**Interfaces:**
- Consumes: `symlink_to` on the default-config response (Task 7), `superposition_types::symlink::SYMLINK_KEYWORD` (Task 1)
- Produces: no code interface

- [ ] **Step 1: Show the link in the list**

Add `symlink_to: Option<String>` to the row type in `default_config_list/types.rs`, and in the list page render a badge in the key column when it is `Some`:

```rust
view! {
    <span class="flex items-center gap-2">
        {key.clone()}
        {move || symlink_to.clone().map(|target| view! {
            <span class="badge badge-sm badge-ghost" title="symlink">
                {format!("→ {target}")}
            </span>
        })}
    </span>
}
```

- [ ] **Step 2: Add symlink mode to the create form**

In `default_config_form.rs`, add a mode toggle. When symlink mode is on, hide the value and schema inputs, show a target selector, and submit `value = <target>` with `schema = {"x-superposition-symlink": true}` built from `SYMLINK_KEYWORD` rather than a literal.

**Gate the target selector behind `client_side_ready`.** A dropdown subtree rendered during SSR and again during hydration is the exact shape that caused the workspaces-page hydration panic (#1148):

```rust
{move || client_side_ready.get().then(|| view! {
    <TargetSelector ... />
})}
```

Follow the `client_side_ready` pattern already used in this codebase — grep for it to copy the local idiom rather than inventing one.

- [ ] **Step 3: Badge links in the override key picker**

In `override_form.rs`, links stay selectable like any other key — a symlink is a key everywhere — but render the same `→ target` badge beside the name so the write-through is visible before commit, not as a surprise on reload.

Then catch the collision the server refuses in Task 5, so the user sees it before saving rather than as a 400:

```rust
/// The key an override entry will actually be stored under.
fn effective_key(key: &str, links: &HashMap<String, String>) -> String {
    links.get(key).cloned().unwrap_or_else(|| key.to_string())
}

let collision = Signal::derive(move || {
    let links = symlink_map.get();
    let mut seen: HashMap<String, String> = HashMap::new();
    selected_keys.get().into_iter().find_map(|key| {
        let target = effective_key(&key, &links);
        match seen.insert(target.clone(), key.clone()) {
            Some(first) if first != key => Some((first, key, target)),
            _ => None,
        }
    })
});
```

Render the warning and disable the submit button while `collision` is `Some`, with a message naming both keys and the target they share — the same information the server's error carries. `symlink_map` is built from the `symlink_to` field the key list already returns after Task 7, so no extra request is needed.

- [ ] **Step 4: Verify in a real browser**

Run the app with **release** wasm, not `--dev`: a `--dev` hydrate build hard-aborts on disposed signals in dropdowns, and a stale site directory silently serves old wasm. Build the frontend, serve it, and check in order:

1. The default-config list renders, with a badge on the link row.
2. Creating a symlink from the form succeeds and the new row shows its badge.
3. The link's detail view shows the target's type in the schema form.
4. The browser console has no hydration panic.

- [ ] **Step 5: Commit**

```bash
make check
git add crates/frontend/src
git commit -m "feat(frontend): show and create default-config symlinks"
```

---

### Task 10: Documentation

**Files:**
- Create: `docs/docs/features/default-config-symlinks.md`

- [ ] **Step 1: Write the page**

Cover, with the worked `payments.retry.count` example from the spec: what a symlink is and the two use cases it serves; how to create one (`value` = target, `schema` = `{"x-superposition-symlink": true}`); that reads of either name always agree, under defaults and under contexts; that writes to a link redirect to its target, including from the UI; that a target cannot be deleted while links point at it; that links cannot chain; that a link carries no validation or compute function of its own; and the rename runbook note that **write** grants held on the old key name must be repointed to the target, since authorization follows the redirect.

Match the frontmatter and sidebar conventions of a neighbouring page in `docs/docs/features/` — read one first.

- [ ] **Step 2: Verify the docs build**

Run: `cd docs && npm ci && npm run build`
Expected: success, with the new page in the sidebar. If `docs/` uses a different toolchain, follow its README.

- [ ] **Step 3: Commit**

```bash
git add docs/docs/features/default-config-symlinks.md
git commit -m "docs: document default-config symlinks"
```
