//! Server-side symlink handling: the queries that find links, and the type that
//! keeps an unexpanded config away from the paths that serve or persist one.

use std::collections::HashMap;

use diesel::{
    ExpressionMethods, OptionalExtension, QueryDsl, RunQueryDsl, SelectableHelper,
    dsl::sql, sql_types::Bool,
};
use serde_json::{Map, Value};
use service_utils::service::types::SchemaName;
use superposition_macros::{bad_argument, db_error};
use superposition_types::{
    Cac, Config, DBConnection, Overrides,
    api::default_config::DefaultConfigResponse,
    database::{models::cac::DefaultConfig, schema::default_configs::dsl},
    result as superposition,
    symlink::{SymlinkRow, is_symlink_schema, symlink_target},
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
        .load::<(String, Value, String)>(conn)
        .map_err(|err| {
            log::error!("failed to fetch symlinks with error: {}", err);
            db_error!(err)
        })?;

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
        .load::<(String, Value)>(conn)
        .map_err(|err| {
            log::error!("failed to fetch symlink map with error: {}", err);
            db_error!(err)
        })?;

    Ok(rows
        .into_iter()
        .filter_map(|(key, value)| Some((key, value.as_str()?.to_string())))
        .collect())
}

/// Rewrites symlinked keys in an override map to their targets.
///
/// Refuses a map that names both a link and its target with different values: the
/// rewrite would collapse them into one entry and silently drop a value the caller
/// asked for.
pub fn apply_symlink_map(
    overrides: Map<String, Value>,
    links: &HashMap<String, String>,
) -> Result<Map<String, Value>, String> {
    if links.is_empty() {
        return Ok(overrides);
    }

    let mut normalized = Map::new();
    let mut origin: HashMap<String, String> = HashMap::new();

    for (key, value) in overrides {
        let target = links.get(&key).cloned().unwrap_or_else(|| key.clone());

        if let Some(existing) = normalized.get(&target) {
            if *existing != value {
                let first = origin
                    .get(&target)
                    .cloned()
                    .unwrap_or_else(|| target.clone());
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
        .map_err(|err| bad_argument!("{}", err))?;

    Cac::<Overrides>::try_from(normalized)
        .map(|cac| cac.into_inner())
        .map_err(|err| bad_argument!("{}", err))
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
        .optional()
        .map_err(|err| {
            log::error!("failed to fetch symlink target with error: {}", err);
            db_error!(err)
        })?
        .ok_or_else(|| bad_argument!("symlink target `{requested}` does not exist"))?;

    if is_symlink_schema(row.schema.inner()) {
        let target = symlink_target(row.schema.inner(), &row.value).ok_or_else(|| {
            bad_argument!(
                "symlink `{requested}` is malformed; its value is not a key name"
            )
        })?;
        return Ok(target.to_string());
    }

    Ok(requested.to_string())
}

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

/// Resolves a single row for a response. Ordinary rows pass through untouched.
pub fn resolve_for_response(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    row: DefaultConfig,
) -> superposition::Result<DefaultConfigResponse> {
    let Some(target_key) =
        symlink_target(row.schema.inner(), &row.value).map(str::to_string)
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
        .optional()
        .map_err(|err| {
            log::error!("failed to fetch symlink target with error: {}", err);
            db_error!(err)
        })?;

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

/// Resolves a batch of rows for a response in one extra query, regardless of how
/// many of them are links. `list_handler`'s `pagination.all = true` path can return
/// every row in the workspace, so resolving link-by-link would be an N+1.
pub fn resolve_many_for_response(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    rows: Vec<DefaultConfig>,
) -> superposition::Result<Vec<DefaultConfigResponse>> {
    let target_keys: Vec<String> = rows
        .iter()
        .filter_map(|row| {
            symlink_target(row.schema.inner(), &row.value).map(str::to_string)
        })
        .collect();

    let targets: HashMap<String, DefaultConfig> = if target_keys.is_empty() {
        HashMap::new()
    } else {
        dsl::default_configs
            .filter(dsl::key.eq_any(&target_keys))
            .select(DefaultConfig::as_select())
            .schema_name(schema_name)
            .load::<DefaultConfig>(conn)
            .map_err(|err| {
                log::error!("failed to batch-fetch symlink targets with error: {}", err);
                db_error!(err)
            })?
            .into_iter()
            .map(|target| (target.key.clone(), target))
            .collect()
    };

    Ok(rows
        .into_iter()
        .map(|row| {
            let Some(target_key) =
                symlink_target(row.schema.inner(), &row.value).map(str::to_string)
            else {
                return DefaultConfigResponse {
                    config: row,
                    symlink_to: None,
                };
            };

            match targets.get(&target_key) {
                Some(target) => {
                    let config = merge_target_into_link(row, target);
                    DefaultConfigResponse {
                        config,
                        symlink_to: Some(target_key),
                    }
                }
                None => {
                    log::error!(
                        "symlink {} -> {target_key}: target missing, returning the pointer unresolved",
                        row.key
                    );
                    DefaultConfigResponse {
                        config: row,
                        symlink_to: Some(target_key),
                    }
                }
            }
        })
        .collect())
}

#[cfg(test)]
mod tests {
    use serde_json::{Value, from_value, json};
    use superposition_types::{
        Config,
        database::models::cac::DefaultConfig,
        symlink::{SYMLINK_KEYWORD, SymlinkRow},
    };

    use super::RawConfig;

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
        let mut target =
            row("payments.retry.count", json!(3), json!({"type": "integer"}));
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

        // Renaming the keyword must not be able to silently desync the SQL
        // literals above from the on-disk representation in Task 1.
        assert!(super::IS_A_SYMLINK_SQL.contains(SYMLINK_KEYWORD));
        assert!(super::NOT_A_SYMLINK_SQL.contains(SYMLINK_KEYWORD));
    }

    #[test]
    fn apply_rewrites_a_link_to_its_target() {
        use std::collections::HashMap;

        let mut links = HashMap::new();
        links.insert(
            "payments.retry_count".to_string(),
            "payments.retry.count".to_string(),
        );

        let overrides = json!({ "payments.retry_count": 5 })
            .as_object()
            .expect("fixture")
            .clone();

        let normalized =
            super::apply_symlink_map(overrides, &links).expect("should rewrite");

        assert_eq!(normalized.get("payments.retry.count"), Some(&json!(5)));
        assert!(!normalized.contains_key("payments.retry_count"));
    }

    #[test]
    fn apply_leaves_ordinary_keys_alone() {
        use std::collections::HashMap;

        let overrides = json!({ "a.b": 1, "c.d": 2 })
            .as_object()
            .expect("fixture")
            .clone();
        let normalized = super::apply_symlink_map(overrides.clone(), &HashMap::new())
            .expect("no links");

        assert_eq!(normalized, overrides);
    }

    #[test]
    fn a_link_and_its_target_with_different_values_is_rejected() {
        // Review Focus 1: normalizing would otherwise collapse two entries into one
        // and silently lose a value the caller asked for.
        use std::collections::HashMap;

        let mut links = HashMap::new();
        links.insert(
            "payments.retry_count".to_string(),
            "payments.retry.count".to_string(),
        );

        let overrides = json!({ "payments.retry_count": 5, "payments.retry.count": 7 })
            .as_object()
            .expect("fixture")
            .clone();

        let err = super::apply_symlink_map(overrides, &links)
            .expect_err("a colliding override must be refused");

        assert!(
            err.contains("payments.retry_count"),
            "error names the link: {err}"
        );
        assert!(
            err.contains("payments.retry.count"),
            "error names the target: {err}"
        );
    }

    #[test]
    fn a_link_and_its_target_with_the_same_value_collapses_quietly() {
        use std::collections::HashMap;

        let mut links = HashMap::new();
        links.insert(
            "payments.retry_count".to_string(),
            "payments.retry.count".to_string(),
        );

        let overrides = json!({ "payments.retry_count": 5, "payments.retry.count": 5 })
            .as_object()
            .expect("fixture")
            .clone();

        let normalized =
            super::apply_symlink_map(overrides, &links).expect("agreeing values");

        assert_eq!(normalized.len(), 1);
        assert_eq!(normalized.get("payments.retry.count"), Some(&json!(5)));
    }
}
