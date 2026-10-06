//! Server-side symlink handling: the queries that find links, and the type that
//! keeps an unexpanded config away from the paths that serve or persist one.

use std::collections::HashMap;

use diesel::{
    ExpressionMethods, OptionalExtension, QueryDsl, RunQueryDsl, SelectableHelper,
    dsl::sql, sql_types::Bool,
};
use serde_json::Value;
use service_utils::service::types::SchemaName;
use superposition_macros::{bad_argument, db_error};
use superposition_types::{
    Config, DBConnection,
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

#[cfg(test)]
mod tests {
    use serde_json::{from_value, json};
    use superposition_types::{
        Config,
        symlink::{SYMLINK_KEYWORD, SymlinkRow},
    };

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

        // Renaming the keyword must not be able to silently desync the SQL
        // literals above from the on-disk representation in Task 1.
        assert!(super::IS_A_SYMLINK_SQL.contains(SYMLINK_KEYWORD));
        assert!(super::NOT_A_SYMLINK_SQL.contains(SYMLINK_KEYWORD));
    }
}
