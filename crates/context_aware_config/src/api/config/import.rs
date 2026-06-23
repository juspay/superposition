//! Import support for SuperTOML.
//!
//! This module powers the "import" side of the TOML/JSON config endpoints.
//! When a body is POSTed to `/config/toml` or `/config/json`, it is parsed
//! (and fully validated) into a [`DetailedConfig`] via the format's
//! [`ConfigFormat::parse_into_detailed`] and then persisted to the workspace.
//!
//! The behaviour is controlled through request headers:
//! - `x-import-strategy`: `upsert` (default) creates missing entities and
//!   updates existing entities; `create_only` only creates missing entities and
//!   skips existing entities; `replace` mirrors the file by also deleting any
//!   dimension/default-config/context that is absent from it.
//! - `x-import-on-error`: `abort` (default) fails the whole import on the
//!   first error; `continue` records per-entity errors and applies the rest.
//! - `x-import-dry-run`: `true` parses, validates and computes the summary
//!   without persisting anything (the transaction is rolled back).

use std::collections::HashSet;

use actix_web::{HttpRequest, HttpResponse, web::Data};
use chrono::Utc;
use diesel::{Connection, ExpressionMethods, QueryDsl, RunQueryDsl};
use serde::Serialize;
use serde_json::Value;
use service_utils::{
    helpers::{WebhookData, execute_webhook_call, parse_config_tags},
    service::types::{AppState, CustomHeaders, SchemaName, WorkspaceContext},
};
use superposition_core::{ConfigFormat, helpers::calculate_context_weight};
use superposition_macros::{bad_argument, db_error};
use superposition_types::{
    Context as ConfigContext, DBConnection, DefaultConfigInfo, DimensionInfo,
    ExtendedMap, Overrides, Resource, User,
    api::webhook::Action,
    database::models::{
        ChangeReason, Description,
        cac::{Context as DbContext, DefaultConfig, Dimension, Position},
        others::WebhookEvent,
    },
    database::schema::{
        contexts::dsl as ctx_dsl, default_configs::dsl as dc_dsl,
        dimensions::dsl as dim_dsl,
    },
    result as superposition,
};

use crate::{
    api::context::operations,
    helpers::{add_config_version, put_config_in_redis},
};

// ---------------------------------------------------------------------------
// Options
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ImportStrategy {
    CreateOnly,
    Upsert,
    Replace,
}

#[derive(Clone, Copy, PartialEq)]
pub enum OnError {
    Abort,
    Continue,
}

#[derive(Clone, Copy)]
pub struct ImportOptions {
    pub strategy: ImportStrategy,
    pub on_error: OnError,
    pub dry_run: bool,
}

fn header_bool(req: &HttpRequest, name: &str, default: bool) -> bool {
    req.headers()
        .get(name)
        .and_then(|v| v.to_str().ok())
        .and_then(|s| s.parse::<bool>().ok())
        .unwrap_or(default)
}

impl ImportOptions {
    pub fn from_request(req: &HttpRequest) -> superposition::Result<Self> {
        for header in [
            "x-import-mode",
            "x-import-overwrite",
            "x-import-value-merge",
        ] {
            if req.headers().contains_key(header) {
                return Err(bad_argument!(
                    "Header '{}' is no longer supported; use x-import-strategy",
                    header
                ));
            }
        }

        let strategy = match req
            .headers()
            .get("x-import-strategy")
            .and_then(|v| v.to_str().ok())
        {
            None => ImportStrategy::Upsert,
            Some(s) if s.eq_ignore_ascii_case("create_only") => {
                ImportStrategy::CreateOnly
            }
            Some(s) if s.eq_ignore_ascii_case("upsert") => ImportStrategy::Upsert,
            Some(s) if s.eq_ignore_ascii_case("replace") => ImportStrategy::Replace,
            Some(s) => {
                return Err(bad_argument!(
                    "Invalid x-import-strategy '{}', expected 'create_only', 'upsert' or 'replace'",
                    s
                ));
            }
        };
        let on_error = match req
            .headers()
            .get("x-import-on-error")
            .and_then(|v| v.to_str().ok())
        {
            None => OnError::Abort,
            Some(s) if s.eq_ignore_ascii_case("abort") => OnError::Abort,
            Some(s) if s.eq_ignore_ascii_case("continue") => OnError::Continue,
            Some(s) => {
                return Err(bad_argument!(
                    "Invalid x-import-on-error '{}', expected 'abort' or 'continue'",
                    s
                ));
            }
        };
        Ok(Self {
            strategy,
            on_error,
            dry_run: header_bool(req, "x-import-dry-run", false),
        })
    }
}

// ---------------------------------------------------------------------------
// Summary
// ---------------------------------------------------------------------------

#[derive(Serialize)]
pub struct ImportError {
    pub id: String,
    pub message: String,
}

#[derive(Default, Serialize)]
pub struct EntityReport {
    pub created: usize,
    pub updated: usize,
    pub skipped: usize,
    pub deleted: usize,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub errors: Vec<ImportError>,
}

impl EntityReport {
    fn record(&mut self, outcome: Outcome) {
        match outcome {
            Outcome::Created => self.created += 1,
            Outcome::Updated => self.updated += 1,
            Outcome::Skipped => self.skipped += 1,
            Outcome::Deleted => self.deleted += 1,
        }
    }
}

#[derive(Serialize)]
pub struct ImportSummary {
    pub strategy: ImportStrategy,
    pub dry_run: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub config_version: Option<String>,
    pub dimensions: EntityReport,
    pub default_configs: EntityReport,
    pub contexts: EntityReport,
}

impl ImportSummary {
    fn new(opts: &ImportOptions) -> Self {
        Self {
            strategy: opts.strategy,
            dry_run: opts.dry_run,
            config_version: None,
            dimensions: EntityReport::default(),
            default_configs: EntityReport::default(),
            contexts: EntityReport::default(),
        }
    }
}

#[derive(Clone, Copy)]
enum Outcome {
    Created,
    Updated,
    Skipped,
    Deleted,
}

// ---------------------------------------------------------------------------
// Transaction error plumbing
// ---------------------------------------------------------------------------

/// Error type used inside the import transaction. `DryRun` carries the summary
/// out of the (deliberately rolled-back) transaction so it can still be
/// returned to the caller.
enum TxError {
    App(superposition::AppError),
    DryRun(Box<ImportSummary>),
}

impl From<superposition::AppError> for TxError {
    fn from(e: superposition::AppError) -> Self {
        TxError::App(e)
    }
}

impl From<diesel::result::Error> for TxError {
    fn from(e: diesel::result::Error) -> Self {
        TxError::App(db_error!(e))
    }
}

/// Run `f` within a SAVEPOINT so that a failure rolls back only the work done
/// by `f` (leaving the surrounding transaction usable). This is what makes the
/// `continue-on-error` option safe in the face of Postgres aborting the
/// current transaction on a database error.
fn with_savepoint<T>(
    conn: &mut DBConnection,
    name: &str,
    f: impl FnOnce(&mut DBConnection) -> superposition::Result<T>,
) -> superposition::Result<T> {
    diesel::sql_query(format!("SAVEPOINT {name}"))
        .execute(conn)
        .map_err(|e| db_error!(e))?;
    match f(conn) {
        Ok(v) => {
            diesel::sql_query(format!("RELEASE SAVEPOINT {name}"))
                .execute(conn)
                .map_err(|e| db_error!(e))?;
            Ok(v)
        }
        Err(e) => {
            diesel::sql_query(format!("ROLLBACK TO SAVEPOINT {name}"))
                .execute(conn)
                .map_err(|e| db_error!(e))?;
            Err(e)
        }
    }
}

/// Apply the `on-error` policy to a single entity's result, recording it into
/// the report. Returns `Err` only when the policy is `abort`.
fn apply_outcome(
    report: &mut EntityReport,
    on_error: OnError,
    id: &str,
    result: superposition::Result<Outcome>,
) -> Result<(), TxError> {
    match result {
        Ok(outcome) => {
            report.record(outcome);
            Ok(())
        }
        Err(e) => match on_error {
            OnError::Continue => {
                report.errors.push(ImportError {
                    id: id.to_string(),
                    message: e.message(),
                });
                Ok(())
            }
            OnError::Abort => Err(TxError::App(e)),
        },
    }
}

fn import_change_reason() -> ChangeReason {
    ChangeReason::try_from("Imported via SuperTOML config import".to_string())
        .unwrap_or_default()
}

fn import_description() -> Description {
    Description::try_from("Config imported via TOML/JSON import".to_string())
        .unwrap_or_default()
}

// ---------------------------------------------------------------------------
// Pure builders / decisions (no database access — unit tested below)
// ---------------------------------------------------------------------------

/// Whether an entity that already exists should be left untouched.
fn should_skip(exists: bool, strategy: ImportStrategy) -> bool {
    exists && strategy == ImportStrategy::CreateOnly
}

fn dimension_position(
    name: &str,
    info: &DimensionInfo,
) -> superposition::Result<Position> {
    Position::try_from(info.position)
        .map_err(|e| bad_argument!("Invalid position for dimension '{}': {}", name, e))
}

fn build_dimension_row(
    name: &str,
    info: &DimensionInfo,
    email: &str,
) -> superposition::Result<Dimension> {
    Ok(Dimension {
        dimension: name.to_string(),
        schema: info.schema.clone(),
        position: dimension_position(name, info)?,
        dimension_type: info.dimension_type.clone(),
        dependency_graph: info.dependency_graph.clone(),
        value_compute_function_name: info.value_compute_function_name.clone(),
        // Function bindings are not carried by the import file.
        value_validation_function_name: None,
        created_at: Utc::now(),
        created_by: email.to_string(),
        last_modified_at: Utc::now(),
        last_modified_by: email.to_string(),
        description: import_description(),
        change_reason: import_change_reason(),
    })
}

fn build_schema(key: &str, schema: &Value) -> superposition::Result<ExtendedMap> {
    ExtendedMap::try_from(schema.clone())
        .map_err(|e| bad_argument!("Invalid schema for default config '{}': {}", key, e))
}

fn build_default_config_row(
    key: &str,
    value: Value,
    schema: ExtendedMap,
    email: &str,
) -> DefaultConfig {
    DefaultConfig {
        key: key.to_string(),
        value,
        schema,
        value_validation_function_name: None,
        value_compute_function_name: None,
        created_at: Utc::now(),
        created_by: email.to_string(),
        last_modified_at: Utc::now(),
        last_modified_by: email.to_string(),
        description: import_description(),
        change_reason: import_change_reason(),
    }
}

// ---------------------------------------------------------------------------
// Per-entity writers
// ---------------------------------------------------------------------------

fn write_dimension(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    name: &str,
    info: &DimensionInfo,
    opts: &ImportOptions,
    email: &str,
) -> superposition::Result<Outcome> {
    let exists = dim_dsl::dimensions
        .filter(dim_dsl::dimension.eq(name))
        .count()
        .schema_name(schema_name)
        .get_result::<i64>(conn)?
        > 0;

    if should_skip(exists, opts.strategy) {
        return Ok(Outcome::Skipped);
    }

    if exists {
        let position = dimension_position(name, info)?;
        diesel::update(dim_dsl::dimensions.filter(dim_dsl::dimension.eq(name)))
            .set((
                dim_dsl::schema.eq(info.schema.clone()),
                dim_dsl::position.eq(position),
                dim_dsl::dimension_type.eq(info.dimension_type.clone()),
                dim_dsl::dependency_graph.eq(info.dependency_graph.clone()),
                dim_dsl::value_compute_function_name
                    .eq(info.value_compute_function_name.clone()),
                dim_dsl::last_modified_at.eq(Utc::now()),
                dim_dsl::last_modified_by.eq(email),
                dim_dsl::change_reason.eq(import_change_reason()),
            ))
            .schema_name(schema_name)
            .execute(conn)?;
        Ok(Outcome::Updated)
    } else {
        let dimension = build_dimension_row(name, info, email)?;
        diesel::insert_into(dim_dsl::dimensions)
            .values(&dimension)
            .schema_name(schema_name)
            .execute(conn)?;
        Ok(Outcome::Created)
    }
}

fn write_default_config(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    key: &str,
    info: &DefaultConfigInfo,
    opts: &ImportOptions,
    email: &str,
) -> superposition::Result<Outcome> {
    let exists = dc_dsl::default_configs
        .filter(dc_dsl::key.eq(key))
        .count()
        .schema_name(schema_name)
        .get_result::<i64>(conn)?
        > 0;

    if should_skip(exists, opts.strategy) {
        return Ok(Outcome::Skipped);
    }

    let value = info.value.clone();
    let schema = build_schema(key, &info.schema)?;

    if exists {
        diesel::update(dc_dsl::default_configs.filter(dc_dsl::key.eq(key)))
            .set((
                dc_dsl::value.eq(value),
                dc_dsl::schema.eq(schema),
                dc_dsl::last_modified_at.eq(Utc::now()),
                dc_dsl::last_modified_by.eq(email),
                dc_dsl::change_reason.eq(import_change_reason()),
            ))
            .schema_name(schema_name)
            .execute(conn)?;
        Ok(Outcome::Updated)
    } else {
        let default_config = build_default_config_row(key, value, schema, email);
        diesel::insert_into(dc_dsl::default_configs)
            .values(&default_config)
            .schema_name(schema_name)
            .execute(conn)?;
        Ok(Outcome::Created)
    }
}

#[allow(clippy::too_many_arguments)]
fn write_context(
    conn: &mut DBConnection,
    workspace_context: &WorkspaceContext,
    ctx: &ConfigContext,
    overrides: &std::collections::HashMap<String, Overrides>,
    dimensions: &std::collections::HashMap<String, DimensionInfo>,
    opts: &ImportOptions,
    user: &User,
    email: &str,
) -> superposition::Result<Outcome> {
    let schema_name = &workspace_context.schema_name;
    let override_key = ctx.override_with_keys.get_key();
    let override_ = overrides.get(override_key).cloned().ok_or_else(|| {
        bad_argument!(
            "Override '{}' referenced by context '{}' not found in file",
            override_key,
            ctx.id
        )
    })?;

    let exists = ctx_dsl::contexts
        .filter(ctx_dsl::id.eq(&ctx.id))
        .count()
        .schema_name(schema_name)
        .get_result::<i64>(conn)?
        > 0;

    if should_skip(exists, opts.strategy) {
        return Ok(Outcome::Skipped);
    }

    let weight = calculate_context_weight(&ctx.condition, dimensions)
        .map_err(|e| bad_argument!("Failed to compute context weight: {}", e))?;

    let new_ctx = DbContext {
        id: ctx.id.clone(),
        value: ctx.condition.clone(),
        override_id: override_key.clone(),
        override_,
        weight,
        created_at: Utc::now(),
        created_by: email.to_string(),
        last_modified_at: Utc::now(),
        last_modified_by: email.to_string(),
        description: import_description(),
        change_reason: import_change_reason(),
    };

    operations::upsert(conn, true, user, workspace_context, true, new_ctx)?;
    Ok(if exists {
        Outcome::Updated
    } else {
        Outcome::Created
    })
}

// ---------------------------------------------------------------------------
// Core import
// ---------------------------------------------------------------------------

pub async fn import_config<F: ConfigFormat>(
    body: &str,
    opts: ImportOptions,
    tags: Option<Vec<String>>,
    user: &User,
    workspace_context: &WorkspaceContext,
    state: &Data<AppState>,
    conn: &mut DBConnection,
) -> superposition::Result<ImportSummary> {
    let parsed = F::parse_into_detailed(body)
        .map_err(|e| bad_argument!("Failed to parse config: {}", e))?;
    let schema_name = &workspace_context.schema_name;
    let email = user.get_email();

    let tx_result = conn.transaction::<_, TxError, _>(|conn| {
        let mut summary = ImportSummary::new(&opts);

        // Order matters for referential consistency: dimensions and default
        // configs are written before contexts that reference them.
        for (name, info) in &parsed.dimensions {
            let res = with_savepoint(conn, "cac_import_dim", |c| {
                write_dimension(c, schema_name, name, info, &opts, &email)
            });
            apply_outcome(&mut summary.dimensions, opts.on_error, name, res)?;
        }

        for (key, info) in parsed.default_configs.iter() {
            let res = with_savepoint(conn, "cac_import_dc", |c| {
                write_default_config(c, schema_name, key, info, &opts, &email)
            });
            apply_outcome(&mut summary.default_configs, opts.on_error, key, res)?;
        }

        for ctx in &parsed.contexts {
            let res = with_savepoint(conn, "cac_import_ctx", |c| {
                write_context(
                    c,
                    workspace_context,
                    ctx,
                    &parsed.overrides,
                    &parsed.dimensions,
                    &opts,
                    user,
                    &email,
                )
            });
            apply_outcome(&mut summary.contexts, opts.on_error, &ctx.id, res)?;
        }

        // Replace strategy: delete anything in the workspace that is not
        // present in the imported file. Contexts first, then the entities they
        // can reference.
        if opts.strategy == ImportStrategy::Replace {
            let file_ctx_ids: HashSet<&String> =
                parsed.contexts.iter().map(|c| &c.id).collect();
            let db_ctx_ids: Vec<String> = ctx_dsl::contexts
                .select(ctx_dsl::id)
                .schema_name(schema_name)
                .load::<String>(conn)?;
            for id in db_ctx_ids {
                if file_ctx_ids.contains(&id) {
                    continue;
                }
                let res = with_savepoint(conn, "cac_import_del_ctx", |c| {
                    diesel::delete(ctx_dsl::contexts.filter(ctx_dsl::id.eq(&id)))
                        .schema_name(schema_name)
                        .execute(c)
                        .map_err(|e| db_error!(e))?;
                    Ok(Outcome::Deleted)
                });
                apply_outcome(&mut summary.contexts, opts.on_error, &id, res)?;
            }

            let db_dc_keys: Vec<String> = dc_dsl::default_configs
                .select(dc_dsl::key)
                .schema_name(schema_name)
                .load::<String>(conn)?;
            for key in db_dc_keys {
                if parsed.default_configs.contains_key(&key) {
                    continue;
                }
                let res = with_savepoint(conn, "cac_import_del_dc", |c| {
                    diesel::delete(dc_dsl::default_configs.filter(dc_dsl::key.eq(&key)))
                        .schema_name(schema_name)
                        .execute(c)
                        .map_err(|e| db_error!(e))?;
                    Ok(Outcome::Deleted)
                });
                apply_outcome(&mut summary.default_configs, opts.on_error, &key, res)?;
            }

            let db_dim_names: Vec<String> = dim_dsl::dimensions
                .select(dim_dsl::dimension)
                .schema_name(schema_name)
                .load::<String>(conn)?;
            for name in db_dim_names {
                if parsed.dimensions.contains_key(&name) {
                    continue;
                }
                let res = with_savepoint(conn, "cac_import_del_dim", |c| {
                    diesel::delete(
                        dim_dsl::dimensions.filter(dim_dsl::dimension.eq(&name)),
                    )
                    .schema_name(schema_name)
                    .execute(c)
                    .map_err(|e| db_error!(e))?;
                    Ok(Outcome::Deleted)
                });
                apply_outcome(&mut summary.dimensions, opts.on_error, &name, res)?;
            }
        }

        if opts.dry_run {
            // Roll back everything; the summary travels out via the error.
            return Err(TxError::DryRun(Box::new(summary)));
        }

        let config_version =
            add_config_version(state, tags, import_description(), conn, schema_name)?;
        Ok((summary, config_version))
    });

    match tx_result {
        Ok((mut summary, config_version)) => {
            summary.config_version = Some(config_version.id.to_string());

            let _ = put_config_in_redis(&config_version, state, schema_name, conn).await;

            let data = WebhookData {
                payload: &summary,
                resource: Resource::Config,
                action: Action::Update,
                event: WebhookEvent::ConfigChanged,
                config_version_opt: Some(config_version.id.to_string()),
            };
            let _ = execute_webhook_call(data, workspace_context, state, conn).await;

            Ok(summary)
        }
        Err(TxError::DryRun(summary)) => Ok(*summary),
        Err(TxError::App(e)) => Err(e),
    }
}

/// HTTP entry-point used by the `/config/toml` and `/config/json` handlers when
/// a body is POSTed. Parses options/tags from the request and returns the
/// import summary as JSON.
pub async fn handle_import<F: ConfigFormat>(
    body: &[u8],
    req: &HttpRequest,
    custom_headers: CustomHeaders,
    user: &User,
    workspace_context: &WorkspaceContext,
    state: &Data<AppState>,
    conn: &mut DBConnection,
) -> superposition::Result<HttpResponse> {
    let body_str = std::str::from_utf8(body)
        .map_err(|_| bad_argument!("Request body is not valid UTF-8"))?;
    let opts = ImportOptions::from_request(req)?;
    let tags = parse_config_tags(custom_headers.config_tags)?;

    let summary =
        import_config::<F>(body_str, opts, tags, user, workspace_context, state, conn)
            .await?;

    Ok(HttpResponse::Ok().json(summary))
}

#[cfg(test)]
mod tests {
    use actix_web::test::TestRequest;
    use serde_json::json;
    use superposition_types::database::models::cac::{DependencyGraph, DimensionType};

    use super::*;

    fn dim_info(position: i32) -> DimensionInfo {
        DimensionInfo {
            schema: ExtendedMap::try_from(json!({ "type": "string" })).unwrap(),
            position,
            dimension_type: DimensionType::Regular {},
            dependency_graph: DependencyGraph::default(),
            value_compute_function_name: None,
            description: String::new(),
        }
    }

    #[test]
    fn options_default_to_upsert() {
        let req = TestRequest::default().to_http_request();
        let opts = ImportOptions::from_request(&req).unwrap();
        assert!(opts.strategy == ImportStrategy::Upsert);
        assert!(opts.on_error == OnError::Abort);
        assert!(!opts.dry_run);
    }

    #[test]
    fn options_parsed_from_headers() {
        let req = TestRequest::default()
            .insert_header(("x-import-strategy", "replace"))
            .insert_header(("x-import-on-error", "continue"))
            .insert_header(("x-import-dry-run", "true"))
            .to_http_request();
        let opts = ImportOptions::from_request(&req).unwrap();
        assert!(opts.strategy == ImportStrategy::Replace);
        assert!(opts.on_error == OnError::Continue);
        assert!(opts.dry_run);
    }

    #[test]
    fn invalid_strategy_is_rejected() {
        let req = TestRequest::default()
            .insert_header(("x-import-strategy", "bogus"))
            .to_http_request();
        assert!(ImportOptions::from_request(&req).is_err());
    }

    #[test]
    fn removed_import_headers_are_rejected() {
        for header in [
            "x-import-mode",
            "x-import-overwrite",
            "x-import-value-merge",
        ] {
            let req = TestRequest::default()
                .insert_header((header, "true"))
                .to_http_request();
            assert!(ImportOptions::from_request(&req).is_err());
        }
    }

    #[test]
    fn invalid_on_error_is_rejected() {
        let req = TestRequest::default()
            .insert_header(("x-import-on-error", "maybe"))
            .to_http_request();
        assert!(ImportOptions::from_request(&req).is_err());
    }

    #[test]
    fn should_skip_only_existing_create_only_entities() {
        assert!(should_skip(true, ImportStrategy::CreateOnly));
        assert!(!should_skip(false, ImportStrategy::CreateOnly));
        assert!(!should_skip(true, ImportStrategy::Upsert));
        assert!(!should_skip(true, ImportStrategy::Replace));
    }

    #[test]
    fn dimension_position_rejects_negative() {
        assert!(dimension_position("city", &dim_info(-1)).is_err());
        assert!(dimension_position("city", &dim_info(0)).is_ok());
        assert!(dimension_position("city", &dim_info(7)).is_ok());
    }

    #[test]
    fn build_dimension_row_maps_fields_and_clears_validation_fn() {
        let mut info = dim_info(3);
        info.value_compute_function_name = Some("compute_fn".to_string());

        let row = build_dimension_row("city", &info, "tester@example.com").unwrap();

        assert_eq!(row.dimension, "city");
        assert_eq!(i32::from(row.position), 3);
        assert_eq!(
            row.value_compute_function_name.as_deref(),
            Some("compute_fn")
        );
        // validation-fn bindings are not carried by the file
        assert!(row.value_validation_function_name.is_none());
        assert_eq!(row.created_by, "tester@example.com");
        assert!(matches!(row.dimension_type, DimensionType::Regular {}));
    }

    #[test]
    fn build_schema_requires_an_object() {
        assert!(build_schema("k", &json!({ "type": "number" })).is_ok());
        // a non-object schema is rejected
        assert!(build_schema("k", &json!("not-an-object")).is_err());
    }

    #[test]
    fn build_default_config_row_carries_value_and_schema() {
        let schema = build_schema("per_km_rate", &json!({ "type": "number" })).unwrap();
        let row = build_default_config_row(
            "per_km_rate",
            json!(20.0),
            schema,
            "tester@example.com",
        );

        assert_eq!(row.key, "per_km_rate");
        assert_eq!(row.value, json!(20.0));
        assert!(row.value_validation_function_name.is_none());
        assert!(row.value_compute_function_name.is_none());
        assert_eq!(row.created_by, "tester@example.com");
    }

    #[test]
    fn summary_serialises_with_strategy_and_hides_empty_errors() {
        let opts = ImportOptions {
            strategy: ImportStrategy::Replace,
            on_error: OnError::Abort,
            dry_run: true,
        };
        let summary = ImportSummary::new(&opts);
        let value = serde_json::to_value(&summary).unwrap();

        assert_eq!(value["strategy"], json!("replace"));
        assert_eq!(value["dry_run"], json!(true));
        // config_version omitted until the import commits
        assert!(value.get("config_version").is_none());
        // no errors array when empty
        assert!(value["dimensions"].get("errors").is_none());
        assert_eq!(value["dimensions"]["created"], json!(0));
    }

    #[test]
    fn entity_report_records_outcomes() {
        let mut report = EntityReport::default();
        report.record(Outcome::Created);
        report.record(Outcome::Created);
        report.record(Outcome::Updated);
        report.record(Outcome::Skipped);
        report.record(Outcome::Deleted);

        assert_eq!(report.created, 2);
        assert_eq!(report.updated, 1);
        assert_eq!(report.skipped, 1);
        assert_eq!(report.deleted, 1);
    }
}
