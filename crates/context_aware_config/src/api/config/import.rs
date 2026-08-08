use std::collections::{HashMap, HashSet};

use actix_web::{web::Data, HttpRequest, HttpResponse};
use chrono::Utc;
use diesel::{Connection, ExpressionMethods, QueryDsl, RunQueryDsl};
use service_utils::{
    helpers::{
        execute_webhook_call, fetch_dimensions_info_map, parse_config_tags, WebhookData,
    },
    service::types::{AppState, CustomHeaders, WorkspaceContext},
};
use superposition_core::{
    helpers::{
        calculate_context_weight,
        create_connections_with_dependents as build_dependency_connections,
    },
    validations, ConfigFormat,
};
use superposition_macros::{bad_argument, db_error, unexpected_error};
use superposition_types::{
    api::{
        config::{ImportEntityReport, ImportStrategy, ImportSummary},
        webhook::Action,
    },
    database::models::{
        cac::{Context as DbContext, DefaultConfig, Dimension, DimensionType, Position},
        others::WebhookEvent,
        ChangeReason, Description,
    },
    database::schema::{
        contexts::dsl as ctx_dsl, default_configs::dsl as dc_dsl,
        dimensions::dsl as dim_dsl,
    },
    result as superposition, Context as ConfigContext, DBConnection, DefaultConfigInfo,
    DetailedConfig, DimensionInfo, ExtendedMap, Resource, User,
};

use crate::helpers::{add_config_version, generate_detailed_cac, put_config_in_redis};

#[derive(Clone, Copy)]
pub struct ImportOptions {
    pub strategy: ImportStrategy,
    pub dry_run: bool,
}

impl ImportOptions {
    pub fn from_request(req: &HttpRequest) -> superposition::Result<Self> {
        let strategy = match req.headers().get("x-import-strategy") {
            None => ImportStrategy::Upsert,
            Some(value) => match value
                .to_str()
                .map_err(|_| bad_argument!("x-import-strategy must be valid text"))?
            {
                "create_only" => ImportStrategy::CreateOnly,
                "upsert" => ImportStrategy::Upsert,
                "replace" => ImportStrategy::Replace,
                value => {
                    return Err(bad_argument!(
                        "Invalid x-import-strategy value '{}'; must be one of: create_only, upsert, replace",
                        value
                    ));
                }
            },
        };

        let dry_run = match req.headers().get("x-import-dry-run") {
            None => false,
            Some(value) => value
                .to_str()
                .map_err(|_| bad_argument!("x-import-dry-run must be valid text"))?
                .parse::<bool>()
                .map_err(|_| {
                    bad_argument!("Invalid x-import-dry-run value; must be true or false")
                })?,
        };

        Ok(Self { strategy, dry_run })
    }
}

#[derive(Clone, Copy)]
enum Outcome {
    Created,
    Updated,
    Skipped,
    Deleted,
}

fn record(report: &mut ImportEntityReport, outcome: Outcome) {
    match outcome {
        Outcome::Created => report.created += 1,
        Outcome::Updated => report.updated += 1,
        Outcome::Skipped => report.skipped += 1,
        Outcome::Deleted => report.deleted += 1,
    }
}

enum ImportTransactionError {
    Failed(superposition::AppError),
    Preview(Box<ImportSummary>),
}

impl From<superposition::AppError> for ImportTransactionError {
    fn from(e: superposition::AppError) -> Self {
        ImportTransactionError::Failed(e)
    }
}

impl From<diesel::result::Error> for ImportTransactionError {
    fn from(e: diesel::result::Error) -> Self {
        ImportTransactionError::Failed(db_error!(e))
    }
}

struct ImportExecutor<'a> {
    conn: &'a mut DBConnection,
    workspace: &'a WorkspaceContext,
    options: &'a ImportOptions,
    email: String,
    description: Description,
    change_reason: ChangeReason,
}

impl<'a> ImportExecutor<'a> {
    fn new(
        conn: &'a mut DBConnection,
        workspace: &'a WorkspaceContext,
        user: &'a User,
        options: &'a ImportOptions,
    ) -> superposition::Result<Self> {
        Ok(Self {
            conn,
            workspace,
            options,
            email: user.get_email(),
            description: Description::try_from(
                "Config imported via TOML/JSON import".to_string(),
            )
            .map_err(|e| unexpected_error!(e))?,
            change_reason: ChangeReason::try_from(
                "Imported via SuperTOML config import".to_string(),
            )
            .map_err(|e| unexpected_error!(e))?,
        })
    }

    fn write_entity(
        conn: &mut DBConnection,
        strategy: ImportStrategy,
        exists: impl FnOnce(&mut DBConnection) -> superposition::Result<bool>,
        persist: impl FnOnce(&mut DBConnection, bool) -> superposition::Result<()>,
    ) -> superposition::Result<Outcome> {
        let exists = exists(conn)?;

        if exists && strategy == ImportStrategy::CreateOnly {
            return Ok(Outcome::Skipped);
        }

        persist(conn, exists)?;

        Ok(if exists {
            Outcome::Updated
        } else {
            Outcome::Created
        })
    }

    fn write_dimension(
        &mut self,
        name: &str,
        info: &DimensionInfo,
    ) -> superposition::Result<Outcome> {
        let schema_name = &self.workspace.schema_name;
        let strategy = self.options.strategy;
        let email = &self.email;
        let change_reason = &self.change_reason;
        let description =
            Description::try_from(info.description.clone()).map_err(|e| {
                bad_argument!("Invalid description for dimension '{}': {}", name, e)
            })?;

        Self::write_entity(
            self.conn,
            strategy,
            |conn| {
                Ok(dim_dsl::dimensions
                    .filter(dim_dsl::dimension.eq(name))
                    .count()
                    .schema_name(schema_name)
                    .get_result::<i64>(conn)?
                    > 0)
            },
            |conn, exists| {
                let position = Position::try_from(info.position).map_err(|e| {
                    bad_argument!("Invalid position for dimension '{}': {}", name, e)
                })?;

                if exists {
                    diesel::update(
                        dim_dsl::dimensions.filter(dim_dsl::dimension.eq(name)),
                    )
                    .set((
                        dim_dsl::schema.eq(info.schema.clone()),
                        dim_dsl::position.eq(position),
                        dim_dsl::dimension_type.eq(info.dimension_type.clone()),
                        dim_dsl::dependency_graph.eq(info.dependency_graph.clone()),
                        dim_dsl::last_modified_at.eq(Utc::now()),
                        dim_dsl::last_modified_by.eq(email),
                        dim_dsl::description.eq(&description),
                        dim_dsl::change_reason.eq(change_reason.clone()),
                    ))
                    .schema_name(schema_name)
                    .execute(conn)?;
                } else {
                    let dimension = Dimension {
                        dimension: name.to_string(),
                        schema: info.schema.clone(),
                        position,
                        dimension_type: info.dimension_type.clone(),
                        dependency_graph: info.dependency_graph.clone(),
                        value_compute_function_name: info
                            .value_compute_function_name
                            .clone(),
                        value_validation_function_name: None,
                        created_at: Utc::now(),
                        created_by: email.clone(),
                        last_modified_at: Utc::now(),
                        last_modified_by: email.clone(),
                        description,
                        change_reason: change_reason.clone(),
                    };

                    diesel::insert_into(dim_dsl::dimensions)
                        .values(&dimension)
                        .schema_name(schema_name)
                        .execute(conn)?;
                }

                Ok(())
            },
        )
    }

    fn write_default_config(
        &mut self,
        key: &str,
        info: &DefaultConfigInfo,
    ) -> superposition::Result<Outcome> {
        let schema_name = &self.workspace.schema_name;
        let strategy = self.options.strategy;
        let email = &self.email;
        let change_reason = &self.change_reason;
        let description =
            Description::try_from(info.description.clone()).map_err(|e| {
                bad_argument!("Invalid description for default config '{}': {}", key, e)
            })?;

        Self::write_entity(
            self.conn,
            strategy,
            |conn| {
                Ok(dc_dsl::default_configs
                    .filter(dc_dsl::key.eq(key))
                    .count()
                    .schema_name(schema_name)
                    .get_result::<i64>(conn)?
                    > 0)
            },
            |conn, exists| {
                let value = info.value.clone();
                let schema = ExtendedMap::try_from(info.schema.clone()).map_err(|e| {
                    bad_argument!("Invalid schema for default config '{}': {}", key, e)
                })?;

                if exists {
                    diesel::update(dc_dsl::default_configs.filter(dc_dsl::key.eq(key)))
                        .set((
                            dc_dsl::value.eq(value),
                            dc_dsl::schema.eq(schema),
                            dc_dsl::last_modified_at.eq(Utc::now()),
                            dc_dsl::last_modified_by.eq(email),
                            dc_dsl::description.eq(&description),
                            dc_dsl::change_reason.eq(change_reason.clone()),
                        ))
                        .schema_name(schema_name)
                        .execute(conn)?;
                } else {
                    let default_config = DefaultConfig {
                        key: key.to_string(),
                        value,
                        schema,
                        value_validation_function_name: None,
                        value_compute_function_name: None,
                        created_at: Utc::now(),
                        created_by: email.clone(),
                        last_modified_at: Utc::now(),
                        last_modified_by: email.clone(),
                        description,
                        change_reason: change_reason.clone(),
                    };

                    diesel::insert_into(dc_dsl::default_configs)
                        .values(&default_config)
                        .schema_name(schema_name)
                        .execute(conn)?;
                }

                Ok(())
            },
        )
    }

    fn write_context(
        &mut self,
        ctx: &ConfigContext,
        parsed: &DetailedConfig,
        effective_config: Option<&DetailedConfig>,
    ) -> superposition::Result<Outcome> {
        let schema_name = &self.workspace.schema_name;
        let strategy = self.options.strategy;
        let email = &self.email;
        let change_reason = &self.change_reason;
        let override_key = ctx.override_with_keys.get_key();
        let description = parsed
            .context_descriptions
            .get(&ctx.id)
            .map(|description| {
                Description::try_from(description.clone()).map_err(|e| {
                    bad_argument!("Invalid description for context '{}': {}", ctx.id, e)
                })
            })
            .transpose()?
            .unwrap_or_else(|| self.description.clone());
        let override_ = parsed.overrides.get(override_key).cloned().ok_or_else(|| {
            bad_argument!(
                "Override '{}' referenced by context '{}' not found in file",
                override_key,
                ctx.id
            )
        })?;

        Self::write_entity(
            self.conn,
            strategy,
            |conn| {
                Ok(ctx_dsl::contexts
                    .filter(ctx_dsl::id.eq(&ctx.id))
                    .count()
                    .schema_name(schema_name)
                    .get_result::<i64>(conn)?
                    > 0)
            },
            |conn, exists| {
                let dimensions = effective_config
                    .map(|config| &config.dimensions)
                    .unwrap_or(&parsed.dimensions);

                if let Some(config) = effective_config {
                    validations::validate_context(&ctx.condition, dimensions).map_err(
                        |errors| {
                            bad_argument!(
                                "Context '{}' is invalid for existing dimensions: {:?}",
                                ctx.id,
                                errors
                            )
                        },
                    )?;
                    validations::validate_overrides(
                        &override_,
                        &config.default_configs,
                    )
                    .map_err(|errors| {
                        bad_argument!(
                            "Context '{}' has invalid overrides for existing default configs: {:?}",
                            ctx.id,
                            errors
                        )
                    })?;
                }

                let weight = calculate_context_weight(&ctx.condition, dimensions)
                    .map_err(|e| {
                        bad_argument!("Failed to compute context weight: {}", e)
                    })?;

                if exists {
                    diesel::update(ctx_dsl::contexts.filter(ctx_dsl::id.eq(&ctx.id)))
                        .set((
                            ctx_dsl::value.eq(ctx.condition.clone()),
                            ctx_dsl::override_id.eq(override_key),
                            ctx_dsl::override_.eq(override_),
                            ctx_dsl::weight.eq(weight),
                            ctx_dsl::last_modified_at.eq(Utc::now()),
                            ctx_dsl::last_modified_by.eq(email),
                            ctx_dsl::description.eq(description),
                            ctx_dsl::change_reason.eq(change_reason.clone()),
                        ))
                        .schema_name(schema_name)
                        .execute(conn)?;
                } else {
                    let new_ctx = DbContext {
                        id: ctx.id.clone(),
                        value: ctx.condition.clone(),
                        override_id: override_key.clone(),
                        override_,
                        weight,
                        created_at: Utc::now(),
                        created_by: email.clone(),
                        last_modified_at: Utc::now(),
                        last_modified_by: email.clone(),
                        description,
                        change_reason: change_reason.clone(),
                    };

                    diesel::insert_into(ctx_dsl::contexts)
                        .values(&new_ctx)
                        .schema_name(schema_name)
                        .execute(conn)?;
                }

                Ok(())
            },
        )
    }

    fn refresh_dependency_graphs(&mut self) -> superposition::Result<()> {
        let schema_name = &self.workspace.schema_name;
        let mut dimensions = fetch_dimensions_info_map(self.conn, schema_name)?;
        let previous_graphs: HashMap<_, _> = dimensions
            .iter_mut()
            .map(|(name, info)| {
                (name.clone(), std::mem::take(&mut info.dependency_graph))
            })
            .collect();

        let cohort_relations: Vec<_> = dimensions
            .iter()
            .filter_map(|(name, info)| match &info.dimension_type {
                DimensionType::LocalCohort(parent)
                | DimensionType::RemoteCohort(parent) => {
                    Some((name.clone(), parent.clone()))
                }
                DimensionType::Regular {} => None,
            })
            .collect();

        for (name, parent) in cohort_relations {
            build_dependency_connections(&parent, &name, &mut dimensions);
        }

        for (name, info) in dimensions {
            if previous_graphs.get(&name) == Some(&info.dependency_graph) {
                continue;
            }

            diesel::update(dim_dsl::dimensions.filter(dim_dsl::dimension.eq(&name)))
                .set(dim_dsl::dependency_graph.eq(info.dependency_graph))
                .schema_name(schema_name)
                .execute(self.conn)?;
        }

        Ok(())
    }

    fn execute(
        &mut self,
        parsed: &DetailedConfig,
    ) -> superposition::Result<ImportSummary> {
        let mut summary = ImportSummary::new(self.options.strategy, self.options.dry_run);

        for (name, info) in &parsed.dimensions {
            record(&mut summary.dimensions, self.write_dimension(name, info)?);
        }

        for (key, info) in parsed.default_configs.iter() {
            record(
                &mut summary.default_configs,
                self.write_default_config(key, info)?,
            );
        }

        let effective_config = (self.options.strategy == ImportStrategy::CreateOnly)
            .then(|| generate_detailed_cac(self.conn, &self.workspace.schema_name))
            .transpose()?;

        for ctx in &parsed.contexts {
            record(
                &mut summary.contexts,
                self.write_context(ctx, parsed, effective_config.as_ref())?,
            );
        }

        if self.options.strategy == ImportStrategy::Replace {
            let schema_name = &self.workspace.schema_name;
            let file_ctx_ids: HashSet<&str> =
                parsed.contexts.iter().map(|ctx| ctx.id.as_str()).collect();
            let db_ctx_ids: Vec<String> = ctx_dsl::contexts
                .select(ctx_dsl::id)
                .schema_name(schema_name)
                .load::<String>(self.conn)?;

            for id in db_ctx_ids {
                if !file_ctx_ids.contains(id.as_str()) {
                    diesel::delete(ctx_dsl::contexts.filter(ctx_dsl::id.eq(&id)))
                        .schema_name(schema_name)
                        .execute(self.conn)?;
                    record(&mut summary.contexts, Outcome::Deleted);
                }
            }

            let db_dc_keys: Vec<String> = dc_dsl::default_configs
                .select(dc_dsl::key)
                .schema_name(schema_name)
                .load::<String>(self.conn)?;

            for key in db_dc_keys {
                if !parsed.default_configs.contains_key(&key) {
                    diesel::delete(dc_dsl::default_configs.filter(dc_dsl::key.eq(&key)))
                        .schema_name(schema_name)
                        .execute(self.conn)?;
                    record(&mut summary.default_configs, Outcome::Deleted);
                }
            }

            let db_dim_names: Vec<String> = dim_dsl::dimensions
                .select(dim_dsl::dimension)
                .schema_name(schema_name)
                .load::<String>(self.conn)?;

            for name in db_dim_names {
                if !parsed.dimensions.contains_key(&name) {
                    diesel::delete(
                        dim_dsl::dimensions.filter(dim_dsl::dimension.eq(&name)),
                    )
                    .schema_name(schema_name)
                    .execute(self.conn)?;
                    record(&mut summary.dimensions, Outcome::Deleted);
                }
            }
        }

        self.refresh_dependency_graphs()?;

        Ok(summary)
    }
}

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

    let tx_result = conn.transaction::<_, ImportTransactionError, _>(|conn| {
        let mut executor = ImportExecutor::new(conn, workspace_context, user, &opts)?;
        let summary = executor.execute(&parsed)?;

        if opts.dry_run {
            // Roll back everything; the summary travels out via the error.
            return Err(ImportTransactionError::Preview(Box::new(summary)));
        }

        let description = executor.description.clone();
        drop(executor);
        let config_version =
            add_config_version(state, tags, description, conn, schema_name)?;
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
        Err(ImportTransactionError::Preview(summary)) => Ok(*summary),
        Err(ImportTransactionError::Failed(e)) => Err(e),
    }
}

/// Imports uploaded config bytes after parsing request options and tags.
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

    use super::*;

    #[test]
    fn options_default_to_upsert() {
        let req = TestRequest::default().to_http_request();
        let opts = ImportOptions::from_request(&req).unwrap();
        assert_eq!(opts.strategy, ImportStrategy::Upsert);
        assert!(!opts.dry_run);
    }

    #[test]
    fn options_parsed_from_headers() {
        let req = TestRequest::default()
            .insert_header(("x-import-strategy", "replace"))
            .insert_header(("x-import-dry-run", "true"))
            .to_http_request();
        let opts = ImportOptions::from_request(&req).unwrap();
        assert_eq!(opts.strategy, ImportStrategy::Replace);
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
    fn invalid_dry_run_is_rejected() {
        let req = TestRequest::default()
            .insert_header(("x-import-dry-run", "yes"))
            .to_http_request();
        assert!(ImportOptions::from_request(&req).is_err());
    }

    #[test]
    fn summary_serialises_with_strategy() {
        let opts = ImportOptions {
            strategy: ImportStrategy::Replace,
            dry_run: true,
        };
        let summary = ImportSummary::new(opts.strategy, opts.dry_run);
        let value = serde_json::to_value(&summary).unwrap();

        assert_eq!(value["strategy"], json!("replace"));
        assert_eq!(value["dry_run"], json!(true));
        // config_version omitted until the import commits
        assert!(value.get("config_version").is_none());
        assert_eq!(value["dimensions"]["created"], json!(0));
    }

    #[test]
    fn entity_report_records_outcomes() {
        let mut report = ImportEntityReport::default();
        record(&mut report, Outcome::Created);
        record(&mut report, Outcome::Created);
        record(&mut report, Outcome::Updated);
        record(&mut report, Outcome::Skipped);
        record(&mut report, Outcome::Deleted);

        assert_eq!(report.created, 2);
        assert_eq!(report.updated, 1);
        assert_eq!(report.skipped, 1);
        assert_eq!(report.deleted, 1);
    }
}
