use std::collections::{BTreeMap, HashMap, HashSet};

use actix_web::{web::Data, HttpRequest, HttpResponse};
use chrono::Utc;
use diesel::{Connection, ExpressionMethods, QueryDsl, RunQueryDsl};
use service_utils::{
    helpers::{execute_webhook_call, parse_config_tags, WebhookData},
    service::types::{AppState, CustomHeaders, SchemaName, WorkspaceContext},
};
use superposition_core::{helpers::calculate_context_weight, validations, ConfigFormat};
use superposition_macros::{bad_argument, db_error, unexpected_error};
use superposition_types::{
    api::{
        config::{ImportEntityReport, ImportStrategy, ImportSummary},
        dimension::DimensionName,
        webhook::Action,
    },
    database::models::{
        cac::{
            Context as DbContext, DefaultConfig, Dimension, DimensionType, FunctionType,
            Position,
        },
        others::WebhookEvent,
        ChangeReason, Description,
    },
    database::schema::{
        contexts::dsl as ctx_dsl, default_configs::dsl as dc_dsl,
        dimensions::dsl as dim_dsl,
    },
    result as superposition, Context as ConfigContext, DBConnection, DefaultConfigInfo,
    DefaultConfigsWithSchema, DetailedConfig, DimensionInfo, ExtendedMap,
    InternalUserContext, Resource, User,
};

use crate::{
    api::{
        context::helpers::{
            validate_condition_with_dependent_dimensions,
            validate_condition_with_functions,
            validate_condition_with_mandatory_dimensions,
            validate_override_with_functions,
        },
        default_config::{validate_default_config_with_function, validate_fn_published},
        dimension::{
            allow_primitive_types, validate_dimension_position,
            validate_validation_function, validate_value_compute_function,
        },
    },
    helpers::{
        add_config_version, generate_detailed_cac, put_config_in_redis,
        validate_change_reason,
    },
};

#[derive(Clone, Copy)]
struct ImportOptions {
    strategy: ImportStrategy,
    dry_run: bool,
}

impl ImportOptions {
    fn from_request(req: &HttpRequest) -> superposition::Result<Self> {
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
}

fn record(report: &mut ImportEntityReport, outcome: Outcome) {
    match outcome {
        Outcome::Created => report.created += 1,
        Outcome::Updated => report.updated += 1,
        Outcome::Skipped => report.skipped += 1,
    }
}

struct ExistingEntities {
    dimensions: HashMap<String, Dimension>,
    default_configs: HashMap<String, DefaultConfig>,
    contexts: HashMap<String, DbContext>,
}

impl ExistingEntities {
    fn load(
        conn: &mut DBConnection,
        schema_name: &SchemaName,
    ) -> superposition::Result<Self> {
        Ok(Self {
            dimensions: dim_dsl::dimensions
                .schema_name(schema_name)
                .load::<Dimension>(conn)?
                .into_iter()
                .map(|row| (row.dimension.clone(), row))
                .collect(),
            default_configs: dc_dsl::default_configs
                .schema_name(schema_name)
                .load::<DefaultConfig>(conn)?
                .into_iter()
                .map(|row| (row.key.clone(), row))
                .collect(),
            contexts: ctx_dsl::contexts
                .schema_name(schema_name)
                .load::<DbContext>(conn)?
                .into_iter()
                .map(|row| (row.id.clone(), row))
                .collect(),
        })
    }
}

fn should_skip(strategy: ImportStrategy, exists: bool) -> bool {
    strategy == ImportStrategy::CreateOnly && exists
}

fn effective_dimensions(
    parsed: &DetailedConfig,
    existing: &ExistingEntities,
    strategy: ImportStrategy,
) -> HashMap<String, DimensionInfo> {
    if strategy == ImportStrategy::Replace {
        return parsed.dimensions.clone();
    }

    let mut dimensions = existing
        .dimensions
        .iter()
        .map(|(name, row)| {
            (
                name.clone(),
                DimensionInfo {
                    schema: row.schema.clone(),
                    position: *row.position,
                    dimension_type: row.dimension_type.clone(),
                    dependency_graph: row.dependency_graph.clone(),
                    value_compute_function_name: row.value_compute_function_name.clone(),
                    description: String::from(&row.description),
                },
            )
        })
        .collect::<HashMap<_, _>>();

    for (name, info) in &parsed.dimensions {
        if strategy != ImportStrategy::CreateOnly || !dimensions.contains_key(name) {
            dimensions.insert(name.clone(), info.clone());
        }
    }
    dimensions
}

fn effective_default_configs(
    parsed: &DetailedConfig,
    existing: &ExistingEntities,
    strategy: ImportStrategy,
) -> DefaultConfigsWithSchema {
    if strategy == ImportStrategy::Replace {
        return parsed.default_configs.clone();
    }

    let mut default_configs = existing
        .default_configs
        .iter()
        .map(|(key, row)| {
            (
                key.clone(),
                DefaultConfigInfo {
                    value: row.value.clone(),
                    schema: row.schema.clone().into(),
                    description: String::from(&row.description),
                },
            )
        })
        .collect::<BTreeMap<_, _>>();

    for (key, info) in parsed.default_configs.iter() {
        if strategy != ImportStrategy::CreateOnly || !default_configs.contains_key(key) {
            default_configs.insert(key.clone(), info.clone());
        }
    }
    default_configs.into()
}

#[allow(clippy::too_many_arguments)]
async fn validate_import(
    parsed: &DetailedConfig,
    existing: &ExistingEntities,
    options: ImportOptions,
    change_reason: &ChangeReason,
    internal_user: &InternalUserContext,
    workspace: &WorkspaceContext,
    state: &Data<AppState>,
    conn: &mut DBConnection,
) -> superposition::Result<()> {
    validate_change_reason(workspace, change_reason, conn, &state.master_encryption_key)
        .await?;

    let dimensions = effective_dimensions(parsed, existing, options.strategy);
    let default_configs = effective_default_configs(parsed, existing, options.strategy);
    let max_position =
        i64::try_from(dimensions.len()).map_err(|error| unexpected_error!(error))? - 1;

    for (name, info) in &parsed.dimensions {
        let existing_dimension = existing.dimensions.get(name);
        if should_skip(options.strategy, existing_dimension.is_some()) {
            continue;
        }

        let position = Position::try_from(info.position).map_err(|error| {
            bad_argument!("Invalid position for dimension '{}': {}", name, error)
        })?;
        let dimension_name = DimensionName::try_from(name.clone()).map_err(|error| {
            bad_argument!("Invalid dimension name '{}': {}", name, error)
        })?;
        validate_dimension_position(dimension_name, position, max_position)?;

        if let DimensionType::LocalCohort(parent) | DimensionType::RemoteCohort(parent) =
            &info.dimension_type
        {
            if matches!(
                dimensions
                    .get(parent)
                    .map(|dimension| &dimension.dimension_type),
                Some(DimensionType::LocalCohort(_))
            ) {
                return Err(bad_argument!(
                    "Cohort dimension '{}' cannot be based on local cohort dimension '{}'",
                    name,
                    parent
                ));
            }
        }
        if matches!(
            &info.dimension_type,
            DimensionType::Regular {} | DimensionType::RemoteCohort(_)
        ) {
            allow_primitive_types(&info.schema)?;
        }

        let validation_function = existing_dimension
            .and_then(|dimension| dimension.value_validation_function_name.as_ref());
        validate_validation_function(
            &validation_function.cloned(),
            conn,
            &workspace.schema_name,
        )?;

        let compute_function = info.value_compute_function_name.as_ref().or_else(|| {
            existing_dimension
                .and_then(|dimension| dimension.value_compute_function_name.as_ref())
        });
        validate_value_compute_function(
            &info.dimension_type,
            &compute_function.cloned(),
            conn,
            &workspace.schema_name,
        )?;
    }

    for (key, info) in parsed.default_configs.iter() {
        let existing_config = existing.default_configs.get(key);
        if should_skip(options.strategy, existing_config.is_some()) {
            continue;
        }
        if info
            .schema
            .as_object()
            .is_none_or(serde_json::Map::is_empty)
        {
            return Err(bad_argument!(
                "Schema cannot be empty for default config '{}'.",
                key
            ));
        }

        if let Some(existing_config) = existing_config {
            validate_default_config_with_function(
                workspace,
                conn,
                &existing_config.value_validation_function_name,
                key,
                &info.value,
                &state.master_encryption_key,
            )
            .await?;
            validate_fn_published(
                &existing_config.value_compute_function_name,
                FunctionType::ValueCompute,
                conn,
                &workspace.schema_name,
            )?;
        }
    }

    if options.strategy == ImportStrategy::Replace {
        let mandatory_dimensions = workspace
            .settings
            .mandatory_dimensions
            .as_deref()
            .unwrap_or_default();
        if let Some(name) = mandatory_dimensions
            .iter()
            .find(|name| !parsed.dimensions.contains_key(*name))
        {
            return Err(bad_argument!(
                "Dimension '{}' is mandatory and must be present in a replace import",
                name
            ));
        }
        if let Some(name) = existing.dimensions.keys().find(|name| {
            !parsed.dimensions.contains_key(*name) && name.as_str() == "variantIds"
        }) {
            return Err(bad_argument!(
                "Dimension '{}' is mandatory and cannot be deleted",
                name
            ));
        }
    }

    for context in &parsed.contexts {
        let override_ = parsed
            .overrides
            .get(context.override_with_keys.get_key())
            .ok_or_else(|| {
                bad_argument!(
                    "Override '{}' referenced by context '{}' not found in file",
                    context.override_with_keys.get_key(),
                    context.id
                )
            })?;

        if let Some(mandatory_dimensions) = &workspace.settings.mandatory_dimensions {
            validate_condition_with_mandatory_dimensions(
                &context.condition,
                mandatory_dimensions,
            )?;
        }
        validate_condition_with_dependent_dimensions(&dimensions, &context.condition)?;
        validations::validate_overrides(override_, &default_configs).map_err(
            |errors| {
                bad_argument!(
                "Context '{}' has invalid overrides for the imported configuration: {:?}",
                context.id,
                errors
            )
            },
        )?;
        validate_condition_with_functions(
            workspace,
            conn,
            &context.condition,
            override_,
            workspace.settings.enable_context_validation,
            &state.master_encryption_key,
            internal_user,
        )
        .await?;
        validate_override_with_functions(
            workspace,
            conn,
            override_,
            &context.condition,
            &state.master_encryption_key,
        )
        .await?;
    }

    Ok(())
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
    existing: &'a ExistingEntities,
    email: String,
    description: Description,
    change_reason: ChangeReason,
}

fn upsert_default_config(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    row: DefaultConfig,
    existing: Option<&DefaultConfig>,
) -> superposition::Result<Outcome> {
    if let Some(existing) = existing {
        let validation_function = row
            .value_validation_function_name
            .as_ref()
            .or(existing.value_validation_function_name.as_ref());
        let compute_function = row
            .value_compute_function_name
            .as_ref()
            .or(existing.value_compute_function_name.as_ref());

        diesel::update(dc_dsl::default_configs.filter(dc_dsl::key.eq(&row.key)))
            .set((
                dc_dsl::value.eq(&row.value),
                dc_dsl::schema.eq(&row.schema),
                dc_dsl::value_validation_function_name.eq(validation_function),
                dc_dsl::value_compute_function_name.eq(compute_function),
                dc_dsl::last_modified_at.eq(row.last_modified_at),
                dc_dsl::last_modified_by.eq(&row.last_modified_by),
                dc_dsl::description.eq(&row.description),
                dc_dsl::change_reason.eq(&row.change_reason),
            ))
            .schema_name(schema_name)
            .execute(conn)?;
        Ok(Outcome::Updated)
    } else {
        diesel::insert_into(dc_dsl::default_configs)
            .values(&row)
            .schema_name(schema_name)
            .execute(conn)?;
        Ok(Outcome::Created)
    }
}

fn overwrite_context(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    row: DbContext,
    exists: bool,
) -> superposition::Result<Outcome> {
    if !exists {
        diesel::insert_into(ctx_dsl::contexts)
            .values(&row)
            .schema_name(schema_name)
            .execute(conn)?;
        Ok(Outcome::Created)
    } else {
        diesel::update(ctx_dsl::contexts.filter(ctx_dsl::id.eq(&row.id)))
            .set((
                ctx_dsl::value.eq(&row.value),
                ctx_dsl::override_id.eq(&row.override_id),
                ctx_dsl::override_.eq(&row.override_),
                ctx_dsl::weight.eq(&row.weight),
                ctx_dsl::last_modified_at.eq(row.last_modified_at),
                ctx_dsl::last_modified_by.eq(&row.last_modified_by),
                ctx_dsl::description.eq(&row.description),
                ctx_dsl::change_reason.eq(&row.change_reason),
            ))
            .schema_name(schema_name)
            .execute(conn)?;
        Ok(Outcome::Updated)
    }
}

impl<'a> ImportExecutor<'a> {
    fn new(
        conn: &'a mut DBConnection,
        workspace: &'a WorkspaceContext,
        user: &'a User,
        options: &'a ImportOptions,
        existing: &'a ExistingEntities,
        change_reason: ChangeReason,
    ) -> superposition::Result<Self> {
        Ok(Self {
            conn,
            workspace,
            options,
            existing,
            email: user.get_email(),
            description: Description::try_from(
                "Config imported via TOML/JSON import".to_string(),
            )
            .map_err(|e| unexpected_error!(e))?,
            change_reason,
        })
    }

    fn write_dimension(
        &mut self,
        name: &str,
        info: &DimensionInfo,
    ) -> superposition::Result<Outcome> {
        if should_skip(
            self.options.strategy,
            self.existing.dimensions.contains_key(name),
        ) {
            return Ok(Outcome::Skipped);
        }

        let description =
            Description::try_from(info.description.clone()).map_err(|e| {
                bad_argument!("Invalid description for dimension '{}': {}", name, e)
            })?;

        let row = Dimension {
            dimension: name.to_string(),
            schema: info.schema.clone(),
            position: Position::try_from(info.position).map_err(|e| {
                bad_argument!("Invalid position for dimension '{}': {}", name, e)
            })?,
            dimension_type: info.dimension_type.clone(),
            dependency_graph: info.dependency_graph.clone(),
            value_compute_function_name: info.value_compute_function_name.clone(),
            value_validation_function_name: None,
            created_at: Utc::now(),
            created_by: self.email.clone(),
            last_modified_at: Utc::now(),
            last_modified_by: self.email.clone(),
            description,
            change_reason: self.change_reason.clone(),
        };

        let outcome = crate::api::dimension::operations::persist_dimension(
            self.conn,
            &self.workspace.schema_name,
            row,
            self.existing.dimensions.get(name),
        );
        match outcome {
            Ok((true, _)) => Ok(Outcome::Created),
            Ok((false, _)) => Ok(Outcome::Updated),
            Err(e) => Err(e),
        }
    }

    fn write_default_config(
        &mut self,
        key: &str,
        info: &DefaultConfigInfo,
    ) -> superposition::Result<Outcome> {
        if should_skip(
            self.options.strategy,
            self.existing.default_configs.contains_key(key),
        ) {
            return Ok(Outcome::Skipped);
        }

        let description =
            Description::try_from(info.description.clone()).map_err(|e| {
                bad_argument!("Invalid description for default config '{}': {}", key, e)
            })?;

        let row = DefaultConfig {
            key: key.to_string(),
            value: info.value.clone(),
            schema: ExtendedMap::try_from(info.schema.clone()).map_err(|e| {
                bad_argument!("Invalid schema for default config '{}': {}", key, e)
            })?,
            value_validation_function_name: None,
            value_compute_function_name: None,
            created_at: Utc::now(),
            created_by: self.email.clone(),
            last_modified_at: Utc::now(),
            last_modified_by: self.email.clone(),
            description,
            change_reason: self.change_reason.clone(),
        };

        upsert_default_config(
            self.conn,
            &self.workspace.schema_name,
            row,
            self.existing.default_configs.get(key),
        )
    }

    fn write_context(
        &mut self,
        ctx: &ConfigContext,
        parsed: &DetailedConfig,
        effective_config: Option<&DetailedConfig>,
    ) -> superposition::Result<Outcome> {
        if should_skip(
            self.options.strategy,
            self.existing.contexts.contains_key(&ctx.id),
        ) {
            return Ok(Outcome::Skipped);
        }

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
            .map_err(|e| bad_argument!("Failed to compute context weight: {}", e))?;
        let row = DbContext {
            id: ctx.id.clone(),
            value: ctx.condition.clone(),
            override_id: override_key.clone(),
            override_,
            weight,
            created_at: Utc::now(),
            created_by: self.email.clone(),
            last_modified_at: Utc::now(),
            last_modified_by: self.email.clone(),
            description,
            change_reason: self.change_reason.clone(),
        };

        overwrite_context(
            self.conn,
            &self.workspace.schema_name,
            row,
            self.existing.contexts.contains_key(&ctx.id),
        )
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
            let context_ids = self
                .existing
                .contexts
                .keys()
                .filter(|id| !file_ctx_ids.contains(id.as_str()))
                .collect::<Vec<_>>();
            if !context_ids.is_empty() {
                summary.contexts.deleted += diesel::delete(
                    ctx_dsl::contexts.filter(ctx_dsl::id.eq_any(context_ids)),
                )
                .schema_name(schema_name)
                .execute(self.conn)?;
            }

            let default_config_keys = self
                .existing
                .default_configs
                .keys()
                .filter(|key| !parsed.default_configs.contains_key(*key))
                .collect::<Vec<_>>();
            if !default_config_keys.is_empty() {
                summary.default_configs.deleted += diesel::delete(
                    dc_dsl::default_configs
                        .filter(dc_dsl::key.eq_any(default_config_keys)),
                )
                .schema_name(schema_name)
                .execute(self.conn)?;
            }

            let dimension_names = self
                .existing
                .dimensions
                .keys()
                .filter(|name| !parsed.dimensions.contains_key(*name))
                .collect::<Vec<_>>();
            if !dimension_names.is_empty() {
                summary.dimensions.deleted += diesel::delete(
                    dim_dsl::dimensions
                        .filter(dim_dsl::dimension.eq_any(dimension_names)),
                )
                .schema_name(schema_name)
                .execute(self.conn)?;
            }
        }

        crate::api::dimension::operations::refresh_dependency_graphs(
            self.conn,
            &self.workspace.schema_name,
        )?;

        Ok(summary)
    }
}

#[allow(clippy::too_many_arguments)]
async fn import_config<F: ConfigFormat>(
    body: &str,
    opts: ImportOptions,
    tags: Option<Vec<String>>,
    user: &User,
    internal_user: &InternalUserContext,
    workspace_context: &WorkspaceContext,
    state: &Data<AppState>,
    conn: &mut DBConnection,
) -> superposition::Result<(ImportSummary, bool)> {
    let parsed = F::parse_into_detailed(body)
        .map_err(|e| bad_argument!("Failed to parse config: {}", e))?;
    let schema_name = &workspace_context.schema_name;
    let existing = ExistingEntities::load(conn, schema_name)?;
    let change_reason =
        ChangeReason::try_from("Imported via SuperTOML config import".to_string())
            .map_err(|error| unexpected_error!(error))?;

    validate_import(
        &parsed,
        &existing,
        opts,
        &change_reason,
        internal_user,
        workspace_context,
        state,
        conn,
    )
    .await?;

    let tx_result = conn.transaction::<_, ImportTransactionError, _>(|conn| {
        let mut executor = ImportExecutor::new(
            conn,
            workspace_context,
            user,
            &opts,
            &existing,
            change_reason,
        )?;
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
            let webhook_status =
                execute_webhook_call(data, workspace_context, state, conn).await;

            Ok((summary, webhook_status))
        }
        Err(ImportTransactionError::Preview(summary)) => Ok((*summary, true)),
        Err(ImportTransactionError::Failed(e)) => Err(e),
    }
}

/// Imports uploaded config bytes after parsing request options and tags.
#[allow(clippy::too_many_arguments)]
pub(super) async fn handle_import<F: ConfigFormat>(
    body: &[u8],
    req: &HttpRequest,
    custom_headers: CustomHeaders,
    user: &User,
    internal_user: &InternalUserContext,
    workspace_context: &WorkspaceContext,
    state: &Data<AppState>,
    conn: &mut DBConnection,
) -> superposition::Result<HttpResponse> {
    let body_str = std::str::from_utf8(body)
        .map_err(|_| bad_argument!("Request body is not valid UTF-8"))?;
    let opts = ImportOptions::from_request(req)?;
    let tags = parse_config_tags(custom_headers.config_tags)?;

    let (summary, webhook_status) = import_config::<F>(
        body_str,
        opts,
        tags,
        user,
        internal_user,
        workspace_context,
        state,
        conn,
    )
    .await?;

    let mut response = if webhook_status {
        HttpResponse::Ok()
    } else {
        HttpResponse::build(
            actix_web::http::StatusCode::from_u16(512)
                .unwrap_or(actix_web::http::StatusCode::INTERNAL_SERVER_ERROR),
        )
    };
    Ok(response.json(summary))
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
    fn create_only_skips_existing_entities_only() {
        assert!(should_skip(ImportStrategy::CreateOnly, true));
        assert!(!should_skip(ImportStrategy::CreateOnly, false));
        assert!(!should_skip(ImportStrategy::Upsert, true));
        assert!(!should_skip(ImportStrategy::Replace, true));
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

        assert_eq!(report.created, 2);
        assert_eq!(report.updated, 1);
        assert_eq!(report.skipped, 1);
        assert_eq!(report.deleted, 0);
    }
}
