use std::collections::HashSet;

use actix_web::{
    HttpResponse, Scope, post,
    web::{Data, Json},
};
use diesel::{
    Connection, ExpressionMethods, OptionalExtension, QueryDsl, RunQueryDsl,
    SelectableHelper,
};
use service_utils::{
    helpers::{WebhookData, execute_webhook_call, parse_config_tags},
    middlewares::auth_z::Action as AuthZAction,
    service::types::{
        AppHeader, AppState, CustomHeaders, DbConnection, WorkspaceContext,
    },
};
use superposition_derives::{authorized, declare_resource};
use superposition_macros::{bad_argument, db_error, not_found};
use superposition_types::{
    DBConnection, Resource, User,
    api::{
        copy_to::{
            CopyEntityType, CopyResultStatus, CopyToRequest, CopyToResponse,
            CopyToResult, RowSelectionMode,
        },
        dimension::{CreateRequest, DimensionName, UpdateRequest},
        webhook::Action,
    },
    database::{
        models::{Workspace, cac::Dimension, others::WebhookEvent},
        schema::dimensions,
        superposition_schema::superposition::workspaces,
    },
    result as superposition,
};

use crate::{
    api::dimension::{
        operations::{create_dimension_entry, update_dimension_entry},
        utils::get_dimensions_data,
    },
    helpers::{add_config_version, put_config_in_redis, validate_change_reason},
};

declare_resource!(Dimension);

struct CopyToAuthZActionCreate;

impl service_utils::middlewares::auth_z::Action for CopyToAuthZActionCreate {
    fn get() -> String {
        "create".to_string()
    }

    fn resource() -> Resource {
        Resource::Dimension
    }
}

struct CopyToAuthZActionUpdate;

impl service_utils::middlewares::auth_z::Action for CopyToAuthZActionUpdate {
    fn get() -> String {
        "update".to_string()
    }

    fn resource() -> Resource {
        Resource::Dimension
    }
}

pub fn endpoints() -> Scope {
    Scope::new("").service(copy_to_handler)
}

#[derive(Clone)]
enum CopyOperation {
    Create,
    Update,
}

impl CopyOperation {
    fn webhook_action(&self) -> Action {
        match self {
            Self::Create => Action::Create,
            Self::Update => Action::Update,
        }
    }
}

fn get_target_workspace_context(
    conn: &mut DBConnection,
    source_workspace_context: &WorkspaceContext,
    target_workspace_name: &str,
) -> superposition::Result<WorkspaceContext> {
    let workspace = workspaces::dsl::workspaces
        .filter(
            workspaces::dsl::organisation_id
                .eq(&source_workspace_context.organisation_id.0),
        )
        .filter(workspaces::dsl::workspace_name.eq(target_workspace_name))
        .select(Workspace::as_select())
        .get_result::<Workspace>(conn)
        .optional()
        .map_err(|err| {
            log::error!("failed to fetch target workspace with error: {err}");
            db_error!(err)
        })?
        .ok_or_else(|| not_found!("Target workspace not found"))?;

    Ok(WorkspaceContext {
        workspace_id: service_utils::service::types::WorkspaceId(
            workspace.workspace_name.clone(),
        ),
        organisation_id: source_workspace_context.organisation_id.clone(),
        schema_name: service_utils::service::types::SchemaName(
            workspace.workspace_schema_name.clone(),
        ),
        settings: workspace,
    })
}

fn dedupe_selected_rows(selected_rows: Vec<String>) -> Vec<String> {
    let mut seen = HashSet::new();
    selected_rows
        .into_iter()
        .filter(|row| seen.insert(row.clone()))
        .collect()
}

fn get_source_dimensions(
    conn: &mut DBConnection,
    workspace_context: &WorkspaceContext,
    selection_mode: &RowSelectionMode,
    selected_rows: &[String],
) -> superposition::Result<Vec<Dimension>> {
    let source_dimensions = match selection_mode {
        RowSelectionMode::All => {
            get_dimensions_data(conn, &workspace_context.schema_name)?
        }
        RowSelectionMode::Selected => {
            let mut query = dimensions::dsl::dimensions
                .schema_name(&workspace_context.schema_name)
                .order(dimensions::dsl::position.asc())
                .into_boxed();
            query = query.filter(dimensions::dsl::dimension.eq_any(selected_rows));
            query.load::<Dimension>(conn).map_err(|err| {
                log::error!("failed to fetch source dimensions with error: {err}");
                db_error!(err)
            })?
        }
    };

    if matches!(selection_mode, RowSelectionMode::Selected) {
        let found = source_dimensions
            .iter()
            .map(|dimension| dimension.dimension.clone())
            .collect::<HashSet<_>>();
        let missing = selected_rows
            .iter()
            .filter(|row| !found.contains(*row))
            .cloned()
            .collect::<Vec<_>>();

        if !missing.is_empty() {
            return Err(bad_argument!(
                "Selected rows not found in source workspace: {}",
                missing.join(", ")
            ));
        }
    }

    Ok(source_dimensions)
}

async fn authorize_copy<A: AuthZAction>(
    auth_z: &service_utils::middlewares::auth_z::AuthZ<A>,
    source_dimensions: &[Dimension],
    existing_target_dimensions: &HashSet<String>,
    skip_existing: bool,
) -> superposition::Result<()> {
    for dimension in source_dimensions {
        if existing_target_dimensions.contains(&dimension.dimension) {
            if skip_existing {
                continue;
            }

            auth_z
                .action_authorized(
                    &CopyToAuthZActionUpdate::get(),
                    &[&dimension.dimension],
                )
                .await?;
        } else {
            auth_z
                .action_authorized(
                    &CopyToAuthZActionCreate::get(),
                    &[&dimension.dimension],
                )
                .await?;
        }
    }

    Ok(())
}

#[authorized(action = "update")]
#[post("")]
async fn copy_to_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    req: Json<CopyToRequest>,
    user: User,
    db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let req = req.into_inner();

    if req.entity_type != CopyEntityType::Dimensions {
        return Err(bad_argument!("Unsupported entity_type"));
    }

    if req.target_workspace == workspace_context.workspace_id.0 {
        return Err(bad_argument!(
            "Source and target workspaces must be different"
        ));
    }

    let selected_rows = dedupe_selected_rows(req.selected_rows);
    if matches!(req.selection_mode, RowSelectionMode::Selected)
        && selected_rows.is_empty()
    {
        return Err(bad_argument!(
            "selected_rows must be provided when selection_mode is selected"
        ));
    }

    let source_dimensions = get_source_dimensions(
        &mut conn,
        &workspace_context,
        &req.selection_mode,
        &selected_rows,
    )?;
    let target_workspace_context = get_target_workspace_context(
        &mut conn,
        &workspace_context,
        &req.target_workspace,
    )?;

    validate_change_reason(
        &target_workspace_context,
        &req.change_reason,
        &mut conn,
        &state.master_encryption_key,
    )
    .await?;

    let target_dimension_names = dimensions::dsl::dimensions
        .select(dimensions::dsl::dimension)
        .schema_name(&target_workspace_context.schema_name)
        .load::<String>(&mut conn)
        .map_err(|err| {
            log::error!("failed to fetch target dimension names with error: {err}");
            db_error!(err)
        })?
        .into_iter()
        .collect::<HashSet<_>>();

    authorize_copy(
        &_auth_z,
        &source_dimensions,
        &target_dimension_names,
        req.skip_existing,
    )
    .await?;

    let tags = parse_config_tags(custom_headers.config_tags)?;
    let initial_selected_create_position = u32::try_from(target_dimension_names.len())
        .map_err(|_| {
            bad_argument!("Too many target dimensions to append selected rows")
        })?;
    let (results, actions, config_version) = conn
        .transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let mut results = Vec::new();
            let mut actions = Vec::new();
            let mut next_selected_create_position = initial_selected_create_position;

            for source_dimension in &source_dimensions {
                if target_dimension_names.contains(&source_dimension.dimension) {
                    if req.skip_existing {
                        results.push(CopyToResult {
                            row_identifier: source_dimension.dimension.clone(),
                            status: CopyResultStatus::Skipped,
                            message: Some(String::from(
                                "Dimension already exists in target workspace",
                            )),
                        });
                        continue;
                    }

                    update_dimension_entry(
                        transaction_conn,
                        &target_workspace_context,
                        &user,
                        source_dimension.dimension.clone(),
                        UpdateRequest {
                            position: match req.selection_mode {
                                RowSelectionMode::All => Some(source_dimension.position),
                                RowSelectionMode::Selected => None,
                            },
                            schema: Some(source_dimension.schema.clone()),
                            value_validation_function_name: Some(
                                source_dimension.value_validation_function_name.clone(),
                            ),
                            value_compute_function_name: Some(
                                source_dimension.value_compute_function_name.clone(),
                            ),
                            description: Some(source_dimension.description.clone()),
                            change_reason: req.change_reason.clone(),
                        },
                    )?;
                    actions.push(CopyOperation::Update);
                    results.push(CopyToResult {
                        row_identifier: source_dimension.dimension.clone(),
                        status: CopyResultStatus::Copied,
                        message: Some(String::from(
                            "Dimension updated in target workspace",
                        )),
                    });
                    continue;
                }

                let create_position = match req.selection_mode {
                    RowSelectionMode::All => source_dimension.position,
                    RowSelectionMode::Selected => {
                        let append_position = next_selected_create_position.into();
                        next_selected_create_position += 1;
                        append_position
                    }
                };

                create_dimension_entry(
                    transaction_conn,
                    &target_workspace_context,
                    &user,
                    CreateRequest {
                        dimension: DimensionName::try_from(
                            source_dimension.dimension.clone(),
                        )
                        .map_err(|err| bad_argument!(err))?,
                        position: create_position,
                        schema: source_dimension.schema.clone(),
                        value_validation_function_name: source_dimension
                            .value_validation_function_name
                            .clone(),
                        description: source_dimension.description.clone(),
                        change_reason: req.change_reason.clone(),
                        value_compute_function_name: source_dimension
                            .value_compute_function_name
                            .clone(),
                        dimension_type: source_dimension.dimension_type.clone(),
                    },
                )?;
                actions.push(CopyOperation::Create);
                results.push(CopyToResult {
                    row_identifier: source_dimension.dimension.clone(),
                    status: CopyResultStatus::Copied,
                    message: Some(String::from("Dimension created in target workspace")),
                });
            }

            let config_version = if actions.is_empty() {
                None
            } else {
                Some(add_config_version(
                    &state,
                    tags.clone(),
                    req.change_reason.clone().into(),
                    transaction_conn,
                    &target_workspace_context.schema_name,
                )?)
            };

            Ok((results, actions, config_version))
        })?;

    if let Some(ref config_version) = config_version {
        let _ = put_config_in_redis(
            config_version,
            &state,
            &target_workspace_context.schema_name,
            &mut conn,
        )
        .await;
    }

    let response = CopyToResponse {
        entity_type: req.entity_type,
        source_workspace: workspace_context.workspace_id.0.clone(),
        target_workspace: req.target_workspace,
        requested_count: source_dimensions.len(),
        copied_count: results
            .iter()
            .filter(|result| result.status == CopyResultStatus::Copied)
            .count(),
        skipped_count: results
            .iter()
            .filter(|result| result.status == CopyResultStatus::Skipped)
            .count(),
        failed_count: 0,
        results,
    };

    let webhook_status = if let Some(ref config_version) = config_version {
        let webhook_action = if actions.len() == 1 {
            actions[0].clone().webhook_action()
        } else {
            Action::Batch(
                actions
                    .iter()
                    .map(|action| action.clone().webhook_action())
                    .collect(),
            )
        };
        execute_webhook_call(
            WebhookData {
                payload: &response,
                resource: Resource::Dimension,
                action: webhook_action,
                event: WebhookEvent::ConfigChanged,
                config_version_opt: Some(config_version.id.to_string()),
            },
            &target_workspace_context,
            &state,
            &mut conn,
        )
        .await
    } else {
        true
    };

    let mut http_resp = if webhook_status {
        HttpResponse::Ok()
    } else {
        HttpResponse::build(
            actix_web::http::StatusCode::from_u16(512)
                .unwrap_or(actix_web::http::StatusCode::INTERNAL_SERVER_ERROR),
        )
    };

    if let Some(config_version) = config_version {
        http_resp.insert_header((
            AppHeader::XConfigVersion.to_string(),
            config_version.id.to_string(),
        ));
    }

    Ok(http_resp.json(response))
}
