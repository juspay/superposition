use super::helper::{fetch_webhook, validate_events};
use actix_web::{
    HttpResponse, Scope, delete, get, patch, post,
    web::{self, Json, Query},
};
use chrono::Utc;
use context_aware_config::helpers::validate_change_reason;
use diesel::{ExpressionMethods, PgArrayExpressionMethods, QueryDsl, RunQueryDsl};
use service_utils::service::types::{DbConnection, WorkspaceContext};
use superposition_derives::authorized;
use superposition_types::{
    PaginatedResponse, User,
    api::webhook::{CreateWebhookRequest, UpdateWebhookRequest, WebhookName},
    custom_query::PaginationParams,
    database::{
        models::others::{Webhook, WebhookEvent},
        schema::webhooks::{self, dsl::*},
    },
    result as superposition,
};
pub fn endpoints() -> Scope {
    Scope::new("")
        .service(create_handler)
        .service(list_handler)
        .service(get_handler)
        .service(update_handler)
        .service(delete_handler)
        .service(get_by_event_handler)
}

#[authorized]
#[post("")]
async fn create_handler(
    workspace_context: WorkspaceContext,
    request: Json<CreateWebhookRequest>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<Json<Webhook>> {
    let DbConnection(mut conn) = db_conn;
    let req = request.into_inner();

    validate_change_reason(&workspace_context, &req.change_reason, &mut conn)?;

    validate_events(&req.events, None, &workspace_context.schema_name, &mut conn)?;
    let now = Utc::now();
    let webhook_data = Webhook {
        name: req.name.to_string(),
        description: req.description,
        enabled: req.enabled,
        url: req.url,
        method: req.method,
        payload_version: req.payload_version.unwrap_or_default(),
        custom_headers: req.custom_headers.unwrap_or_default(),
        events: req.events,
        max_retries: 0,
        last_triggered_at: None,
        change_reason: req.change_reason,
        created_by: user.email.clone(),
        created_at: now,
        last_modified_by: user.email,
        last_modified_at: now,
    };

    diesel::insert_into(webhooks::table)
        .values(&webhook_data)
        .schema_name(&workspace_context.schema_name)
        .execute(&mut conn)?;

    Ok(Json(webhook_data))
}

#[authorized]
#[patch("/{webhook_name}")]
async fn update_handler(
    workspace_context: WorkspaceContext,
    params: web::Path<WebhookName>,
    db_conn: DbConnection,
    user: User,
    request: Json<UpdateWebhookRequest>,
) -> superposition::Result<Json<Webhook>> {
    let DbConnection(mut conn) = db_conn;
    let req = request.into_inner();
    let w_name: String = params.into_inner().into();

    validate_change_reason(&workspace_context, &req.change_reason, &mut conn)?;

    if let Some(webhook_events) = &req.events {
        validate_events(
            webhook_events,
            Some(&w_name),
            &workspace_context.schema_name,
            &mut conn,
        )?;
    }

    let update = diesel::update(webhooks::table)
        .filter(webhooks::name.eq(w_name))
        .set((
            req,
            last_modified_at.eq(Utc::now()),
            last_modified_by.eq(user.get_email()),
        ))
        .schema_name(&workspace_context.schema_name)
        .get_result::<Webhook>(&mut conn)?;

    Ok(Json(update))
}

#[authorized]
#[get("/{webhook_name}")]
async fn get_handler(
    workspace_context: WorkspaceContext,
    params: web::Path<WebhookName>,
    db_conn: DbConnection,
) -> superposition::Result<Json<Webhook>> {
    let DbConnection(mut conn) = db_conn;
    let webhook_row = fetch_webhook(
        &params.into_inner(),
        &workspace_context.schema_name,
        &mut conn,
    )?;
    Ok(Json(webhook_row))
}

#[authorized]
#[get("")]
async fn list_handler(
    workspace_context: WorkspaceContext,
    db_conn: DbConnection,
    pagination: Query<PaginationParams>,
) -> superposition::Result<Json<PaginatedResponse<Webhook>>> {
    let DbConnection(mut conn) = db_conn;

    if let Some(true) = pagination.all {
        let result: Vec<Webhook> = webhooks
            .schema_name(&workspace_context.schema_name)
            .get_results(&mut conn)?;
        return Ok(Json(PaginatedResponse::all(result)));
    }

    let total_items: i64 = webhooks
        .count()
        .schema_name(&workspace_context.schema_name)
        .get_result(&mut conn)?;
    let limit = pagination.count.unwrap_or(10);
    let mut builder = webhooks
        .schema_name(&workspace_context.schema_name)
        .into_boxed()
        .order(webhooks::last_modified_at.desc())
        .limit(limit);
    if let Some(page) = pagination.page {
        let offset = (page - 1) * limit;
        builder = builder.offset(offset);
    }
    let data: Vec<Webhook> = builder.load(&mut conn)?;
    let total_pages = (total_items as f64 / limit as f64).ceil() as i64;

    Ok(Json(PaginatedResponse {
        total_pages,
        total_items,
        data,
    }))
}

#[authorized]
#[delete("/{webhook_name}")]
async fn delete_handler(
    workspace_context: WorkspaceContext,
    params: web::Path<WebhookName>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let w_name: String = params.into_inner().into();

    diesel::update(webhooks::table)
        .filter(webhooks::name.eq(&w_name))
        .set((
            webhooks::last_modified_at.eq(Utc::now()),
            webhooks::last_modified_by.eq(user.get_email()),
        ))
        .schema_name(&workspace_context.schema_name)
        .execute(&mut conn)?;
    diesel::delete(webhooks.filter(webhooks::name.eq(&w_name)))
        .schema_name(&workspace_context.schema_name)
        .execute(&mut conn)?;
    Ok(HttpResponse::NoContent().finish())
}

#[authorized]
#[get("/event/{event}")]
async fn get_by_event_handler(
    workspace_context: WorkspaceContext,
    params: web::Path<WebhookEvent>,
    db_conn: DbConnection,
) -> superposition::Result<Json<Webhook>> {
    let DbConnection(mut conn) = db_conn;
    let event = params.into_inner();
    let webhook_row = webhooks
        .filter(webhooks::events.contains(vec![event]))
        .schema_name(&workspace_context.schema_name)
        .first::<Webhook>(&mut conn)?;
    Ok(Json(webhook_row))
}
