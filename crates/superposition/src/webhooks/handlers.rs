use actix_web::{
    delete, get, post, put,
    web::{self, Json, Query},
    HttpResponse, Scope,
};
use chrono::Utc;
use service_utils::service::types::DbConnection;
use superposition_macros::{bad_argument, db_error, not_found, unexpected_error};
use superposition_types::{
    custom_query::PaginationParams, result as superposition, PaginatedResponse, User,
};

use super::types::{CreateWebhookRequest, WebhookName};
use diesel::{delete, ExpressionMethods, QueryDsl, RunQueryDsl};
use diesel::{
    r2d2::{ConnectionManager, PooledConnection},
    PgConnection,
};
use superposition_types::database::models::cac::Webhooks;
use superposition_types::database::schema::webhooks::{self, dsl};

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(create)
        .service(list_webhooks)
        .service(get)
        .service(update)
        .service(delete_webhook)
}

#[post("")]
async fn create(
    request: web::Json<CreateWebhookRequest>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<Json<Webhooks>> {
    let DbConnection(mut conn) = db_conn;
    let req = request.into_inner();
    let events: Vec<String> = req
        .events
        .into_iter()
        .map(|event| event.to_string())
        .collect();
    validate_events(&events, None, &mut conn)?;
    let now = Utc::now().naive_utc();
    let webhook = Webhooks {
        name: req.name,
        description: req.description,
        enabled: req.enabled,
        url: req.url,
        method: req.method,
        version: req.version.unwrap_or("v1".to_owned()),
        custom_headers: req.custom_headers,
        events,
        max_retries: 0,
        last_triggered_at: None,
        created_by: user.email.clone(),
        created_at: now,
        last_modified_by: user.email,
        last_modified_at: now,
    };

    let insert: Result<Webhooks, diesel::result::Error> =
        diesel::insert_into(dsl::webhooks)
            .values(&webhook)
            .get_result(&mut conn);

    match insert {
        Ok(res) => Ok(Json(res)),
        Err(e) => match e {
            diesel::result::Error::DatabaseError(kind, e) => {
                log::error!("Function error: {:?}", e);
                match kind {
                    diesel::result::DatabaseErrorKind::UniqueViolation => {
                        Err(bad_argument!("Webhook already exists."))
                    }
                    _ => Err(unexpected_error!(
                        "Something went wrong, failed to create the webhook"
                    )),
                }
            }
            _ => {
                log::error!("Webhook creation failed with error: {e}");
                Err(unexpected_error!(
                    "An error occured please contact the admin."
                ))
            }
        },
    }
}

#[get("")]
async fn list_webhooks(
    db_conn: DbConnection,
    filters: Query<PaginationParams>,
) -> superposition::Result<Json<PaginatedResponse<Webhooks>>> {
    let DbConnection(mut conn) = db_conn;

    let (total_pages, total_items, data) = match filters.all {
        Some(true) => {
            let result: Vec<Webhooks> = dsl::webhooks.get_results(&mut conn)?;
            (1, result.len() as i64, result)
        }
        _ => {
            let n_functions: i64 = dsl::webhooks.count().get_result(&mut conn)?;
            let limit = filters.count.unwrap_or(10);
            let mut builder = dsl::webhooks
                .into_boxed()
                .order(webhooks::last_modified_at.desc())
                .limit(limit);
            if let Some(page) = filters.page {
                let offset = (page - 1) * limit;
                builder = builder.offset(offset);
            }
            let result: Vec<Webhooks> = builder.load(&mut conn)?;
            let total_pages = (n_functions as f64 / limit as f64).ceil() as i64;
            (total_pages, n_functions, result)
        }
    };

    Ok(Json(PaginatedResponse {
        total_pages,
        total_items,
        data,
    }))
}

#[get("/{webhook_name}")]
async fn get(
    params: web::Path<WebhookName>,
    db_conn: DbConnection,
) -> superposition::Result<Json<Webhooks>> {
    let DbConnection(mut conn) = db_conn;
    let w_name: String = params.into_inner().into();
    let webhook = fetch_webhook(&w_name, &mut conn)?;
    Ok(Json(webhook))
}

pub fn validate_events(
    events: &Vec<String>,
    exclude_webhook: Option<&String>,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<()> {
    let result: Vec<Webhooks> = dsl::webhooks.get_results(conn)?;
    for webhook in result {
        if exclude_webhook.map_or(false, |val| &webhook.name == val) {
            continue;
        }
        if let Some(duplicate_event) =
            webhook.events.iter().find(|event| events.contains(event))
        {
            return Err(bad_argument!("Duplicate event found: {}", duplicate_event));
        }
    }
    Ok(())
}

pub fn fetch_webhook(
    w_name: &String,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<Webhooks> {
    Ok(dsl::webhooks
        .filter(webhooks::name.eq(w_name))
        .get_result::<Webhooks>(conn)?)
}

#[put("/{webhook_name}")]
async fn update(
    params: web::Path<WebhookName>,
    db_conn: DbConnection,
    user: User,
    request: web::Json<CreateWebhookRequest>,
) -> superposition::Result<Json<Webhooks>> {
    let DbConnection(mut conn) = db_conn;
    let req = request.into_inner();
    let w_name: String = params.into_inner().into();
    let events: Vec<String> = req
        .events
        .into_iter()
        .map(|event| event.to_string())
        .collect();

    validate_events(&events, Some(&w_name), &mut conn)?;

    let update = diesel::update(dsl::webhooks)
        .filter(webhooks::name.eq(w_name))
        .set((
            webhooks::description.eq(req.description),
            webhooks::enabled.eq(req.enabled),
            webhooks::url.eq(req.url),
            webhooks::method.eq(req.method),
            webhooks::version.eq(req.version.unwrap_or("v1".to_owned())),
            webhooks::custom_headers.eq(req.custom_headers),
            webhooks::events.eq(events),
            webhooks::last_modified_by.eq(user.email),
            webhooks::last_modified_at.eq(Utc::now().naive_utc()),
        ))
        .get_result::<Webhooks>(&mut conn)
        .map_err(|err| {
            log::error!("failed to insert custom type with error: {}", err);
            db_error!(err)
        })?;

    Ok(Json(update))
}

#[delete("/{webhook_name}")]
async fn delete_webhook(
    params: web::Path<WebhookName>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let w_name: String = params.into_inner().into();

    diesel::update(dsl::webhooks)
        .filter(webhooks::name.eq(&w_name))
        .set((
            webhooks::last_modified_at.eq(Utc::now().naive_utc()),
            webhooks::last_modified_by.eq(user.get_email()),
        ))
        .execute(&mut conn)?;
    let deleted_row =
        delete(dsl::webhooks.filter(webhooks::name.eq(&w_name))).execute(&mut conn);
    match deleted_row {
        Ok(0) => Err(not_found!("Webhook {} doesn't exists", w_name)),
        Ok(_) => {
            log::info!("{w_name} Webhook deleted by {}", user.get_email());
            Ok(HttpResponse::NoContent().finish())
        }
        Err(e) => {
            log::error!("Webhook delete query failed with error: {e}");
            Err(unexpected_error!(
                "Something went wrong, failed to delete the Webhook"
            ))
        }
    }
}
