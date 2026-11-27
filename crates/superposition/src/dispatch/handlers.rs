use actix_web::{
    HttpResponse, Scope, post,
    web::{Data, Json},
};
use serde::Deserialize;
use service_utils::{
    service::types::{AppState, DbConnection, WorkspaceContext},
    webhook_sender::send_webhook,
};
use superposition_derives::{authorized, declare_resource};
use superposition_types::result as superposition;

use crate::webhooks::helper::fetch_webhook;

declare_resource!(Webhook);

#[derive(Deserialize)]
struct DispatchWebhookRequest {
    webhook_name: String,
    data: serde_json::Value,
}

pub fn endpoints() -> Scope {
    Scope::new("").service(dispatch_handler)
}

#[authorized]
#[post("/webhook")]
async fn dispatch_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    db_conn: DbConnection,
    body: Json<DispatchWebhookRequest>,
) -> superposition::Result<HttpResponse> {
    let DispatchWebhookRequest { webhook_name, data } = body.into_inner();

    let DbConnection(mut conn) = db_conn;
    let webhook =
        fetch_webhook(&webhook_name, &workspace_context.schema_name, &mut conn)?;

    if !webhook.enabled {
        return Ok(HttpResponse::Ok().finish());
    }

    send_webhook(&webhook, &data, &workspace_context, &state, &mut conn).await?;
    Ok(HttpResponse::Ok().finish())
}
