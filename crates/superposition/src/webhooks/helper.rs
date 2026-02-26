use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use service_utils::{db::PgSchemaConnectionPool, run_query};
use superposition_macros::bad_argument;
use superposition_types::{
    database::{
        models::others::{Webhook, WebhookEvent},
        schema::webhooks::{self, dsl},
    },
    result as superposition,
};

pub fn validate_events(
    events: &[WebhookEvent],
    exclude_webhook: Option<&String>,
    schema_name: &String,
    db_pool: &PgSchemaConnectionPool,
) -> superposition::Result<()> {
    let result: Vec<Webhook> = run_query!(
        db_pool,
        conn,
        dsl::webhooks
            .schema_name(schema_name)
            .get_results(&mut conn)
    )?;
    for webhook in result {
        if exclude_webhook == Some(&webhook.name) {
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
    schema_name: &String,
    db_pool: &PgSchemaConnectionPool,
) -> superposition::Result<Webhook> {
    let webhook = run_query!(
        db_pool,
        conn,
        dsl::webhooks
            .filter(webhooks::name.eq(w_name))
            .schema_name(schema_name)
            .get_result::<Webhook>(&mut conn)
    )?;

    Ok(webhook)
}
