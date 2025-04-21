use superposition_macros::bad_argument;
use superposition_types::{
    database::models::others::{Webhook, WebhookEvent},
    result as superposition,
};

use diesel::{
    r2d2::{ConnectionManager, PooledConnection},
    PgConnection,
};
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use superposition_types::database::schema::webhooks::{self, dsl};

pub fn validate_events(
    events: &[WebhookEvent],
    exclude_webhook: Option<&String>,
    schema_name: &String,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<()> {
    let result: Vec<Webhook> =
        dsl::webhooks.schema_name(schema_name).get_results(conn)?;
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
    schema_name: &String,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<Webhook> {
    Ok(dsl::webhooks
        .filter(webhooks::name.eq(w_name))
        .schema_name(schema_name)
        .get_result::<Webhook>(conn)?)
}
