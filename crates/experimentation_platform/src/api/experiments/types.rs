use chrono::{DateTime, Utc};
use diesel::prelude::AsChangeset;
use serde::Serialize;
use superposition_types::database::schema::experiments;

#[derive(Serialize, AsChangeset)]
#[diesel(table_name = experiments)]
pub struct StartedByChangeSet {
    pub started_by: Option<String>,
    pub started_at: Option<DateTime<Utc>>,
}
