use diesel::Insertable;
use serde::Serialize;

use crate::db::schema::contexts;

#[derive(Debug, Insertable, Serialize)]
#[diesel(table_name = contexts)]
pub struct NewContext {
    pub key: String,
    pub value: String,
}
