use diesel::Insertable;
use serde::Serialize;

use crate::db::schema::ctxoverrides;

#[derive(Debug, Insertable, Serialize)]
#[diesel(table_name = ctxoverrides)]
pub struct CtxOverrideInsertion {
    pub key : String,
    pub context_id : String,
    pub override_id : String
}