use crate::db::schema::dimensions;
use diesel::Insertable;
use serde::Serialize;

#[derive(Insertable, Serialize, Clone)]
#[diesel(table_name=dimensions)]
pub struct NewDimension {
    pub dimension: String,
    pub priority: i32,
}
