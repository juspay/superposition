use diesel::Insertable;
use serde::Serialize;
use serde_json::Value;

use crate::db::schema::newcontexts;

#[derive(Debug, Insertable, Serialize)]
#[diesel(table_name = newcontexts)]
pub struct NewContextInsertion {
    pub key: String,
    pub value: Value,
    pub column1: Option<String>,
    pub column2: Option<String>,
    pub column3: Option<String>,
    pub column4: Option<String>,
}
