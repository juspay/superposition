use serde::Serialize;
use uuid::Uuid;
use serde_json::Value;
use chrono::{DateTime, Utc};

#[derive(Serialize)]
pub struct GlobalConfig {
    pub uuid: String,
    pub key: String,
    pub value: Value,
    pub last_modified: DateTime<Utc>,
    pub created_on: DateTime<Utc>
}

impl GlobalConfig {
    pub fn new(key: String, value: Value) -> GlobalConfig {
        GlobalConfig { 
            uuid: Uuid::new_v4().to_string(),
            key,
            value,
            last_modified: Utc::now(),
            created_on: Utc::now()
        }
    }

}
