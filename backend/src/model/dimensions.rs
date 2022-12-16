use serde::Serialize;
use uuid::Uuid;
use chrono::{DateTime, Utc};

#[derive(Serialize)]
pub struct Dimension {
    pub uuid: String,
    pub dimension: String,
    pub priority: i32,
    pub last_modified: DateTime<Utc>,
    pub created_on: DateTime<Utc>
}

impl Dimension {
    pub fn new(dimension: String, priority:  i32) -> Dimension {
        Dimension { 
            uuid: Uuid::new_v4().to_string(),
            dimension,
            priority,
            last_modified: Utc::now(),
            created_on: Utc::now()
        }
    }
}
