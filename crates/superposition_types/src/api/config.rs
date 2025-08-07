use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

use crate::database::models::{cac::ConfigVersion, Description};

#[derive(Deserialize)]
pub struct ContextPayload {
    pub context: Map<String, Value>,
}

#[derive(Deserialize, Serialize, Clone)]
pub struct ConfigVersionResponse {
    pub id: String,
    pub config: Value,
    pub config_hash: String,
    pub tags: Option<Vec<String>>,
    pub created_at: DateTime<Utc>,
    pub description: Description,
}

impl From<ConfigVersion> for ConfigVersionResponse {
    fn from(config_version: ConfigVersion) -> Self {
        Self {
            id: config_version.id.to_string(),
            config: config_version.config,
            config_hash: config_version.config_hash,
            tags: config_version.tags,
            created_at: config_version.created_at,
            description: config_version.description,
        }
    }
}
