#[cfg(feature = "diesel_derives")]
use diesel::AsChangeset;
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::Value;

use crate::database::models::{Metrics, WorkspaceStatus};
#[cfg(feature = "diesel_derives")]
use crate::database::superposition_schema::superposition::workspaces;

#[derive(Debug, Deserialize, Serialize)]
pub struct CreateWorkspaceRequest {
    pub workspace_admin_email: String,
    pub workspace_name: String,
    pub workspace_status: Option<WorkspaceStatus>,
    pub workspace_strict_mode: bool,
    pub metrics: Option<Metrics>,
}

#[derive(Debug, Deserialize, Serialize)]
#[cfg_attr(feature = "diesel_derives", derive(AsChangeset))]
#[cfg_attr(feature = "diesel_derives", diesel(table_name = workspaces))]
pub struct UpdateWorkspaceRequest {
    pub workspace_admin_email: String,
    pub workspace_status: Option<WorkspaceStatus>,
    pub mandatory_dimensions: Option<Vec<String>>,
    #[serde(default, deserialize_with = "deserialize_config_version")]
    pub config_version: Option<Option<i64>>,
    pub metrics: Option<Metrics>,
}

#[derive(Deserialize, Debug)]
pub struct WorkspaceListFilters {
    pub workspace_name: Option<String>,
}

pub fn deserialize_config_version<'de, D>(
    deserializer: D,
) -> Result<Option<Option<i64>>, D::Error>
where
    D: Deserializer<'de>,
{
    let opt: Result<Value, _> = Deserialize::deserialize(deserializer);
    match opt {
        Ok(Value::Number(config_version)) => {
            if let Some(config_version) = config_version.as_i64() {
                Ok(Some(Some(config_version)))
            } else {
                log::error!("Expected a bigint as the config version.");
                Err(serde::de::Error::custom(
                    "Expected a bigint as the config version.",
                ))
            }
        }
        Ok(Value::String(val)) => match val.parse::<i64>() {
            Ok(config_version) => Ok(Some(Some(config_version))),
            Err(_) => {
                log::error!("Expected a bigint or bigint string as the config version.");
                Err(serde::de::Error::custom(
                    "Expected a bigint or bigint string as the config version.",
                ))
            }
        },
        Ok(Value::Null) => Ok(Some(None)),
        Err(_) => Ok(None), // If the field is missing, return None instead of throwing an errors
        _ => {
            log::error!("Expected a bigint or null literal as the config version.");
            Err(serde::de::Error::custom(
                "Expected a bigint or null literal as the config version.",
            ))
        }
    }
}
