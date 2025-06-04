use crate::database::models::{Metrics, Workspace, WorkspaceStatus};
#[cfg(feature = "diesel_derives")]
use crate::database::superposition_schema::superposition::workspaces;
use chrono::{DateTime, Utc};
#[cfg(feature = "diesel_derives")]
use diesel::{
    expression::AsExpression,
    pg::Pg,
    serialize::ToSql,
    sql_types::{BigInt, Nullable},
    AsChangeset,
};
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::Value;

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct WorkspaceResponse {
    pub organisation_id: String,
    pub organisation_name: String,
    pub workspace_name: String,
    pub workspace_schema_name: String,
    pub workspace_status: WorkspaceStatus,
    pub workspace_admin_email: String,
    pub config_version: Option<String>,
    pub created_by: String,
    pub last_modified_by: String,
    pub last_modified_at: DateTime<Utc>,
    pub created_at: DateTime<Utc>,
    pub mandatory_dimensions: Option<Vec<String>>,
    pub strict_mode: bool,
    pub metrics: Metrics,
}

impl From<Workspace> for WorkspaceResponse {
    fn from(workspace: Workspace) -> Self {
        Self {
            organisation_id: workspace.organisation_id,
            organisation_name: workspace.organisation_name,
            workspace_name: workspace.workspace_name,
            workspace_schema_name: workspace.workspace_schema_name,
            workspace_status: workspace.workspace_status,
            workspace_admin_email: workspace.workspace_admin_email,
            config_version: workspace.config_version.map(|v| v.to_string()),
            created_by: workspace.created_by,
            last_modified_by: workspace.last_modified_by,
            last_modified_at: workspace.last_modified_at,
            created_at: workspace.created_at,
            mandatory_dimensions: workspace.mandatory_dimensions,
            strict_mode: workspace.strict_mode,
            metrics: workspace.metrics,
        }
    }
}

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
    pub config_version: Option<ConfigVersionUpdate>,
    pub metrics: Option<Metrics>,
}

#[derive(Debug)]
#[cfg_attr(feature = "diesel_derives", derive(AsExpression))]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = Nullable<BigInt>))]
pub enum ConfigVersionUpdate {
    Add(i64),
    Remove,
}

pub fn deserialize_config_version<'de, D>(
    deserializer: D,
) -> Result<Option<ConfigVersionUpdate>, D::Error>
where
    D: Deserializer<'de>,
{
    let opt: Value = Deserialize::deserialize(deserializer)?;
    match opt {
        Value::Number(config_version) => {
            if let Some(config_version) = config_version.as_i64() {
                Ok(Some(ConfigVersionUpdate::Add(config_version)))
            } else {
                log::error!("Expected a bigint as the config version.");
                Err(serde::de::Error::custom(
                    "Expected a bigint as the config version.",
                ))
            }
        }
        Value::String(val) => {
            if &val == "null" {
                Ok(Some(ConfigVersionUpdate::Remove))
            } else {
                match val.parse::<i64>() {
                    Ok(config_version) => {
                        Ok(Some(ConfigVersionUpdate::Add(config_version)))
                    }
                    Err(_) => {
                        log::error!(
                            "Expected a bigint or bigint string as the config version."
                        );
                        Err(serde::de::Error::custom(
                            "Expected a bigint or bigint string as the config version.",
                        ))
                    }
                }
            }
        }
        Value::Null => Ok(Some(ConfigVersionUpdate::Remove)),
        _ => {
            log::error!(
                "Expected a bigint, bigint string or null literal as the config version."
            );
            Err(serde::de::Error::custom(
                "Expected a bigint, bigint string or null literal as the config version.",
            ))
        }
    }
}

impl<'de> Deserialize<'de> for ConfigVersionUpdate {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserialize_config_version(deserializer)?.ok_or_else(|| {
            serde::de::Error::custom("Config version is missing or invalid.")
        })
    }
}

impl Serialize for ConfigVersionUpdate {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Self::Add(name) => serializer.serialize_i64(*name),
            Self::Remove => serializer.serialize_str("null"),
        }
    }
}

#[cfg(feature = "diesel_derives")]
impl ToSql<Nullable<BigInt>, Pg> for ConfigVersionUpdate {
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, Pg>,
    ) -> diesel::serialize::Result {
        let num = match self {
            Self::Add(name) => Some(*name),
            Self::Remove => None,
        };
        <Option<i64> as ToSql<Nullable<BigInt>, Pg>>::to_sql(&num, &mut out.reborrow())
    }
}

#[derive(Deserialize, Debug)]
pub struct WorkspaceListFilters {
    pub workspace_name: Option<String>,
}
