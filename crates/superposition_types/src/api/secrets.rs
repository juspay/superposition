use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use strum_macros;
use superposition_derives::{IsEmpty, QueryParam};

use crate::custom_query::{CommaSeparatedStringQParams, QueryParam};
use crate::database::models::{
    others::{Secret, SecretName},
    ChangeReason, Description,
};
use crate::{IsEmpty, SortBy};

#[derive(
    Debug,
    Clone,
    Copy,
    Serialize,
    Deserialize,
    Default,
    PartialEq,
    strum_macros::EnumIter,
    strum_macros::Display,
)]
#[serde(rename_all = "snake_case")]
#[strum(serialize_all = "snake_case")]
pub enum SortOn {
    Name,
    CreatedAt,
    #[default]
    LastModifiedAt,
}

#[derive(Clone, Deserialize, PartialEq, IsEmpty, QueryParam, Default)]
pub struct SecretFilters {
    #[query_param(skip_if_empty, iterable)]
    pub name: Option<CommaSeparatedStringQParams>,
    #[query_param(skip_if_empty, iterable)]
    pub created_by: Option<CommaSeparatedStringQParams>,
    #[query_param(skip_if_empty, iterable)]
    pub last_modified_by: Option<CommaSeparatedStringQParams>,
    pub sort_on: Option<SortOn>,
    pub sort_by: Option<SortBy>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateSecretRequest {
    pub name: SecretName,
    pub value: String,
    pub description: Description,
    pub change_reason: ChangeReason,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct UpdateSecretRequest {
    pub value: Option<String>,
    pub description: Option<Description>,
    pub change_reason: ChangeReason,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecretResponse {
    pub name: SecretName,
    pub description: Description,
    pub change_reason: ChangeReason,
    pub created_at: DateTime<Utc>,
    pub last_modified_at: DateTime<Utc>,
    pub created_by: String,
    pub last_modified_by: String,
}

impl From<Secret> for SecretResponse {
    fn from(secret: Secret) -> Self {
        Self {
            name: secret.name,
            description: secret.description,
            change_reason: secret.change_reason,
            created_at: secret.created_at,
            last_modified_at: secret.last_modified_at,
            created_by: secret.created_by,
            last_modified_by: secret.last_modified_by,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MasterEncryptionKeyRotationResponse {
    pub workspaces_rotated: i64,
    pub total_secrets_re_encrypted: i64,
}
