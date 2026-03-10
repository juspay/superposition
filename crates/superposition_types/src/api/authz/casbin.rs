use serde::{Deserialize, Serialize};

use crate::{api::authz::ResourceActionType, Resource};

#[derive(Deserialize, Serialize)]
pub struct PolicyRequest {
    pub sub: String,
    pub obj: Resource,
    pub act: ResourceActionType,
    pub attr: Option<String>,
}

#[derive(Deserialize, Serialize)]
pub struct GroupingPolicyRequest {
    pub user: String,
    pub role: String,
}

#[derive(Deserialize, Serialize)]
pub struct ActionGroupPolicyRequest {
    pub resource: Resource,
    pub action: String,
    pub action_group: String,
}
