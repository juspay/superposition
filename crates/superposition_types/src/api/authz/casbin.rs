use serde::{Deserialize, Serialize};

use crate::{database::models::NonEmptyString, Resource};

#[derive(Deserialize, Serialize)]
pub struct PolicyRequest {
    pub sub: NonEmptyString,
    pub obj: Resource,
    pub act: NonEmptyString,
    pub attr: Option<NonEmptyString>,
}

#[derive(Deserialize, Serialize)]
pub struct GroupingPolicyRequest {
    pub user: NonEmptyString,
    pub role: NonEmptyString,
}

#[derive(Deserialize, Serialize)]
pub struct ActionGroupPolicyRequest {
    pub resource: Resource,
    pub action: NonEmptyString,
    pub action_group: NonEmptyString,
}

#[derive(Deserialize, Serialize)]
pub struct ActionResponse {
    pub success: bool,
    pub message: String,
}
