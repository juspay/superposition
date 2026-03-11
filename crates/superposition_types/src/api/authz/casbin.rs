use serde::{Deserialize, Serialize};

use crate::{api::authz::ResourceActionType, database::models::NonEmptyString, Resource};

#[derive(Deserialize, Serialize)]
pub struct PolicyRequest {
    pub sub: NonEmptyString,
    pub obj: Resource,
    pub act: ResourceActionType,
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
