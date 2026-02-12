use std::fmt;

use derive_more::{AsRef, Deref, DerefMut, Into};
#[cfg(feature = "diesel_derives")]
use diesel::AsChangeset;
use serde::{Deserialize, Serialize};

#[cfg(feature = "diesel_derives")]
use crate::database::schema::webhooks;
use crate::{
    database::models::{
        others::{CustomHeaders, HttpMethod, PayloadVersion, WebhookEvent},
        ChangeReason, Description, NonEmptyString,
    },
    RegexEnum, Resource,
};

#[derive(Debug, Serialize, Deserialize)]
pub struct CreateWebhookRequest {
    pub name: WebhookName,
    pub description: Description,
    pub enabled: bool,
    pub url: NonEmptyString,
    pub method: HttpMethod,
    pub payload_version: Option<PayloadVersion>,
    pub custom_headers: Option<CustomHeaders>,
    pub events: Vec<WebhookEvent>,
    pub change_reason: ChangeReason,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[cfg_attr(feature = "diesel_derives", derive(AsChangeset))]
#[cfg_attr(feature = "diesel_derives", diesel(table_name = webhooks))]
pub struct UpdateWebhookRequest {
    pub description: Option<Description>,
    pub enabled: Option<bool>,
    pub url: Option<NonEmptyString>,
    pub method: Option<HttpMethod>,
    pub payload_version: Option<PayloadVersion>,
    pub custom_headers: Option<CustomHeaders>,
    pub events: Option<Vec<WebhookEvent>>,
    pub change_reason: ChangeReason,
}

#[derive(
    Debug, Serialize, Deserialize, AsRef, Deref, DerefMut, Into, Clone, PartialEq,
)]
#[serde(try_from = "String")]
pub struct WebhookName(String);
impl WebhookName {
    pub fn validate_data(name: String) -> Result<Self, String> {
        let name = name.trim();
        RegexEnum::FunctionName
            .match_regex(name)
            .map(|_| Self(name.to_string()))
    }
}

impl TryFrom<String> for WebhookName {
    type Error = String;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::validate_data(value)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum HeadersEnum {
    ConfigVersion,
    WorkspaceId,
}

impl fmt::Display for HeadersEnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ConfigVersion => write!(f, "x-config-version"),
            Self::WorkspaceId => write!(f, "x-tenant"),
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub enum Action {
    Create,
    Update,
    Delete,
    #[serde(untagged)]
    Batch(Vec<Action>),
}

#[derive(Serialize, Deserialize, Debug)]
pub struct WebhookEventInfo {
    pub webhook_event: WebhookEvent,
    pub resource: Resource,
    pub action: Action,
    pub time: String,
    pub workspace_id: String,
    pub organisation_id: String,
    pub config_version: Option<String>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct WebhookResponse<T> {
    pub event_info: WebhookEventInfo,
    pub payload: T,
}
