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

/// A string wrapper that redacts its value in Debug and Display output.
/// Used for sensitive fields like signing secrets to prevent accidental
/// exposure in logs, error messages, or debug output.
#[derive(Clone, Serialize, Deserialize, Default, PartialEq)]
#[serde(transparent)]
pub struct RedactedString(String);

impl RedactedString {
    pub fn expose(&self) -> &str {
        &self.0
    }

    pub fn into_inner(self) -> String {
        self.0
    }
}

impl From<String> for RedactedString {
    fn from(s: String) -> Self {
        Self(s)
    }
}

impl From<RedactedString> for String {
    fn from(s: RedactedString) -> Self {
        s.0
    }
}

impl fmt::Debug for RedactedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[REDACTED]")
    }
}

impl fmt::Display for RedactedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[REDACTED]")
    }
}

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
    pub signing_secret: Option<RedactedString>,
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
    #[cfg_attr(feature = "diesel_derives", diesel(serialize_as = String))]
    pub signing_secret: Option<RedactedString>,
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
    Signature256,
    Timestamp,
}

impl fmt::Display for HeadersEnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ConfigVersion => write!(f, "x-config-version"),
            Self::WorkspaceId => write!(f, "x-tenant"),
            Self::Signature256 => write!(f, "x-superposition-signature-256"),
            Self::Timestamp => write!(f, "x-superposition-timestamp"),
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
