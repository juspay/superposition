use derive_more::{AsRef, Deref, DerefMut, Into};
#[cfg(feature = "diesel_derives")]
use diesel::AsChangeset;
use serde::{Deserialize, Serialize};
use serde_json::Value;

#[cfg(feature = "diesel_derives")]
use crate::database::schema::webhooks;
use crate::{
    database::models::{
        others::{HttpMethod, PayloadVersion, WebhookEvent},
        ChangeReason, Description, NonEmptyString,
    },
    RegexEnum,
};

#[derive(Debug, Serialize, Deserialize)]
pub struct CreateWebhookRequest {
    pub name: WebhookName,
    pub description: Description,
    pub enabled: bool,
    pub url: NonEmptyString,
    pub method: HttpMethod,
    pub payload_version: Option<PayloadVersion>,
    pub custom_headers: Option<Value>,
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
    pub custom_headers: Option<Value>,
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
