use derive_more::{AsRef, Deref, DerefMut, Into};
use serde::Deserialize;
use serde_json::Value;
use superposition_types::{webhook::WebhookEvent, RegexEnum};

#[derive(Debug, Deserialize)]
pub struct CreateWebhookRequest {
    pub name: String,
    pub description: String,
    pub enabled: bool,
    pub url: String,
    pub method: String,
    pub version: Option<String>,
    pub custom_headers: Option<Value>,
    pub events: Vec<WebhookEvent>,
}

#[derive(Debug, Deserialize, AsRef, Deref, DerefMut, Into)]
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
        Ok(Self::validate_data(value)?)
    }
}
