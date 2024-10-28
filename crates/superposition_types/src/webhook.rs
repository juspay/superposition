use serde::{Deserialize, Deserializer, Serialize};
use std::fmt::{self};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum HeadersEnum {
    ConfigVersion,
}

impl fmt::Display for HeadersEnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ConfigVersion => write!(f, "x-config-version"),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum HttpMethod {
    Get,
    Post,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Authorization {
    pub key: String,
    pub value: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Webhook {
    pub url: String,
    pub method: HttpMethod,
    pub headers: Vec<HeadersEnum>,
    pub authorization: Authorization,
}

#[derive(Serialize, Deserialize)]
pub enum WebhookEvent {
    ExperimentCreated,
    ExperimentStarted,
    ExperimentInprogress,
    ExperimentUpdated,
    ExperimentConcluded,
}

#[derive(Serialize, Deserialize)]
pub struct WebhookResponse<T> {
    pub event: WebhookEvent,
    pub payload: T,
}

#[derive(Clone, Serialize)]
pub enum WebhookConfig {
    Disbled,
    Enabled(Webhook),
}

impl<'de> Deserialize<'de> for WebhookConfig {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct WebhookConfigHelper {
            enabled: bool,
            configuration: Option<Webhook>,
        }

        let helper = WebhookConfigHelper::deserialize(deserializer)?;
        match (helper.enabled, helper.configuration) {
            (true, None) => Err(serde::de::Error::custom(
                "Configuration must be provided when enabled is true",
            )),
            (true, Some(webhook)) => Ok(Self::Enabled(webhook)),
            (false, _) => Ok(Self::Disbled),
        }
    }
}
