use serde::{Deserialize, Deserializer, Serialize};
use std::{
    collections::HashMap,
    fmt::{self},
};

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
    pub custom_headers: Option<HashMap<String, String>>,
    pub service_headers: Option<Vec<HeadersEnum>>,
    pub authorization: Option<Authorization>,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
pub enum WebhookEvent {
    ExperimentCreated,
    ExperimentStarted,
    ExperimentInprogress,
    ExperimentUpdated,
    ExperimentConcluded,
    ExperimentDiscarded,
}

impl fmt::Display for WebhookEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WebhookEvent::ExperimentCreated => write!(f, "ExperimentCreated"),
            WebhookEvent::ExperimentStarted => write!(f, "ExperimentStarted"),
            WebhookEvent::ExperimentInprogress => write!(f, "ExperimentInprogress"),
            WebhookEvent::ExperimentUpdated => write!(f, "ExperimentUpdated"),
            WebhookEvent::ExperimentConcluded => write!(f, "ExperimentConcluded"),
            WebhookEvent::ExperimentDiscarded => write!(f, "ExperimentDiscarded"),
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct WebhookEventInfo {
    pub webhook_event: WebhookEvent,
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

#[derive(Clone, Serialize)]
pub enum WebhookConfig {
    Disabled,
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
            (false, _) => Ok(Self::Disabled),
        }
    }
}
