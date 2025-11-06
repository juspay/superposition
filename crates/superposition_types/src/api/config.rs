use std::str::FromStr;

#[cfg(feature = "server")]
use actix_web::http::header::{
    Header, HeaderName, HeaderValue, InvalidHeaderValue, TryIntoHeaderValue,
};
use serde::Deserialize;
use serde_json::{Map, Value};
use superposition_derives::{IsEmpty, QueryParam};

use crate::{
    custom_query::{CommaSeparatedStringQParams, QueryParam},
    IsEmpty,
};

#[derive(Deserialize)]
pub struct ContextPayload {
    pub context: Map<String, Value>,
}

#[derive(Deserialize, IsEmpty, QueryParam, Default)]
pub struct ConfigQuery {
    #[query_param(skip_if_empty)]
    pub version: Option<String>,
    #[query_param(skip_if_empty, iterable)]
    pub prefix: Option<CommaSeparatedStringQParams>,
}

#[derive(Deserialize, IsEmpty, QueryParam, Default)]
pub struct ResolveConfigQuery {
    #[query_param(skip_if_empty)]
    pub version: Option<String>,
    pub show_reasoning: Option<bool>,
    pub resolve_remote: Option<bool>,
    #[query_param(skip_if_empty)]
    pub context_id: Option<String>,
    #[query_param(skip_if_empty, iterable)]
    pub prefix: Option<CommaSeparatedStringQParams>,
}

#[derive(
    strum_macros::EnumString, Clone, strum_macros::Display, Default, uniffi::Enum,
)]
#[strum(serialize_all = "snake_case")]
pub enum MergeStrategy {
    #[default]
    MERGE,
    REPLACE,
}

impl From<String> for MergeStrategy {
    fn from(value: String) -> Self {
        Self::from_str(&value).unwrap_or_default()
    }
}

#[cfg(feature = "server")]
impl TryIntoHeaderValue for MergeStrategy {
    type Error = InvalidHeaderValue;

    fn try_into_value(self) -> Result<HeaderValue, Self::Error> {
        HeaderValue::from_str(&self.to_string())
    }
}

#[cfg(feature = "server")]
impl Header for MergeStrategy {
    fn name() -> HeaderName {
        HeaderName::from_static("x-merge-strategy")
    }

    fn parse<M>(msg: &M) -> Result<Self, actix_web::error::ParseError>
    where
        M: actix_web::HttpMessage,
    {
        let header_value = msg
            .headers()
            .get(Self::name())
            .and_then(|hv| hv.to_str().ok())
            .and_then(|header_str| Self::from_str(header_str).ok())
            .unwrap_or_default();

        Ok(header_value)
    }
}
