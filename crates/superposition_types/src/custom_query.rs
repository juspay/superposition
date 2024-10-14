use std::collections::HashMap;

use actix_web::web::Query;
use regex::Regex;
use serde::{
    de::{self, DeserializeOwned},
    Deserialize, Deserializer,
};
use serde_json::{Map, Value};

pub trait CustomQuery: Sized {
    type Inner: DeserializeOwned;

    fn regex_pattern() -> &'static str;
    fn capture_group() -> &'static str;

    fn query_regex() -> Regex {
        Regex::new(Self::regex_pattern()).unwrap()
    }

    fn extract_query(
        query_string: &str,
    ) -> Result<Self, actix_web::error::QueryPayloadError> {
        let query_map = Query::<HashMap<String, String>>::from_query(query_string)?;
        let filtered_query = Self::filter_and_transform_query(query_map.into_inner());
        let inner = Query::<Self::Inner>::from_query(&filtered_query)?.into_inner();
        Ok(Self::new(inner))
    }

    fn filter_and_transform_query(query_map: HashMap<String, String>) -> String {
        let regex = Self::query_regex();
        query_map
            .into_iter()
            .filter_map(|(k, v)| {
                Self::extract_key(&regex, &k).map(|pk| format!("{pk}={v}"))
            })
            .collect::<Vec<String>>()
            .join("&")
    }

    fn extract_key(regex: &Regex, key: &str) -> Option<String> {
        regex
            .captures(key)
            .and_then(|captures| captures.name(Self::capture_group()))
            .map(|m| m.as_str().to_owned())
    }

    fn new(inner: Self::Inner) -> Self;
    fn into_inner(self) -> Self::Inner;
}

/// Provides struct to extract those query params from the request which are `wrapped` in `platform[param_name]`
#[derive(Debug, Clone)]
pub struct PlatformQuery<T: DeserializeOwned>(pub T);

impl<T> CustomQuery for PlatformQuery<T>
where
    T: DeserializeOwned,
{
    type Inner = T;

    fn regex_pattern() -> &'static str {
        r"platform\[(?<query_name>.*)\]"
    }

    fn capture_group() -> &'static str {
        "query_name"
    }

    fn into_inner(self) -> T {
        self.0
    }

    fn new(inner: Self::Inner) -> Self {
        Self(inner)
    }
}

impl<T> actix_web::FromRequest for PlatformQuery<T>
where
    T: DeserializeOwned,
{
    type Error = actix_web::Error;
    type Future = std::future::Ready<Result<Self, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        use std::future::ready;
        ready(Self::extract_query(req.query_string()).map_err(actix_web::Error::from))
    }
}

/// Provides struct to extract those query params from the request which are `not wrapped` in contrusts like `platform[param_name]`
#[derive(Debug, Clone)]
pub struct DynamicQuery<T: DeserializeOwned>(pub T);

impl<T> CustomQuery for DynamicQuery<T>
where
    T: DeserializeOwned,
{
    type Inner = T;

    fn regex_pattern() -> &'static str {
        r"^(?<query_name>[^\[\]]+)$"
    }

    fn capture_group() -> &'static str {
        "query_name"
    }

    fn into_inner(self) -> T {
        self.0
    }

    fn new(inner: Self::Inner) -> Self {
        Self(inner)
    }
}

impl<T> actix_web::FromRequest for DynamicQuery<T>
where
    T: DeserializeOwned,
{
    type Error = actix_web::Error;
    type Future = std::future::Ready<Result<Self, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        use std::future::ready;
        ready(Self::extract_query(req.query_string()).map_err(actix_web::Error::from))
    }
}

/// Provides struct to `Deserialize` `HashMap<String, String>` as `serde_json::Map<String, serde_json::Value>`
#[derive(Deserialize)]
#[serde(from = "HashMap<String,String>")]
pub struct QueryMap(Map<String, Value>);

impl QueryMap {
    pub fn into_inner(self) -> Map<String, Value> {
        self.0
    }
}

impl From<HashMap<String, String>> for QueryMap {
    fn from(value: HashMap<String, String>) -> Self {
        let value = value
            .into_iter()
            .map(|(key, value)| (key, value.parse().unwrap_or(Value::String(value))))
            .collect::<Map<_, _>>();

        Self(value)
    }
}

#[derive(Debug, Clone)]
pub struct QueryFilters {
    pub count: Option<i64>,
    pub page: Option<i64>,
}

impl<'de> Deserialize<'de> for QueryFilters {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct Helper {
            count: Option<i64>,
            page: Option<i64>,
        }

        let helper = Helper::deserialize(deserializer)?;

        if let Some(count) = helper.count {
            if count <= 0 {
                return Err(de::Error::custom("Count should be greater than 0."));
            }
        }

        if let Some(page) = helper.page {
            if page <= 0 {
                return Err(de::Error::custom("Page should be greater than 0."));
            }
        }

        Ok(Self {
            count: helper.count,
            page: helper.page,
        })
    }
}
