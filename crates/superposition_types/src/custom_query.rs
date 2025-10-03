use std::{collections::HashMap, fmt::Display, str::FromStr};

use derive_more::{Deref, DerefMut};
use regex::Regex;
use serde::{
    de::{self, DeserializeOwned},
    Deserialize, Deserializer, Serialize,
};
use serde_json::{Map, Value};
#[cfg(feature = "experimentation")]
use strum::IntoEnumIterator;
use superposition_derives::{IsEmpty, QueryParam};

#[cfg(feature = "experimentation")]
use crate::database::models::experimentation::ExperimentStatusType;
use crate::IsEmpty;

pub trait QueryParam {
    fn to_query_param(&self) -> String;
}

pub trait CustomQuery: Sized {
    type Inner: DeserializeOwned;

    fn regex_pattern() -> &'static str;
    fn capture_group() -> &'static str;

    fn query_regex() -> Regex {
        Regex::new(Self::regex_pattern()).unwrap()
    }

    fn extract_query(query_string: &str) -> Result<Self, String> {
        let query_map =
            serde_urlencoded::from_str::<HashMap<String, String>>(query_string).map_err(
                |e| format!("Failed to parse query string: {query_string}. Error: {e}"),
            )?;
        let filtered_query = Self::filter_and_transform_query(query_map);
        let inner =
            serde_urlencoded::from_str::<Self::Inner>(&filtered_query).map_err(|e| {
                format!("Failed to parse query string: {query_string}. Error: {e}")
            })?;
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

    fn extract_non_empty(query_string: &str) -> Self
    where
        Self::Inner: Default + IsEmpty,
    {
        let res = Self::extract_query(query_string)
            .ok()
            .map(|value| value.into_inner())
            .filter(|value| !value.is_empty())
            .unwrap_or_default();

        Self::new(res)
    }

    fn new(inner: Self::Inner) -> Self;
    fn into_inner(self) -> Self::Inner;
}

/// Provides struct to extract those query params from the request which are `wrapped` in `dimension[param_name]`
#[derive(Debug, Clone, PartialEq, Deref, DerefMut)]
pub struct DimensionQuery<T: DeserializeOwned>(pub T);

impl<T> CustomQuery for DimensionQuery<T>
where
    T: DeserializeOwned,
{
    type Inner = T;

    fn regex_pattern() -> &'static str {
        r"dimension\[(?<query_name>.*)\]"
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

#[cfg(feature = "server")]
impl<T> actix_web::FromRequest for DimensionQuery<T>
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
        ready(
            Self::extract_query(req.query_string())
                .map_err(actix_web::error::ErrorBadRequest),
        )
    }
}

/// Provides struct to extract those query params from the request which are `not wrapped` in contrusts like `platform[param_name]`
#[derive(Debug, Clone)]
pub struct Query<T: DeserializeOwned>(pub T);

impl<T> CustomQuery for Query<T>
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

#[cfg(feature = "server")]
impl<T> actix_web::FromRequest for Query<T>
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
        ready(
            Self::extract_query(req.query_string())
                .map_err(actix_web::error::ErrorBadRequest),
        )
    }
}

/// Provides struct to `Deserialize` `HashMap<String, String>` as `serde_json::Map<String, serde_json::Value>`
#[derive(Deserialize, Deref, DerefMut, Clone, PartialEq, Default)]
#[cfg_attr(test, derive(Debug))]
#[serde(from = "HashMap<String,String>")]
pub struct QueryMap(Map<String, Value>);

impl IsEmpty for QueryMap {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl From<Map<String, Value>> for QueryMap {
    fn from(value: Map<String, Value>) -> Self {
        Self(value)
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

impl QueryParam for DimensionQuery<QueryMap> {
    fn to_query_param(&self) -> String {
        let parts = self
            .clone()
            .into_inner()
            .iter()
            .map(|(key, value)| format!("dimension[{key}]={value}"))
            .collect::<Vec<_>>();

        parts.join("&")
    }
}

impl From<Map<String, Value>> for DimensionQuery<QueryMap> {
    fn from(value: Map<String, Value>) -> Self {
        Self(QueryMap::from(value))
    }
}

impl Default for DimensionQuery<QueryMap> {
    fn default() -> Self {
        Self::from(Map::new())
    }
}

#[derive(Debug, Clone, PartialEq, IsEmpty, QueryParam)]
pub struct PaginationParams {
    pub count: Option<i64>,
    pub page: Option<i64>,
    pub all: Option<bool>,
}

impl PaginationParams {
    pub fn all_entries() -> Self {
        Self {
            count: None,
            page: None,
            all: Some(true),
        }
    }

    pub fn reset_page(&mut self) {
        self.page = if let Some(true) = self.all {
            None
        } else {
            Some(1)
        };
    }
}

impl Default for PaginationParams {
    fn default() -> Self {
        Self {
            count: Some(10),
            page: Some(1),
            all: None,
        }
    }
}

impl<'de> Deserialize<'de> for PaginationParams {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct Helper {
            count: Option<i64>,
            page: Option<i64>,
            all: Option<bool>,
        }

        let helper = Helper::deserialize(deserializer)?;

        if helper.all == Some(true) && (helper.count.is_some() || helper.page.is_some()) {
            return Err(de::Error::custom("When 'all' is true, 'count' and 'page' parameters should not be provided"));
        }

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
            all: helper.all,
        })
    }
}

#[derive(Debug, Clone, Deref, PartialEq, Default)]
#[deref(forward)]
pub struct CommaSeparatedQParams<T: Display + FromStr>(pub Vec<T>);

impl<T: Display + FromStr> Display for CommaSeparatedQParams<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str_arr = self.0.iter().map(|s| s.to_string()).collect::<Vec<_>>();
        write!(f, "{}", str_arr.join(","))
    }
}

impl<'de, T: Display + FromStr> Deserialize<'de> for CommaSeparatedQParams<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let items = String::deserialize(deserializer)?
            .split(',')
            .map(|item| item.trim().to_string())
            .map(|s| T::from_str(&s))
            .collect::<Result<Vec<_>, _>>()
            .map_err(|_| {
                serde::de::Error::custom(String::from("Error in converting type"))
            })?;
        Ok(Self(items))
    }
}

impl<T: Display + FromStr> Serialize for CommaSeparatedQParams<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

pub type CommaSeparatedStringQParams = CommaSeparatedQParams<String>;

#[cfg(feature = "experimentation")]
impl Default for CommaSeparatedQParams<ExperimentStatusType> {
    fn default() -> Self {
        Self(ExperimentStatusType::iter().collect())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use serde_json::{from_value, json};

    use crate::custom_query::QueryMap;

    #[test]
    fn querymap_from_hashmap() {
        let hashmap: HashMap<String, String> = from_value(json!({
            "key1": "123",
            "key2": "\"123\"",
            "key3": "test",
            "key4": "true",
            "key5": "\"true\"",
            "key6": "null",
            "key7": "\"null\""
        }))
        .unwrap();

        let map = json!({
            "key1": 123,
            "key2": "123",
            "key3": "test",
            "key4": true,
            "key5": "true",
            "key6": null,
            "key7": "null"
        })
        .as_object()
        .unwrap()
        .clone();

        assert_eq!(QueryMap::from(hashmap), QueryMap(map));
    }
}
