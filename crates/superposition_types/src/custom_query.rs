use core::fmt;
use std::collections::HashMap;

use derive_more::{Deref, DerefMut};
use regex::Regex;
use serde::{
    de::{self, DeserializeOwned, IntoDeserializer},
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
        let query_map =
            actix_web::web::Query::<HashMap<String, String>>::from_query(query_string)?;
        let filtered_query = Self::filter_and_transform_query(query_map.into_inner());
        let inner = actix_web::web::Query::<Self::Inner>::from_query(&filtered_query)?
            .into_inner();
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
        ready(Self::extract_query(req.query_string()).map_err(actix_web::Error::from))
    }
}

/// Provides struct to `Deserialize` `HashMap<String, String>` as `serde_json::Map<String, serde_json::Value>`
#[derive(Deserialize, Deref, DerefMut)]
#[cfg_attr(test, derive(Debug, PartialEq))]
#[serde(from = "HashMap<String,String>")]
pub struct QueryMap(Map<String, Value>);

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
pub struct PaginationParams {
    pub count: Option<i64>,
    pub page: Option<i64>,
    pub all: Option<bool>,
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

#[derive(Debug, Deserialize, Clone, Deref)]
#[deref(forward)]
pub struct CommaSeparatedStringQParams(
    #[serde(deserialize_with = "deserialize_stringified_list")] pub Vec<String>,
);

pub fn deserialize_stringified_list<'de, D, I>(
    deserializer: D,
) -> std::result::Result<Vec<I>, D::Error>
where
    D: de::Deserializer<'de>,
    I: de::DeserializeOwned,
{
    struct StringVecVisitor<I>(std::marker::PhantomData<I>);

    impl<'de, I> de::Visitor<'de> for StringVecVisitor<I>
    where
        I: de::DeserializeOwned,
    {
        type Value = Vec<I>;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str(
                "a string containing comma separated values eg: CREATED,INPROGRESS",
            )
        }

        fn visit_str<E>(self, v: &str) -> std::result::Result<Self::Value, E>
        where
            E: de::Error,
        {
            let mut query_vector = Vec::new();
            for param in v.split(',') {
                let p: I = I::deserialize(param.into_deserializer())?;
                query_vector.push(p);
            }
            Ok(query_vector)
        }
    }

    deserializer.deserialize_any(StringVecVisitor(std::marker::PhantomData::<I>))
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
