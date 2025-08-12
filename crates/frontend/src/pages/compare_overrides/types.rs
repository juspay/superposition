use std::collections::HashMap;

use derive_more::{Deref, DerefMut};
use serde::{Deserialize, Deserializer};
use serde_json::{Map, Value};
use superposition_derives::{IsEmpty, QueryParam};
use superposition_types::{
    custom_query::{CustomQuery, DimensionQuery, QueryParam},
    IsEmpty,
};

#[derive(PartialEq, Clone, IsEmpty, QueryParam)]
pub(super) struct PageParams {
    pub(super) grouped: bool,
    pub(super) prefix: Option<String>,
}

impl Default for PageParams {
    fn default() -> Self {
        Self {
            grouped: true,
            prefix: None,
        }
    }
}

impl<'de> Deserialize<'de> for PageParams {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct PageParamsHelper {
            pub grouped: Option<bool>,
            pub prefix: Option<String>,
        }
        let helper = PageParamsHelper::deserialize(deserializer)?;
        Ok(Self {
            grouped: helper.prefix.is_some() || helper.grouped.unwrap_or(true),
            prefix: helper.prefix,
        })
    }
}

/// Provides struct to `Deserialize` `HashMap<String, String>` as `Vec<serde_json::Map<String, serde_json::Value>>` converting `String` values to `serde_json::Value`
/// This is useful for contexts where multiple contexts are provided in the query string, like `dimension[context1]=value1&dimension[context2]=value2`
#[derive(Deserialize, Deref, DerefMut, Clone, PartialEq, Default)]
#[serde(from = "HashMap<String,String>")]
pub(super) struct ContextListInner(HashMap<String, Map<String, Value>>);

impl IsEmpty for ContextListInner {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl From<HashMap<String, Map<String, Value>>> for ContextListInner {
    fn from(value: HashMap<String, Map<String, Value>>) -> Self {
        Self(value)
    }
}

impl From<HashMap<String, String>> for ContextListInner {
    fn from(value: HashMap<String, String>) -> Self {
        let value = HashMap::from_iter(value.into_iter().filter_map(|(_, value)| {
            value.parse().ok().and_then(|v: Value| {
                v.as_object()
                    .map(|m| (Value::Object(m.clone()).to_string(), m.clone()))
            })
        }));

        Self(value)
    }
}

#[derive(Deref, DerefMut, Clone, PartialEq)]
pub(super) struct ContextList(DimensionQuery<ContextListInner>);

impl ContextList {
    pub fn extract_non_empty(query_string: &str) -> Self {
        Self(DimensionQuery::<ContextListInner>::extract_non_empty(
            query_string,
        ))
    }
}

impl QueryParam for ContextList {
    fn to_query_param(&self) -> String {
        self.clone()
            .0
            .into_inner()
            .iter()
            .enumerate()
            .map(|(index, (_, context))| {
                format!(
                    "dimension[context{index}]={}",
                    Value::Object(context.clone())
                )
            })
            .collect::<Vec<_>>()
            .join("&")
    }
}

impl Default for ContextList {
    fn default() -> Self {
        Self(DimensionQuery(ContextListInner::default()))
    }
}

// this maps the column context and the row config key to a particular value
pub(super) type ComparisonTable = HashMap<String, Map<String, Value>>;
