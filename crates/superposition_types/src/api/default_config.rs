use derive_more::{AsRef, Deref, DerefMut, Into};
#[cfg(feature = "diesel_derives")]
use diesel::AsChangeset;
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::Value;
use superposition_derives::{IsEmpty, QueryParam};

#[cfg(feature = "diesel_derives")]
use crate::database::schema::default_configs;
use crate::{
    custom_query::{CommaSeparatedStringQParams, QueryParam},
    database::models::{
        cac::{deserialize_function_name, DefaultConfig},
        ChangeReason, Description,
    },
    ExtendedMap, IsEmpty, RegexEnum, SortBy,
};

#[derive(
    Deserialize,
    PartialEq,
    Clone,
    Copy,
    strum_macros::EnumIter,
    strum_macros::Display,
    Default,
)]
#[serde(rename_all = "snake_case")]
#[strum(serialize_all = "snake_case")]
pub enum SortOn {
    #[default]
    Key,
    CreatedAt,
    LastModifiedAt,
}

#[derive(Clone, PartialEq, Default, QueryParam, IsEmpty, Deserialize)]
pub struct DefaultConfigFilters {
    #[query_param(skip_if_empty, iterable)]
    pub name: Option<CommaSeparatedStringQParams>,
    pub grouped: Option<bool>,
    #[query_param(skip_if_empty)]
    pub prefix: Option<String>,
    pub sort_by: Option<SortBy>,
    pub sort_on: Option<SortOn>,
    #[query_param(skip_if_empty)]
    pub search: Option<String>,
}

impl DefaultConfigFilters {
    pub fn init_with_grouping(&mut self) {
        if self.search.is_none() && self.name.is_none() {
            self.grouped = Some(true);
        }
    }

    pub fn set_search(&mut self, search: Option<String>) {
        self.search = search;
        self.name = None;
    }

    pub fn set_name(&mut self, name: Option<CommaSeparatedStringQParams>) {
        self.name = name;
        self.search = None;
        if self.prefix.is_none() {
            self.grouped = None;
        }
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Deserialize, Serialize, Clone)]
pub enum ListDefaultConfigResponse {
    Group(String),
    Config(DefaultConfig),
}

#[derive(Debug, Deserialize, Serialize)]
pub struct DefaultConfigCreateRequest {
    pub key: DefaultConfigKey,
    pub value: Value,
    pub schema: ExtendedMap,
    #[serde(alias = "function_name")]
    pub value_validation_function_name: Option<String>,
    pub description: Description,
    pub change_reason: ChangeReason,
    #[serde(alias = "autocomplete_function_name")]
    pub value_compute_function_name: Option<String>,
}

#[derive(Debug, Deserialize, Serialize, AsRef, Deref, DerefMut, Into)]
#[serde(try_from = "String")]
pub struct DefaultConfigKey(String);
impl DefaultConfigKey {
    pub fn validate_data(name: String) -> Result<Self, String> {
        let name = name.trim();
        RegexEnum::DefaultConfigKey
            .match_regex(name)
            .map(|_| Self(name.to_string()))
    }
}

impl TryFrom<String> for DefaultConfigKey {
    type Error = String;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::validate_data(value)
    }
}

#[derive(Debug, Deserialize, Serialize, Clone)]
#[cfg_attr(feature = "diesel_derives", derive(AsChangeset))]
#[cfg_attr(feature = "diesel_derives", diesel(table_name = default_configs))]
pub struct DefaultConfigUpdateRequest {
    #[serde(default, deserialize_with = "deserialize_option")]
    pub value: Option<Value>,
    pub schema: Option<ExtendedMap>,
    #[serde(
        alias = "function_name",
        default,
        deserialize_with = "deserialize_function_name"
    )]
    pub value_validation_function_name: Option<Option<String>>,
    #[serde(
        alias = "autocomplete_function_name",
        default,
        deserialize_with = "deserialize_function_name"
    )]
    pub value_compute_function_name: Option<Option<String>>,
    pub description: Option<Description>,
    pub change_reason: ChangeReason,
}

fn deserialize_option<'de, D>(deserializer: D) -> Result<Option<Value>, D::Error>
where
    D: Deserializer<'de>,
{
    let value: Value = Deserialize::deserialize(deserializer)?;
    Ok(Some(value))
}
