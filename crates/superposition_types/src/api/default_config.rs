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

#[derive(Clone, PartialEq, Default, QueryParam, IsEmpty)]
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
    pub fn toggle_grouped(&mut self) {
        let new_value = match self.grouped {
            Some(true) => None,
            _ => Some(true),
        };
        self.grouped = new_value;
        self.search = None;
        self.prefix = None;
        self.name = None;
    }

    pub fn init_with_grouping(&mut self) {
        if (self.search.is_none()
            && !(self.grouped.unwrap_or_default() || self.name.is_some()))
            || self.prefix.is_some()
        {
            self.grouped = Some(true);
        }
    }
}

impl<'de> Deserialize<'de> for DefaultConfigFilters {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct Helper {
            name: Option<CommaSeparatedStringQParams>,
            grouped: Option<bool>,
            prefix: Option<String>,
            sort_by: Option<SortBy>,
            sort_on: Option<SortOn>,
            search: Option<String>,
        }
        let helper = Helper::deserialize(deserializer)?;

        let search = match helper.name {
            Some(_) => None,
            None => helper.search,
        };

        let prefix = match search {
            None if helper.prefix.is_some() => helper.prefix,
            _ => None,
        };

        let grouped = match search {
            None if prefix.is_some() || helper.grouped.unwrap_or_default() => Some(true),
            _ => None,
        };

        Ok(Self {
            name: helper.name,
            search,
            grouped,
            prefix,
            sort_by: helper.sort_by,
            sort_on: helper.sort_on,
        })
    }
}

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
