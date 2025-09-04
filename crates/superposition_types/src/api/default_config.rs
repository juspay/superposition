use derive_more::{AsRef, Deref, DerefMut, Into};
#[cfg(feature = "diesel_derives")]
use diesel::AsChangeset;
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::{Map, Value};
use superposition_derives::{IsEmpty, QueryParam};

use crate::custom_query::QueryParam;
#[cfg(feature = "diesel_derives")]
use crate::database::schema::default_configs;
use crate::{
    database::models::{cac::deserialize_function_name, ChangeReason, Description},
    IsEmpty, RegexEnum,
};

#[derive(
    Debug, Clone, PartialEq, Serialize, Deserialize, Default, QueryParam, IsEmpty,
)]
pub struct DefaultConfigFilters {
    pub name: Option<String>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct DefaultConfigCreateRequest {
    pub key: DefaultConfigKey,
    pub value: Value,
    pub schema: Map<String, Value>,
    pub function_name: Option<String>,
    pub description: Description,
    pub change_reason: ChangeReason,
    pub autocomplete_function_name: Option<String>,
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
    pub schema: Option<Value>,
    #[serde(default, deserialize_with = "deserialize_function_name")]
    pub function_name: Option<Option<String>>,
    #[serde(default, deserialize_with = "deserialize_function_name")]
    pub autocomplete_function_name: Option<Option<String>>,
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
