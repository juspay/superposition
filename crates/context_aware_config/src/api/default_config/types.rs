use derive_more::{AsRef, Deref, DerefMut, Into};
use diesel::AsChangeset;
use serde::{Deserialize, Deserializer};
use serde_json::{Map, Value};
use superposition_types::{
    database::models::cac::deserialize_function_name, database::schema::default_configs,
    RegexEnum,
};

#[derive(Debug, Deserialize)]
pub struct CreateReq {
    pub key: DefaultConfigKey,
    pub value: Value,
    pub schema: Map<String, Value>,
    pub function_name: Option<String>,
    pub description: String,
    pub change_reason: String,
}

#[derive(Debug, Deserialize, AsChangeset)]
#[diesel(table_name = default_configs)]
pub struct UpdateReq {
    #[serde(default, deserialize_with = "deserialize_option")]
    pub value: Option<Value>,
    pub schema: Option<Value>,
    #[serde(default, deserialize_with = "deserialize_function_name")]
    pub function_name: Option<Option<String>>,
    pub description: Option<String>,
    pub change_reason: String,
}

#[derive(Debug, Deserialize, AsRef, Deref, DerefMut, Into)]
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

fn deserialize_option<'de, D>(deserializer: D) -> Result<Option<Value>, D::Error>
where
    D: Deserializer<'de>,
{
    let value: Value = Deserialize::deserialize(deserializer)?;
    Ok(Some(value))
}
