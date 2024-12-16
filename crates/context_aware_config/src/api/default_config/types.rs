use derive_more::{AsRef, Deref, DerefMut, Into};
use serde::{Deserialize, Deserializer};
use serde_json::{Map, Value};
use superposition_types::RegexEnum;

#[derive(Debug, Deserialize)]
pub struct CreateReq {
    #[serde(default, deserialize_with = "deserialize_option")]
    pub value: Option<Value>,
    pub schema: Option<Map<String, Value>>,
    #[serde(default, deserialize_with = "deserialize_option")]
    pub function_name: Option<Value>,
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
        Ok(Self::validate_data(value)?)
    }
}

fn deserialize_option<'de, D>(deserializer: D) -> Result<Option<Value>, D::Error>
where
    D: Deserializer<'de>,
{
    let value: Value = Deserialize::deserialize(deserializer)?;
    Ok(Some(value))
}

#[derive(Deserialize, Debug)]
pub struct DefaultConfigFilters {
    pub name: Option<String>,
}
