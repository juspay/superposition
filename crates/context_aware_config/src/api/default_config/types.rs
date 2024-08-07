use derive_more::{AsRef, Deref, DerefMut, Into};
use regex::Regex;
use serde::{Deserialize, Deserializer};
use serde_json::{Map, Value};

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
        let regex_str: &str = "^[a-zA-Z0-9-_]([a-zA-Z0-9-_.]{0,254}[a-zA-Z0-9-_])?$";

        let regex = Regex::new(regex_str).map_err(|err| {
            log::error!("error while validating defaultConfigKey : {err}");
            "Something went wrong".to_string()
        })?;

        if !regex.is_match(&name) {
            return Err(format!(
                "The key name {} is invalid, it should obey the regex {}. \
                It can contain the following characters only [a-zA-Z0-9-_.] \
                and it should not start or end with a '.' character.",
                name, regex_str
            ));
        } else {
            Ok(Self(name))
        }
    }
}

impl TryFrom<String> for DefaultConfigKey {
    type Error = String;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Ok(DefaultConfigKey::validate_data(value)?)
    }
}

fn deserialize_option<'de, D>(deserializer: D) -> Result<Option<Value>, D::Error>
where
    D: Deserializer<'de>,
{
    let value: Value = Deserialize::deserialize(deserializer)?;
    Ok(Some(value))
}
