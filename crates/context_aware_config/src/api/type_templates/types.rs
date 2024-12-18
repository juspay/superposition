use derive_more::{AsRef, Deref, DerefMut, Into};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use superposition_types::RegexEnum;

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct TypeTemplateCreateRequest {
    pub type_schema: Value,
    pub type_name: TypeTemplateName,
    pub description: String,
    pub change_reason: String,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct TypeTemplateResponse {
    pub type_schema: Value,
    pub type_name: TypeTemplateName,
    pub created_at: String,
    pub last_modified: String,
    pub created_by: String,
    pub description: String,
    pub change_reason: String,
}

#[derive(Debug, Deserialize, Serialize, AsRef, Deref, DerefMut, Into, Clone)]
#[serde(try_from = "String")]
pub struct TypeTemplateName(String);
impl TypeTemplateName {
    pub fn validate_data(name: String) -> Result<Self, String> {
        let name = name.trim();
        RegexEnum::TypeTemplateName
            .match_regex(name)
            .map(|_| Self(name.to_string()))
    }
}

impl TryFrom<String> for TypeTemplateName {
    type Error = String;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Ok(Self::validate_data(value)?)
    }
}
