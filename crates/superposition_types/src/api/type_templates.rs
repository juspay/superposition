use derive_more::{AsRef, Deref, DerefMut, Into};
use serde::{Deserialize, Serialize};

use crate::{
    database::models::{ChangeReason, Description},
    ExtendedMap, RegexEnum,
};

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct TypeTemplateCreateRequest {
    pub type_schema: ExtendedMap,
    pub type_name: TypeTemplateName,
    pub description: Description,
    pub change_reason: ChangeReason,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct TypeTemplateUpdateRequest {
    pub type_schema: ExtendedMap,
    pub description: Option<Description>,
    pub change_reason: ChangeReason,
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
        Self::validate_data(value)
    }
}
