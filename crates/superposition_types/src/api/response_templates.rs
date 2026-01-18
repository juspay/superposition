use derive_more::{AsRef, Deref, DerefMut, Into};
#[cfg(feature = "diesel_derives")]
use diesel::AsChangeset;
use serde::{Deserialize, Serialize};

#[cfg(feature = "diesel_derives")]
use crate::database::schema::response_templates;
use crate::{
    database::models::{ChangeReason, Description}, ExtendedMap, RegexEnum
};

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct ResponseTemplateCreateRequest {
    pub name: ResponseTemplateName,
    pub description: Description,
    pub change_reason: ChangeReason,
    pub context: ExtendedMap,
    pub content_type: String,
    pub template: String,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
#[cfg_attr(feature = "diesel_derives", derive(AsChangeset))]
#[cfg_attr(feature = "diesel_derives", diesel(table_name = response_templates))]
pub struct ResponseTemplateUpdateRequest {
    pub change_reason: ChangeReason,
    pub content_type: Option<String>,
    pub template: Option<String>,
}

#[derive(Debug, Deserialize, Serialize, AsRef, Deref, DerefMut, Into, Clone)]
#[serde(try_from = "String")]
pub struct ResponseTemplateName(String);

impl ResponseTemplateName {
    pub fn validate_data(name: String) -> Result<Self, String> {
        let name = name.trim();
        RegexEnum::ResponseTemplateName
            .match_regex(name)
            .map(|_| Self(name.to_string()))
    }
}

impl TryFrom<String> for ResponseTemplateName {
    type Error = String;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::validate_data(value)
    }
}
