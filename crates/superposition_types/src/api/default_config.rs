use derive_more::{AsRef, Deref, DerefMut, Into};
#[cfg(feature = "diesel_derives")]
use diesel::AsChangeset;
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::Value;
use superposition_derives::{IsEmpty, QueryParam};

#[cfg(feature = "diesel_derives")]
use crate::database::schema::default_configs;
use crate::{custom_query::QueryParam, ExtendedMap};
use crate::{
    database::models::{cac::deserialize_function_name, ChangeReason, Description},
    IsEmpty, RegexEnum,
};

#[derive(
    Debug, Clone, PartialEq, Serialize, Deserialize, Default, QueryParam, IsEmpty,
)]
pub struct DefaultConfigFilters {
    #[query_param(skip_if_empty)]
    pub name: Option<String>,
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

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum DefaultConfigAction {
    Create(DefaultConfigCreateRequest),
    Update {
        key: DefaultConfigKey,
        request: DefaultConfigUpdateRequest,
    },
    Delete(DefaultConfigKey),
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum DefaultConfigBulkResponse {
    Create(crate::database::models::cac::DefaultConfig),
    Update(crate::database::models::cac::DefaultConfig),
    Delete(String),
}

#[derive(Debug, Deserialize, Serialize)]
pub struct BulkOperation {
    pub operations: Vec<DefaultConfigAction>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct BulkOperationResponse {
    pub output: Vec<DefaultConfigBulkResponse>,
}

fn deserialize_option<'de, D>(deserializer: D) -> Result<Option<Value>, D::Error>
where
    D: Deserializer<'de>,
{
    let value: Value = Deserialize::deserialize(deserializer)?;
    Ok(Some(value))
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    #[test]
    fn bulk_operation_deserializes_all_actions() {
        let request: BulkOperation = serde_json::from_value(json!({
            "operations": [
                {
                    "CREATE": {
                        "key": "timeout",
                        "value": 30,
                        "schema": { "type": "number" },
                        "value_validation_function_name": null,
                        "description": "Request timeout",
                        "change_reason": "Add timeout",
                        "value_compute_function_name": null
                    }
                },
                {
                    "UPDATE": {
                        "key": "timeout",
                        "request": { "change_reason": "Change timeout" }
                    }
                },
                { "DELETE": "timeout" }
            ]
        }))
        .expect("all default-config bulk actions should deserialize");

        assert!(matches!(
            request.operations.as_slice(),
            [
                DefaultConfigAction::Create(_),
                DefaultConfigAction::Update { .. },
                DefaultConfigAction::Delete(_)
            ]
        ));
    }
}
