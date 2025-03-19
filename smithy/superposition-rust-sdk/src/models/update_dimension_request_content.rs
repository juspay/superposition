/*
 * Superposition
 *
 * No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)
 *
 * The version of the OpenAPI document: 2025-03-05
 * 
 * Generated by: https://openapi-generator.tech
 */

use crate::models;
use serde::{Deserialize, Serialize};

#[derive(Clone, Default, Debug, PartialEq, Serialize, Deserialize)]
pub struct UpdateDimensionRequestContent {
    #[serde(rename = "schema", default, with = "::serde_with::rust::double_option", skip_serializing_if = "Option::is_none")]
    pub schema: Option<Option<serde_json::Value>>,
    #[serde(rename = "function_name", skip_serializing_if = "Option::is_none")]
    pub function_name: Option<String>,
    #[serde(rename = "description", skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(rename = "change_reason")]
    pub change_reason: String,
}

impl UpdateDimensionRequestContent {
    pub fn new(change_reason: String) -> UpdateDimensionRequestContent {
        UpdateDimensionRequestContent {
            schema: None,
            function_name: None,
            description: None,
            change_reason,
        }
    }
}

