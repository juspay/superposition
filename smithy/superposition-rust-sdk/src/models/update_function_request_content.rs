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
pub struct UpdateFunctionRequestContent {
    #[serde(rename = "description", skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(rename = "change_reason")]
    pub change_reason: String,
    #[serde(rename = "function")]
    pub function: String,
    #[serde(rename = "runtime_version")]
    pub runtime_version: String,
}

impl UpdateFunctionRequestContent {
    pub fn new(change_reason: String, function: String, runtime_version: String) -> UpdateFunctionRequestContent {
        UpdateFunctionRequestContent {
            description: None,
            change_reason,
            function,
            runtime_version,
        }
    }
}

