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
pub struct ConcludeExperimentRequestContent {
    #[serde(rename = "chosen_variant")]
    pub chosen_variant: String,
    #[serde(rename = "description", skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(rename = "change_reason")]
    pub change_reason: String,
}

impl ConcludeExperimentRequestContent {
    pub fn new(chosen_variant: String, change_reason: String) -> ConcludeExperimentRequestContent {
        ConcludeExperimentRequestContent {
            chosen_variant,
            description: None,
            change_reason,
        }
    }
}

