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
pub struct MoveContextResponseContent {
    #[serde(rename = "context_id")]
    pub context_id: String,
    #[serde(rename = "override_id")]
    pub override_id: String,
    #[serde(rename = "weight")]
    pub weight: f64,
    #[serde(rename = "description")]
    pub description: String,
    #[serde(rename = "change_reason")]
    pub change_reason: String,
}

impl MoveContextResponseContent {
    pub fn new(context_id: String, override_id: String, weight: f64, description: String, change_reason: String) -> MoveContextResponseContent {
        MoveContextResponseContent {
            context_id,
            override_id,
            weight,
            description,
            change_reason,
        }
    }
}

