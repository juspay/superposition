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
pub struct TypeTemplatesResponse {
    #[serde(rename = "type_name")]
    pub type_name: String,
    #[serde(rename = "type_schema", deserialize_with = "Option::deserialize")]
    pub type_schema: Option<serde_json::Value>,
    #[serde(rename = "description")]
    pub description: String,
    #[serde(rename = "change_reason")]
    pub change_reason: String,
    #[serde(rename = "created_by")]
    pub created_by: String,
    #[serde(rename = "created_at")]
    pub created_at: f64,
    #[serde(rename = "last_modified_at")]
    pub last_modified_at: f64,
    #[serde(rename = "last_modified_by")]
    pub last_modified_by: String,
}

impl TypeTemplatesResponse {
    pub fn new(type_name: String, type_schema: Option<serde_json::Value>, description: String, change_reason: String, created_by: String, created_at: f64, last_modified_at: f64, last_modified_by: String) -> TypeTemplatesResponse {
        TypeTemplatesResponse {
            type_name,
            type_schema,
            description,
            change_reason,
            created_by,
            created_at,
            last_modified_at,
            last_modified_by,
        }
    }
}

