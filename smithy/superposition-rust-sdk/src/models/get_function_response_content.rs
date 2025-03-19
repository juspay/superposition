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
pub struct GetFunctionResponseContent {
    #[serde(rename = "function_name")]
    pub function_name: String,
    #[serde(rename = "published_code", skip_serializing_if = "Option::is_none")]
    pub published_code: Option<String>,
    #[serde(rename = "draft_code")]
    pub draft_code: String,
    #[serde(rename = "published_runtime_version", skip_serializing_if = "Option::is_none")]
    pub published_runtime_version: Option<String>,
    #[serde(rename = "draft_runtime_version")]
    pub draft_runtime_version: String,
    #[serde(rename = "published_at", skip_serializing_if = "Option::is_none")]
    pub published_at: Option<f64>,
    #[serde(rename = "draft_edited_at")]
    pub draft_edited_at: f64,
    #[serde(rename = "published_by", skip_serializing_if = "Option::is_none")]
    pub published_by: Option<String>,
    #[serde(rename = "draft_edited_by")]
    pub draft_edited_by: String,
    #[serde(rename = "last_modified_at")]
    pub last_modified_at: f64,
    #[serde(rename = "last_modified_by")]
    pub last_modified_by: String,
    #[serde(rename = "change_reason")]
    pub change_reason: String,
    #[serde(rename = "description")]
    pub description: String,
}

impl GetFunctionResponseContent {
    pub fn new(function_name: String, draft_code: String, draft_runtime_version: String, draft_edited_at: f64, draft_edited_by: String, last_modified_at: f64, last_modified_by: String, change_reason: String, description: String) -> GetFunctionResponseContent {
        GetFunctionResponseContent {
            function_name,
            published_code: None,
            draft_code,
            published_runtime_version: None,
            draft_runtime_version,
            published_at: None,
            draft_edited_at,
            published_by: None,
            draft_edited_by,
            last_modified_at,
            last_modified_by,
            change_reason,
            description,
        }
    }
}

