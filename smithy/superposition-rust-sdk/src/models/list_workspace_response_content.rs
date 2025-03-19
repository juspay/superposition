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
pub struct ListWorkspaceResponseContent {
    #[serde(rename = "total_pages")]
    pub total_pages: f64,
    #[serde(rename = "total_items")]
    pub total_items: f64,
    #[serde(rename = "data")]
    pub data: Vec<models::WorkspaceResponse>,
}

impl ListWorkspaceResponseContent {
    pub fn new(total_pages: f64, total_items: f64, data: Vec<models::WorkspaceResponse>) -> ListWorkspaceResponseContent {
        ListWorkspaceResponseContent {
            total_pages,
            total_items,
            data,
        }
    }
}

