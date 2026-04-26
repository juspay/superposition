use serde::{Deserialize, Serialize};

use crate::database::models::ChangeReason;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum CopyEntityType {
    Dimensions,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum RowSelectionMode {
    All,
    Selected,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum CopyResultStatus {
    Copied,
    Skipped,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CopyToRequest {
    pub entity_type: CopyEntityType,
    pub target_workspace: String,
    pub selection_mode: RowSelectionMode,
    #[serde(default)]
    pub selected_rows: Vec<String>,
    #[serde(default)]
    pub skip_existing: bool,
    pub change_reason: ChangeReason,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CopyToResult {
    pub row_identifier: String,
    pub status: CopyResultStatus,
    pub message: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CopyToResponse {
    pub entity_type: CopyEntityType,
    pub source_workspace: String,
    pub target_workspace: String,
    pub requested_count: usize,
    pub copied_count: usize,
    pub skipped_count: usize,
    pub failed_count: usize,
    pub results: Vec<CopyToResult>,
}
