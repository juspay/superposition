use crate::database::models::cac::Variable as DbVariable;
use crate::database::models::{ChangeReason, Description};
use chrono::{DateTime, Utc};
use derive_more::{AsRef, Deref, DerefMut, Into};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Variable {
    pub name: VariableName,
    pub value: String,
    pub description: Description, // check if this is the correct value
    pub change_reason: ChangeReason, // check for this one as well
    pub created_at: DateTime<Utc>,
    pub last_modified_at: DateTime<Utc>,
    pub created_by: String,
    pub last_modified_by: String,
}

#[derive(
    Debug, Serialize, Deserialize, AsRef, Deref, DerefMut, Into, Clone, PartialEq,
)]
#[serde(try_from = "String")]
pub struct VariableName(pub String);

impl VariableName {
    pub fn validate(name: String) -> Result<Self, String> {
        let name: &str = name.trim();

        if name.is_empty() {
            return Err("Variable name cannot be empty".to_string());
        }
        if !name
            .chars()
            .all(|c| c.is_ascii_uppercase() || c.is_ascii_digit() || c == '_')
        {
            return Err(
                "variable name must contain only uppercase, digits and underscore"
                    .to_string(),
            );
        }

        if name.starts_with(|c: char| c.is_ascii_digit()) {
            return Err("Variable name cannot start with number".to_string());
        }

        if name.len() > 50 {
            return Err("Variable name cannot exceed 100 characters".to_string());
        }

        Ok(Self(name.to_string()))
    }
}

impl TryFrom<String> for VariableName {
    type Error = String;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::validate(value)
    }
}

impl From<DbVariable> for Variable {
    fn from(db_var: DbVariable) -> Self {
        Variable {
            name: VariableName(db_var.name),
            value: db_var.value,
            description: db_var.description,
            change_reason: db_var.change_reason,
            created_at: db_var.created_at,
            last_modified_at: db_var.last_modified_at,
            created_by: db_var.created_by,
            last_modified_by: db_var.last_modified_by,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateVariableRequest {
    pub name: VariableName,
    pub value: String,
    pub description: Description,
    pub change_reason: ChangeReason,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct UpdateVariableRequest {
    pub name: VariableName,
    pub value: String,
    pub change_reason: ChangeReason,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListVariablesResponse {
    pub variables: Vec<Variable>,
    pub total: usize, // pagination
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetVariableResponse {
    pub variable: Variable,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeleteVariableRequest {
    pub change_reason: ChangeReason,
}
