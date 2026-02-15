use serde::{Deserialize, Serialize};
use std::fmt;

/// Unified error type for all configuration formats
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FormatError {
    SyntaxError {
        format: String,
        message: String,
    },
    InvalidDimension(String),
    InvalidCohortDimensionPosition {
        dimension: String,
        dimension_position: i32,
        cohort_dimension: String,
        cohort_dimension_position: i32,
    },
    UndeclaredDimension {
        dimension: String,
        context: String,
    },
    InvalidOverrideKey {
        key: String,
        context: String,
    },
    DuplicatePosition {
        position: i32,
        dimensions: Vec<String>,
    },
    ConversionError {
        format: String,
        message: String,
    },
    SerializationError {
        format: String,
        message: String,
    },
    ValidationError {
        key: String,
        errors: String,
    },
}

impl fmt::Display for FormatError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::SyntaxError { format, message } => {
                write!(f, "{} syntax error: {}", format, message)
            }
            Self::InvalidCohortDimensionPosition {
                dimension,
                dimension_position,
                cohort_dimension,
                cohort_dimension_position,
            } => {
                write!(
                    f,
                    "Validation error: Dimension {} position {} should be greater than cohort dimension {} position {}",
                    dimension, dimension_position, cohort_dimension, cohort_dimension_position
                )
            }
            Self::UndeclaredDimension { dimension, context } => {
                write!(
                    f,
                    "Parsing error: Undeclared dimension '{}' used in context '{}'",
                    dimension, context
                )
            }
            Self::InvalidOverrideKey { key, context } => {
                write!(
                    f,
                    "Parsing error: Override key '{}' not found in default-config (context: '{}')",
                    key, context
                )
            }
            Self::DuplicatePosition {
                position,
                dimensions,
            } => {
                write!(
                    f,
                    "Parsing error: Duplicate position '{}' found in dimensions: {}",
                    position,
                    dimensions.join(", ")
                )
            }
            Self::ConversionError { format, message } => {
                write!(f, "{} conversion error: {}", format, message)
            }
            Self::SerializationError { format, message } => {
                write!(f, "{} serialization error: {}", format, message)
            }
            Self::InvalidDimension(d) => {
                write!(f, "Dimension does not exist: {}", d)
            }
            Self::ValidationError { key, errors } => {
                write!(f, "Schema validation failed for key '{}': {}", key, errors)
            }
        }
    }
}

impl std::error::Error for FormatError {}

/// Helper to create syntax error for a specific format
pub fn syntax_error(format: &str, message: impl Into<String>) -> FormatError {
    FormatError::SyntaxError {
        format: format.to_string(),
        message: message.into(),
    }
}

/// Helper to create conversion error for a specific format
pub fn conversion_error(format: &str, message: impl Into<String>) -> FormatError {
    FormatError::ConversionError {
        format: format.to_string(),
        message: message.into(),
    }
}

/// Helper to create serialization error for a specific format
pub fn serialization_error(format: &str, message: impl Into<String>) -> FormatError {
    FormatError::SerializationError {
        format: format.to_string(),
        message: message.into(),
    }
}
