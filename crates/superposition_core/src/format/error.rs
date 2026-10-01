use std::fmt;
use std::path::{Path, PathBuf};

/// Unified error type for all configuration formats
#[derive(Debug)]
pub enum FormatError {
    /// An import rule was broken: a bad path, a missing file, a key next to
    /// `import`, a file holding the wrong section, and so on. `file` is the
    /// file holding the offending text (usually `main.stoml`), or `None` when
    /// the config was parsed from a string. `span` is a byte range in `file`.
    ImportError {
        file: Option<PathBuf>,
        span: Option<std::ops::Range<usize>>,
        message: String,
    },
    /// An ordinary error that came from an imported file. Spans and override
    /// indices inside `error` are local to `file`. Never nested, and never
    /// wraps an `ImportError`.
    InFile {
        file: PathBuf,
        error: Box<FormatError>,
    },
    SyntaxError {
        format: super::MarkupFormat,
        message: String,
        span: Option<std::ops::Range<usize>>,
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
            Self::SyntaxError {
                format,
                message,
                span: _,
            } => {
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
            Self::ImportError {
                file: Some(file),
                message,
                ..
            } => write!(f, "{}: Import error: {}", file.display(), message),
            Self::ImportError {
                file: None,
                message,
                ..
            } => write!(f, "Import error: {}", message),
            Self::InFile { file, error } => write!(f, "{}: {}", file.display(), error),
        }
    }
}

impl FormatError {
    /// The file an error points at (`None` means the file that was parsed,
    /// e.g. `main.stoml`) and the error to report there, with `InFile`
    /// unwrapped.
    pub fn location(&self) -> (Option<&Path>, &FormatError) {
        match self {
            Self::InFile { file, error } => (Some(file.as_path()), error),
            Self::ImportError { file, .. } => (file.as_deref(), self),
            other => (None, other),
        }
    }
}

impl std::error::Error for FormatError {}

/// Result type alias for format operations
pub type FormatResult<T> = Result<T, FormatError>;

/// Format validation errors into a single string
pub fn format_validation_errors(errors: &[String]) -> String {
    errors.join("; ")
}
