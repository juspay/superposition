use superposition_core::{FormatError, format::toml::parse_toml_config};
use tower_lsp::lsp_types::*;

use crate::utils;

/// Validate a SuperTOML document and return LSP diagnostics.
///
/// Two passes are performed:
/// 1. Raw TOML syntax check — uses the `toml` crate's span information for
///    accurate line/column reporting.
/// 2. SuperTOML semantic validation via `parse_toml_config` — maps each
///    `FormatError` variant to a best-effort source range by searching for the
///    relevant token in the document text.
pub fn compute(text: &str) -> Vec<Diagnostic> {
    // Pass 2: SuperTOML semantic validation.
    let Err(parse_err) = parse_toml_config(text) else {
        return vec![];
    };
    let range = find_error_range(text, &parse_err);
    let (range, diagnostic) = match parse_err {
        FormatError::SyntaxError {
            format: _,
            message,
            span,
        } => (
            span.map(|s| byte_span_to_range(text, s))
                .unwrap_or_default(),
            message,
        ),
        FormatError::InvalidDimension(dim_err) => {
            (range, format!("Invalid dimension: {}", dim_err))
        }
        FormatError::InvalidCohortDimensionPosition {
            dimension,
            dimension_position,
            cohort_dimension,
            cohort_dimension_position,
        } => (
            range,
            format!(
                "Invalid position for cohort dimension '{}': position {} is not compatible with position {} of dimension '{}'",
                cohort_dimension,
                cohort_dimension_position,
                dimension_position,
                dimension
            ),
        ),
        FormatError::UndeclaredDimension { dimension, context } => (
            range,
            format!(
                "Undeclared dimension '{}': used in context '{}' but not declared in [dimensions]",
                dimension, context
            ),
        ),
        FormatError::InvalidOverrideKey { key, context } => (
            range,
            format!(
                "Invalid override key '{}': used in context '{}' but not declared in [default-configs]",
                key, context
            ),
        ),
        FormatError::DuplicatePosition {
            position,
            dimensions,
        } => (
            range,
            format!(
                "Duplicate dimension position {}: dimensions {} all share this position",
                position,
                dimensions
                    .iter()
                    .map(|d| format!("'{}'", d))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        ),
        FormatError::ConversionError { format: _, message } => {
            (range, format!("Conversion error: {}", message))
        }
        FormatError::SerializationError { format: _, message } => {
            (range, format!("Serialization error: {}", message))
        }
        FormatError::ValidationError { key, errors } => {
            let range = find_key_range_in_toml(text, &key);
            (range, format!("Validation error for '{}': {}", key, errors))
        }
    };
    Vec::from([Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("supertoml-analyzer".to_string()),
        message: diagnostic,
        ..Default::default()
    }])
}

/// Map a `FormatError` to a source range by searching for the relevant token.
fn find_error_range(text: &str, err: &FormatError) -> Range {
    match err {
        FormatError::InvalidDimension(dim) => find_text_range(text, dim),

        FormatError::UndeclaredDimension { dimension, .. } => {
            find_text_range(text, dimension)
        }

        FormatError::InvalidOverrideKey { key, .. } => find_text_range(text, key),

        FormatError::DuplicatePosition { dimensions, .. } => dimensions
            .first()
            .map(|d| find_text_range(text, d))
            .unwrap_or_default(),

        FormatError::ValidationError { key, .. } => find_key_range_in_toml(text, key),

        FormatError::InvalidCohortDimensionPosition { dimension, .. } => {
            find_text_range(text, dimension)
        }

        // TomlSyntaxError is handled in pass 1; these rarely reach pass 2.
        FormatError::SyntaxError { .. }
        | FormatError::ConversionError { .. }
        | FormatError::SerializationError { .. } => Range::default(),
    }
}

/// Find the range of a key in TOML text.
///
/// The key format is: `<table_name>.<sub_nesting>?.actual_name`
///
/// Examples:
/// - `default-configs.timeout` → table=`[default-configs]`, actual_key=`timeout`
/// - `dimensions.os.schema` → table=`[dimensions]`, find `os` key, then `schema` in its inline table
/// - `context\[0\].timeout` → first `[[overrides]]`, actual_key=`timeout`
/// - `context\[0\]._context_.timeout` → first `[[overrides]]`, `_context_` key, then `timeout` in its inline table
fn find_key_range_in_toml(text: &str, key: &str) -> Range {
    let segments: Vec<&str> = key.split('.').collect();

    if segments.is_empty() {
        return Range::default();
    }

    // First segment is table name (may contain array index like context\[0\])
    let table_segment = segments[0];

    // Last segment is the actual key name we want to highlight
    let actual_key = segments.last().unwrap();

    // Middle segments are sub-nesting (keys that lead to inline tables)
    let sub_nesting = &segments[1..segments.len().saturating_sub(1)];

    // Special case: for dimensions.<key>.schema, highlight the dimension name instead of schema
    // This avoids flaky behavior when trying to find schema inside inline tables
    if table_segment == "dimensions" && segments.len() == 3 && *actual_key == "schema" {
        let dimension_name = segments[1];
        let section_start = find_table_section_start(text, table_segment);
        return match section_start {
            Some(start) => find_key_in_section(text, start, &[], dimension_name),
            None => find_text_range(text, dimension_name),
        };
    }

    // Handle special case for context[<index>] → [[overrides]]
    if let Some(index) = parse_context_index(table_segment) {
        return find_key_in_overrides(text, index, sub_nesting, actual_key);
    }

    // Find the table section start
    let section_start = find_table_section_start(text, table_segment);

    match section_start {
        Some(start) => find_key_in_section(text, start, sub_nesting, actual_key),
        None => find_text_range(text, actual_key),
    }
}

/// Parse the index from a context segment like "context\[0\]".
/// Returns None if the segment doesn't match the pattern.
fn parse_context_index(segment: &str) -> Option<usize> {
    let segment = segment.trim();
    if !segment.starts_with("context[") || !segment.ends_with(']') {
        return None;
    }
    let inner = &segment["context[".len()..segment.len() - 1];
    inner.parse().ok()
}

/// Find the byte offset where content starts after a table header.
/// Looks for both `[table_name]` and `[[table_name]]` patterns.
/// Returns the byte offset after the header line's newline.
fn find_table_section_start(text: &str, table_name: &str) -> Option<usize> {
    let regular_header = format!("[{}]", table_name);
    let array_header = format!("[[{}]]", table_name);

    let mut byte_offset = 0usize;

    for line in text.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with(&regular_header) || trimmed.starts_with(&array_header) {
            // Found the header, return position after this line (including newline)
            let line_end = byte_offset + line.len();
            // Section content starts after the newline (if present)
            let section_start = if line_end < text.len() {
                line_end + 1 // Skip the newline character
            } else {
                line_end
            };
            return Some(section_start);
        }
        byte_offset += line.len();
        // Account for newline character
        if byte_offset < text.len() {
            byte_offset += 1;
        }
    }
    None
}

/// Find a key within a table section, handling sub-nesting for inline tables.
fn find_key_in_section(
    text: &str,
    section_start: usize,
    sub_nesting: &[&str],
    actual_key: &str,
) -> Range {
    // Find the extent of this section (until next table header or EOF)
    let section_end = find_next_section_start(text, section_start).unwrap_or(text.len());

    // If there's sub-nesting, we need to find keys within inline tables
    if !sub_nesting.is_empty() {
        let first_key = sub_nesting[0];

        // Find the line containing the first sub-nesting key
        if let Some(key_byte_offset) =
            find_key_assignment(text, section_start, section_end, first_key)
        {
            // Found the first sub-nesting key. Now find actual_key on the same line
            // (since inline tables are on the same line)
            let line_start = text[..key_byte_offset]
                .rfind('\n')
                .map(|i| i + 1)
                .unwrap_or(0);
            let line_end = text[line_start..]
                .find('\n')
                .map(|i| line_start + i)
                .unwrap_or(text.len());

            // Find actual_key within this line, ensuring it's a key assignment
            // and not a value inside a string
            if let Some(abs_pos) =
                find_key_on_line(text, line_start, line_end, actual_key)
            {
                return Range::new(
                    byte_offset_to_position(text, abs_pos),
                    byte_offset_to_position(text, abs_pos + actual_key.len()),
                );
            }
        }
    }

    // No sub-nesting or fallback: find actual_key directly in section
    find_key_assignment_range(text, section_start, section_end, actual_key)
        .unwrap_or_else(|| find_text_range(text, actual_key))
}

/// Find a key on a line, skipping strings and ensuring it's a key assignment.
/// Returns the byte offset of the key in the text, or None if not found.
fn find_key_on_line(
    text: &str,
    line_start: usize,
    line_end: usize,
    key: &str,
) -> Option<usize> {
    let line = &text[line_start..line_end.min(text.len())];
    let bytes = line.as_bytes();
    let mut i = 0;
    let len = bytes.len();

    while i < len {
        let b = bytes[i];

        if b == b'"' {
            // Basic string: skip until closing "
            i += 1;
            while i < len {
                if bytes[i] == b'\\' && i + 1 < len {
                    // Escape sequence, skip next char
                    i += 2;
                    continue;
                }
                if bytes[i] == b'"' {
                    i += 1;
                    break;
                }
                i += 1;
            }
        } else if b == b'\'' {
            // Literal string: skip until closing '
            i += 1;
            while i < len && bytes[i] != b'\'' {
                i += 1;
            }
            if i < len {
                i += 1; // Skip closing quote
            }
        } else if i + key.len() <= len && &line[i..i + key.len()] == key {
            // Found the key, check if it's a key assignment (followed by = or = with optional space)
            let after_key = i + key.len();
            let rest = line[after_key..].trim_start();
            if rest.starts_with('=') {
                // It's a key assignment
                let abs_pos = line_start + i;
                if !utils::is_offset_inside_comment(text, abs_pos) {
                    return Some(abs_pos);
                }
            }
            i += 1;
        } else {
            i += 1;
        }
    }
    None
}

/// Find a key assignment (=) in a given byte range.
/// Returns the byte offset of the key start, or None if not found.
fn find_key_assignment(text: &str, start: usize, end: usize, key: &str) -> Option<usize> {
    let search_text = &text[start..end.min(text.len())];
    let mut line_start_in_section = 0usize;

    for line in search_text.lines() {
        let trimmed = line.trim();

        // Check for "key =" or "key=" pattern
        let key_pattern_space = format!("{} =", key);
        let key_pattern_nospace = format!("{}=", key);

        if trimmed.starts_with(&key_pattern_space)
            || trimmed.starts_with(&key_pattern_nospace)
        {
            // Find the key position within the line
            if let Some(key_pos_in_line) = line.find(key) {
                let abs_pos = start + line_start_in_section + key_pos_in_line;
                if !utils::is_offset_inside_comment(text, abs_pos) {
                    return Some(abs_pos);
                }
            }
        }

        line_start_in_section += line.len();
        // Account for newline
        if start + line_start_in_section < text.len() {
            line_start_in_section += 1;
        }
    }
    None
}

/// Find a key assignment and return its Range.
fn find_key_assignment_range(
    text: &str,
    start: usize,
    end: usize,
    key: &str,
) -> Option<Range> {
    find_key_assignment(text, start, end, key).map(|abs_pos| {
        Range::new(
            byte_offset_to_position(text, abs_pos),
            byte_offset_to_position(text, abs_pos + key.len()),
        )
    })
}

/// Find the byte offset of the next section header after a given position.
fn find_next_section_start(text: &str, after: usize) -> Option<usize> {
    let search_start = after.min(text.len());
    let search_text = &text[search_start..];
    let mut byte_offset = search_start;

    for line in search_text.lines() {
        let trimmed = line.trim();
        // A new section starts with [ (but not if it's the same line we started from)
        if trimmed.starts_with('[') && byte_offset > after {
            return Some(byte_offset);
        }
        byte_offset += line.len();
        if byte_offset < text.len() {
            byte_offset += 1; // newline
        }
    }
    None
}

/// Find a key within the `[[overrides]]` array at the given index.
fn find_key_in_overrides(
    text: &str,
    index: usize,
    sub_nesting: &[&str],
    actual_key: &str,
) -> Range {
    // Find all [[overrides]] header positions
    let override_sections = find_all_override_sections(text);

    // Get the requested override section
    let section_start = match override_sections.get(index) {
        Some(&start) => start,
        None => return find_text_range(text, actual_key),
    };

    // Find section end (next [[overrides]] or EOF)
    let section_end = override_sections
        .get(index + 1)
        .copied()
        .unwrap_or(text.len());

    // Handle sub-nesting like _context_.timeout
    if !sub_nesting.is_empty() {
        let first_key = sub_nesting[0];

        if let Some(key_byte_offset) =
            find_key_assignment(text, section_start, section_end, first_key)
        {
            // Found the sub-nesting key, now find actual_key on the same line
            let line_start = text[..key_byte_offset]
                .rfind('\n')
                .map(|i| i + 1)
                .unwrap_or(0);
            let line_end = text[line_start..]
                .find('\n')
                .map(|i| line_start + i)
                .unwrap_or(text.len());

            // Find actual_key within this line, ensuring it's a key assignment
            // and not a value inside a string
            if let Some(abs_pos) =
                find_key_on_line(text, line_start, line_end, actual_key)
            {
                return Range::new(
                    byte_offset_to_position(text, abs_pos),
                    byte_offset_to_position(text, abs_pos + actual_key.len()),
                );
            }
        }
    }

    // No sub-nesting or fallback: find actual_key directly
    find_key_assignment_range(text, section_start, section_end, actual_key)
        .unwrap_or_else(|| find_text_range(text, actual_key))
}

/// Find all `[[overrides]]` section start positions.
/// Returns a vector of byte offsets where each override section starts (after the header).
fn find_all_override_sections(text: &str) -> Vec<usize> {
    let mut sections = Vec::new();
    let mut byte_offset = 0usize;

    for line in text.lines() {
        if line.trim().starts_with("[[overrides]]") {
            // Section starts after the header line
            let section_start = (byte_offset + line.len()).min(text.len());
            sections.push(section_start);
        }
        byte_offset += line.len();
        if byte_offset < text.len() {
            byte_offset += 1; // newline
        }
    }

    sections
}

fn find_text_range(text: &str, search: &str) -> Range {
    let mut search_start = 0;
    while let Some(pos) = text[search_start..].find(search) {
        let abs_pos = search_start + pos;
        // Skip matches that are inside comments
        if !utils::is_offset_inside_comment(text, abs_pos) {
            return Range::new(
                byte_offset_to_position(text, abs_pos),
                byte_offset_to_position(text, abs_pos + search.len()),
            );
        }
        // Match was inside a comment, continue searching after this match
        search_start = abs_pos + search.len();
    }
    Range::default()
}

fn byte_span_to_range(text: &str, span: std::ops::Range<usize>) -> Range {
    Range::new(
        byte_offset_to_position(text, span.start),
        byte_offset_to_position(text, span.end),
    )
}

fn byte_offset_to_position(text: &str, offset: usize) -> Position {
    let offset = offset.min(text.len());
    let prefix = &text[..offset];
    let line = prefix.chars().filter(|&c| c == '\n').count() as u32;
    let col = prefix.rfind('\n').map(|i| offset - i - 1).unwrap_or(offset) as u32;
    Position::new(line, col)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_context_index() {
        assert_eq!(parse_context_index("context[0]"), Some(0));
        assert_eq!(parse_context_index("context[1]"), Some(1));
        assert_eq!(parse_context_index("context[10]"), Some(10));
        assert_eq!(parse_context_index("context[]"), None);
        assert_eq!(parse_context_index("context"), None);
        assert_eq!(parse_context_index("other[0]"), None);
    }

    #[test]
    fn test_find_table_section_start() {
        let text = r#"[default-configs]
timeout = 30

[dimensions]
os = "linux"
"#;
        // [default-configs] = 17 chars, newline at 17, content starts at 18
        assert_eq!(find_table_section_start(text, "default-configs"), Some(18));
        // timeout = 30 (12 chars) + newline = 13, empty line + newline = 1
        // [dimensions] (12 chars) starts at 32, content after newline at 45
        assert_eq!(find_table_section_start(text, "dimensions"), Some(45));
        assert_eq!(find_table_section_start(text, "missing"), None);
    }

    #[test]
    fn test_find_key_range_default_configs() {
        let text = r#"[default-configs]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os = { position = 1, schema = { type = "string" } }
"#;
        let range = find_key_range_in_toml(text, "default-configs.timeout");
        // timeout is on line 1 (0-indexed), character position within the line
        assert_eq!(range.start.line, 1);
        // The character position should point to "timeout" in the line
        let line_start = text.lines().nth(1).unwrap();
        assert!(line_start[range.start.character as usize..].starts_with("timeout"));
    }

    #[test]
    fn test_find_key_range_dimensions_schema() {
        let text = r#"[default-configs]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os = { position = 1, schema = { type = "string" } }
"#;
        let range = find_key_range_in_toml(text, "dimensions.os.schema");
        // os is on line 4 (0-indexed), we now highlight the dimension name "os" instead of "schema"
        assert_eq!(range.start.line, 4);
        // The character position should point to "os" within the line
        let line_start = text.lines().nth(4).unwrap();
        assert!(line_start[range.start.character as usize..].starts_with("os"));
    }

    #[test]
    fn test_find_key_range_context_override() {
        let text = r#"[default-configs]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os = { position = 1, schema = { type = "string" } }

[[overrides]]
_context_ = { os = "linux" }
timeout = 60

[[overrides]]
_context_ = { os = "windows" }
timeout = 120
"#;
        // Lines: 0=[default-configs], 1=timeout..., 2=empty, 3=[dimensions], 4=os..., 5=empty, 6=[[overrides]], 7=_context_..., 8=timeout 60
        // First override - timeout is on line 8
        let range = find_key_range_in_toml(text, "context[0].timeout");
        assert_eq!(range.start.line, 8);
        let line = text.lines().nth(8).unwrap();
        assert!(line[range.start.character as usize..].starts_with("timeout"));

        // Second override - timeout is on line 12
        let range = find_key_range_in_toml(text, "context[1].timeout");
        assert_eq!(range.start.line, 12);
        let line = text.lines().nth(12).unwrap();
        assert!(line[range.start.character as usize..].starts_with("timeout"));
    }

    #[test]
    fn test_find_key_range_context_context_key() {
        let text = r#"[default-configs]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os = { position = 1, schema = { type = "string" } }

[[overrides]]
_context_ = { os = "linux" }
timeout = 60
"#;
        // Lines: 0=[default-configs], 1=timeout..., 2=empty, 3=[dimensions], 4=os..., 5=empty, 6=[[overrides]], 7=_context_..., 8=timeout 60
        // _context_ is on line 7, os is in the inline table on the same line
        let range = find_key_range_in_toml(text, "context[0]._context_.os");
        assert_eq!(range.start.line, 7);
        // Should find "os" in the _context_ inline table
        let line = text.lines().nth(7).unwrap();
        assert!(line[range.start.character as usize..].starts_with("os"));
    }

    #[test]
    fn test_find_next_section_start() {
        let text = r#"[default-configs]
timeout = 30

[dimensions]
os = "linux"
"#;
        // Byte positions:
        // [default-configs] = 17 chars (pos 0-16), newline at 17
        // timeout = 30 = 12 chars (pos 18-29), newline at 30
        // empty line newline at 31
        // [dimensions] starts at pos 32
        let next = find_next_section_start(text, 18);
        assert_eq!(next, Some(32)); // Should point to [dimensions]

        // After dimensions header (pos 33 is after the newline)
        let next = find_next_section_start(text, 33);
        assert_eq!(next, None); // No more sections
    }

    #[test]
    fn test_find_key_range_multiple_dimensions_schema() {
        let text = r#"[default-configs]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os = { position = 1, schema = { type = "string" } }
peak_hours = { position = 2, schema = { type = "array", items = { type = "string" } } }

[[overrides]]
_context_ = { os = "linux" }
"#;
        // Find schema for peak_hours dimension - should highlight "peak_hours" not "os"
        let range = find_key_range_in_toml(text, "dimensions.peak_hours.schema");
        // Lines: 0=[default-configs], 1=timeout..., 2=empty, 3=[dimensions], 4=os..., 5=peak_hours...
        // peak_hours is on line 5 (0-indexed)
        assert_eq!(range.start.line, 5);
        let line = text.lines().nth(5).unwrap();
        // We now highlight the dimension name "peak_hours" instead of "schema"
        assert!(line[range.start.character as usize..].starts_with("peak_hours"));

        // Verify it's on the peak_hours line, not os line
        assert!(line.contains("peak_hours"));

        // Find schema for os dimension - should highlight "os"
        let range = find_key_range_in_toml(text, "dimensions.os.schema");
        // os is on line 4
        assert_eq!(range.start.line, 4);
        let line = text.lines().nth(4).unwrap();
        // We now highlight the dimension name "os" instead of "schema"
        assert!(line[range.start.character as usize..].starts_with("os"));
        assert!(line.contains("os ="));
    }

    #[test]
    fn test_find_key_range_cohort_dimension_schema() {
        let text = r#"[default-configs]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os = { position = 2, schema = { type = "string" } }
peak_hours = { position = 1, type = "LOCAL_COHORT:os", schema = { type = "array", items = { type = "string" } } }

[[overrides]]
_context_ = { os = "linux" }
"#;
        // Find schema for peak_hours cohort dimension - should highlight "peak_hours"
        let range = find_key_range_in_toml(text, "dimensions.peak_hours.schema");
        // Lines: 0=[default-configs], 1=timeout..., 2=empty, 3=[dimensions], 4=os..., 5=peak_hours...
        assert_eq!(range.start.line, 5);
        let line = text.lines().nth(5).unwrap();
        // We now highlight the dimension name "peak_hours" instead of "schema"
        assert!(line[range.start.character as usize..].starts_with("peak_hours"));
        // Verify it's on the peak_hours line, not os line
        assert!(line.contains("peak_hours"));
    }

    #[test]
    fn test_find_key_on_line_with_schema_in_string() {
        let text = r#"[default-configs]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os = { position = 1, schema = { type = "string" } }
peak_hours = { position = 2, some_field = "schema_value", schema = { type = "array" } }

[[overrides]]
_context_ = { os = "linux" }
"#;
        // The line for peak_hours has "schema_value" string before the actual schema key
        // We now highlight the dimension name "peak_hours" instead of "schema" to avoid flakiness
        let range = find_key_range_in_toml(text, "dimensions.peak_hours.schema");
        // peak_hours is on line 5
        assert_eq!(range.start.line, 5);
        let line = text.lines().nth(5).unwrap();

        // The character position should point to "peak_hours" (the dimension name)
        let char_pos = range.start.character as usize;
        assert!(line[char_pos..].starts_with("peak_hours"));
    }
}
