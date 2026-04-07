use tower_lsp::lsp_types::Position;

/// Check if a cursor position is inside a TOML comment.
///
/// In TOML, comments start with `#` and continue to the end of the line.
/// This function handles:
/// - Full line comments: `# this is a comment`
/// - Inline comments: `key = "value" # this is a comment`
///
/// It properly ignores `#` characters inside strings (both basic "..." and literal '...').
pub fn is_inside_comment(text: &str, pos: Position) -> bool {
    let lines: Vec<&str> = text.lines().collect();
    let line = match lines.get(pos.line as usize) {
        Some(l) => l,
        None => return false,
    };

    let col = pos.character as usize;
    let comment_start = find_comment_start(line);

    matches!(comment_start, Some(start) if col >= start)
}

/// Check if a byte offset in the text is inside a TOML comment.
///
/// This is useful for diagnostics when searching for error tokens -
/// we want to skip matches that occur inside comments.
pub fn is_offset_inside_comment(text: &str, offset: usize) -> bool {
    let offset = offset.min(text.len());

    // Find the start of the line containing this offset
    let line_start = text[..offset].rfind('\n').map(|i| i + 1).unwrap_or(0);

    // Find the end of the line
    let line_end = text[offset..]
        .find('\n')
        .map(|i| offset + i)
        .unwrap_or(text.len());

    let line = &text[line_start..line_end];
    let col = offset - line_start;

    matches!(find_comment_start(line), Some(comment_col) if col >= comment_col)
}

/// Find the column position where a comment starts on a line.
///
/// Returns `None` if there is no comment on the line.
/// Properly handles `#` inside strings.
pub fn find_comment_start(line: &str) -> Option<usize> {
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
            // Check for literal string ''' or single '
            if i + 2 < len && bytes[i + 1] == b'\'' && bytes[i + 2] == b'\'' {
                // Multi-line literal string: skip until '''
                i += 3;
                while i + 2 < len {
                    if bytes[i] == b'\'' && bytes[i + 1] == b'\'' && bytes[i + 2] == b'\''
                    {
                        i += 3;
                        break;
                    }
                    i += 1;
                }
            } else {
                // Single-quoted string (literal string): skip until closing '
                i += 1;
                while i < len {
                    if bytes[i] == b'\'' {
                        i += 1;
                        break;
                    }
                    i += 1;
                }
            }
        } else if b == b'#' {
            // Found a comment start
            return Some(i);
        } else {
            i += 1;
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Check if a line is entirely a comment (starts with optional whitespace then #).
    pub fn is_comment_line(line: &str) -> bool {
        line.trim().starts_with('#')
    }

    /// Strip all comments from TOML text.
    ///
    /// Returns a new string with comments removed but preserving line structure
    /// for accurate position mapping.
    pub fn strip_comments(text: &str) -> String {
        text.lines()
            .map(|line| match find_comment_start(line) {
                Some(pos) => &line[..pos],
                None => line,
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    #[test]
    fn test_find_comment_start_basic() {
        assert_eq!(find_comment_start("key = value # comment"), Some(12));
        assert_eq!(find_comment_start("# full line comment"), Some(0));
        assert_eq!(find_comment_start("key = value"), None);
    }

    #[test]
    fn test_find_comment_start_with_strings() {
        // # inside string should not count as comment
        assert_eq!(find_comment_start("key = \"value#notcomment\""), None);
        assert_eq!(
            find_comment_start("key = \"value#notcomment\" # real comment"),
            Some(25)
        );
        assert_eq!(find_comment_start("key = 'value#notcomment'"), None);
        assert_eq!(
            find_comment_start("key = 'value#notcomment' # real comment"),
            Some(25)
        );
    }

    #[test]
    fn test_is_inside_comment() {
        let text = "key = value # comment\nnext = item";

        // Position on "key" - not in comment
        assert!(!is_inside_comment(text, Position::new(0, 2)));

        // Position on "# comment" - in comment
        assert!(is_inside_comment(text, Position::new(0, 12)));
        assert!(is_inside_comment(text, Position::new(0, 14)));

        // Position on next line - not in comment
        assert!(!is_inside_comment(text, Position::new(1, 5)));
    }

    #[test]
    fn test_is_offset_inside_comment() {
        let text = "# comment with schema\nkey = \"schema\"\nschema = \"issue here\"";

        // Offset 0-19: first line is a comment, "schema" at offset 12
        assert!(is_offset_inside_comment(text, 12)); // "schema" in comment

        // Offset 20-39: second line, "schema" inside string at offset 27
        assert!(!is_offset_inside_comment(text, 27)); // inside string, not comment

        // Offset 40+: third line, "schema" as key at offset 40
        assert!(!is_offset_inside_comment(text, 40)); // "schema" as key, not in comment
    }

    #[test]
    fn test_strip_comments() {
        let input = "key = value # comment\n# full comment\nother = thing";
        let expected = "key = value \n\nother = thing";
        assert_eq!(strip_comments(input), expected);
    }

    #[test]
    fn test_is_comment_line() {
        assert!(is_comment_line("# comment"));
        assert!(is_comment_line("  # indented comment"));
        assert!(!is_comment_line("key = value # inline comment"));
        assert!(!is_comment_line("key = value"));
    }
}
