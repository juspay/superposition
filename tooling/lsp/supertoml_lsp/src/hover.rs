use tower_lsp::lsp_types::*;

use crate::utils;

/// Produce hover documentation for the token under the cursor.
///
/// Looks up the word at `pos` in:
/// - `[dimensions]` → shows position, type, and schema.
/// - `[default-configs]` → shows the default value and schema.
pub fn compute(text: &str, pos: Position) -> Option<Hover> {
    // Don't provide hover if cursor is inside a comment
    if utils::is_inside_comment(text, pos) {
        return None;
    }

    let lines: Vec<&str> = text.lines().collect();
    let line = lines.get(pos.line as usize)?;
    let word = extract_word(line, pos.character as usize)?;

    // A best-effort parse; we still try to provide hover even when the file
    // has minor errors by working with the raw table.
    let raw: toml::Table = toml::from_str(text).ok()?;

    // --- Dimension hover ---
    if let Some(info) = raw
        .get("dimensions")
        .and_then(|d| d.as_table())
        .and_then(|dims| dims.get(word))
    {
        let mut md = format!("**Dimension: `{}`**\n\n", word);
        if let Some(p) = info.get("position") {
            md.push_str(&format!("- **Position:** {}\n", p));
        }
        if let Some(t) = info.get("type") {
            md.push_str(&format!("- **Type:** {}\n", t));
        }
        if let Some(s) = info.get("schema") {
            md.push_str(&format!("- **Schema:** `{}`\n", s));
        }
        return Some(markdown_hover(md));
    }

    // --- Default-config hover ---
    if let Some(info) = raw
        .get("default-configs")
        .and_then(|d| d.as_table())
        .and_then(|configs| configs.get(word))
    {
        let mut md = format!("**Config key: `{}`**\n\n", word);
        if let Some(v) = info.get("value") {
            md.push_str(&format!("- **Default value:** `{}`\n", v));
        }
        if let Some(s) = info.get("schema") {
            md.push_str(&format!("- **Schema:** `{}`\n", s));
        }
        return Some(markdown_hover(md));
    }

    None
}

fn markdown_hover(value: String) -> Hover {
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        }),
        range: None,
    }
}

/// Extract the word (identifier characters: alphanumeric, `_`, `-`) under `col`.
fn extract_word(line: &str, col: usize) -> Option<&str> {
    let col = col.min(line.len());
    let bytes = line.as_bytes();
    let is_ident = |b: u8| b.is_ascii_alphanumeric() || b == b'_' || b == b'-';

    let start = (0..col)
        .rev()
        .find(|&i| !is_ident(bytes[i]))
        .map(|i| i + 1)
        .unwrap_or(0);

    let end = (col..bytes.len())
        .find(|&i| !is_ident(bytes[i]))
        .unwrap_or(bytes.len());

    if start < end {
        Some(&line[start..end])
    } else {
        None
    }
}
