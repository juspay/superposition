use std::collections::HashMap;

use tower_lsp::lsp_types::*;

use crate::{definition, diagnostics, utils};

/// Offer quick fixes for the cursor position and for any diagnostic overlapping it.
///
/// Diagnostic-driven fixes match on the diagnostic's `code` (see
/// [`crate::diagnostics`]) rather than on message text, so rewording a message
/// cannot silently detach its fix.
pub fn compute(text: &str, params: &CodeActionParams) -> Option<CodeActionResponse> {
    let uri = &params.text_document.uri;
    let raw = toml::from_str::<toml::Table>(text).ok();
    let mut actions: Vec<CodeActionOrCommand> = Vec::new();

    for diag in &params.context.diagnostics {
        let Some(NumberOrString::String(code)) = &diag.code else {
            continue;
        };
        match code.as_str() {
            diagnostics::CODE_UNDECLARED_DIMENSION => {
                actions.extend(add_missing_dimension(text, raw.as_ref(), diag, uri));
            }
            diagnostics::CODE_INVALID_OVERRIDE_KEY => {
                actions.extend(add_missing_config(text, diag, uri));
            }
            diagnostics::CODE_VALIDATION => {
                actions.extend(fix_enum_value(text, raw.as_ref(), diag, uri));
            }
            diagnostics::CODE_DUPLICATE_POSITION => {
                actions.extend(fix_duplicate_position(text, raw.as_ref(), diag, uri));
            }
            diagnostics::CODE_COHORT_POSITION => {
                actions.extend(fix_cohort_position(text, diag, uri));
            }
            _ => {}
        }
    }

    // A missing `schema` surfaces as a bare `missing field \`schema\`` syntax
    // error, which carries no structured data to key a fix off. So this one is
    // driven by the cursor position instead of by the diagnostic.
    actions.extend(add_missing_schema(
        text,
        raw.as_ref(),
        params.range.start,
        uri,
    ));

    (!actions.is_empty()).then_some(actions)
}

// --- Quick fix: declare a dimension used in a context but never defined ------

fn add_missing_dimension(
    text: &str,
    raw: Option<&toml::Table>,
    diag: &Diagnostic,
    uri: &Url,
) -> Option<CodeActionOrCommand> {
    let name = diag.data.as_ref()?.get("dimension")?.as_str()?.to_string();
    let position = next_free_position(raw);
    let entry =
        format!("{name} = {{ position = {position}, schema = {{ type = \"string\" }} }}");

    Some(quick_fix(
        format!("Declare dimension '{name}' in [dimensions]"),
        diag,
        uri,
        append_to_section(text, definition::DIMENSIONS, &entry)?,
    ))
}

/// Dimension positions must be unique, and position 0 is reserved.
fn next_free_position(raw: Option<&toml::Table>) -> i64 {
    let highest = raw
        .and_then(|t| t.get(definition::DIMENSIONS))
        .and_then(|d| d.as_table())
        .map(|dims| {
            dims.values()
                .filter_map(|d| d.get("position")?.as_integer())
                .max()
                .unwrap_or(0)
        })
        .unwrap_or(0);
    highest.max(0) + 1
}

// --- Quick fix: declare a config key used in an override but never defined ---

fn add_missing_config(
    text: &str,
    diag: &Diagnostic,
    uri: &Url,
) -> Option<CodeActionOrCommand> {
    let key = diag.data.as_ref()?.get("key")?.as_str()?.to_string();
    let lines: Vec<&str> = text.lines().collect();

    // Model the default on the override's own value, so the generated schema
    // matches what the author already wrote.
    let (value, ty) = value_after_key(&lines, diag.range.start)
        .and_then(|(_, token)| infer_type(&token).map(|ty| (token, ty)))
        .unwrap_or_else(|| ("\"\"".to_string(), "string"));

    let entry = format!("{key} = {{ value = {value}, schema = {{ type = \"{ty}\" }} }}");
    let section =
        if diagnostics::find_table_section_start(text, definition::DEFAULT_CONFIGS)
            .is_none()
            && diagnostics::find_table_section_start(text, definition::DEFAULT_CONFIG_ALT)
                .is_some()
        {
            definition::DEFAULT_CONFIG_ALT
        } else {
            definition::DEFAULT_CONFIGS
        };

    Some(quick_fix(
        format!("Declare '{key}' in [{section}]"),
        diag,
        uri,
        append_to_section(text, section, &entry)?,
    ))
}

/// Map a TOML value token onto the JSON Schema type that describes it.
fn infer_type(token: &str) -> Option<&'static str> {
    let parsed: toml::Table = format!("v = {token}").parse().ok()?;
    Some(json_type_of(parsed.get("v")?))
}

/// The JSON Schema type corresponding to a parsed TOML value.
fn json_type_of(value: &toml::Value) -> &'static str {
    match value {
        toml::Value::String(_) | toml::Value::Datetime(_) => "string",
        toml::Value::Integer(_) => "integer",
        toml::Value::Float(_) => "number",
        toml::Value::Boolean(_) => "boolean",
        toml::Value::Array(_) => "array",
        toml::Value::Table(_) => "object",
    }
}

// --- Quick fix: replace a value that is not in its schema's enum ------------

fn fix_enum_value(
    text: &str,
    raw: Option<&toml::Table>,
    diag: &Diagnostic,
    uri: &Url,
) -> Vec<CodeActionOrCommand> {
    let Some(key) = diag.data.as_ref().and_then(|d| d.get("key")?.as_str()) else {
        return vec![];
    };
    let Some(allowed) = lookup_enum(raw, key) else {
        return vec![];
    };
    let lines: Vec<&str> = text.lines().collect();
    let Some((range, current)) = value_after_key(&lines, diag.range.start) else {
        return vec![];
    };

    allowed
        .iter()
        .map(|v| v.to_string())
        .filter(|replacement| replacement != &current)
        .map(|replacement| {
            let edit = TextEdit {
                range,
                new_text: replacement.clone(),
            };
            quick_fix(format!("Replace with {replacement}"), diag, uri, vec![edit])
        })
        .collect()
}

/// Resolve the `enum` of the schema governing a dotted diagnostic key path.
///
/// Paths look like `context[0]._context_.city`, `context[1].currency` or
/// `default-configs.currency`; the governing schema lives under `[dimensions]`
/// for context keys and under `[default-configs]` for everything else.
fn lookup_enum(raw: Option<&toml::Table>, key_path: &str) -> Option<Vec<toml::Value>> {
    let raw = raw?;
    let segments: Vec<&str> = key_path.split('.').collect();
    let name = *segments.last()?;

    let section = if segments.first()?.starts_with("context[") {
        if segments.contains(&"_context_") {
            definition::DIMENSIONS
        } else {
            definition::DEFAULT_CONFIGS
        }
    } else if *segments.first()? == definition::DIMENSIONS {
        definition::DIMENSIONS
    } else {
        definition::DEFAULT_CONFIGS
    };

    // `schema` itself may be the trailing segment, e.g. `dimensions.city.schema`.
    let name = if name == "schema" || name == "value" {
        segments.get(segments.len().checked_sub(2)?).copied()?
    } else {
        name
    };

    let table = raw
        .get(section)
        .or_else(|| {
            (section == definition::DEFAULT_CONFIGS)
                .then(|| raw.get(definition::DEFAULT_CONFIG_ALT))
                .flatten()
        })?
        .as_table()?;

    let values = table
        .get(name)?
        .get("schema")?
        .get("enum")?
        .as_array()?
        .clone();
    (!values.is_empty()).then_some(values)
}

// --- Quick fix: give a dimension a position of its own ---------------------

/// Offer to move each dimension involved in a position clash onto the next
/// free position.
///
/// One action is offered per dimension rather than picking a winner: the order
/// of the names in the diagnostic does not reflect file order, so there is no
/// principled "first" to keep. Applying one resolves a two-way clash; a wider
/// clash simply offers again on the next pass.
fn fix_duplicate_position(
    text: &str,
    raw: Option<&toml::Table>,
    diag: &Diagnostic,
    uri: &Url,
) -> Vec<CodeActionOrCommand> {
    let Some(names) = diag
        .data
        .as_ref()
        .and_then(|d| d.get("dimensions"))
        .and_then(|d| d.as_array())
    else {
        return vec![];
    };
    let free = next_free_position(raw);

    names
        .iter()
        .filter_map(|n| n.as_str())
        .filter_map(|name| {
            let range = position_value_range(text, name)?;
            Some(quick_fix(
                format!("Move '{name}' to position {free}"),
                diag,
                uri,
                vec![TextEdit {
                    range,
                    new_text: free.to_string(),
                }],
            ))
        })
        .collect()
}

/// Range of the integer assigned to `position` within a dimension's entry.
fn position_value_range(text: &str, dimension: &str) -> Option<Range> {
    let start = diagnostics::find_table_section_start(text, definition::DIMENSIONS)?;
    let end = diagnostics::find_next_section_start(text, start).unwrap_or(text.len());
    let key = diagnostics::find_key_assignment_range(text, start, end, dimension)?;

    let lines: Vec<&str> = text.lines().collect();
    let line = lines.get(key.start.line as usize)?;
    // +1 lands the cursor inside the word so word_span() picks it up.
    let at = line.find("position")? as u32 + 1;
    value_after_key(&lines, Position::new(key.start.line, at)).map(|(range, _)| range)
}

// --- Quick fix: order a cohort dimension against its base ------------------

/// Offer to swap the positions of a cohort dimension and the dimension it
/// references.
///
/// The core requires the referenced base dimension to sit at a position at or
/// above the dimension declaring the cohort. Swapping is the one repair that
/// cannot introduce a fresh clash: both positions are already in use and
/// unique, so exchanging them leaves the set of occupied positions unchanged.
fn fix_cohort_position(
    text: &str,
    diag: &Diagnostic,
    uri: &Url,
) -> Option<CodeActionOrCommand> {
    let data = diag.data.as_ref()?;
    let dimension = data.get("dimension")?.as_str()?;
    let cohort = data.get("cohort_dimension")?.as_str()?;
    let dimension_position = data.get("dimension_position")?.as_i64()?;
    let cohort_position = data.get("cohort_dimension_position")?.as_i64()?;

    let dimension_range = position_value_range(text, dimension)?;
    let cohort_range = position_value_range(text, cohort)?;

    Some(quick_fix(
        format!("Swap positions of '{cohort}' and '{dimension}'"),
        diag,
        uri,
        vec![
            TextEdit {
                range: dimension_range,
                new_text: cohort_position.to_string(),
            },
            TextEdit {
                range: cohort_range,
                new_text: dimension_position.to_string(),
            },
        ],
    ))
}

// --- Quick fix: add a schema to an entry that has none ----------------------

fn add_missing_schema(
    text: &str,
    raw: Option<&toml::Table>,
    pos: Position,
    uri: &Url,
) -> Option<CodeActionOrCommand> {
    if utils::is_inside_comment(text, pos) {
        return None;
    }
    let lines: Vec<&str> = text.lines().collect();
    let line = lines.get(pos.line as usize)?;
    let section = utils::section_at_line(&lines, pos.line as usize)?;
    if !matches!(
        section.as_str(),
        definition::DIMENSIONS
            | definition::DEFAULT_CONFIGS
            | definition::DEFAULT_CONFIG_ALT
    ) {
        return None;
    }

    // Only inline-table entries that genuinely lack a schema.
    let (key, _) = line.split_once('=')?;
    let key = key.trim();
    if key.is_empty() || !line.contains('{') || has_schema(raw, &section, key, line) {
        return None;
    }

    let ty = if section == definition::DIMENSIONS {
        // A dimension declares no `value`, so learn its type from the values
        // it is actually given in `_context_` blocks.
        dimension_type_from_usage(raw, key).unwrap_or("string")
    } else {
        entry_value_type(raw, &section, key)
            .or_else(|| {
                // Fall back to the raw line while the document is mid-edit.
                let (_, rest) = line.split_once("value")?;
                let rest = rest.trim_start().strip_prefix('=')?;
                infer_type(value_token(rest.trim_start())?.as_str())
            })
            .unwrap_or("string")
    };

    // Insert against the last non-space character so the entry reads
    // `{ position = 2, schema = ... }` rather than `{ position = 2 , schema = ... }`.
    let close = line.rfind('}')?;
    let at = line[..close].trim_end().len() as u32;
    let edit = TextEdit {
        range: Range::new(Position::new(pos.line, at), Position::new(pos.line, at)),
        new_text: format!(", schema = {{ type = \"{ty}\" }}"),
    };

    Some(CodeActionOrCommand::CodeAction(CodeAction {
        title: format!("Add schema to '{key}'"),
        kind: Some(CodeActionKind::QUICKFIX),
        edit: Some(workspace_edit(uri, vec![edit])),
        ..Default::default()
    }))
}

/// Does this entry already declare a schema? Prefers the parsed document and
/// falls back to the raw line when the file does not currently parse.
fn has_schema(raw: Option<&toml::Table>, section: &str, key: &str, line: &str) -> bool {
    match raw.and_then(|t| t.get(section)).and_then(|s| s.get(key)) {
        Some(entry) => entry.get("schema").is_some(),
        None => line.contains("schema"),
    }
}

/// The type of a default-config's declared `value`.
fn entry_value_type(
    raw: Option<&toml::Table>,
    section: &str,
    key: &str,
) -> Option<&'static str> {
    let entry = raw?.get(section)?.get(key)?;
    Some(json_type_of(entry.get("value")?))
}

/// Infer a dimension's type from the values assigned to it in `_context_`
/// blocks. Returns `None` when the dimension is not used anywhere.
fn dimension_type_from_usage(
    raw: Option<&toml::Table>,
    name: &str,
) -> Option<&'static str> {
    raw?.get("overrides")?
        .as_array()?
        .iter()
        .find_map(|ov| Some(json_type_of(ov.get("_context_")?.get(name)?)))
}

// --- Shared helpers ---------------------------------------------------------

/// Read the value token assigned to the key starting at `key_pos`, with the
/// range it occupies. Handles both `key = v` lines and inline-table members.
fn value_after_key(lines: &[&str], key_pos: Position) -> Option<(Range, String)> {
    let line = lines.get(key_pos.line as usize)?;
    let (_, key_end) = utils::word_span(line, key_pos.character as usize)?;

    let rest = &line[key_end..];
    let eq = rest.find('=')?;
    let after_eq = &rest[eq + 1..];
    let lead = after_eq.len() - after_eq.trim_start().len();
    let value_start = key_end + eq + 1 + lead;

    let token = value_token(&line[value_start..])?;
    let range = Range::new(
        Position::new(key_pos.line, value_start as u32),
        Position::new(key_pos.line, (value_start + token.len()) as u32),
    );
    Some((range, token))
}

/// Read a single TOML value from the start of `s`.
///
/// Tracks `[]`/`{}` nesting and quoting so arrays and inline tables come back
/// whole; a naive scan to the first `,` or `}` would truncate `[1, 2]` to `[1`.
fn value_token(s: &str) -> Option<String> {
    let bytes = s.as_bytes();
    let mut i = 0;
    let mut depth = 0usize;

    while i < bytes.len() {
        match bytes[i] {
            quote @ (b'"' | b'\'') => {
                i += 1;
                while i < bytes.len() {
                    if quote == b'"' && bytes[i] == b'\\' {
                        i += 2;
                        continue;
                    }
                    if bytes[i] == quote {
                        break;
                    }
                    i += 1;
                }
                // An unterminated string means the line is mid-edit.
                if i >= bytes.len() {
                    return None;
                }
                i += 1;
            }
            b'[' | b'{' => {
                depth += 1;
                i += 1;
            }
            b']' | b'}' if depth > 0 => {
                depth -= 1;
                i += 1;
            }
            // At the top level these close the enclosing table or start a
            // comment, so the value ends here.
            b'}' | b']' | b',' | b'#' if depth == 0 => break,
            _ => i += 1,
        }
    }

    let token = s[..i].trim_end();
    (!token.is_empty()).then(|| token.to_string())
}

/// Build the edit that appends `entry` as a new line at the end of `section`.
///
/// Returns `None` when the section is absent; both `[dimensions]` and
/// `[default-configs]` are required by the parser, so a document missing one
/// reports a syntax error long before any of these fixes are offered.
fn append_to_section(text: &str, section: &str, entry: &str) -> Option<Vec<TextEdit>> {
    let start = diagnostics::find_table_section_start(text, section)?;

    // Back off over trailing blank lines so the entry joins its neighbours
    // rather than landing after the gap before the next section.
    let end = diagnostics::find_next_section_start(text, start).unwrap_or(text.len());
    let content_end = start + text[start..end].trim_end().len();
    let at = diagnostics::byte_offset_to_position(text, content_end);

    Some(vec![TextEdit {
        range: Range::new(at, at),
        new_text: format!("\n{entry}"),
    }])
}

fn quick_fix(
    title: String,
    diag: &Diagnostic,
    uri: &Url,
    edits: Vec<TextEdit>,
) -> CodeActionOrCommand {
    CodeActionOrCommand::CodeAction(CodeAction {
        title,
        kind: Some(CodeActionKind::QUICKFIX),
        diagnostics: Some(vec![diag.clone()]),
        edit: Some(workspace_edit(uri, edits)),
        ..Default::default()
    })
}

fn workspace_edit(uri: &Url, edits: Vec<TextEdit>) -> WorkspaceEdit {
    WorkspaceEdit {
        changes: Some(HashMap::from([(uri.clone(), edits)])),
        ..Default::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const CLEAN: &str = r#"[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number" } }
currency = { value = "INR", schema = { type = "string", enum = ["INR", "USD"] } }

[dimensions]
city = { position = 1, schema = { type = "string", enum = ["Bangalore", "Delhi"] } }
vehicle_type = { position = 2, schema = { type = "string" } }

[[overrides]]
_context_ = { city = "Bangalore" }
per_km_rate = 25.0
"#;

    fn uri() -> Url {
        Url::parse("file:///test.super.toml").unwrap()
    }

    /// Build params the way an editor would: real diagnostics for the document,
    /// plus the cursor position.
    fn params(text: &str, pos: Position) -> CodeActionParams {
        CodeActionParams {
            text_document: TextDocumentIdentifier { uri: uri() },
            range: Range::new(pos, pos),
            context: CodeActionContext {
                diagnostics: diagnostics::compute(text),
                ..Default::default()
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        }
    }

    fn titles(actions: &[CodeActionOrCommand]) -> Vec<String> {
        actions
            .iter()
            .map(|a| match a {
                CodeActionOrCommand::CodeAction(a) => a.title.clone(),
                CodeActionOrCommand::Command(c) => c.title.clone(),
            })
            .collect()
    }

    fn offset(text: &str, pos: Position) -> usize {
        let mut off = 0;
        for (i, line) in text.split('\n').enumerate() {
            if i == pos.line as usize {
                return (off + pos.character as usize).min(text.len());
            }
            off += line.len() + 1;
        }
        text.len()
    }

    /// Apply an action's edits so assertions can read the resulting document.
    fn apply(text: &str, action: &CodeActionOrCommand) -> String {
        let CodeActionOrCommand::CodeAction(action) = action else {
            panic!("expected a code action");
        };
        let mut edits = action
            .edit
            .as_ref()
            .and_then(|e| e.changes.as_ref())
            .and_then(|c| c.get(&uri()))
            .cloned()
            .unwrap_or_default();
        edits.sort_by_key(|e| std::cmp::Reverse(offset(text, e.range.start)));

        let mut out = text.to_string();
        for edit in edits {
            let (s, e) = (offset(text, edit.range.start), offset(text, edit.range.end));
            out.replace_range(s..e, &edit.new_text);
        }
        out
    }

    fn find<'a>(
        actions: &'a [CodeActionOrCommand],
        needle: &str,
    ) -> &'a CodeActionOrCommand {
        actions
            .iter()
            .find(|a| titles(std::slice::from_ref(*a))[0].contains(needle))
            .unwrap_or_else(|| {
                panic!("no action matching {needle:?} in {:?}", titles(actions))
            })
    }

    #[test]
    fn declares_a_dimension_used_but_never_defined() {
        let text = CLEAN.replace(
            r#"_context_ = { city = "Bangalore" }"#,
            r#"_context_ = { region = "west" }"#,
        );
        let actions = compute(&text, &params(&text, Position::new(9, 16))).unwrap();
        let action = find(&actions, "Declare dimension 'region'");
        let out = apply(&text, action);

        // Lands inside [dimensions], with the next free position.
        assert!(
            out.contains(r#"region = { position = 3, schema = { type = "string" } }"#),
            "{out}"
        );
        let dims = out.split("[dimensions]").nth(1).unwrap();
        assert!(
            dims.split("[[overrides]]")
                .next()
                .unwrap()
                .contains("region")
        );
    }

    #[test]
    fn declares_a_config_key_used_but_never_defined() {
        let text = CLEAN.replace("per_km_rate = 25.0", "surge_multiplier = 1.5");
        let actions = compute(&text, &params(&text, Position::new(10, 3))).unwrap();
        let action = find(&actions, "Declare 'surge_multiplier'");

        // The generated schema mirrors the type the author already wrote (1.5 → number).
        assert!(
            apply(&text, action).contains(
                r#"surge_multiplier = { value = 1.5, schema = { type = "number" } }"#
            ),
            "{}",
            apply(&text, action)
        );
    }

    #[test]
    fn offers_each_valid_enum_value_as_a_replacement() {
        let text = CLEAN.replace(r#"city = "Bangalore" }"#, r#"city = "Mumbai" }"#);
        let actions = compute(&text, &params(&text, Position::new(9, 16))).unwrap();

        let offered = titles(&actions);
        assert!(
            offered.iter().any(|t| t == r#"Replace with "Bangalore""#),
            "{offered:?}"
        );
        assert!(
            offered.iter().any(|t| t == r#"Replace with "Delhi""#),
            "{offered:?}"
        );
    }

    #[test]
    fn enum_fix_replaces_the_value_not_the_key() {
        let text = CLEAN.replace(r#"city = "Bangalore" }"#, r#"city = "Mumbai" }"#);
        let actions = compute(&text, &params(&text, Position::new(9, 16))).unwrap();
        let out = apply(&text, find(&actions, r#"Replace with "Delhi""#));

        assert!(out.contains(r#"_context_ = { city = "Delhi" }"#), "{out}");
        // And the fixed document is actually valid.
        assert!(diagnostics::compute(&out).is_empty());
    }

    #[test]
    fn adds_a_schema_to_an_entry_that_lacks_one() {
        let text = CLEAN.replace(
            r#"vehicle_type = { position = 2, schema = { type = "string" } }"#,
            r#"vehicle_type = { position = 2 }"#,
        );
        let actions = compute(&text, &params(&text, Position::new(6, 2))).unwrap();
        let out = apply(&text, find(&actions, "Add schema to 'vehicle_type'"));

        assert!(
            out.contains(
                r#"vehicle_type = { position = 2, schema = { type = "string" } }"#
            ),
            "{out}"
        );
    }

    #[test]
    fn infers_the_schema_type_from_an_existing_value() {
        let text = CLEAN.replace(
            r#"per_km_rate = { value = 20.0, schema = { type = "number" } }"#,
            r#"per_km_rate = { value = 20.0 }"#,
        );
        let actions = compute(&text, &params(&text, Position::new(1, 2))).unwrap();
        let out = apply(&text, find(&actions, "Add schema to 'per_km_rate'"));

        assert!(out.contains(r#"schema = { type = "number" }"#), "{out}");
    }

    #[test]
    fn a_valid_document_offers_nothing_at_a_complete_entry() {
        assert!(compute(CLEAN, &params(CLEAN, Position::new(5, 2))).is_none());
    }

    #[test]
    fn a_document_without_a_dimensions_section_reports_a_syntax_error_instead() {
        // The section is mandatory, so this never becomes an "undeclared
        // dimension" that a quick fix could repair.
        let text = "[default-configs]\nrate = { value = 1, schema = { type = \"integer\" } }\n\n[[overrides]]\n_context_ = { city = \"Delhi\" }\nrate = 2\n";
        let diags = diagnostics::compute(text);
        assert_eq!(diags.len(), 1);
        assert_eq!(
            diags[0].code,
            Some(NumberOrString::String(diagnostics::CODE_SYNTAX.to_string()))
        );

        let offered = compute(text, &params(text, Position::new(4, 16)))
            .map(|a| titles(&a))
            .unwrap_or_default();
        assert!(
            !offered.iter().any(|t| t.contains("Declare dimension")),
            "{offered:?}"
        );
    }

    #[test]
    fn offers_to_move_each_dimension_out_of_a_position_clash() {
        // city and vehicle_type both sit at position 1.
        let text = CLEAN.replace(
            r#"vehicle_type = { position = 2, schema = { type = "string" } }"#,
            r#"vehicle_type = { position = 1, schema = { type = "string" } }"#,
        );
        let diags = diagnostics::compute(&text);
        assert_eq!(
            diags[0].code,
            Some(NumberOrString::String(
                diagnostics::CODE_DUPLICATE_POSITION.to_string()
            )),
            "{}",
            diags[0].message
        );

        let actions = compute(&text, &params(&text, Position::new(5, 2))).unwrap();
        let offered = titles(&actions);
        // One per clashing dimension, both targeting the next free position.
        assert!(
            offered.iter().any(|t| t == "Move 'city' to position 2"),
            "{offered:?}"
        );
        assert!(
            offered
                .iter()
                .any(|t| t == "Move 'vehicle_type' to position 2"),
            "{offered:?}"
        );
    }

    #[test]
    fn moving_a_dimension_resolves_the_clash() {
        let text = CLEAN.replace(
            r#"vehicle_type = { position = 2, schema = { type = "string" } }"#,
            r#"vehicle_type = { position = 1, schema = { type = "string" } }"#,
        );
        let actions = compute(&text, &params(&text, Position::new(5, 2))).unwrap();
        let out = apply(&text, find(&actions, "Move 'vehicle_type' to position 2"));

        assert!(
            out.contains(
                r#"vehicle_type = { position = 2, schema = { type = "string" } }"#
            ),
            "{out}"
        );
        // city keeps its own position, and the document now validates.
        assert!(out.contains(r#"city = { position = 1,"#), "{out}");
        assert!(diagnostics::compute(&out).is_empty());
    }

    #[test]
    fn the_renumber_edit_targets_the_integer_not_the_key() {
        let text = CLEAN.replace(
            r#"vehicle_type = { position = 2, schema = { type = "string" } }"#,
            r#"vehicle_type = { position = 1, schema = { type = "string" } }"#,
        );
        let actions = compute(&text, &params(&text, Position::new(5, 2))).unwrap();
        let CodeActionOrCommand::CodeAction(action) =
            find(&actions, "Move 'vehicle_type'")
        else {
            panic!("expected a code action");
        };
        let edits = action.edit.as_ref().unwrap().changes.as_ref().unwrap();
        let edit = &edits.get(&uri()).unwrap()[0];

        assert_eq!(edit.new_text, "2");
        let line = text.lines().nth(edit.range.start.line as usize).unwrap();
        let replaced =
            &line[edit.range.start.character as usize..edit.range.end.character as usize];
        assert_eq!(replaced, "1", "should replace the position value only");
    }

    /// A cohort whose base sits *below* it, which the core rejects.
    ///
    /// The cohort meta-schema requires `definitions` nested inside `schema`
    /// and an `otherwise` member in the enum, so this fixture is shaped to be
    /// valid in every respect except the position ordering under test.
    const BAD_COHORT: &str = r#"[default-configs]
rate = { value = 1, schema = { type = "integer" } }

[dimensions]
city = { position = 1, schema = { type = "string", enum = ["Delhi", "Pune"] } }
city_tier = { position = 2, type = "LOCAL_COHORT:city", schema = { type = "string", enum = ["metro", "otherwise"], definitions = { metro = { in = [{ var = "city" }, ["Delhi"]] } } } }

[[overrides]]
_context_ = { city = "Delhi" }
rate = 2
"#;

    #[test]
    fn offers_to_swap_a_cohort_against_its_base() {
        let diags = diagnostics::compute(BAD_COHORT);
        assert_eq!(
            diags[0].code,
            Some(NumberOrString::String(
                diagnostics::CODE_COHORT_POSITION.to_string()
            )),
            "{}",
            diags[0].message
        );

        let actions =
            compute(BAD_COHORT, &params(BAD_COHORT, Position::new(5, 2))).unwrap();
        let offered = titles(&actions);
        assert!(
            offered
                .iter()
                .any(|t| t == "Swap positions of 'city' and 'city_tier'"),
            "{offered:?}"
        );
    }

    #[test]
    fn swapping_exchanges_both_positions_and_clears_the_error() {
        let actions =
            compute(BAD_COHORT, &params(BAD_COHORT, Position::new(5, 2))).unwrap();
        let out = apply(BAD_COHORT, find(&actions, "Swap positions"));

        assert!(out.contains("city = { position = 2,"), "{out}");
        assert!(out.contains("city_tier = { position = 1,"), "{out}");
        // The swap must not have created a duplicate-position clash.
        assert!(
            diagnostics::compute(&out).is_empty(),
            "{:?}",
            diagnostics::compute(&out).first().map(|d| &d.message)
        );
    }

    #[test]
    fn value_token_reads_scalars_and_stops_at_delimiters() {
        assert_eq!(value_token(r#""Mumbai" }"#).as_deref(), Some(r#""Mumbai""#));
        assert_eq!(value_token("25.0\n").as_deref(), Some("25.0"));
        assert_eq!(value_token("true, x = 1").as_deref(), Some("true"));
        assert_eq!(value_token("1 # trailing").as_deref(), Some("1"));
    }

    #[test]
    fn value_token_keeps_nested_values_whole() {
        // A scan to the first `,` or `}` would truncate these.
        assert_eq!(value_token("[1, 2] }").as_deref(), Some("[1, 2]"));
        assert_eq!(
            value_token(r#"["a", "b"] }"#).as_deref(),
            Some(r#"["a", "b"]"#)
        );
        assert_eq!(value_token("{ a = 1 } }").as_deref(), Some("{ a = 1 }"));
        assert_eq!(
            value_token("[[1, 2], [3]] }").as_deref(),
            Some("[[1, 2], [3]]")
        );
        // Delimiters inside strings are not delimiters.
        assert_eq!(value_token(r#""a, b" }"#).as_deref(), Some(r#""a, b""#));
        assert_eq!(value_token(r#""x}y" }"#).as_deref(), Some(r#""x}y""#));
        // A half-typed string is not a usable value.
        assert_eq!(value_token(r#""unterminated"#), None);
    }

    /// The type written into a generated schema for the entry on `line`.
    fn schema_type_for(text: &str, line: u32) -> String {
        let actions = compute(text, &params(text, Position::new(line, 2)))
            .unwrap_or_else(|| panic!("no actions for line {line}"));
        let out = apply(text, find(&actions, "Add schema to"));
        let at = out.lines().nth(line as usize).unwrap();
        at.split("type = ")
            .nth(1)
            .unwrap()
            .split(" }")
            .next()
            .unwrap()
            .to_string()
    }

    #[test]
    fn schema_type_follows_the_value_for_every_toml_type() {
        for (value, expected) in [
            ("3", r#""integer""#),
            ("3.5", r#""number""#),
            ("true", r#""boolean""#),
            (r#""three""#, r#""string""#),
            ("[1, 2]", r#""array""#),
            (r#"["a", "b"]"#, r#""array""#),
            ("{ a = 1 }", r#""object""#),
        ] {
            let text = CLEAN.replace(
                r#"per_km_rate = { value = 20.0, schema = { type = "number" } }"#,
                &format!("per_km_rate = {{ value = {value} }}"),
            );
            assert_eq!(schema_type_for(&text, 1), expected, "value = {value}");
        }
    }

    #[test]
    fn changing_the_value_changes_the_generated_schema_type() {
        // The reported case: a value edited from string to integer must not
        // keep producing a string schema.
        let as_string = CLEAN.replace(
            r#"per_km_rate = { value = 20.0, schema = { type = "number" } }"#,
            r#"per_km_rate = { value = "20" }"#,
        );
        let as_integer = CLEAN.replace(
            r#"per_km_rate = { value = 20.0, schema = { type = "number" } }"#,
            r#"per_km_rate = { value = 20 }"#,
        );
        assert_eq!(schema_type_for(&as_string, 1), r#""string""#);
        assert_eq!(schema_type_for(&as_integer, 1), r#""integer""#);
    }

    #[test]
    fn dimension_schema_type_is_learned_from_context_usage() {
        // A dimension has no `value`, so the type comes from how it is used.
        let text = CLEAN
            .replace(
                r#"vehicle_type = { position = 2, schema = { type = "string" } }"#,
                "seats = { position = 2 }",
            )
            .replace(
                r#"_context_ = { city = "Bangalore" }"#,
                r#"_context_ = { seats = 4 }"#,
            );
        assert_eq!(schema_type_for(&text, 6), r#""integer""#);
    }

    #[test]
    fn an_unused_dimension_falls_back_to_string() {
        let text = CLEAN.replace(
            r#"vehicle_type = { position = 2, schema = { type = "string" } }"#,
            "seats = { position = 2 }",
        );
        assert_eq!(schema_type_for(&text, 6), r#""string""#);
    }

    #[test]
    fn an_entry_that_already_has_a_schema_is_left_alone() {
        let offered = compute(CLEAN, &params(CLEAN, Position::new(1, 2)))
            .map(|a| titles(&a))
            .unwrap_or_default();
        assert!(
            !offered.iter().any(|t| t.contains("Add schema")),
            "{offered:?}"
        );
    }

    #[test]
    fn infers_json_schema_types_from_toml_tokens() {
        assert_eq!(infer_type(r#""x""#), Some("string"));
        assert_eq!(infer_type("1"), Some("integer"));
        assert_eq!(infer_type("1.5"), Some("number"));
        assert_eq!(infer_type("true"), Some("boolean"));
        assert_eq!(infer_type("[1, 2]"), Some("array"));
    }
}
