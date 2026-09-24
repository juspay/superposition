use tower_lsp::lsp_types::*;

use crate::{diagnostics, utils};

pub(crate) const DIMENSIONS: &str = "dimensions";
pub(crate) const DEFAULT_CONFIGS: &str = "default-configs";
/// The parser accepts the singular spelling too, so navigation must as well.
pub(crate) const DEFAULT_CONFIG_ALT: &str = "default-config";

/// A name together with the section its declaration lives in.
#[derive(Debug, PartialEq)]
struct Target {
    section: &'static str,
    name: String,
}

/// Resolve the declaration site of the symbol under the cursor.
///
/// Three jumps are supported:
/// - a dimension name inside `_context_ = { … }` → its `[dimensions]` entry
/// - an override key in an `[[overrides]]` block → its `[default-configs]` entry
/// - a cohort reference in `type = "LOCAL_COHORT:<dim>"` → `<dim>`'s entry
pub fn compute(text: &str, pos: Position, uri: &Url) -> Option<GotoDefinitionResponse> {
    if utils::is_inside_comment(text, pos) {
        return None;
    }

    let lines: Vec<&str> = text.lines().collect();
    let line = lines.get(pos.line as usize)?;
    let col = pos.character as usize;

    let target = resolve_target(&lines, line, pos.line as usize, col)?;
    let range = declaration_range(text, &target)?;

    Some(GotoDefinitionResponse::Scalar(Location {
        uri: uri.clone(),
        range,
    }))
}

/// Work out what the cursor is pointing at, if anything navigable.
fn resolve_target(
    lines: &[&str],
    line: &str,
    line_num: usize,
    col: usize,
) -> Option<Target> {
    // A cohort reference can appear anywhere a dimension `type` is set, so it
    // is checked before the section-based cases.
    if let Some(name) = cohort_reference(line, col) {
        return Some(Target {
            section: DIMENSIONS,
            name,
        });
    }

    let word = utils::extract_word(line, col)?;
    let section = utils::section_at_line(lines, line_num)?;

    if section != "overrides" {
        return None;
    }

    if in_context_table(line, col) {
        // Inside `_context_`, keys are dimension names; values are not.
        utils::is_key_at(line, col).then(|| Target {
            section: DIMENSIONS,
            name: word.to_string(),
        })
    } else if word != "_context_" && utils::is_key_at(line, col) {
        Some(Target {
            section: DEFAULT_CONFIGS,
            name: word.to_string(),
        })
    } else {
        None
    }
}

/// Extract the dimension named by a `LOCAL_COHORT:`/`REMOTE_COHORT:` reference,
/// but only when the cursor actually sits on that name.
fn cohort_reference(line: &str, col: usize) -> Option<String> {
    let marker = line
        .find("LOCAL_COHORT:")
        .map(|i| i + "LOCAL_COHORT:".len())
        .or_else(|| {
            line.find("REMOTE_COHORT:")
                .map(|i| i + "REMOTE_COHORT:".len())
        })?;

    // The reference runs to the closing quote of the enclosing string.
    let end = line[marker..]
        .find(['"', '\''])
        .map(|i| marker + i)
        .unwrap_or(line.len());

    let name = line[marker..end].trim();
    (col >= marker && col <= end && !name.is_empty()).then(|| name.to_string())
}

/// Is the cursor inside the inline table of a `_context_ = { … }` line?
fn in_context_table(line: &str, col: usize) -> bool {
    match line.find('{') {
        Some(brace) => line.trim_start().starts_with("_context_") && col > brace,
        None => false,
    }
}

/// Locate a key's declaration within its section.
fn declaration_range(text: &str, target: &Target) -> Option<Range> {
    let section_start = diagnostics::find_table_section_start(text, target.section)
        .or_else(|| {
            (target.section == DEFAULT_CONFIGS)
                .then(|| diagnostics::find_table_section_start(text, DEFAULT_CONFIG_ALT))
                .flatten()
        })?;
    let section_end =
        diagnostics::find_next_section_start(text, section_start).unwrap_or(text.len());

    diagnostics::find_key_assignment_range(text, section_start, section_end, &target.name)
}

#[cfg(test)]
mod tests {
    use super::*;

    const DOC: &str = r#"[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number" } }
currency = { value = "INR", schema = { type = "string" } }

[dimensions]
city = { position = 1, schema = { type = "string" } }
vehicle_type = { position = 2, schema = { type = "string" } }

[[overrides]]
_context_ = { city = "Bangalore" }
per_km_rate = 25.0
"#;

    fn goto(text: &str, line: u32, character: u32) -> Option<Range> {
        let uri = Url::parse("file:///test.super.toml").unwrap();
        match compute(text, Position::new(line, character), &uri) {
            Some(GotoDefinitionResponse::Scalar(loc)) => Some(loc.range),
            _ => None,
        }
    }

    #[test]
    fn jumps_from_context_dimension_to_its_declaration() {
        // `city` inside _context_ on line 9 → [dimensions] city on line 5
        let range = goto(DOC, 9, 15).expect("expected a definition");
        assert_eq!(range.start.line, 5);
        assert_eq!(range.start.character, 0);
        assert_eq!(range.end.character, 4);
    }

    #[test]
    fn jumps_from_override_key_to_default_config() {
        // `per_km_rate` override on line 10 → [default-configs] on line 1
        let range = goto(DOC, 10, 3).expect("expected a definition");
        assert_eq!(range.start.line, 1);
        assert_eq!(range.start.character, 0);
    }

    #[test]
    fn context_values_are_not_navigable() {
        // "Bangalore" is a value, not a dimension name
        assert_eq!(goto(DOC, 9, 24), None);
    }

    #[test]
    fn context_marker_itself_is_not_navigable() {
        assert_eq!(goto(DOC, 9, 3), None);
    }

    #[test]
    fn unknown_names_resolve_to_nothing() {
        let text = DOC.replace("per_km_rate = 25.0", "not_a_config = 25.0");
        assert_eq!(goto(&text, 10, 3), None);
    }

    #[test]
    fn comments_are_ignored() {
        let text = format!("{}\n# per_km_rate = 25.0\n", DOC);
        assert_eq!(goto(&text, 12, 5), None);
    }

    #[test]
    fn jumps_from_cohort_reference_to_base_dimension() {
        let text = r#"[dimensions]
city = { position = 1, schema = { type = "string" } }
city_cohort = { position = 2, type = "LOCAL_COHORT:city", schema = { type = "string" } }
"#;
        let col = text
            .lines()
            .nth(2)
            .unwrap()
            .find("LOCAL_COHORT:city")
            .unwrap()
            + "LOCAL_COHORT:".len()
            + 1;
        let range = goto(text, 2, col as u32).expect("expected a definition");
        assert_eq!(range.start.line, 1);
        assert_eq!(range.start.character, 0);
    }

    #[test]
    fn singular_default_config_section_is_supported() {
        let text = DOC.replace("[default-configs]", "[default-config]");
        let range = goto(&text, 10, 3).expect("expected a definition");
        assert_eq!(range.start.line, 1);
    }

    #[test]
    fn resolve_target_picks_the_right_section() {
        let lines: Vec<&str> = DOC.lines().collect();
        assert_eq!(
            resolve_target(&lines, lines[9], 9, 15),
            Some(Target {
                section: DIMENSIONS,
                name: "city".to_string()
            })
        );
        assert_eq!(
            resolve_target(&lines, lines[10], 10, 3),
            Some(Target {
                section: DEFAULT_CONFIGS,
                name: "per_km_rate".to_string()
            })
        );
    }
}
