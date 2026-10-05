//! Multi-file support: a `main.stoml` and the typed files it imports form a
//! group that is checked together. Errors land in the file they come from,
//! and every file in the group can complete and hover the dimensions and
//! config keys defined anywhere in it.

use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};

use superposition_core::format::toml::{
    MAIN_FILE_NAMES, Section, SourceLoader, is_main_file, parse_toml_file_detailed,
    resolve_toml_imports,
};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Range};

use crate::{diagnostics, utils};

/// What role a file plays, judged by its name.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileKind {
    /// `main.stoml` / `main.super.toml`: may import.
    Main,
    /// `<name>.<section>.stoml`: imported by a main file, holds one section.
    Imported(Section),
    /// Any other SuperTOML file: checked on its own.
    Other,
}

impl FileKind {
    pub fn of(path: &Path) -> Self {
        if is_main_file(path) {
            FileKind::Main
        } else if let Some(section) = Section::of_file(path) {
            FileKind::Imported(section)
        } else {
            FileKind::Other
        }
    }
}

/// Reads open editor buffers first, so unsaved edits count, then falls back.
pub struct Overlay<'a> {
    pub open: HashMap<PathBuf, String>,
    pub fallback: &'a dyn SourceLoader,
}

impl SourceLoader for Overlay<'_> {
    fn read(&self, path: &Path) -> io::Result<String> {
        match self.open.get(path) {
            Some(text) => Ok(text.clone()),
            None => self.fallback.read(path),
        }
    }
}

/// The nearest main file, in `file`'s folder or above, that imports `file`.
/// Imports can't use `..`, so a main file can only sit in a parent folder.
/// Stops after a folder holding `.git`.
pub fn find_main(file: &Path, loader: &dyn SourceLoader) -> Option<PathBuf> {
    for dir in file.ancestors().skip(1) {
        for name in MAIN_FILE_NAMES {
            let main = dir.join(name);
            let Ok(src) = loader.read(&main) else {
                continue;
            };
            let imports_file = match resolve_toml_imports(&main, &src) {
                Ok(plan) => plan.imported_files().any(|import| import.path == file),
                // A broken import line elsewhere in main shouldn't orphan
                // this file: fall back to looking for it as a quoted entry.
                Err(_) => file
                    .strip_prefix(dir)
                    .is_ok_and(|rel| lists_import(&src, rel)),
            };
            if imports_file {
                return Some(main);
            }
        }
        if dir.join(".git").exists() {
            break;
        }
    }
    None
}

/// Whether `src` has `rel` as a quoted string (`"rel"`, `"./rel"`, or the
/// single-quoted forms) outside a comment: what an import entry looks like.
/// Used only when main's imports don't resolve, so this can't use the plan.
fn lists_import(src: &str, rel: &Path) -> bool {
    let Some(parts) = rel
        .components()
        .map(|part| part.as_os_str().to_str())
        .collect::<Option<Vec<_>>>()
    else {
        return false;
    };
    let rel = parts.join("/");
    let entries: Vec<String> = ['"', '\'']
        .into_iter()
        .flat_map(|q| [format!("{q}{rel}{q}"), format!("{q}./{rel}{q}")])
        .collect();
    src.lines()
        .map(|line| line.split('#').next().unwrap_or_default())
        .any(|code| entries.iter().any(|entry| code.contains(entry.as_str())))
}

/// The main file whose group to re-check when `path` is closed, so its
/// diagnostics stay (from disk now that unsaved edits are gone). `owner` is
/// the main file known to import `path`. `None` means just clear `path`'s
/// diagnostics: an imported file with no known owner, or any other file.
pub fn main_to_recheck_on_close(path: &Path, owner: Option<PathBuf>) -> Option<PathBuf> {
    match FileKind::of(path) {
        // Even when its imports don't resolve, so the error stays visible.
        FileKind::Main => Some(path.to_path_buf()),
        FileKind::Imported(_) => owner,
        FileKind::Other => None,
    }
}

/// The result of checking a main file and everything it imports.
#[derive(Debug)]
pub struct GroupCheck {
    /// The main file first, then each imported file.
    pub files: Vec<PathBuf>,
    /// Diagnostics for every file in `files` (empty when it's clean).
    pub diagnostics: HashMap<PathBuf, Vec<Diagnostic>>,
    /// The dimensions and default configs defined across the group, for
    /// completion and hover.
    pub schema: toml::Table,
}

/// Check `main` (any file that isn't an imported one) together with the files
/// it imports.
pub fn check_group(main: &Path, loader: &dyn SourceLoader) -> GroupCheck {
    let text = |path: &Path| loader.read(path).unwrap_or_default();

    let mut files = vec![main.to_path_buf()];
    if let Ok(plan) = resolve_toml_imports(main, &text(main)) {
        files.extend(plan.imported_files().map(|import| import.path.clone()));
    }
    let mut diagnostics: HashMap<PathBuf, Vec<Diagnostic>> = files
        .iter()
        .map(|file| (file.clone(), Vec::new()))
        .collect();

    if let Err(err) = parse_toml_file_detailed(main, loader) {
        let (file, inner) = err.location();
        let mut target = file.unwrap_or(main).to_path_buf();
        let mut diagnostic = diagnostics::diagnostic_for(&text(&target), inner);

        // An error tied to no file (e.g. a cohort naming a missing
        // dimension): show it in the first file that mentions its token.
        if file.is_none() && diagnostic.range == Range::default() {
            let found = files.iter().skip(1).find_map(|file| {
                let range = diagnostics::find_error_range(&text(file), inner);
                (range != Range::default()).then(|| (file.clone(), range))
            });
            if let Some((file, range)) = found {
                target = file;
                diagnostic.range = range;
            }
        }
        diagnostics.entry(target).or_default().push(diagnostic);
    }

    let tables: Vec<toml::Table> = files
        .iter()
        .filter_map(|file| toml::from_str(&text(file)).ok())
        .collect();
    let schema = utils::merge_schema(&tables);

    GroupCheck {
        files,
        diagnostics,
        schema,
    }
}

/// Diagnostics for an imported-style file that no main file imports: TOML
/// syntax, the one-section rule, and a warning saying why nothing else is
/// checked.
pub fn check_orphan(path: &Path, section: Section, text: &str) -> Vec<Diagnostic> {
    let mut out = Vec::new();
    match toml::from_str::<toml::Table>(text) {
        Err(e) => out.push(diagnostics::error(
            e.span()
                .map(|span| diagnostics::byte_span_to_range(text, span))
                .unwrap_or_default(),
            e.message().to_string(),
        )),
        Ok(raw) => {
            if let Some(other) = raw.keys().find(|key| *key != section.key()) {
                let name = path
                    .file_name()
                    .map(|name| name.to_string_lossy().into_owned())
                    .unwrap_or_default();
                out.push(diagnostics::error(
                    diagnostics::find_text_range(text, other),
                    format!(
                        "`{}` may only contain {}, but it also defines `{}`",
                        name,
                        section.header(),
                        other
                    ),
                ));
            }
        }
    }
    out.push(Diagnostic {
        range: Range::default(),
        severity: Some(DiagnosticSeverity::WARNING),
        source: Some("supertoml-analyzer".to_string()),
        message: format!(
            "Not imported by any {} in this folder or above, so only TOML syntax is checked. Add it to `{}.import` in main to check it fully.",
            MAIN_FILE_NAMES.join(" or "),
            section.key()
        ),
        ..Default::default()
    });
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn files(entries: &[(&str, &str)]) -> HashMap<PathBuf, String> {
        entries
            .iter()
            .map(|(path, text)| (PathBuf::from(path), text.to_string()))
            .collect()
    }

    const MAIN: &str = r#"
default-configs.import = ["pricing.default-configs.stoml"]
dimensions.import = ["geo.dimensions.stoml"]
overrides.import = ["city/delhi.overrides.stoml"]
"#;
    const PRICING: &str = "[default-configs]\nper_km_rate = { value = 20.0, schema = { type = \"number\" } }\n";
    const GEO: &str =
        "[dimensions]\ncity = { position = 1, schema = { type = \"string\" } }\n";
    const DELHI: &str =
        "[[overrides]]\n_context_ = { city = \"Delhi\" }\nper_km_rate = 21.0\n";

    fn group() -> HashMap<PathBuf, String> {
        files(&[
            ("/ws/main.stoml", MAIN),
            ("/ws/pricing.default-configs.stoml", PRICING),
            ("/ws/geo.dimensions.stoml", GEO),
            ("/ws/city/delhi.overrides.stoml", DELHI),
        ])
    }

    #[test]
    fn file_kinds() {
        assert_eq!(FileKind::of(Path::new("/a/main.stoml")), FileKind::Main);
        assert_eq!(
            FileKind::of(Path::new("/a/geo.dimensions.super.toml")),
            FileKind::Imported(Section::Dimensions)
        );
        assert_eq!(
            FileKind::of(Path::new("/a/config.super.toml")),
            FileKind::Other
        );
    }

    #[test]
    fn finds_the_main_file_in_a_parent_folder() {
        let loader = group();
        assert_eq!(
            find_main(Path::new("/ws/city/delhi.overrides.stoml"), &loader),
            Some(PathBuf::from("/ws/main.stoml"))
        );
        assert_eq!(
            find_main(Path::new("/ws/city/blr.overrides.stoml"), &loader),
            None
        );
    }

    #[test]
    fn the_nearest_main_that_imports_the_file_wins() {
        let mut loader = group();
        // A main closer to the file that doesn't import it is skipped...
        loader.insert(
            PathBuf::from("/ws/city/main.stoml"),
            "dimensions.import = [\"other.dimensions.stoml\"]\n".to_string(),
        );
        assert_eq!(
            find_main(Path::new("/ws/city/delhi.overrides.stoml"), &loader),
            Some(PathBuf::from("/ws/main.stoml"))
        );
        // ...and one that does import it wins.
        loader.insert(
            PathBuf::from("/ws/city/main.stoml"),
            "overrides.import = [\"delhi.overrides.stoml\"]\n".to_string(),
        );
        assert_eq!(
            find_main(Path::new("/ws/city/delhi.overrides.stoml"), &loader),
            Some(PathBuf::from("/ws/city/main.stoml"))
        );
    }

    #[test]
    fn a_main_with_a_broken_import_still_owns_files_it_lists() {
        let child = Path::new("/ws/city/delhi.overrides.stoml");
        let found = |main: &str| {
            let loader = files(&[("/ws/main.stoml", main)]);
            find_main(child, &loader)
        };
        // `../x` makes the imports fail to resolve, so the fallback runs.
        let broken = "dimensions.import = [\"../x.dimensions.stoml\"]\n";

        for listed in [
            "overrides.import = [\"city/delhi.overrides.stoml\"]",
            "overrides.import = ['./city/delhi.overrides.stoml']",
        ] {
            assert_eq!(
                found(&format!("{broken}{listed}\n")),
                Some(PathBuf::from("/ws/main.stoml")),
                "{listed}"
            );
        }
        for not_listed in [
            "overrides.import = [\"xcity/delhi.overrides.stoml\"]",
            "overrides.import = [\"city/delhi.overrides.stoml.bak\"]",
            "# overrides.import = [\"city/delhi.overrides.stoml\"]",
            "overrides.import = []  # \"city/delhi.overrides.stoml\"",
        ] {
            assert_eq!(
                found(&format!("{broken}{not_listed}\n")),
                None,
                "{not_listed}"
            );
        }
    }

    #[test]
    fn closing_a_file_rechecks_its_group_unless_it_has_none() {
        let main = PathBuf::from("/ws/main.stoml");
        // A main file is re-checked even if its imports didn't resolve.
        assert_eq!(main_to_recheck_on_close(&main, None), Some(main.clone()));
        let child = Path::new("/ws/city/delhi.overrides.stoml");
        assert_eq!(
            main_to_recheck_on_close(child, Some(main.clone())),
            Some(main)
        );
        assert_eq!(main_to_recheck_on_close(child, None), None);
        assert_eq!(
            main_to_recheck_on_close(Path::new("/ws/config.super.toml"), None),
            None
        );
    }

    #[test]
    fn a_clean_group_has_empty_diagnostics_for_every_file() {
        let check = check_group(Path::new("/ws/main.stoml"), &group());
        assert_eq!(check.files.len(), 4);
        assert!(check.diagnostics.values().all(Vec::is_empty));
        assert_eq!(check.diagnostics.len(), 4);
    }

    #[test]
    fn an_error_in_an_imported_file_lands_in_that_file() {
        let mut loader = group();
        loader.insert(
            PathBuf::from("/ws/city/delhi.overrides.stoml"),
            DELHI.replace("per_km_rate", "base_fare"),
        );
        let check = check_group(Path::new("/ws/main.stoml"), &loader);
        let delhi = &check.diagnostics[Path::new("/ws/city/delhi.overrides.stoml")];
        assert_eq!(delhi.len(), 1);
        assert!(
            delhi[0].message.contains("base_fare"),
            "{}",
            delhi[0].message
        );
        assert_eq!(delhi[0].range.start.line, 2);
        assert!(check.diagnostics[Path::new("/ws/main.stoml")].is_empty());
    }

    #[test]
    fn an_import_error_lands_on_the_import_line_in_main() {
        let mut loader = group();
        loader.remove(Path::new("/ws/geo.dimensions.stoml"));
        let check = check_group(Path::new("/ws/main.stoml"), &loader);
        let main = &check.diagnostics[Path::new("/ws/main.stoml")];
        assert_eq!(main.len(), 1);
        assert!(
            main[0].message.starts_with("can't find"),
            "{}",
            main[0].message
        );
        assert_eq!(main[0].range.start.line, 2);
    }

    #[test]
    fn open_buffers_win_over_disk() {
        let disk = group();
        let overlay = Overlay {
            open: files(&[(
                "/ws/geo.dimensions.stoml",
                "[dimensions]\ncity = { position = \"one\", schema = { type = \"string\" } }\n",
            )]),
            fallback: &disk,
        };
        let check = check_group(Path::new("/ws/main.stoml"), &overlay);
        assert_eq!(
            check.diagnostics[Path::new("/ws/geo.dimensions.stoml")].len(),
            1
        );
    }

    #[test]
    fn the_group_schema_spans_all_files() {
        let check = check_group(Path::new("/ws/main.stoml"), &group());
        let dims = check.schema["dimensions"].as_table().unwrap();
        assert!(dims.contains_key("city"));
        assert!(!dims.contains_key("import"));
        assert!(
            check.schema["default-configs"]
                .as_table()
                .unwrap()
                .contains_key("per_km_rate")
        );
    }

    #[test]
    fn orphans_get_a_warning_and_syntax_checks() {
        let path = Path::new("/ws/blr.overrides.stoml");
        let diags = check_orphan(path, Section::Overrides, DELHI);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].severity, Some(DiagnosticSeverity::WARNING));

        let diags = check_orphan(path, Section::Overrides, &format!("{GEO}{DELHI}"));
        assert_eq!(diags.len(), 2);
        assert!(diags[0].message.contains("may only contain [[overrides]]"));
    }
}
