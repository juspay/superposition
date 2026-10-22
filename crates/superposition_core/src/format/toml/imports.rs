//! Imports: `main.stoml` (or `main.super.toml`) pulling whole sections from
//! typed files.
//!
//! ```toml
//! # main.stoml: import lines go before any [section]
//! dimensions.import      = ["geo.dimensions.stoml"]
//! default-configs.import = ["pricing.default-configs.stoml"]
//! overrides.import       = ["surge.overrides.stoml", "city/blr.overrides.stoml"]
//! ```
//!
//! - Only `main.stoml` / `main.super.toml` may import, one level deep.
//! - A section that imports may define nothing else; a section without
//!   `import` stays inline in main.
//! - Imported files are typed by name (`<stem>.dimensions.stoml`,
//!   `<stem>.default-configs.stoml`, `<stem>.overrides.stoml`, or the
//!   `.super.toml` forms) and hold only their own section.
//! - Paths are relative to main's folder and can't leave it.
//!
//! The merged result is the same as one file holding every section, with
//! overrides in import order.

use std::collections::{BTreeMap, HashMap};
use std::io;
use std::ops::Range;
use std::path::{Component, Path, PathBuf};

use serde::Deserialize;
use superposition_types::{
    Config, DefaultConfigsWithSchema, DetailedConfig, DimensionInfo,
};
use toml::Spanned;

use super::{
    convert_dimension, fill_default_config_descriptions, parse_detailed_str,
    split_context, ContextToml, DimensionInfoToml, TomlFormat,
};
use crate::format::{
    build_contexts, finalize_contexts, validate_default_configs, validate_dimensions,
    ConfigFormat, FormatError,
};

/// The only file names that may declare imports.
pub const MAIN_FILE_NAMES: [&str; 2] = ["main.stoml", "main.super.toml"];

const EXTENSIONS: [&str; 2] = ["stoml", "super.toml"];

/// A top-level section of a SuperTOML file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Section {
    DefaultConfigs,
    Dimensions,
    Overrides,
}

impl Section {
    /// In processing order: overrides are checked against the other two.
    pub const ALL: [Section; 3] = [
        Section::DefaultConfigs,
        Section::Dimensions,
        Section::Overrides,
    ];

    /// The section's key, e.g. `dimensions`.
    pub fn key(self) -> &'static str {
        match self {
            Section::DefaultConfigs => "default-configs",
            Section::Dimensions => "dimensions",
            Section::Overrides => "overrides",
        }
    }

    /// The section's header as written in a file, e.g. `[dimensions]`.
    pub fn header(self) -> &'static str {
        match self {
            Section::DefaultConfigs => "[default-configs]",
            Section::Dimensions => "[dimensions]",
            Section::Overrides => "[[overrides]]",
        }
    }

    /// The section a typed file holds, judged by its name
    /// (`geo.dimensions.stoml` holds `Dimensions`). `None` for other names.
    pub fn of_file(path: &Path) -> Option<Section> {
        let name = path.file_name()?.to_str()?;
        Self::ALL.into_iter().find(|section| {
            EXTENSIONS.iter().any(|ext| {
                name.strip_suffix(&format!(".{}.{}", section.key(), ext))
                    .is_some_and(|stem| !stem.is_empty())
            })
        })
    }
}

/// Whether `path` names a file that may declare imports.
pub fn is_main_file(path: &Path) -> bool {
    path.file_name()
        .and_then(|name| name.to_str())
        .is_some_and(|name| MAIN_FILE_NAMES.contains(&name))
}

/// One entry of an `import` list.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportRef {
    /// The path as written in main.
    pub raw: String,
    /// The file's location: main's folder joined with the normalized path.
    pub path: PathBuf,
    /// Byte range of the path string in main.
    pub span: Range<usize>,
}

/// Where a section of the config comes from.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SectionSource {
    /// Not in main and not imported.
    Absent,
    /// Written in main itself.
    Inline,
    /// Imported from these files, in order.
    Import(Vec<ImportRef>),
}

/// What main imports, worked out from main alone without reading any other
/// file. Available even when parsing the imported files fails, so callers
/// can still watch them or route errors to them.
#[derive(Debug, Clone)]
pub struct ImportPlan {
    pub main: PathBuf,
    pub default_configs: SectionSource,
    pub dimensions: SectionSource,
    pub overrides: SectionSource,
}

impl ImportPlan {
    pub fn section(&self, section: Section) -> &SectionSource {
        match section {
            Section::DefaultConfigs => &self.default_configs,
            Section::Dimensions => &self.dimensions,
            Section::Overrides => &self.overrides,
        }
    }

    fn section_mut(&mut self, section: Section) -> &mut SectionSource {
        match section {
            Section::DefaultConfigs => &mut self.default_configs,
            Section::Dimensions => &mut self.dimensions,
            Section::Overrides => &mut self.overrides,
        }
    }

    pub fn has_imports(&self) -> bool {
        self.imported_files().next().is_some()
    }

    /// Every imported file, section by section in list order.
    pub fn imported_files(&self) -> impl Iterator<Item = &ImportRef> {
        Section::ALL
            .into_iter()
            .flat_map(move |section| match self.section(section) {
                SectionSource::Import(refs) => refs.as_slice(),
                SectionSource::Absent | SectionSource::Inline => &[],
            })
    }
}

/// Reads the files a config is made of. Lets the LSP serve unsaved editor
/// buffers and tests use in-memory files.
pub trait SourceLoader {
    fn read(&self, path: &Path) -> io::Result<String>;
}

/// Reads files from disk.
#[derive(Debug, Default, Clone, Copy)]
pub struct FsLoader;

impl SourceLoader for FsLoader {
    fn read(&self, path: &Path) -> io::Result<String> {
        std::fs::read_to_string(path)
    }
}

impl SourceLoader for HashMap<PathBuf, String> {
    fn read(&self, path: &Path) -> io::Result<String> {
        self.get(path).cloned().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::NotFound,
                format!("{} not found", path.display()),
            )
        })
    }
}

/// Parse a SuperTOML file from disk, following its imports.
pub fn parse_toml_file(path: &Path) -> Result<Config, FormatError> {
    parse_toml_file_detailed(path, &FsLoader).map(Config::from)
}

/// Parse a SuperTOML file, reading it and its imports through `loader`.
///
/// A file without imports parses exactly like [`super::parse_toml_config`].
/// Errors from an imported file come back as [`FormatError::InFile`]; errors
/// about the imports themselves as [`FormatError::ImportError`].
pub fn parse_toml_file_detailed(
    path: &Path,
    loader: &dyn SourceLoader,
) -> Result<DetailedConfig, FormatError> {
    let src = loader.read(path).map_err(|e| {
        import_error(
            Some(path),
            None,
            format!("can't read {}: {}", path.display(), e),
        )
    })?;
    let plan = resolve_toml_imports(path, &src)?;
    if !plan.has_imports() {
        return parse_detailed_str(&src, short_syntax_error);
    }
    parse_with_imports(&plan, &src, loader)
}

/// Work out what `main_src` (the text of `main_path`) imports, and check the
/// import lines: syntax, placement, nothing else in an importing section, and
/// the path rules. Reads no other file.
pub fn resolve_toml_imports(
    main_path: &Path,
    main_src: &str,
) -> Result<ImportPlan, FormatError> {
    let raw = toml::from_str::<toml::Table>(main_src)
        .map_err(|e| main_syntax_error(main_path, main_src, &e))?;
    if let Some(err) = misplaced_import(Some(main_path), main_src, &raw) {
        return Err(err);
    }

    let main_dir = main_path.parent().unwrap_or(Path::new(""));
    let mut plan = ImportPlan {
        main: main_path.to_path_buf(),
        default_configs: SectionSource::Absent,
        dimensions: SectionSource::Absent,
        overrides: SectionSource::Absent,
    };
    let mut seen: HashMap<PathBuf, Section> = HashMap::new();

    for section in Section::ALL {
        let source = match raw.get(section.key()) {
            None => SectionSource::Absent,
            Some(value) if !is_directive(value) => SectionSource::Inline,
            Some(value) => {
                let list = import_list(section, main_path, main_src, value)?;
                if !is_main_file(main_path) {
                    return Err(import_error(
                        Some(main_path),
                        Some(list.span()),
                        format!(
                            "only {} can import; `{}` can't",
                            MAIN_FILE_NAMES.join(" or "),
                            file_name(main_path)
                        ),
                    ));
                }

                let mut refs = Vec::new();
                for item in list.into_inner() {
                    let span = item.span();
                    let raw_path = item.into_inner();
                    let at = |message: String| {
                        import_error(Some(main_path), Some(span.clone()), message)
                    };

                    let rel = normalize_import_path(&raw_path).map_err(at)?;
                    if Section::of_file(&rel) != Some(section) {
                        return Err(at(format!(
                            "files under `{0}.import` must be named `<name>.{0}.stoml` (or `.{0}.super.toml`); `{1}` isn't",
                            section.key(),
                            raw_path
                        )));
                    }
                    if let Some(previous) = seen.insert(rel.clone(), section) {
                        let message = if previous == section {
                            format!("`{}` is imported twice", raw_path)
                        } else {
                            format!(
                                "`{}` is already imported under `{}.import`",
                                raw_path,
                                previous.key()
                            )
                        };
                        return Err(at(message));
                    }
                    refs.push(ImportRef {
                        path: main_dir.join(&rel),
                        raw: raw_path,
                        span,
                    });
                }
                SectionSource::Import(refs)
            }
        };
        *plan.section_mut(section) = source;
    }

    Ok(plan)
}

/// Turn an error from parsing a config passed as a string into a clearer one
/// when the text uses imports: a string has no path to resolve them against.
pub(super) fn explain_string_error(src: &str, err: FormatError) -> FormatError {
    let Ok(raw) = toml::from_str::<toml::Table>(src) else {
        return err;
    };
    if let Some(section) = Section::ALL
        .into_iter()
        .find(|section| raw.get(section.key()).is_some_and(is_directive))
    {
        let span = find_span(src, &format!("{}.import", section.key()))
            .or_else(|| find_span(src, "import"));
        return import_error(
            None,
            span,
            "imports need a file path: parse main.stoml with `parse_toml_file` instead of passing its contents as a string".to_string(),
        );
    }
    misplaced_import(None, src, &raw).unwrap_or(err)
}

/// A TOML syntax or type error, reported without a source snippet: the span
/// says where it is.
pub(super) fn short_syntax_error(e: toml::de::Error) -> FormatError {
    TomlFormat::syntax_error(e.message(), e.span())
}

fn import_error(
    file: Option<&Path>,
    span: Option<Range<usize>>,
    message: String,
) -> FormatError {
    FormatError::ImportError {
        file: file.map(Path::to_path_buf),
        span,
        message,
    }
}

/// Attach an error to the imported file it came from. `None` means main, whose
/// errors are returned as they are.
fn in_file(file: Option<&Path>, error: FormatError) -> FormatError {
    match (file, error) {
        (None, error) => error,
        (
            Some(_),
            error @ (FormatError::ImportError { .. } | FormatError::InFile { .. }),
        ) => error,
        (Some(file), error) => FormatError::InFile {
            file: file.to_path_buf(),
            error: Box::new(error),
        },
    }
}

fn file_name(path: &Path) -> String {
    path.file_name()
        .map(|name| name.to_string_lossy().into_owned())
        .unwrap_or_else(|| path.display().to_string())
}

/// `<section>.import = [...]` counts as an import only when its value isn't a
/// table: dimensions and default configs always have table values, so a
/// dimension named `import` stays a dimension.
fn is_directive(section: &toml::Value) -> bool {
    section
        .as_table()
        .and_then(|table| table.get("import"))
        .is_some_and(|import| !import.is_table())
}

/// A syntax error in main. TOML rejects `dimensions.import = [...]` followed by
/// a `[dimensions]` header (and `overrides.import` with `[[overrides]]`) as a
/// duplicate key; say why instead.
fn main_syntax_error(file: &Path, src: &str, e: &toml::de::Error) -> FormatError {
    let duplicate = Section::ALL.into_iter().find(|section| {
        e.message()
            .contains(&format!("duplicate key `{}`", section.key()))
            && src.contains(&format!("{}.import", section.key()))
    });
    if let Some(section) = duplicate {
        return import_error(
            Some(file),
            find_span(src, section.header()).or_else(|| e.span()),
            format!(
                "`{0}.import` replaces the whole {1} section, so this file can't also have a {1} table",
                section.key(),
                section.header()
            ),
        );
    }
    TomlFormat::syntax_error(e.message(), e.span())
}

/// TOML puts an import line written after a `[section]` header inside that
/// section, e.g. `[default-configs]` then `dimensions.import = [...]` defines a
/// default config named `dimensions`. Catch that before it turns into a
/// confusing schema error.
fn misplaced_import(
    file: Option<&Path>,
    src: &str,
    raw: &toml::Table,
) -> Option<FormatError> {
    let mut tables: Vec<(&str, &toml::Table)> =
        [Section::DefaultConfigs, Section::Dimensions]
            .into_iter()
            .filter_map(|section| {
                raw.get(section.key())
                    .and_then(toml::Value::as_table)
                    .map(|table| (section.header(), table))
            })
            .collect();
    if let Some(overrides) = raw
        .get(Section::Overrides.key())
        .and_then(toml::Value::as_array)
    {
        tables.extend(
            overrides
                .iter()
                .filter_map(toml::Value::as_table)
                .map(|table| (Section::Overrides.header(), table)),
        );
    }

    for (header, table) in tables {
        for section in Section::ALL {
            if table.get(section.key()).is_some_and(is_directive) {
                let line = format!("{}.import", section.key());
                return Some(import_error(
                    file,
                    find_span(src, &line),
                    format!(
                        "`{}` ended up inside {}: imports must be at the top of the file, before any [section]",
                        line, header
                    ),
                ));
            }
        }
    }
    None
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ImportTable {
    import: Spanned<Vec<Spanned<String>>>,
}

#[derive(Deserialize)]
struct DefaultConfigsImport {
    #[serde(rename = "default-configs")]
    section: ImportTable,
}

#[derive(Deserialize)]
struct DimensionsImport {
    #[serde(rename = "dimensions")]
    section: ImportTable,
}

#[derive(Deserialize)]
struct OverridesImport {
    #[serde(rename = "overrides")]
    section: ImportTable,
}

/// Read `<section>.import` from main's text so every path keeps its span.
fn import_list(
    section: Section,
    main_path: &Path,
    main_src: &str,
    value: &toml::Value,
) -> Result<Spanned<Vec<Spanned<String>>>, FormatError> {
    let parsed = match section {
        Section::DefaultConfigs => {
            toml::from_str::<DefaultConfigsImport>(main_src).map(|s| s.section)
        }
        Section::Dimensions => {
            toml::from_str::<DimensionsImport>(main_src).map(|s| s.section)
        }
        Section::Overrides => {
            toml::from_str::<OverridesImport>(main_src).map(|s| s.section)
        }
    };
    parsed.map(|table| table.import).map_err(|e| {
        let table = value.as_table();
        let is_path_list = table
            .and_then(|table| table.get("import"))
            .and_then(toml::Value::as_array)
            .is_some_and(|items| items.iter().all(toml::Value::is_str));
        let extra_key = table.and_then(|table| table.keys().find(|key| *key != "import"));
        let message = match (is_path_list, extra_key) {
            (false, _) => format!("`{}.import` must be a list of file paths", section.key()),
            (true, Some(key)) => format!(
                "`{0}.import` replaces the whole {1} section, so it can't also define `{2}`",
                section.key(),
                section.header(),
                key
            ),
            (true, None) => e.message().to_string(),
        };
        import_error(Some(main_path), e.span(), message)
    })
}

/// Check an import path and return it relative to main's folder, with `.`
/// segments dropped so `./a` and `a` compare equal.
fn normalize_import_path(raw: &str) -> Result<PathBuf, String> {
    if raw.trim().is_empty() {
        return Err("import path is empty".to_string());
    }
    if raw.contains('\\') {
        return Err(format!("use `/` in import paths, not `\\`: `{}`", raw));
    }
    let bytes = raw.as_bytes();
    let has_drive =
        bytes.len() >= 2 && bytes[0].is_ascii_alphabetic() && bytes[1] == b':';

    let mut normalized = PathBuf::new();
    for component in Path::new(raw).components() {
        match component {
            Component::Normal(part) if !has_drive => normalized.push(part),
            Component::CurDir => {}
            Component::ParentDir => {
                return Err(format!(
                    "`{}` leaves main's folder; imported files must be in the same folder as main or below it",
                    raw
                ))
            }
            Component::Normal(_) | Component::RootDir | Component::Prefix(_) => {
                return Err(format!(
                    "`{}` is an absolute path; use a path relative to main's folder",
                    raw
                ))
            }
        }
    }
    if normalized.as_os_str().is_empty() {
        return Err(format!("`{}` doesn't name a file", raw));
    }
    Ok(normalized)
}

/// Byte range of the first occurrence of `needle` in `src`.
fn find_span(src: &str, needle: &str) -> Option<Range<usize>> {
    src.find(needle).map(|start| start..start + needle.len())
}

/// Byte range of `key` where a line starts with it, as a key (`key = ...`,
/// `key.x = ...`) or a header (`[key]`, `[[key]]`, `[key.x]`).
fn find_key_span(src: &str, key: &str) -> Option<Range<usize>> {
    let mut offset = 0;
    for line in src.split_inclusive('\n') {
        let trimmed = line.trim_start();
        let rest = trimmed.trim_start_matches('[');
        let start = offset + (line.len() - rest.len());
        let follows_key = rest
            .strip_prefix(key)
            .and_then(|after| after.chars().next())
            .is_some_and(|c| matches!(c, ']' | '.' | '=') || c.is_whitespace());
        if follows_key {
            return Some(start..start + key.len());
        }
        offset += line.len();
    }
    None
}

/// Read an imported file and check it holds only its own section and no
/// imports of its own.
fn load_imported(
    loader: &dyn SourceLoader,
    main: &Path,
    import: &ImportRef,
    section: Section,
) -> Result<String, FormatError> {
    let src = loader.read(&import.path).map_err(|e| {
        let message = if e.kind() == io::ErrorKind::NotFound {
            format!(
                "can't find `{}` (looked for {})",
                import.raw,
                import.path.display()
            )
        } else {
            format!("can't read `{}`: {}", import.raw, e)
        };
        import_error(Some(main), Some(import.span.clone()), message)
    })?;

    let file = import.path.as_path();
    let raw = toml::from_str::<toml::Table>(&src)
        .map_err(|e| in_file(Some(file), short_syntax_error(e)))?;
    if let Some(other) = raw.keys().find(|key| *key != section.key()) {
        return Err(import_error(
            Some(file),
            find_key_span(&src, other),
            format!(
                "`{}` may only contain {}, but it also defines `{}`",
                file_name(file),
                section.header(),
                other
            ),
        ));
    }
    match raw.get(section.key()) {
        None if section != Section::Overrides => Err(import_error(
            Some(file),
            None,
            format!("`{}` has no {} section", file_name(file), section.header()),
        )),
        Some(value) if is_directive(value) => Err(import_error(
            Some(file),
            find_span(&src, "import"),
            format!(
                "only {} can import; `{}` is imported by main, so it can't import other files",
                MAIN_FILE_NAMES.join(" or "),
                file_name(file)
            ),
        )),
        _ => Ok(src),
    }
}

/// The texts a section comes from: main's own (`None`) or each imported file
/// in list order.
fn section_sources(
    plan: &ImportPlan,
    section: Section,
    main_src: &str,
    loader: &dyn SourceLoader,
) -> Result<Vec<(Option<PathBuf>, String)>, FormatError> {
    match plan.section(section) {
        SectionSource::Absent if section == Section::Overrides => Ok(Vec::new()),
        SectionSource::Absent => Err(TomlFormat::syntax_error(
            format!("missing field `{}`", section.key()),
            None,
        )),
        SectionSource::Inline => Ok(vec![(None, main_src.to_string())]),
        SectionSource::Import(refs) => refs
            .iter()
            .map(|import| {
                load_imported(loader, &plan.main, import, section)
                    .map(|src| (Some(import.path.clone()), src))
            })
            .collect(),
    }
}

#[derive(Deserialize)]
struct DefaultConfigsOnly {
    #[serde(rename = "default-configs")]
    default_configs: DefaultConfigsWithSchema,
}

#[derive(Deserialize)]
struct DimensionsOnly {
    dimensions: BTreeMap<String, DimensionInfoToml>,
}

#[derive(Deserialize)]
struct OverridesOnly {
    #[serde(default)]
    overrides: Vec<ContextToml>,
}

/// Deserialize one section straight from a file's own text, so type errors
/// point into that file.
fn parse_section<T: for<'de> Deserialize<'de>>(
    file: Option<&Path>,
    src: &str,
) -> Result<T, FormatError> {
    toml::from_str::<T>(src).map_err(|e| in_file(file, short_syntax_error(e)))
}

/// A key defined in two files.
fn duplicate_error(
    main: &Path,
    file: Option<&Path>,
    src: &str,
    kind: &str,
    key: &str,
    previous: Option<&Path>,
) -> FormatError {
    let main_dir = main.parent().unwrap_or(Path::new(""));
    let previous = previous.unwrap_or(main);
    let previous = previous.strip_prefix(main_dir).unwrap_or(previous);
    import_error(
        Some(file.unwrap_or(main)),
        find_key_span(src, key),
        format!(
            "{} `{}` is already defined in `{}`",
            kind,
            key,
            previous.display()
        ),
    )
}

/// Point a dimension error at the file defining the dimension it names. For a
/// duplicate position, that's the dimension imported last.
fn dimension_error_in_file(
    error: FormatError,
    origins: &HashMap<String, (usize, Option<PathBuf>)>,
) -> FormatError {
    let names: Vec<&str> = match &error {
        FormatError::ValidationError { key, .. } => {
            key.strip_suffix(".schema").into_iter().collect()
        }
        FormatError::InvalidCohortDimensionPosition { dimension, .. } => vec![dimension],
        FormatError::DuplicatePosition { dimensions, .. } => {
            dimensions.iter().map(String::as_str).collect()
        }
        _ => Vec::new(),
    };
    let file = names
        .iter()
        .filter_map(|name| origins.get(*name))
        .max_by_key(|(order, _)| *order)
        .and_then(|(_, file)| file.clone());
    in_file(file.as_deref(), error)
}

fn parse_with_imports(
    plan: &ImportPlan,
    main_src: &str,
    loader: &dyn SourceLoader,
) -> Result<DetailedConfig, FormatError> {
    let main = plan.main.as_path();

    let mut default_configs = DefaultConfigsWithSchema::default();
    let mut config_files: HashMap<String, Option<PathBuf>> = HashMap::new();
    for (file, src) in section_sources(plan, Section::DefaultConfigs, main_src, loader)? {
        let file = file.as_deref();
        let mut section =
            parse_section::<DefaultConfigsOnly>(file, &src)?.default_configs;
        fill_default_config_descriptions(&mut section);
        validate_default_configs(&section).map_err(|e| in_file(file, e))?;
        for (key, info) in section.into_inner() {
            if let Some(previous) = config_files.get(&key) {
                return Err(duplicate_error(
                    main,
                    file,
                    &src,
                    "default config",
                    &key,
                    previous.as_deref(),
                ));
            }
            config_files.insert(key.clone(), file.map(Path::to_path_buf));
            default_configs.insert(key, info);
        }
    }

    let mut dimensions: HashMap<String, DimensionInfo> = HashMap::new();
    let mut dimension_files: HashMap<String, (usize, Option<PathBuf>)> = HashMap::new();
    let sources = section_sources(plan, Section::Dimensions, main_src, loader)?;
    for (order, (file, src)) in sources.into_iter().enumerate() {
        let file = file.as_deref();
        for (name, dimension) in parse_section::<DimensionsOnly>(file, &src)?.dimensions {
            if let Some((_, previous)) = dimension_files.get(&name) {
                return Err(duplicate_error(
                    main,
                    file,
                    &src,
                    "dimension",
                    &name,
                    previous.as_deref(),
                ));
            }
            let info =
                convert_dimension(&name, dimension).map_err(|e| in_file(file, e))?;
            dimension_files.insert(name.clone(), (order, file.map(Path::to_path_buf)));
            dimensions.insert(name, info);
        }
    }
    validate_dimensions(&mut dimensions)
        .map_err(|e| dimension_error_in_file(e, &dimension_files))?;

    let mut contexts = Vec::new();
    let mut overrides = HashMap::new();
    for (file, src) in section_sources(plan, Section::Overrides, main_src, loader)? {
        let file = file.as_deref();
        let entries = parse_section::<OverridesOnly>(file, &src)?.overrides;
        let (file_contexts, file_overrides) = build_contexts::<TomlFormat, _, _>(
            entries,
            &dimensions,
            &default_configs,
            split_context,
        )
        .map_err(|e| in_file(file, e))?;
        contexts.extend(file_contexts);
        overrides.extend(file_overrides);
    }
    finalize_contexts(&mut contexts);

    Ok(DetailedConfig {
        default_configs,
        dimensions,
        contexts,
        overrides,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn typed_file_names() {
        let of = |name: &str| Section::of_file(Path::new(name));
        assert_eq!(of("geo.dimensions.stoml"), Some(Section::Dimensions));
        assert_eq!(
            of("dir/geo.dimensions.super.toml"),
            Some(Section::Dimensions)
        );
        assert_eq!(
            of("pricing.default-configs.stoml"),
            Some(Section::DefaultConfigs)
        );
        assert_eq!(of("a.b.overrides.stoml"), Some(Section::Overrides));
        assert_eq!(of(".dimensions.stoml"), None);
        assert_eq!(of("dimensions.stoml"), None);
        assert_eq!(of("main.stoml"), None);
        assert_eq!(of("geo.dimension.stoml"), None);
    }

    #[test]
    fn main_file_names() {
        assert!(is_main_file(Path::new("config/main.stoml")));
        assert!(is_main_file(Path::new("main.super.toml")));
        assert!(!is_main_file(Path::new("my-main.stoml")));
        assert!(!is_main_file(Path::new("super.toml")));
    }

    #[test]
    fn import_paths() {
        assert_eq!(
            normalize_import_path("a/b.stoml"),
            Ok(PathBuf::from("a/b.stoml"))
        );
        assert_eq!(
            normalize_import_path("./a//b.stoml"),
            Ok(PathBuf::from("a/b.stoml"))
        );
        for bad in [
            "",
            "  ",
            "../a.stoml",
            "a/../b.stoml",
            "/a.stoml",
            "C:a.stoml",
            "a\\b.stoml",
            ".",
        ] {
            assert!(
                normalize_import_path(bad).is_err(),
                "{bad:?} should be rejected"
            );
        }
    }

    #[test]
    fn key_spans() {
        let src = "[dimensions]\ncity = 1\n  [[overrides]]\ncity_id = 2\n";
        assert_eq!(find_key_span(src, "city").map(|r| &src[r]), Some("city"));
        assert_eq!(find_key_span(src, "city"), Some(13..17));
        assert_eq!(
            find_key_span(src, "overrides").map(|r| &src[r]),
            Some("overrides")
        );
        assert_eq!(find_key_span(src, "city_id"), Some(38..45));
        assert_eq!(find_key_span(src, "town"), None);
    }
}
