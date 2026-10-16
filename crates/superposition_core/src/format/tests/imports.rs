//! Tests for imports in TOML configs (`main.stoml` pulling sections from typed
//! files).

use std::collections::HashMap;
use std::ops::Range;
use std::path::{Path, PathBuf};

use superposition_types::{Config, DetailedConfig};

use crate::format::toml::{
    parse_toml_config, parse_toml_file_detailed, resolve_toml_imports, SectionSource,
};
use crate::format::FormatError;

const DIR: &str = "/cfg";

/// In-memory files under `/cfg`, keyed by full path.
fn files(entries: &[(&str, &str)]) -> HashMap<PathBuf, String> {
    entries
        .iter()
        .map(|(name, src)| (Path::new(DIR).join(name), src.to_string()))
        .collect()
}

fn path(name: &str) -> PathBuf {
    Path::new(DIR).join(name)
}

fn parse(files: &HashMap<PathBuf, String>) -> Result<DetailedConfig, FormatError> {
    parse_toml_file_detailed(&path("main.stoml"), files)
}

fn to_json(config: DetailedConfig) -> serde_json::Value {
    serde_json::to_value(Config::from(config)).unwrap()
}

/// The `ImportError` a parse failed with: (file name, text under the span, message).
fn import_error(
    files: &HashMap<PathBuf, String>,
    result: Result<DetailedConfig, FormatError>,
) -> (String, Option<String>, String) {
    match result {
        Err(FormatError::ImportError {
            file: Some(file),
            span,
            message,
        }) => {
            let text = span.map(|span| span_text(files, &file, span));
            let name = file.strip_prefix(DIR).unwrap().display().to_string();
            (name, text, message)
        }
        other => panic!("expected an ImportError, got {:?}", other),
    }
}

fn span_text(
    files: &HashMap<PathBuf, String>,
    file: &Path,
    span: Range<usize>,
) -> String {
    files[file][span].to_string()
}

const SINGLE: &str = r#"
[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number" } }
surge_factor = { value = 0.0, schema = { type = "number" } }

[dimensions]
city = { position = 1, schema = { type = "string", enum = ["Bangalore", "Delhi"] } }
vehicle_type = { position = 2, schema = { type = "string", enum = ["auto", "cab"] } }
hour_of_day = { position = 3, schema = { type = "integer", minimum = 0, maximum = 23 } }

[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0

[[overrides]]
_context_ = { city = "Bangalore" }
per_km_rate = 22.0

[[overrides]]
_context_ = { city = "Delhi", vehicle_type = "cab", hour_of_day = 18 }
surge_factor = 5.0

[[overrides]]
_context_ = { city = "Delhi" }
per_km_rate = 21.0
"#;

const MAIN: &str = r#"
default-configs.import = ["pricing.default-configs.stoml"]
dimensions.import = ["geo.dimensions.stoml", "ride.dimensions.stoml"]
overrides.import = ["cab.overrides.stoml", "city/delhi.overrides.stoml"]
"#;

const PRICING: &str = r#"
[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number" } }
surge_factor = { value = 0.0, schema = { type = "number" } }
"#;

const GEO: &str = r#"
[dimensions]
city = { position = 1, schema = { type = "string", enum = ["Bangalore", "Delhi"] } }
"#;

const RIDE: &str = r#"
[dimensions]
vehicle_type = { position = 2, schema = { type = "string", enum = ["auto", "cab"] } }
hour_of_day = { position = 3, schema = { type = "integer", minimum = 0, maximum = 23 } }
"#;

const CAB: &str = r#"
[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0

[[overrides]]
_context_ = { city = "Bangalore" }
per_km_rate = 22.0
"#;

const DELHI: &str = r#"
[[overrides]]
_context_ = { city = "Delhi", vehicle_type = "cab", hour_of_day = 18 }
surge_factor = 5.0

[[overrides]]
_context_ = { city = "Delhi" }
per_km_rate = 21.0
"#;

/// The split config, with `main.stoml` replaced by `main`.
fn split_with_main(main: &str) -> HashMap<PathBuf, String> {
    files(&[
        ("main.stoml", main),
        ("pricing.default-configs.stoml", PRICING),
        ("geo.dimensions.stoml", GEO),
        ("ride.dimensions.stoml", RIDE),
        ("cab.overrides.stoml", CAB),
        ("city/delhi.overrides.stoml", DELHI),
    ])
}

fn single_file_json() -> serde_json::Value {
    serde_json::to_value(parse_toml_config(SINGLE).unwrap()).unwrap()
}

#[test]
fn split_files_match_a_single_file() {
    let split = parse(&split_with_main(MAIN)).unwrap();
    // The two `city` overrides tie on priority; their order must survive too.
    assert_eq!(to_json(split), single_file_json());
}

#[test]
fn main_super_toml_can_import() {
    let mut files = split_with_main(MAIN);
    let main = files.remove(&path("main.stoml")).unwrap();
    files.insert(path("main.super.toml"), main);
    let split = parse_toml_file_detailed(&path("main.super.toml"), &files).unwrap();
    assert_eq!(to_json(split), single_file_json());
}

#[test]
fn inline_and_imported_sections_mix() {
    let main = r#"
dimensions.import = ["geo.dimensions.stoml", "ride.dimensions.stoml"]

[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number" } }
surge_factor = { value = 0.0, schema = { type = "number" } }

[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0

[[overrides]]
_context_ = { city = "Bangalore" }
per_km_rate = 22.0

[[overrides]]
_context_ = { city = "Delhi", vehicle_type = "cab", hour_of_day = 18 }
surge_factor = 5.0

[[overrides]]
_context_ = { city = "Delhi" }
per_km_rate = 21.0
"#;
    let split = parse(&split_with_main(main)).unwrap();
    assert_eq!(to_json(split), single_file_json());
}

#[test]
fn header_and_inline_table_forms_work() {
    let header_form = r#"
[default-configs]
import = ["pricing.default-configs.stoml"]

[dimensions]
import = ["geo.dimensions.stoml", "ride.dimensions.stoml"]

[overrides]
import = ["cab.overrides.stoml", "city/delhi.overrides.stoml"]
"#;
    let inline_form = r#"
default-configs = { import = ["pricing.default-configs.stoml"] }
dimensions = { import = ["geo.dimensions.stoml", "./ride.dimensions.stoml"] }
overrides = { import = ["cab.overrides.stoml", "city/delhi.overrides.stoml"] }
"#;
    for main in [header_form, inline_form] {
        let split = parse(&split_with_main(main)).unwrap();
        assert_eq!(to_json(split), single_file_json());
    }
}

#[test]
fn resolve_lists_imported_files_without_reading_them() {
    let plan = resolve_toml_imports(&path("main.stoml"), MAIN).unwrap();
    let imported: Vec<PathBuf> = plan.imported_files().map(|i| i.path.clone()).collect();
    assert_eq!(
        imported,
        [
            "pricing.default-configs.stoml",
            "geo.dimensions.stoml",
            "ride.dimensions.stoml",
            "cab.overrides.stoml",
            "city/delhi.overrides.stoml",
        ]
        .map(path)
    );

    let plan = resolve_toml_imports(&path("main.stoml"), SINGLE).unwrap();
    assert!(!plan.has_imports());
    assert_eq!(plan.dimensions, SectionSource::Inline);
}

#[test]
fn import_must_be_the_only_key_in_its_section() {
    let main = r#"
dimensions.import = ["geo.dimensions.stoml"]
dimensions.vehicle_type = { position = 2, schema = { type = "string" } }
default-configs.import = ["pricing.default-configs.stoml"]
"#;
    let files = split_with_main(main);
    let (file, _, message) = import_error(&files, parse(&files));
    assert_eq!(file, "main.stoml");
    assert!(
        message.contains("can't also define `vehicle_type`"),
        "{message}"
    );
}

#[test]
fn import_cant_sit_next_to_a_sub_table() {
    let main = r#"
dimensions.import = ["geo.dimensions.stoml"]
default-configs.import = ["pricing.default-configs.stoml"]

[dimensions.vehicle_type]
position = 2
schema = { type = "string" }
"#;
    let files = split_with_main(main);
    let (_, _, message) = import_error(&files, parse(&files));
    assert!(
        message.contains("can't also define `vehicle_type`"),
        "{message}"
    );
}

#[test]
fn import_cant_be_followed_by_the_section_header() {
    let main = r#"
dimensions.import = ["geo.dimensions.stoml"]
default-configs.import = ["pricing.default-configs.stoml"]

[dimensions]
vehicle_type = { position = 2, schema = { type = "string" } }
"#;
    let files = split_with_main(main);
    let (file, text, message) = import_error(&files, parse(&files));
    assert_eq!(file, "main.stoml");
    assert!(
        message.contains("so this file can't also have a [dimensions] table"),
        "{message}"
    );
    assert_eq!(text.unwrap(), "[dimensions]");

    let main = r#"
default-configs.import = ["pricing.default-configs.stoml"]
dimensions.import = ["geo.dimensions.stoml"]
overrides.import = ["cab.overrides.stoml"]

[[overrides]]
_context_ = { city = "Delhi" }
per_km_rate = 21.0
"#;
    let files = split_with_main(main);
    let (_, _, message) = import_error(&files, parse(&files));
    assert!(
        message.contains("so this file can't also have a [[overrides]] table"),
        "{message}"
    );
}

#[test]
fn import_must_be_a_list_of_paths() {
    let files = split_with_main("dimensions.import = \"geo.dimensions.stoml\"\n");
    let (_, _, message) = import_error(&files, parse(&files));
    assert_eq!(message, "`dimensions.import` must be a list of file paths");
}

#[test]
fn a_dimension_named_import_is_still_a_dimension() {
    let src = r#"
[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number" } }

[dimensions]
import = { position = 1, schema = { type = "string" } }

[[overrides]]
_context_ = { import = "yes" }
per_km_rate = 25.0
"#;
    let config = parse(&files(&[("main.stoml", src)])).unwrap();
    assert!(config.dimensions.contains_key("import"));
    assert!(parse_toml_config(src).is_ok());
}

#[test]
fn import_paths_must_stay_inside_mains_folder() {
    for (bad, expected) in [
        ("../geo.dimensions.stoml", "leaves main's folder"),
        ("x/../geo.dimensions.stoml", "leaves main's folder"),
        ("/etc/geo.dimensions.stoml", "is an absolute path"),
        ("C:geo.dimensions.stoml", "is an absolute path"),
        ("x\\geo.dimensions.stoml", "use `/`"),
        ("", "import path is empty"),
    ] {
        let main = format!(
            "default-configs.import = [\"pricing.default-configs.stoml\"]\ndimensions.import = [{:?}]\n",
            bad
        );
        let files = split_with_main(&main);
        let (file, text, message) = import_error(&files, parse(&files));
        assert_eq!(file, "main.stoml");
        assert!(message.contains(expected), "{bad:?}: {message}");
        assert!(
            text.unwrap().contains(&bad.replace('\\', "\\\\")),
            "{bad:?}"
        );
    }
}

#[test]
fn imported_files_must_match_the_import_key() {
    for (main, wrong) in [
        (
            "overrides.import = [\"geo.dimensions.stoml\"]",
            "geo.dimensions.stoml",
        ),
        ("dimensions.import = [\"main.stoml\"]", "main.stoml"),
        ("dimensions.import = [\"geo.stoml\"]", "geo.stoml"),
    ] {
        let files = split_with_main(main);
        let (_, text, message) = import_error(&files, parse(&files));
        assert!(message.contains("must be named"), "{message}");
        assert_eq!(text.unwrap(), format!("\"{}\"", wrong));
    }
}

#[test]
fn a_file_cant_be_imported_twice() {
    let main = r#"
default-configs.import = ["pricing.default-configs.stoml"]
dimensions.import = ["geo.dimensions.stoml", "./geo.dimensions.stoml"]
"#;
    let files = split_with_main(main);
    let (_, text, message) = import_error(&files, parse(&files));
    assert_eq!(message, "`./geo.dimensions.stoml` is imported twice");
    assert_eq!(text.unwrap(), "\"./geo.dimensions.stoml\"");
}

#[test]
fn missing_files_are_reported_on_the_import_line() {
    let main = r#"
default-configs.import = ["pricing.default-configs.stoml"]
dimensions.import = ["geo.dimensions.stoml", "town.dimensions.stoml"]
"#;
    let files = split_with_main(main);
    let (file, text, message) = import_error(&files, parse(&files));
    assert_eq!(file, "main.stoml");
    assert!(
        message.starts_with("can't find `town.dimensions.stoml`"),
        "{message}"
    );
    assert_eq!(text.unwrap(), "\"town.dimensions.stoml\"");
}

#[test]
fn imported_files_hold_only_their_own_section() {
    let mut files = split_with_main(MAIN);
    files.insert(path("geo.dimensions.stoml"), format!("{GEO}\n{CAB}"));
    let (file, text, message) = import_error(&files, parse(&files));
    assert_eq!(file, "geo.dimensions.stoml");
    assert_eq!(
        message,
        "`geo.dimensions.stoml` may only contain [dimensions], but it also defines `overrides`"
    );
    assert_eq!(text.unwrap(), "overrides");
}

#[test]
fn imported_files_cant_import() {
    let mut files = split_with_main(MAIN);
    files.insert(
        path("geo.dimensions.stoml"),
        "dimensions.import = [\"ride.dimensions.stoml\"]\n".to_string(),
    );
    let (file, text, message) = import_error(&files, parse(&files));
    assert_eq!(file, "geo.dimensions.stoml");
    assert!(
        message.contains("is imported by main, so it can't import"),
        "{message}"
    );
    assert_eq!(text.unwrap(), "import");
}

#[test]
fn imported_files_need_their_section_except_overrides() {
    let mut files = split_with_main(MAIN);
    files.insert(path("geo.dimensions.stoml"), "# nothing yet\n".to_string());
    let (file, _, message) = import_error(&files, parse(&files));
    assert_eq!(file, "geo.dimensions.stoml");
    assert_eq!(
        message,
        "`geo.dimensions.stoml` has no [dimensions] section"
    );

    let mut files = split_with_main(MAIN);
    files.insert(path("cab.overrides.stoml"), "# nothing yet\n".to_string());
    let config = parse(&files).unwrap();
    assert_eq!(config.contexts.len(), 2);
}

#[test]
fn a_dimension_defined_in_two_files_is_an_error() {
    let mut files = split_with_main(MAIN);
    files.insert(
        path("ride.dimensions.stoml"),
        format!("{RIDE}city = {{ position = 4, schema = {{ type = \"string\" }} }}\n"),
    );
    let (file, text, message) = import_error(&files, parse(&files));
    assert_eq!(file, "ride.dimensions.stoml");
    assert_eq!(
        message,
        "dimension `city` is already defined in `geo.dimensions.stoml`"
    );
    assert_eq!(text.unwrap(), "city");
}

#[test]
fn a_default_config_defined_in_two_files_is_an_error() {
    let main = r#"
default-configs.import = ["pricing.default-configs.stoml", "more.default-configs.stoml"]
dimensions.import = ["geo.dimensions.stoml", "ride.dimensions.stoml"]
"#;
    let mut files = split_with_main(main);
    files.insert(
        path("more.default-configs.stoml"),
        "[default-configs]\nsurge_factor = { value = 1.0, schema = { type = \"number\" } }\n"
            .to_string(),
    );
    let (file, text, message) = import_error(&files, parse(&files));
    assert_eq!(file, "more.default-configs.stoml");
    assert_eq!(
        message,
        "default config `surge_factor` is already defined in `pricing.default-configs.stoml`"
    );
    assert_eq!(text.unwrap(), "surge_factor");
}

#[test]
fn duplicate_positions_across_files_point_at_the_later_file() {
    let mut files = split_with_main(MAIN);
    files.insert(
        path("ride.dimensions.stoml"),
        RIDE.replace("position = 2", "position = 1"),
    );
    match parse(&files) {
        Err(FormatError::InFile { file, error }) => {
            assert_eq!(file, path("ride.dimensions.stoml"));
            assert!(matches!(
                *error,
                FormatError::DuplicatePosition { position: 1, .. }
            ));
        }
        other => panic!("expected InFile, got {:?}", other),
    }
}

#[test]
fn errors_inside_imported_files_carry_their_file_and_local_spans() {
    // A type error: the span points into the imported file.
    let mut files = split_with_main(MAIN);
    files.insert(
        path("ride.dimensions.stoml"),
        RIDE.replace("position = 3", "position = \"3\""),
    );
    match parse(&files) {
        Err(FormatError::InFile { file, error }) => {
            assert_eq!(file, path("ride.dimensions.stoml"));
            let FormatError::SyntaxError {
                span: Some(span), ..
            } = *error
            else {
                panic!("expected a SyntaxError with a span, got {:?}", error);
            };
            assert_eq!(span_text(&files, &file, span), "\"3\"");
        }
        other => panic!("expected InFile, got {:?}", other),
    }

    // A schema error in a default config.
    let mut files = split_with_main(MAIN);
    files.insert(
        path("pricing.default-configs.stoml"),
        PRICING.replace("20.0", "\"twenty\""),
    );
    match parse(&files) {
        Err(FormatError::InFile { file, error }) => {
            assert_eq!(file, path("pricing.default-configs.stoml"));
            assert!(
                matches!(&*error, FormatError::ValidationError { key, .. } if key == "default-configs.per_km_rate"),
                "{error:?}"
            );
        }
        other => panic!("expected InFile, got {:?}", other),
    }
}

#[test]
fn override_indices_count_within_their_own_file() {
    let mut files = split_with_main(MAIN);
    files.insert(
        path("city/delhi.overrides.stoml"),
        DELHI.replace("surge_factor = 5.0", "base_fare = 5.0"),
    );
    match parse(&files) {
        Err(FormatError::InFile { file, error }) => {
            assert_eq!(file, path("city/delhi.overrides.stoml"));
            assert!(
                matches!(&*error, FormatError::InvalidOverrideKey { key, context } if key == "base_fare" && context == "[0]"),
                "{error:?}"
            );
        }
        other => panic!("expected InFile, got {:?}", other),
    }
}

#[test]
fn imports_after_a_section_header_are_misplaced() {
    let main = r#"
[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number" } }
dimensions.import = ["geo.dimensions.stoml"]
"#;
    let files = split_with_main(main);
    let (file, text, message) = import_error(&files, parse(&files));
    assert_eq!(file, "main.stoml");
    assert!(
        message.contains("ended up inside [default-configs]"),
        "{message}"
    );
    assert_eq!(text.unwrap(), "dimensions.import");

    let main = format!("{SINGLE}dimensions.import = [\"geo.dimensions.stoml\"]\n");
    let files = split_with_main(&main);
    let (_, _, message) = import_error(&files, parse(&files));
    assert!(
        message.contains("ended up inside [[overrides]]"),
        "{message}"
    );

    // Parsing the text as a string gives the same hint.
    let err = parse_toml_config(&main).unwrap_err();
    assert!(
        err.to_string().contains("ended up inside [[overrides]]"),
        "{err}"
    );
}

#[test]
fn only_main_can_import() {
    let files = files(&[
        ("other.stoml", MAIN),
        ("pricing.default-configs.stoml", PRICING),
    ]);
    let result = parse_toml_file_detailed(&path("other.stoml"), &files);
    match result {
        Err(FormatError::ImportError { file, message, .. }) => {
            assert_eq!(file, Some(path("other.stoml")));
            assert_eq!(
                message,
                "only main.stoml or main.super.toml can import; `other.stoml` can't"
            );
        }
        other => panic!("expected ImportError, got {:?}", other),
    }
}

#[test]
fn string_parsing_explains_that_imports_need_a_path() {
    match parse_toml_config(MAIN) {
        Err(FormatError::ImportError {
            file: None,
            message,
            ..
        }) => {
            assert!(message.starts_with("imports need a file path"), "{message}");
        }
        other => panic!("expected ImportError, got {:?}", other),
    }
    let err = <crate::TomlFormat as crate::ConfigFormat>::parse_config(MAIN).unwrap_err();
    assert!(
        err.to_string().contains("imports need a file path"),
        "{err}"
    );
}

#[test]
fn files_without_imports_parse_like_strings() {
    let ok = parse(&files(&[("main.stoml", SINGLE)])).unwrap();
    assert_eq!(to_json(ok), single_file_json());

    for bad in [
        SINGLE.replace("per_km_rate = 25.0", "base_fare = 25.0"),
        SINGLE.replace("[dimensions]", ""),
        SINGLE.replace("position = 3", "position = \"3\""),
        "not toml = = 1".to_string(),
    ] {
        let from_file = parse(&files(&[("main.stoml", &bad)])).unwrap_err();
        let from_string = parse_toml_config(&bad).unwrap_err();
        assert_eq!(from_file.to_string(), from_string.to_string());
    }
}

#[test]
fn errors_display_the_file_they_are_in() {
    let mut files = split_with_main(MAIN);
    files.insert(
        path("cab.overrides.stoml"),
        CAB.replace("per_km_rate = 25.0", "base_fare = 25.0"),
    );
    let err = parse(&files).unwrap_err();
    assert_eq!(
        err.to_string(),
        "/cfg/cab.overrides.stoml: Parsing error: Override key 'base_fare' not found in default-config (context: '[0]')"
    );
    let (file, inner) = err.location();
    assert_eq!(file, Some(path("cab.overrides.stoml").as_path()));
    assert!(matches!(inner, FormatError::InvalidOverrideKey { .. }));
}
