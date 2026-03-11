use tower_lsp::lsp_types::*;

use crate::utils;

/// Context of the cursor within a SuperTOML document.
#[derive(Debug, PartialEq)]
enum CursorContext {
    /// Inside the `_context_ = { … }` inline table of an `[[overrides]]` block.
    /// → suggest declared dimension names.
    ContextInlineTable,
    /// On a key line after `_context_` in an `[[overrides]]` block.
    /// → suggest declared default-config keys.
    OverrideKey,
    /// On a `type = …` value in a `[dimensions]` entry.
    /// → suggest REGULAR / LOCAL_COHORT:… / REMOTE_COHORT:…
    DimensionType,
    /// After `=` in the `_context_` inline table, completing a dimension value.
    /// → suggest values based on the dimension's schema.
    DimensionValue(String),
    /// After `=` in an overrides block (after `_context_`), completing a config value.
    /// → suggest values based on the default-config's schema.
    ConfigValue(String),
    /// Inside `[default-config]` or `[default-configs]` section, on a new key line.
    /// → suggest existing default-config keys (for reference/navigation).
    DefaultConfigKey,
    /// Inside `[dimensions]` section, on a new key line.
    /// → suggest existing dimension keys (for reference/navigation).
    DimensionKey,
    /// Inside a default-config entry's inline table (after `{`).
    /// → suggest `value` and `schema` properties.
    DefaultConfigProperty,
    /// Inside a dimension entry's inline table (after `{`).
    /// → suggest `position`, `schema`, `type`, `definitions` properties.
    DimensionProperty,
    /// Inside a `schema = { … }` inline table.
    /// → suggest JSON Schema draft7 properties.
    SchemaProperty,
    /// After `LOCAL_COHORT:` or `REMOTE_COHORT:` in dimension type.
    /// → suggest existing dimension names.
    CohortDimensionRef(String),
    /// Cursor position not in a recognised completion context.
    Unknown,
}

/// Produce completion items for the given cursor position.
pub fn compute(text: &str, pos: Position) -> Option<CompletionResponse> {
    // Don't provide completions if cursor is inside a comment
    if utils::is_inside_comment(text, pos) {
        return None;
    }

    let lines: Vec<&str> = text.lines().collect();
    let line_num = pos.line as usize;

    let ctx = determine_cursor_context(&lines, line_num, pos.character as usize);

    // Parse raw TOML for dimension / config names; failures are tolerated
    // (the file may be mid-edit and syntactically incomplete).
    // If parsing fails, try again with the current (incomplete) line removed.
    let raw = toml::from_str::<toml::Table>(text).ok().or_else(|| {
        let text_without_current_line: String = lines
            .iter()
            .enumerate()
            .filter(|(i, _)| *i != line_num)
            .map(|(_, l)| *l)
            .collect::<Vec<&str>>()
            .join("\n");
        toml::from_str::<toml::Table>(&text_without_current_line).ok()
    });

    let items: Vec<CompletionItem> = match ctx {
        CursorContext::ContextInlineTable => raw
            .as_ref()
            .and_then(|t| t.get("dimensions"))
            .and_then(|d| d.as_table())
            .map(|dims| {
                dims.keys()
                    .map(|k| CompletionItem {
                        label: k.clone(),
                        kind: Some(CompletionItemKind::FIELD),
                        detail: Some("dimension".to_string()),
                        ..Default::default()
                    })
                    .collect()
            })
            .unwrap_or_default(),

        CursorContext::OverrideKey => raw
            .as_ref()
            .and_then(|t| get_default_config_table(t))
            .map(|configs| {
                configs
                    .keys()
                    .map(|k| CompletionItem {
                        label: k.clone(),
                        kind: Some(CompletionItemKind::FIELD),
                        detail: Some("config key".to_string()),
                        ..Default::default()
                    })
                    .collect()
            })
            .unwrap_or_default(),

        CursorContext::DimensionType => {
            let dim_names: Vec<String> = raw
                .as_ref()
                .and_then(|t| t.get("dimensions"))
                .and_then(|d| d.as_table())
                .map(|dims| dims.keys().cloned().collect())
                .unwrap_or_default();

            let mut items = vec![CompletionItem {
                label: "REGULAR".to_string(),
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some("Standard dimension (default)".to_string()),
                ..Default::default()
            }];

            for dim in &dim_names {
                items.push(CompletionItem {
                    label: format!("LOCAL_COHORT:{}", dim),
                    kind: Some(CompletionItemKind::ENUM_MEMBER),
                    detail: Some(format!("Local cohort of dimension '{}'", dim)),
                    ..Default::default()
                });
                items.push(CompletionItem {
                    label: format!("REMOTE_COHORT:{}", dim),
                    kind: Some(CompletionItemKind::ENUM_MEMBER),
                    detail: Some(format!("Remote cohort of dimension '{}'", dim)),
                    ..Default::default()
                });
            }
            items
        }

        CursorContext::DimensionValue(dim_name) => {
            // Find the schema for this dimension and provide value completions
            let schema = raw
                .as_ref()
                .and_then(|t| t.get("dimensions"))
                .and_then(|d| d.as_table())
                .and_then(|dims| dims.get(&dim_name))
                .and_then(|dim| dim.as_table())
                .and_then(|dim_table| dim_table.get("schema"))
                .and_then(|s| s.as_table());

            schema_to_completions(schema, &dim_name)
        }

        CursorContext::ConfigValue(config_key) => {
            // Find the schema for this config key and provide value completions
            let schema = raw
                .as_ref()
                .and_then(|t| get_default_config_table(t))
                .and_then(|configs| configs.get(&config_key))
                .and_then(|config| config.as_table())
                .and_then(|config_table| config_table.get("schema"))
                .and_then(|s| s.as_table());

            schema_to_completions(schema, &config_key)
        }

        CursorContext::DefaultConfigKey => {
            // Suggest existing default-config keys for reference
            raw.as_ref()
                .and_then(|t| get_default_config_table(t))
                .map(|configs| {
                    configs
                        .keys()
                        .map(|k| CompletionItem {
                            label: k.clone(),
                            kind: Some(CompletionItemKind::FIELD),
                            detail: Some("existing config key".to_string()),
                            ..Default::default()
                        })
                        .collect()
                })
                .unwrap_or_default()
        }

        CursorContext::DimensionKey => {
            // Suggest existing dimension keys for reference
            raw.as_ref()
                .and_then(|t| t.get("dimensions"))
                .and_then(|d| d.as_table())
                .map(|dims| {
                    dims.keys()
                        .map(|k| CompletionItem {
                            label: k.clone(),
                            kind: Some(CompletionItemKind::FIELD),
                            detail: Some("existing dimension".to_string()),
                            ..Default::default()
                        })
                        .collect()
                })
                .unwrap_or_default()
        }

        CursorContext::DefaultConfigProperty => {
            // Suggest properties for default-config entries: value, schema
            get_default_config_property_completions()
        }

        CursorContext::DimensionProperty => {
            // Suggest properties for dimension entries: position, schema, type, definitions
            get_dimension_property_completions()
        }

        CursorContext::SchemaProperty => {
            // Suggest JSON Schema draft7 properties
            get_json_schema_draft7_completions()
        }

        CursorContext::CohortDimensionRef(cohort_type) => {
            // Suggest existing dimension names for cohort reference
            raw.as_ref()
                .and_then(|t| t.get("dimensions"))
                .and_then(|d| d.as_table())
                .map(|dims| {
                    dims.keys()
                        .map(|k| CompletionItem {
                            label: k.clone(),
                            kind: Some(CompletionItemKind::FIELD),
                            detail: Some(format!(
                                "{} cohort source dimension",
                                cohort_type
                            )),
                            ..Default::default()
                        })
                        .collect()
                })
                .unwrap_or_default()
        }

        CursorContext::Unknown => return None,
    };

    if items.is_empty() {
        None
    } else {
        Some(CompletionResponse::Array(items))
    }
}

/// Get the default-config table, supporting both "default-config" and "default-configs" keys.
fn get_default_config_table(toml: &toml::Table) -> Option<&toml::Table> {
    toml.get("default-configs")
        .and_then(|d| d.as_table())
        .or_else(|| toml.get("default-config").and_then(|d| d.as_table()))
}

/// Convert a JSONSchema to completion items based on type and enum.
/// - If `enum` is present, suggest all enum values.
/// - If `type` is "string", suggest `""`.
/// - If `type` is "boolean", suggest `true` and `false`.
/// - If `type` is "array", suggest `[]`.
/// - If `type` is "object", suggest `{}`.
fn schema_to_completions(
    schema: Option<&toml::Table>,
    key_name: &str,
) -> Vec<CompletionItem> {
    let schema = match schema {
        Some(s) => s,
        None => return vec![],
    };

    // Check for enum first - highest priority
    if let Some(enum_values) = schema.get("enum").and_then(|e| e.as_array()) {
        return enum_values
            .iter()
            .filter_map(|v| {
                // Convert TOML value to string representation
                let label = match v {
                    toml::Value::String(s) => format!("\"{}\"", s),
                    toml::Value::Integer(i) => i.to_string(),
                    toml::Value::Float(f) => f.to_string(),
                    toml::Value::Boolean(b) => b.to_string(),
                    _ => return None,
                };
                Some(CompletionItem {
                    label: label.clone(),
                    kind: Some(CompletionItemKind::VALUE),
                    detail: Some(format!("enum value for '{}'", key_name)),
                    insert_text: Some(label),
                    ..Default::default()
                })
            })
            .collect();
    }

    // Fall back to type-based completions
    let type_str = schema.get("type").and_then(|t| t.as_str()).unwrap_or("");

    match type_str {
        "string" => vec![CompletionItem {
            label: "\"\"".to_string(),
            kind: Some(CompletionItemKind::VALUE),
            detail: Some(format!("string value for '{}'", key_name)),
            insert_text: Some("\"\"".to_string()),
            ..Default::default()
        }],
        "boolean" => vec![
            CompletionItem {
                label: "true".to_string(),
                kind: Some(CompletionItemKind::VALUE),
                detail: Some(format!("boolean value for '{}'", key_name)),
                insert_text: Some("true".to_string()),
                ..Default::default()
            },
            CompletionItem {
                label: "false".to_string(),
                kind: Some(CompletionItemKind::VALUE),
                detail: Some(format!("boolean value for '{}'", key_name)),
                insert_text: Some("false".to_string()),
                ..Default::default()
            },
        ],
        "array" => vec![CompletionItem {
            label: "[]".to_string(),
            kind: Some(CompletionItemKind::VALUE),
            detail: Some(format!("array value for '{}'", key_name)),
            insert_text: Some("[]".to_string()),
            ..Default::default()
        }],
        "object" => vec![CompletionItem {
            label: "{}".to_string(),
            kind: Some(CompletionItemKind::VALUE),
            detail: Some(format!("object value for '{}'", key_name)),
            insert_text: Some("{}".to_string()),
            ..Default::default()
        }],
        "integer" | "number" => vec![], // No sensible default for numbers
        _ => vec![],
    }
}

/// Get completions for default-config entry properties.
fn get_default_config_property_completions() -> Vec<CompletionItem> {
    vec![
        CompletionItem {
            label: "value".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("Default value for this configuration".to_string()),
            documentation: Some(Documentation::String(
                "The default value that will be used when no override matches.".to_string(),
            )),
            insert_text: Some("value = ".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "schema".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema for validation".to_string()),
            documentation: Some(Documentation::String(
                "JSON Schema (draft7) defining the structure and constraints for this configuration value.".to_string(),
            )),
            insert_text: Some("schema = { ".to_string()),
            ..Default::default()
        },
    ]
}

/// Get completions for dimension entry properties.
fn get_dimension_property_completions() -> Vec<CompletionItem> {
    vec![
        CompletionItem {
            label: "position".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("Priority position (1-based)".to_string()),
            documentation: Some(Documentation::String(
                "Position determines priority when computing overrides. Higher positions contribute more to the priority score.".to_string(),
            )),
            insert_text: Some("position = ".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "schema".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema for dimension values".to_string()),
            documentation: Some(Documentation::String(
                "JSON Schema (draft7) defining the structure and constraints for dimension values.".to_string(),
            )),
            insert_text: Some("schema = { ".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "type".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("Dimension type (REGULAR, LOCAL_COHORT, REMOTE_COHORT)".to_string()),
            documentation: Some(Documentation::String(
                "REGULAR: standard dimension. LOCAL_COHORT: computed client-side from another dimension. REMOTE_COHORT: computed server-side.".to_string(),
            )),
            insert_text: Some("type = ".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "definitions".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("Cohort value definitions".to_string()),
            documentation: Some(Documentation::String(
                "For cohort dimensions, defines how cohort values are computed from the source dimension.".to_string(),
            )),
            insert_text: Some("definitions = { ".to_string()),
            ..Default::default()
        },
    ]
}

/// Get completions for JSON Schema draft7 properties.
fn get_json_schema_draft7_completions() -> Vec<CompletionItem> {
    vec![
        // Type keywords
        CompletionItem {
            label: "type".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7".to_string()),
            documentation: Some(Documentation::String(
                "The type keyword defines the data type. Values: string, number, integer, boolean, array, object, null.".to_string(),
            )),
            insert_text: Some("\"type\" = \"".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "enum".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7".to_string()),
            documentation: Some(Documentation::String(
                "Enumerates all valid values for this schema.".to_string(),
            )),
            insert_text: Some("\"enum\" = [".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "const".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7".to_string()),
            documentation: Some(Documentation::String(
                "Specifies a single allowed value.".to_string(),
            )),
            insert_text: Some("\"const\" = ".to_string()),
            ..Default::default()
        },
        // String keywords
        CompletionItem {
            label: "minLength".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - string".to_string()),
            documentation: Some(Documentation::String(
                "Minimum length for string values.".to_string(),
            )),
            insert_text: Some("\"minLength\" = ".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "maxLength".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - string".to_string()),
            documentation: Some(Documentation::String(
                "Maximum length for string values.".to_string(),
            )),
            insert_text: Some("\"maxLength\" = ".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "pattern".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - string".to_string()),
            documentation: Some(Documentation::String(
                "Regular expression pattern that the string must match.".to_string(),
            )),
            insert_text: Some("\"pattern\" = \"".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "format".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - string".to_string()),
            documentation: Some(Documentation::String(
                "Predefined format: date-time, email, uri, hostname, ipv4, ipv6, uuid, etc.".to_string(),
            )),
            insert_text: Some("\"format\" = \"".to_string()),
            ..Default::default()
        },
        // Number keywords
        CompletionItem {
            label: "minimum".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - number/integer".to_string()),
            documentation: Some(Documentation::String(
                "Minimum value (inclusive) for numbers.".to_string(),
            )),
            insert_text: Some("\"minimum\" = ".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "maximum".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - number/integer".to_string()),
            documentation: Some(Documentation::String(
                "Maximum value (inclusive) for numbers.".to_string(),
            )),
            insert_text: Some("\"maximum\" = ".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "exclusiveMinimum".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - number/integer".to_string()),
            documentation: Some(Documentation::String(
                "Minimum value (exclusive) for numbers.".to_string(),
            )),
            insert_text: Some("\"exclusiveMinimum\" = ".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "exclusiveMaximum".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - number/integer".to_string()),
            documentation: Some(Documentation::String(
                "Maximum value (exclusive) for numbers.".to_string(),
            )),
            insert_text: Some("\"exclusiveMaximum\" = ".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "multipleOf".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - number/integer".to_string()),
            documentation: Some(Documentation::String(
                "Value must be a multiple of this number.".to_string(),
            )),
            insert_text: Some("\"multipleOf\" = ".to_string()),
            ..Default::default()
        },
        // Array keywords
        CompletionItem {
            label: "items".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - array".to_string()),
            documentation: Some(Documentation::String(
                "Schema for array items. Can be a single schema or a tuple of schemas.".to_string(),
            )),
            insert_text: Some("\"items\" = { ".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "minItems".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - array".to_string()),
            documentation: Some(Documentation::String(
                "Minimum number of items in the array.".to_string(),
            )),
            insert_text: Some("\"minItems\" = ".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "maxItems".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - array".to_string()),
            documentation: Some(Documentation::String(
                "Maximum number of items in the array.".to_string(),
            )),
            insert_text: Some("\"maxItems\" = ".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "uniqueItems".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - array".to_string()),
            documentation: Some(Documentation::String(
                "Whether all items in the array must be unique.".to_string(),
            )),
            insert_text: Some("\"uniqueItems\" = ".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "contains".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - array".to_string()),
            documentation: Some(Documentation::String(
                "Schema that at least one item in the array must match.".to_string(),
            )),
            insert_text: Some("\"contains\" = { ".to_string()),
            ..Default::default()
        },
        // Object keywords
        CompletionItem {
            label: "properties".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - object".to_string()),
            documentation: Some(Documentation::String(
                "Defines the properties of an object and their schemas.".to_string(),
            )),
            insert_text: Some("\"properties\" = { ".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "required".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - object".to_string()),
            documentation: Some(Documentation::String(
                "Array of property names that must be present.".to_string(),
            )),
            insert_text: Some("\"required\" = [".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "additionalProperties".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - object".to_string()),
            documentation: Some(Documentation::String(
                "Whether additional properties are allowed, or schema for additional properties.".to_string(),
            )),
            insert_text: Some("\"additionalProperties\" = ".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "minProperties".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - object".to_string()),
            documentation: Some(Documentation::String(
                "Minimum number of properties in the object.".to_string(),
            )),
            insert_text: Some("\"minProperties\" = ".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "maxProperties".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - object".to_string()),
            documentation: Some(Documentation::String(
                "Maximum number of properties in the object.".to_string(),
            )),
            insert_text: Some("\"maxProperties\" = ".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "propertyNames".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - object".to_string()),
            documentation: Some(Documentation::String(
                "Schema for property names in the object.".to_string(),
            )),
            insert_text: Some("\"propertyNames\" = { ".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "patternProperties".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - object".to_string()),
            documentation: Some(Documentation::String(
                "Maps regex patterns to schemas for matching property names.".to_string(),
            )),
            insert_text: Some("\"patternProperties\" = { ".to_string()),
            ..Default::default()
        },
        // Metadata keywords
        CompletionItem {
            label: "title".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - metadata".to_string()),
            documentation: Some(Documentation::String(
                "A human-readable title for the schema.".to_string(),
            )),
            insert_text: Some("\"title\" = \"".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "description".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - metadata".to_string()),
            documentation: Some(Documentation::String(
                "A human-readable description of the schema.".to_string(),
            )),
            insert_text: Some("\"description\" = \"".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "default".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - metadata".to_string()),
            documentation: Some(Documentation::String(
                "Default value for the schema.".to_string(),
            )),
            insert_text: Some("\"default\" = ".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "examples".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - metadata".to_string()),
            documentation: Some(Documentation::String(
                "Array of example values for the schema.".to_string(),
            )),
            insert_text: Some("\"examples\" = [".to_string()),
            ..Default::default()
        },
        // Composition keywords
        CompletionItem {
            label: "allOf".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - composition".to_string()),
            documentation: Some(Documentation::String(
                "All schemas must be valid.".to_string(),
            )),
            insert_text: Some("\"allOf\" = [".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "anyOf".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - composition".to_string()),
            documentation: Some(Documentation::String(
                "At least one schema must be valid.".to_string(),
            )),
            insert_text: Some("\"anyOf\" = [".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "oneOf".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - composition".to_string()),
            documentation: Some(Documentation::String(
                "Exactly one schema must be valid.".to_string(),
            )),
            insert_text: Some("\"oneOf\" = [".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "not".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - composition".to_string()),
            documentation: Some(Documentation::String(
                "The schema must NOT be valid.".to_string(),
            )),
            insert_text: Some("\"not\" = { ".to_string()),
            ..Default::default()
        },
        // Reference keyword
        CompletionItem {
            label: "$ref".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - reference".to_string()),
            documentation: Some(Documentation::String(
                "Reference to another schema by URI.".to_string(),
            )),
            insert_text: Some("\"$ref\" = \"".to_string()),
            ..Default::default()
        },
        // Conditional keywords
        CompletionItem {
            label: "if".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - conditional".to_string()),
            documentation: Some(Documentation::String(
                "Condition schema for if-then-else validation.".to_string(),
            )),
            insert_text: Some("\"if\" = { ".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "then".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - conditional".to_string()),
            documentation: Some(Documentation::String(
                "Schema to apply if the 'if' condition is valid.".to_string(),
            )),
            insert_text: Some("\"then\" = { ".to_string()),
            ..Default::default()
        },
        CompletionItem {
            label: "else".to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some("JSON Schema draft7 - conditional".to_string()),
            documentation: Some(Documentation::String(
                "Schema to apply if the 'if' condition is not valid.".to_string(),
            )),
            insert_text: Some("\"else\" = { ".to_string()),
            ..Default::default()
        },
    ]
}

/// Determine what kind of completion is appropriate at `line_num`.
///
/// Strategy: scan from the top of the file to `line_num`, tracking which
/// section header (`[default-configs]`, `[dimensions]`, `[[overrides]]`) was
/// most recently seen and whether `_context_` has appeared in the current
/// overrides block.
fn determine_cursor_context(
    lines: &[&str],
    line_num: usize,
    col: usize,
) -> CursorContext {
    let up_to = (line_num + 1).min(lines.len());

    let mut in_overrides = false;
    let mut in_dimensions = false;
    let mut in_default_config = false;
    let mut context_seen = false;

    // Track if we're inside an inline table for schema
    let mut in_schema_inline_table = false;
    let mut schema_inline_table_depth = 0;

    // Track if we're inside an inline table for default-config or dimension entry
    let mut in_entry_inline_table = false;
    let mut entry_inline_table_depth = 0;

    for (idx, line) in lines[..up_to].iter().enumerate() {
        let trimmed = line.trim();

        if trimmed == "[[overrides]]" {
            in_overrides = true;
            in_dimensions = false;
            in_default_config = false;
            context_seen = false;
            in_schema_inline_table = false;
            in_entry_inline_table = false;
        } else if trimmed == "[dimensions]" {
            in_dimensions = true;
            in_overrides = false;
            in_default_config = false;
            context_seen = false;
            in_schema_inline_table = false;
            in_entry_inline_table = false;
        } else if trimmed == "[default-config]" || trimmed == "[default-configs]" {
            in_default_config = true;
            in_overrides = false;
            in_dimensions = false;
            context_seen = false;
            in_schema_inline_table = false;
            in_entry_inline_table = false;
        } else if trimmed.starts_with('[') && !trimmed.starts_with("[[") {
            // Any other section header
            in_overrides = false;
            in_dimensions = false;
            in_default_config = false;
            context_seen = false;
            in_schema_inline_table = false;
            in_entry_inline_table = false;
        } else if in_overrides && trimmed.starts_with("_context_") {
            context_seen = true;
        }

        // Track schema inline table depth
        if idx < line_num {
            // Count braces on previous lines to track nesting
            let mut in_string = false;
            let mut escape_next = false;
            for ch in trimmed.chars() {
                if escape_next {
                    escape_next = false;
                    continue;
                }
                match ch {
                    '\\' if in_string => escape_next = true,
                    '"' => in_string = !in_string,
                    '{' if !in_string => {
                        // Check if this is a schema = { line
                        if trimmed.contains("schema") && trimmed.contains("=") {
                            in_schema_inline_table = true;
                        }
                        if in_schema_inline_table {
                            schema_inline_table_depth += 1;
                        }
                        // Check if this is a default-config or dimension entry
                        if (in_default_config || in_dimensions)
                            && trimmed.contains("=")
                            && !trimmed.contains("schema")
                        {
                            in_entry_inline_table = true;
                        }
                        if in_entry_inline_table {
                            entry_inline_table_depth += 1;
                        }
                    }
                    '}' if !in_string => {
                        if schema_inline_table_depth > 0 {
                            schema_inline_table_depth -= 1;
                            if schema_inline_table_depth == 0 {
                                in_schema_inline_table = false;
                            }
                        }
                        if entry_inline_table_depth > 0 {
                            entry_inline_table_depth -= 1;
                            if entry_inline_table_depth == 0 {
                                in_entry_inline_table = false;
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    let current_line = lines.get(line_num).map(|l| l.trim()).unwrap_or("");

    // Check if we're inside a schema inline table
    if is_inside_schema_inline_table(current_line, col) {
        return CursorContext::SchemaProperty;
    }

    // Check if we're inside a default-config entry's inline table (for property completion)
    if in_default_config && is_inside_entry_inline_table(current_line, col) {
        return CursorContext::DefaultConfigProperty;
    }

    // Check if we're inside a dimension entry's inline table (for property completion)
    if in_dimensions && is_inside_entry_inline_table(current_line, col) {
        // Check for cohort type reference
        if let Some(dim_ref) = extract_cohort_dimension_ref(current_line, col) {
            return CursorContext::CohortDimensionRef(dim_ref);
        }
        return CursorContext::DimensionProperty;
    }

    // Check for dimension type value completion
    if in_dimensions && is_after_type_equals(current_line, col) {
        return CursorContext::DimensionType;
    }

    if in_overrides {
        // Check if we're inside _context_ inline table completing a dimension value
        if current_line.starts_with("_context_") {
            // Try to extract a key after the opening brace that has an = after it
            if let Some(value) = extract_key_after_equals_in_context(current_line) {
                return CursorContext::DimensionValue(value);
            }
            CursorContext::ContextInlineTable
        } else if context_seen {
            // Check if this is a config value line (key = ...)
            if let Some(key) = extract_key_after_equals(current_line) {
                return CursorContext::ConfigValue(key);
            }
            CursorContext::OverrideKey
        } else {
            CursorContext::Unknown
        }
    } else if in_default_config {
        // Check if we're on a new key line (not inside an inline table)
        if !is_inside_entry_inline_table(current_line, col)
            && is_on_key_position(current_line, col)
        {
            return CursorContext::DefaultConfigKey;
        }
        CursorContext::Unknown
    } else if in_dimensions {
        // Check if we're on a new key line (not inside an inline table)
        if !is_inside_entry_inline_table(current_line, col)
            && is_on_key_position(current_line, col)
        {
            return CursorContext::DimensionKey;
        }
        CursorContext::Unknown
    } else {
        CursorContext::Unknown
    }
}

/// Check if the cursor is inside a schema inline table.
fn is_inside_schema_inline_table(line: &str, col: usize) -> bool {
    // Look for pattern: schema = { ... where cursor is inside the braces
    let schema_pos = match line.find("schema") {
        Some(pos) => pos,
        None => return false,
    };

    // Find the opening brace after schema =
    let after_schema = &line[schema_pos..];
    let brace_pos = match after_schema.find('{') {
        Some(pos) => schema_pos + pos,
        None => return false,
    };

    // Check if cursor is after the opening brace
    if col <= brace_pos {
        return false;
    }

    // Count braces to find matching close
    let mut depth = 0;
    let mut in_string = false;
    let mut escape_next = false;

    for (i, ch) in line[brace_pos..].chars().enumerate() {
        if escape_next {
            escape_next = false;
            continue;
        }
        match ch {
            '\\' if in_string => escape_next = true,
            '"' => in_string = !in_string,
            '{' if !in_string => depth += 1,
            '}' if !in_string => {
                depth -= 1;
                if depth == 0 {
                    // Found the matching close brace
                    // Cursor is inside if it's before this position
                    return col < brace_pos + i + 1;
                }
            }
            _ => {}
        }
    }

    // No matching close brace found, cursor might be inside
    depth > 0
}

/// Check if the cursor is inside an entry's inline table (for default-config or dimension).
fn is_inside_entry_inline_table(line: &str, col: usize) -> bool {
    // Look for pattern: key = { ... where cursor is inside the braces
    // but NOT inside a schema = { ... }
    let equals_pos = match line.find('=') {
        Some(pos) => pos,
        None => return false,
    };

    // Check if there's a brace after the equals
    let after_equals = &line[equals_pos..];
    let brace_pos = match after_equals.find('{') {
        Some(pos) => equals_pos + pos,
        None => return false,
    };

    // Check if this is a schema line - if so, skip
    let before_equals = &line[..equals_pos];
    if before_equals.trim() == "schema" {
        return false;
    }

    // Check if cursor is after the opening brace
    if col <= brace_pos {
        return false;
    }

    // Count braces to find matching close
    let mut depth = 0;
    let mut in_string = false;
    let mut escape_next = false;

    for (i, ch) in line[brace_pos..].chars().enumerate() {
        if escape_next {
            escape_next = false;
            continue;
        }
        match ch {
            '\\' if in_string => escape_next = true,
            '"' => in_string = !in_string,
            '{' if !in_string => depth += 1,
            '}' if !in_string => {
                depth -= 1;
                if depth == 0 {
                    // Found the matching close brace
                    // Cursor is inside if it's before this position
                    return col < brace_pos + i + 1;
                }
            }
            _ => {}
        }
    }

    // No matching close brace found, cursor might be inside
    depth > 0
}

/// Check if cursor is on a position where a key would be typed.
fn is_on_key_position(line: &str, col: usize) -> bool {
    let trimmed = line.trim();

    // Empty line or line starting with identifier (not inside inline table)
    if trimmed.is_empty() {
        return true;
    }

    // Check if we're at the start of a line (where a key would be)
    // Allow whitespace before the key
    let prefix = &line[..col.min(line.len())];
    if prefix.trim().is_empty() {
        return true;
    }

    // Check if we're typing a key (alphanumeric, underscore, hyphen)
    let col = col.min(line.len());
    if col == 0 {
        return true;
    }

    // Check if we're at the start of a new key (after newline or on a fresh line)
    let is_ident_char = |c: char| c.is_ascii_alphanumeric() || c == '_' || c == '-';

    // Cursor is on a key position if it's at the beginning of a potential key
    // or right after some identifier characters
    let before_cursor = &line[..col];
    let _after_cursor = if col < line.len() { &line[col..] } else { "" };

    // Check if we're in a position to type a key (before = sign)
    if line.contains('=') {
        let eq_pos = line.find('=').unwrap();
        // If cursor is before the =, we might be typing a key
        if col <= eq_pos {
            // Check if the part before cursor is just whitespace or a partial identifier
            return before_cursor.trim().is_empty()
                || before_cursor
                    .chars()
                    .all(|c| is_ident_char(c) || c.is_whitespace());
        }
        return false;
    }

    // No = on line, so we could be typing a key
    before_cursor
        .chars()
        .all(|c| is_ident_char(c) || c.is_whitespace())
}

/// Check if cursor is after `type = ` in a dimension entry.
fn is_after_type_equals(line: &str, col: usize) -> bool {
    let type_pos = match line.find("type") {
        Some(pos) => pos,
        None => return false,
    };

    // Find the = after type
    let after_type = &line[type_pos..];
    let eq_pos = match after_type.find('=') {
        Some(pos) => type_pos + pos,
        None => return false,
    };

    // Check if cursor is after the = and before any value
    if col <= eq_pos {
        return false;
    }

    let after_eq = &line[eq_pos + 1..col.min(line.len())];

    // Cursor is after type = if the content after = is just whitespace or empty
    after_eq.trim().is_empty() || after_eq.trim().starts_with('"')
}

/// Extract the dimension name from a cohort type reference.
/// e.g., "type = \"LOCAL_COHORT:" returns Some("LOCAL_COHORT")
fn extract_cohort_dimension_ref(line: &str, _col: usize) -> Option<String> {
    let type_pos = line.find("type")?;
    let after_type = &line[type_pos..];
    let eq_pos = after_type.find('=')? + type_pos;

    let after_eq = &line[eq_pos + 1..];
    let value = after_eq.trim();

    // Check if value starts with a cohort prefix
    if let Some(stripped_val) = value.strip_prefix('"') {
        let end_quote = stripped_val.find('"').unwrap_or(value.len());
        let inner = &value[1..end_quote + 1];

        if inner.starts_with("LOCAL_COHORT:") {
            return Some("LOCAL_COHORT".to_string());
        } else if inner.starts_with("REMOTE_COHORT:") {
            return Some("REMOTE_COHORT".to_string());
        }
    }

    None
}

/// Extract the key name from a line if cursor is after `=` for a config value.
/// e.g., "timeout = " returns Some("timeout")
fn extract_key_after_equals(line: &str) -> Option<String> {
    let line = line.trim();

    // Look for pattern: key = (with cursor after =)
    if let Some(eq_pos) = line.find('=') {
        let key_part = line[..eq_pos].trim();
        // Make sure it's a valid key (not empty, not starting with special chars)
        if !key_part.is_empty() && !key_part.starts_with('#') {
            // Check if there's nothing or just whitespace/placeholder after =
            let after_eq = line[eq_pos + 1..].trim();
            if after_eq.is_empty() || after_eq == "\"" {
                return Some(key_part.to_string());
            }
        }
    }
    None
}

/// Extract a dimension key from inside the _context_ inline table.
/// e.g., '_context_ = { os = ' returns Some("os")
/// Returns the LAST incomplete key since cursor is at end of line.
fn extract_key_after_equals_in_context(line: &str) -> Option<String> {
    // Find the opening brace
    let brace_pos = line.find('{')?;
    let inside = &line[brace_pos + 1..];

    // Look for pattern: key = inside the braces
    // Return the LAST incomplete key since cursor is at end of line
    let mut last_incomplete_key: Option<String> = None;

    for pair in inside.split(',') {
        let pair = pair.trim();
        // Skip the closing brace
        if pair == "}" || pair.is_empty() {
            continue;
        }
        // Check if this pair has an = and the cursor is after it
        if let Some(eq_pos) = pair.find('=') {
            let key = pair[..eq_pos].trim();
            let value = pair[eq_pos + 1..].trim();
            // If value is empty or just a quote, cursor is probably here
            if !key.is_empty() && (value.is_empty() || value == "\"" || value == "}") {
                last_incomplete_key = Some(key.to_string());
            }
        }
    }
    last_incomplete_key
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_schema_to_completions_with_enum() {
        let mut schema = toml::Table::new();
        schema.insert(
            "type".to_string(),
            toml::Value::String("string".to_string()),
        );
        schema.insert(
            "enum".to_string(),
            toml::Value::Array(vec![
                toml::Value::String("Bangalore".to_string()),
                toml::Value::String("Delhi".to_string()),
                toml::Value::String("Mumbai".to_string()),
            ]),
        );

        let items = schema_to_completions(Some(&schema), "city");

        assert_eq!(items.len(), 3);
        assert!(items.iter().any(|i| i.label == "\"Bangalore\""));
        assert!(items.iter().any(|i| i.label == "\"Delhi\""));
        assert!(items.iter().any(|i| i.label == "\"Mumbai\""));
    }

    #[test]
    fn test_schema_to_completions_with_integer_enum() {
        let mut schema = toml::Table::new();
        schema.insert(
            "type".to_string(),
            toml::Value::String("integer".to_string()),
        );
        schema.insert(
            "enum".to_string(),
            toml::Value::Array(vec![
                toml::Value::Integer(0),
                toml::Value::Integer(1),
                toml::Value::Integer(2),
            ]),
        );

        let items = schema_to_completions(Some(&schema), "status");

        assert_eq!(items.len(), 3);
        assert!(items.iter().any(|i| i.label == "0"));
        assert!(items.iter().any(|i| i.label == "1"));
        assert!(items.iter().any(|i| i.label == "2"));
    }

    #[test]
    fn test_schema_to_completions_string_type() {
        let mut schema = toml::Table::new();
        schema.insert(
            "type".to_string(),
            toml::Value::String("string".to_string()),
        );

        let items = schema_to_completions(Some(&schema), "name");

        assert_eq!(items.len(), 1);
        assert_eq!(items[0].label, "\"\"");
    }

    #[test]
    fn test_schema_to_completions_boolean_type() {
        let mut schema = toml::Table::new();
        schema.insert(
            "type".to_string(),
            toml::Value::String("boolean".to_string()),
        );

        let items = schema_to_completions(Some(&schema), "enabled");

        assert_eq!(items.len(), 2);
        assert!(items.iter().any(|i| i.label == "true"));
        assert!(items.iter().any(|i| i.label == "false"));
    }

    #[test]
    fn test_schema_to_completions_array_type() {
        let mut schema = toml::Table::new();
        schema.insert("type".to_string(), toml::Value::String("array".to_string()));

        let items = schema_to_completions(Some(&schema), "tags");

        assert_eq!(items.len(), 1);
        assert_eq!(items[0].label, "[]");
    }

    #[test]
    fn test_schema_to_completions_object_type() {
        let mut schema = toml::Table::new();
        schema.insert(
            "type".to_string(),
            toml::Value::String("object".to_string()),
        );

        let items = schema_to_completions(Some(&schema), "config");

        assert_eq!(items.len(), 1);
        assert_eq!(items[0].label, "{}");
    }

    #[test]
    fn test_schema_to_completions_integer_type() {
        let mut schema = toml::Table::new();
        schema.insert(
            "type".to_string(),
            toml::Value::String("integer".to_string()),
        );

        let items = schema_to_completions(Some(&schema), "count");

        // No sensible default for integers
        assert!(items.is_empty());
    }

    #[test]
    fn test_schema_to_completions_none() {
        let items = schema_to_completions(None, "unknown");
        assert!(items.is_empty());
    }

    #[test]
    fn test_extract_key_after_equals() {
        assert_eq!(
            extract_key_after_equals("timeout = "),
            Some("timeout".to_string())
        );
        assert_eq!(
            extract_key_after_equals("timeout =  "),
            Some("timeout".to_string())
        );
        assert_eq!(
            extract_key_after_equals("timeout = \""),
            Some("timeout".to_string())
        );
        assert_eq!(extract_key_after_equals("timeout = 30"), None);
        assert_eq!(extract_key_after_equals("timeout = \"30\""), None);
        assert_eq!(extract_key_after_equals("timeout = true"), None);
        assert_eq!(extract_key_after_equals("timeout = [1, 2]"), None);
        assert_eq!(extract_key_after_equals("# comment = "), None);
        assert_eq!(extract_key_after_equals("= "), None);
    }

    #[test]
    fn test_extract_key_after_equals_in_context() {
        assert_eq!(
            extract_key_after_equals_in_context("_context_ = { os = "),
            Some("os".to_string())
        );
        assert_eq!(
            extract_key_after_equals_in_context("_context_ = { os = , city = "),
            Some("city".to_string())
        );
        assert_eq!(
            extract_key_after_equals_in_context("_context_ = { os = \"linux\" }"),
            None
        );
        assert_eq!(
            extract_key_after_equals_in_context("_context_ = { os = \"linux\", city = "),
            Some("city".to_string())
        );
        assert_eq!(extract_key_after_equals_in_context("_context_ = { }"), None);
        // Incomplete value with closing brace
        assert_eq!(
            extract_key_after_equals_in_context("_context_ = { city = }"),
            Some("city".to_string())
        );
        // Incomplete value with closing brace and multiple entries
        assert_eq!(
            extract_key_after_equals_in_context("_context_ = { os = \"linux\", city = }"),
            Some("city".to_string())
        );
    }

    #[test]
    fn test_compute_dimension_value_with_enum() {
        let text = r#"[default-configs]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
city = { position = 1, schema = { type = "string", enum = ["Bangalore", "Delhi"] } }

[[overrides]]
_context_ = { city =
"#;
        // Cursor at end of line 7 (after "city = ")
        let pos = Position {
            line: 7,
            character: 20,
        };
        let result = compute(text, pos);

        assert!(
            result.is_some(),
            "Expected completions for dimension value with enum"
        );
        let items = match result {
            Some(CompletionResponse::Array(items)) => items,
            _ => panic!("Expected Array response"),
        };
        assert_eq!(items.len(), 2);
        assert!(items.iter().any(|i| i.label == "\"Bangalore\""));
        assert!(items.iter().any(|i| i.label == "\"Delhi\""));
    }

    #[test]
    fn test_compute_config_value_with_enum() {
        let text = r#"[default-configs]
currency = { value = "INR", schema = { type = "string", enum = ["INR", "USD", "EUR"] } }

[dimensions]
os = { position = 1, schema = { type = "string" } }

[[overrides]]
_context_ = { os = "linux" }
currency =
"#;
        // Cursor at end of line 8 (after "currency = ")
        let pos = Position {
            line: 8,
            character: 11,
        };
        let result = compute(text, pos);

        assert!(
            result.is_some(),
            "Expected completions for config value with enum"
        );
        let items = match result {
            Some(CompletionResponse::Array(items)) => items,
            _ => panic!("Expected Array response"),
        };
        assert_eq!(items.len(), 3);
        assert!(items.iter().any(|i| i.label == "\"INR\""));
        assert!(items.iter().any(|i| i.label == "\"USD\""));
        assert!(items.iter().any(|i| i.label == "\"EUR\""));
    }

    #[test]
    fn test_compute_config_value_boolean() {
        let text = r#"[default-configs]
enabled = { value = true, schema = { type = "boolean" } }

[dimensions]
os = { position = 1, schema = { type = "string" } }

[[overrides]]
_context_ = { os = "linux" }
enabled =
"#;
        // Cursor at end of line 8 (after "enabled = ")
        let pos = Position {
            line: 8,
            character: 10,
        };
        let result = compute(text, pos);

        assert!(
            result.is_some(),
            "Expected completions for config value with boolean type"
        );
        let items = match result {
            Some(CompletionResponse::Array(items)) => items,
            _ => panic!("Expected Array response"),
        };
        assert_eq!(items.len(), 2);
        assert!(items.iter().any(|i| i.label == "true"));
        assert!(items.iter().any(|i| i.label == "false"));
    }

    #[test]
    fn test_compute_dimension_value_string_no_enum() {
        let text = r#"[default-configs]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
zone = { position = 1, schema = { type = "string" } }

[[overrides]]
_context_ = { zone =
"#;
        // Cursor at end of line 7 (after "zone = ")
        let pos = Position {
            line: 7,
            character: 20,
        };
        let result = compute(text, pos);

        assert!(
            result.is_some(),
            "Expected completions for dimension value with string type"
        );
        let items = match result {
            Some(CompletionResponse::Array(items)) => items,
            _ => panic!("Expected Array response"),
        };
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].label, "\"\"");
    }

    #[test]
    fn test_compute_schema_property_completions() {
        let text = r#"[default-configs]
per_km_rate = { value = 20.0, schema = { } }

[dimensions]
city = { position = 1, schema = { type = "string" } }
"#;
        // Cursor inside the schema inline table on line 2 (position 40 is inside the schema braces)
        let pos = Position {
            line: 1,
            character: 40,
        };
        let result = compute(text, pos);

        assert!(
            result.is_some(),
            "Expected completions for schema properties"
        );
        let items = match result {
            Some(CompletionResponse::Array(items)) => items,
            _ => panic!("Expected Array response"),
        };
        // Should have JSON Schema draft7 properties
        assert!(items.iter().any(|i| i.label == "type"));
        assert!(items.iter().any(|i| i.label == "enum"));
        assert!(items.iter().any(|i| i.label == "minimum"));
        assert!(items.iter().any(|i| i.label == "maximum"));
    }

    #[test]
    fn test_compute_dimension_property_completions() {
        let text = r#"[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number" } }

[dimensions]
city = { }
"#;
        // Cursor inside the dimension inline table on line 5 (position 8 is inside the braces)
        let pos = Position {
            line: 4,
            character: 8,
        };
        let result = compute(text, pos);

        assert!(
            result.is_some(),
            "Expected completions for dimension properties"
        );
        let items = match result {
            Some(CompletionResponse::Array(items)) => items,
            _ => panic!("Expected Array response"),
        };
        // Should have dimension properties
        assert!(items.iter().any(|i| i.label == "position"));
        assert!(items.iter().any(|i| i.label == "schema"));
        assert!(items.iter().any(|i| i.label == "type"));
        assert!(items.iter().any(|i| i.label == "definitions"));
    }

    #[test]
    fn test_compute_default_config_property_completions() {
        let text = r#"[default-configs]
per_km_rate = { }
"#;
        // Cursor inside the default-config inline table on line 2 (position 15 is inside the braces)
        let pos = Position {
            line: 1,
            character: 15,
        };
        let result = compute(text, pos);

        assert!(
            result.is_some(),
            "Expected completions for default-config properties"
        );
        let items = match result {
            Some(CompletionResponse::Array(items)) => items,
            _ => panic!("Expected Array response"),
        };
        // Should have default-config properties
        assert!(items.iter().any(|i| i.label == "value"));
        assert!(items.iter().any(|i| i.label == "schema"));
    }

    #[test]
    fn test_compute_default_config_key_completions() {
        let text = r#"[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number" } }
surge_factor = { value = 0.0, schema = { type = "number" } }

"#;
        // Cursor on empty line after default-configs section (line 4)
        let pos = Position {
            line: 3,
            character: 0,
        };
        let result = compute(text, pos);

        assert!(
            result.is_some(),
            "Expected completions for default-config keys"
        );
        let items = match result {
            Some(CompletionResponse::Array(items)) => items,
            _ => panic!("Expected Array response"),
        };
        // Should suggest existing config keys
        assert!(items.iter().any(|i| i.label == "per_km_rate"));
        assert!(items.iter().any(|i| i.label == "surge_factor"));
    }

    #[test]
    fn test_compute_dimension_key_completions() {
        let text = r#"[dimensions]
city = { position = 1, schema = { type = "string" } }
vehicle_type = { position = 2, schema = { type = "string" } }

"#;
        // Cursor on empty line after dimensions section (line 4)
        let pos = Position {
            line: 3,
            character: 0,
        };
        let result = compute(text, pos);

        assert!(result.is_some(), "Expected completions for dimension keys");
        let items = match result {
            Some(CompletionResponse::Array(items)) => items,
            _ => panic!("Expected Array response"),
        };
        // Should suggest existing dimension keys
        assert!(items.iter().any(|i| i.label == "city"));
        assert!(items.iter().any(|i| i.label == "vehicle_type"));
    }

    #[test]
    fn test_get_default_config_table_both_formats() {
        // Test with "default-configs" (plural)
        let toml_str = r#"[default-configs]
key1 = { value = 1, schema = { type = "integer" } }
"#;
        let table: toml::Table = toml::from_str(toml_str).unwrap();
        let result = get_default_config_table(&table);
        assert!(result.is_some());
        assert!(result.unwrap().contains_key("key1"));

        // Test with "default-config" (singular)
        let toml_str = r#"[default-config]
key2 = { value = 2, schema = { type = "integer" } }
"#;
        let table: toml::Table = toml::from_str(toml_str).unwrap();
        let result = get_default_config_table(&table);
        assert!(result.is_some());
        assert!(result.unwrap().contains_key("key2"));
    }
}
