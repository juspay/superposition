use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};
use std::string::String;

use pest::Parser;
use pest::iterators::Pair;
use pest_derive::Parser;
use toml::Value;

// the grammar for context expressions written using PEST
#[derive(Parser)]
#[grammar_inline = r###"
expression = { SOI ~ whitespace* ~ logical ~ whitespace* ~ EOI }

logical = _{ logical_or }
logical_or = { logical_and ~ (whitespace* ~ "||" ~ whitespace* ~ logical_and)* }
logical_and = { comparison ~ (whitespace* ~ "&&" ~ whitespace* ~ comparison)* }

comparison = { term ~ whitespace* ~ comparison_operator ~ whitespace* ~ term }
comparison_operator = { ">=" | "<=" | "<" | ">" | "==" | "!=" }
term = { bool_literal | string_literal | float | integer | dimension }

string_literal = @{ "'" ~ char+ ~ "'" }
bool_literal = @{ "true" | "false" }
integer = @{ ASCII_DIGIT+ }
float = @{ ASCII_DIGIT+ ~ "." ~ ASCII_DIGIT+ }
char = { ASCII_ALPHANUMERIC | "." | "_" }
dimension = @{ "$" ~ char+ }

whitespace = _{ " " | "\t" | "\n" }
"###]
struct CACParser;

#[derive(Debug)]
struct ContextualOverride {
    expression: String,
    extracted_dimensions: Vec<String>,
    overrides: Value,
    priority: i64,
}

impl PartialEq for ContextualOverride {
    fn eq(&self, other: &Self) -> bool {
        (self.extracted_dimensions == other.extracted_dimensions)
            && (self.expression == other.expression)
    }
}

impl Eq for ContextualOverride {}

impl Ord for ContextualOverride {
    fn cmp(&self, other: &Self) -> Ordering {
        other.priority.cmp(&self.priority)
    }
}

impl PartialOrd for ContextualOverride {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Debug)]
pub struct ContextAwareConfig {
    file: String,
    dimension_priority: HashMap<String, i64>,
    default_config: HashMap<String, Value>,
    toml_value: Value,
}

#[derive(Debug, Clone)]
pub struct CACParseError {
    pub message: String,
}

impl CACParseError {
    fn new(msg: &str) -> Self {
        CACParseError {
            message: msg.to_string(),
        }
    }
}

impl fmt::Display for CACParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Unable to parse CAC TOML file: {}", self.message)
    }
}

/// Resolve the absolute path of a file relative to a base file's directory.
fn resolve_import_path(base_file: &str, import_file: &str) -> PathBuf {
    let base_path = Path::new(base_file);
    let base_dir = base_path.parent().unwrap_or(Path::new("."));
    let import_path = Path::new(import_file);

    if import_path.is_absolute() {
        import_path.to_path_buf()
    } else {
        base_dir.join(import_path)
    }
}

/// Load a TOML file and recursively process any `[import]` sections,
/// merging all content into a single TOML Value. Tracks visited files
/// to prevent circular imports.
fn load_and_merge_toml(
    file: &str,
    visited: &mut HashSet<PathBuf>,
) -> Result<Value, CACParseError> {
    let canonical = fs::canonicalize(file).map_err(|e| {
        CACParseError::new(&format!("Failed to resolve path '{}': {}", file, e))
    })?;

    if !visited.insert(canonical.clone()) {
        return Err(CACParseError::new(&format!(
            "Circular import detected: '{}'",
            file
        )));
    }

    let toml_content = fs::read_to_string(&canonical).map_err(|e| {
        CACParseError::new(&format!("Failed to read '{}': {}", file, e))
    })?;

    let mut toml_value: Value = toml::from_str(&toml_content).map_err(|e| {
        CACParseError::new(&format!("Failed to parse '{}': {}", file, e))
    })?;

    // Process imports if present
    let imports = if let Some(import_section) = toml_value.get("import") {
        let imports = match import_section {
            Value::Table(table) => {
                // Single import: [import] file = "..."
                let import_file = table
                    .get("file")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| {
                        CACParseError::new(&format!(
                            "[import] section in '{}' must have a 'file' key with a string value",
                            file
                        ))
                    })?;
                vec![import_file.to_string()]
            }
            Value::Array(arr) => {
                // Multiple imports: [[import]] file = "..."
                let mut files = Vec::new();
                for item in arr {
                    let import_file = item
                        .get("file")
                        .and_then(|v| v.as_str())
                        .ok_or_else(|| {
                            CACParseError::new(&format!(
                                "Each [[import]] entry in '{}' must have a 'file' key with a string value",
                                file
                            ))
                        })?;
                    files.push(import_file.to_string());
                }
                files
            }
            _ => {
                return Err(CACParseError::new(&format!(
                    "'import' in '{}' must be a table or array of tables",
                    file
                )));
            }
        };
        imports
    } else {
        vec![]
    };

    // Remove the import section from the value
    if let Value::Table(ref mut table) = toml_value {
        table.remove("import");
    }

    // Process each import
    for import_file in &imports {
        let resolved_path = resolve_import_path(file, import_file);
        let resolved_str = resolved_path.to_str().ok_or_else(|| {
            CACParseError::new(&format!(
                "Invalid path for import: '{}'",
                import_file
            ))
        })?;

        let imported_value = load_and_merge_toml(resolved_str, visited)?;

        // Merge imported value into current value
        merge_toml_values(&mut toml_value, &imported_value, file, resolved_str)?;
    }

    Ok(toml_value)
}

/// Merge `imported` TOML into `base`, erroring on duplicate keys in
/// default-config, dimensions, and context sections.
fn merge_toml_values(
    base: &mut Value,
    imported: &Value,
    base_file: &str,
    imported_file: &str,
) -> Result<(), CACParseError> {
    let base_table = base.as_table_mut().ok_or_else(|| {
        CACParseError::new(&format!("Root of '{}' is not a TOML table", base_file))
    })?;
    let imported_table = imported.as_table().ok_or_else(|| {
        CACParseError::new(&format!(
            "Root of '{}' is not a TOML table",
            imported_file
        ))
    })?;

    for section in &["default-config", "dimensions", "context"] {
        if let Some(imported_section) = imported_table.get(*section) {
            let imported_section_table = imported_section.as_table().ok_or_else(|| {
                CACParseError::new(&format!(
                    "'{}' in '{}' is not a table",
                    section, imported_file
                ))
            })?;

            if let Some(base_section) = base_table.get(*section) {
                let base_section_table = base_section.as_table().ok_or_else(|| {
                    CACParseError::new(&format!(
                        "'{}' in '{}' is not a table",
                        section, base_file
                    ))
                })?;

                // Check for duplicate keys
                for key in imported_section_table.keys() {
                    if base_section_table.contains_key(key) {
                        return Err(CACParseError::new(&format!(
                            "Duplicate key '{}' in section '{}' found in both '{}' and '{}'",
                            key, section, base_file, imported_file
                        )));
                    }
                }

                // Merge: insert all imported keys into base section
                let merged = base_table.get_mut(*section).unwrap().as_table_mut().unwrap();
                for (key, value) in imported_section_table {
                    merged.insert(key.clone(), value.clone());
                }
            } else {
                // Section doesn't exist in base, just insert it
                base_table.insert(section.to_string(), imported_section.clone());
            }
        }
    }

    Ok(())
}

impl ContextAwareConfig {
    pub fn parse(file: &str) -> Result<ContextAwareConfig, CACParseError> {
        let mut visited = HashSet::new();
        let toml_value = load_and_merge_toml(file, &mut visited)?;

        let mut cac = ContextAwareConfig {
            file: String::from(file),
            dimension_priority: HashMap::new(),
            default_config: HashMap::new(),
            toml_value,
        };

        cac.check()?;
        Ok(cac)
    }

    fn check(&mut self) -> Result<(), CACParseError> {
        if let Some(default_config) = self.toml_value.get("default-config") {
            if let Value::Table(table) = default_config {
                for (key, value) in table {
                    if value.get("value").is_none() {
                        return Err(CACParseError::new(&format!(
                            "configuration: {:?} does not have default value set",
                            key
                        )));
                    }
                    if value.get("schema").is_none() {
                        return Err(CACParseError::new(&format!(
                            "configuration: {:?} does not have schema set",
                            key
                        )));
                    }

                    self.default_config
                        .insert(key.to_string(), value.get("value").unwrap().clone());
                }
            } else {
                return Err(CACParseError::new(&format!(
                    "'default-config' is not a section in file:{}",
                    self.file
                )));
            }
        } else {
            return Err(CACParseError::new(&format!(
                "No 'default-config' section found in file:{}",
                self.file
            )));
        }

        // check sanity of dimensions
        if let Some(dimensions) = self.toml_value.get("dimensions") {
            if let Value::Table(table) = dimensions {
                let mut index = 1;
                for (key, value) in table {
                    if value.get("schema").is_none() {
                        return Err(CACParseError::new(&format!(
                            "dimension: {:?} does not have schema set",
                            key
                        )));
                    }

                    self.dimension_priority.insert(key.to_string(), index);
                    index += 1;
                }
            } else {
                return Err(CACParseError::new("'dimensions' is not a section"));
            }
        } else {
            return Err(CACParseError::new(&format!(
                "No 'dimensions' section found in file:{}",
                self.file
            )));
        }

        // check sanity of overrides
        if let Some(overrides) = self.toml_value.get("context") {
            if let Value::Table(table) = overrides {
                for (context_expression, _override) in table {
                    let parsed = CACParser::parse(Rule::expression, context_expression);
                    match parsed {
                        Ok(_parsed) => {}
                        Err(e) => {
                            return Err(CACParseError::new(&format!(
                                "Could not parse expression for override: {}, Error: {}",
                                context_expression, e
                            )));
                        }
                    }
                    if let Some(contextual_overrides) = _override.as_table() {
                        for (key, _value) in contextual_overrides {
                            if self.default_config.get(key).is_none() {
                                return Err(CACParseError::new(&format!(
                                    "key:'{}' not present in default config",
                                    key
                                )));
                            }
                        }
                    }
                }
            } else {
                return Err(CACParseError::new(&format!(
                    "'overrides' is not a table in file:{}",
                    self.file
                )));
            }
        } else {
            return Err(CACParseError::new(&format!(
                "No 'overrides' table found in file:{}",
                self.file
            )));
        }

        Ok(())
    }

    pub fn get_resolved_config(
        &self,
        dimensions: &HashMap<String, Value>,
    ) -> HashMap<String, Value> {
        let mut chosen_overrides = BinaryHeap::new();
        if let Some(overrides) = self.toml_value.get("context") {
            // Check if it's a Table type
            if let Value::Table(table) = overrides {
                // Iterate over the table
                for (context_expression, overrides) in table {
                    let parsed = CACParser::parse(Rule::expression, context_expression);
                    // println!("{:?}", parsed);
                    match parsed {
                        Ok(_parsed) => {
                            let expression = _parsed.into_iter().next().unwrap();
                            let mut extracted_dimensions: Vec<String> = Vec::new();
                            let result = evaluate_context_expression(
                                expression,
                                dimensions,
                                &mut extracted_dimensions,
                            );
                            match result {
                                Value::Boolean(true) => {
                                    // compute priority of override and insert into matching overrides
                                    let priority = compute_priority(
                                        &extracted_dimensions,
                                        &self.dimension_priority,
                                    );
                                    // println!("expression: {:#?}, extracted_dimensions: {:#?}, priority: {:#?}, override: {:#?}",
                                    // context_expression, extracted_dimensions, priority, overrides);
                                    chosen_overrides.push(ContextualOverride {
                                        expression: context_expression.to_string(),
                                        overrides: overrides.clone(),
                                        extracted_dimensions,
                                        priority,
                                    });
                                }
                                Value::Boolean(false) => {
                                    // println!("expression: {:#?}, did not match", context_expression);
                                }
                                _ => {
                                    eprintln!(
                                        "did not get a true/false value for override: {}",
                                        context_expression
                                    );
                                }
                            }
                        }
                        Err(e) => {
                            eprintln!(
                                "Could not parse expression for Key: {}, Error: {}",
                                context_expression, e
                            );
                        }
                    }
                }
            } else {
                eprintln!("'overrides' is not a table");
            }
        } else {
            eprintln!("No 'overrides' table found");
        }

        let mut merged_data: HashMap<String, Value> = self.default_config.clone();
        while let Some(item) = chosen_overrides.pop() {
            for (key, _value) in self.default_config.iter() {
                match item.overrides.get(key) {
                    None => {
                        // do nothing
                    }
                    _ => {
                        // println!("expression: {:?}, key: {:?}, value: {:?}", item.expression, key, item.overrides.get(key).unwrap());
                        merged_data.insert(
                            key.to_string(),
                            item.overrides.get(key).unwrap().clone(),
                        );
                    }
                }
            }
        }

        merged_data
    }
}

fn compute_priority(
    dimensions: &[String],
    allowed_dimensions: &HashMap<String, i64>,
) -> i64 {
    let mut priority = 0;

    for dimension in dimensions.iter() {
        priority += allowed_dimensions.get(dimension).unwrap();
    }

    priority
}

fn evaluate_context_expression(
    pair: Pair<Rule>,
    dimensions: &HashMap<String, Value>,
    extracted_dimensions: &mut Vec<String>,
) -> Value {
    // println!("pair: {:?}", pair.as_rule());
    match pair.as_rule() {
        Rule::expression => {
            // For the 'expression' rule, just evaluate its inner logical expression
            evaluate_context_expression(
                pair.into_inner().next().unwrap(),
                dimensions,
                extracted_dimensions,
            )
        }
        // Rule::logical => {
        //     // For the 'logical' rule, just evaluate its inner logical expression
        //     evaluate_context_expression(pair.into_inner().next().unwrap())
        // }
        Rule::logical_or => {
            let mut pairs = pair.into_inner();
            let mut result = evaluate_context_expression(
                pairs.next().unwrap(),
                dimensions,
                extracted_dimensions,
            );
            for pair in pairs {
                let next_value =
                    evaluate_context_expression(pair, dimensions, extracted_dimensions);
                if let (Value::Boolean(lhs), Value::Boolean(rhs)) = (result, next_value) {
                    result = Value::Boolean(lhs || rhs);
                } else {
                    panic!("OR operation requires boolean values");
                }
            }
            result
        }
        Rule::logical_and => {
            let mut pairs = pair.into_inner();
            let mut result = evaluate_context_expression(
                pairs.next().unwrap(),
                dimensions,
                extracted_dimensions,
            );
            for pair in pairs {
                let next_value =
                    evaluate_context_expression(pair, dimensions, extracted_dimensions);
                if let (Value::Boolean(lhs), Value::Boolean(rhs)) = (result, next_value) {
                    // println!("lhs: {}, rhs: {}", lhs, rhs);
                    result = Value::Boolean(lhs && rhs);
                } else {
                    panic!("AND operation requires boolean values");
                }
            }
            result
        }
        // Rule::logical_not => {
        //     let inner = evaluate_context_expression(pair.into_inner().next().unwrap());
        //     if let Value::Bool(val) = inner {
        //         Value::Bool(!val)
        //     } else {
        //         panic!("NOT operation requires a boolean value");
        //     }
        // }
        Rule::comparison => {
            let mut pairs = pair.into_inner();
            let left = evaluate_context_expression(
                pairs.next().unwrap(),
                dimensions,
                extracted_dimensions,
            );
            if let Some(op_pair) = pairs.next() {
                let operator = op_pair.as_str();
                let right = evaluate_context_expression(
                    pairs.next().unwrap(),
                    dimensions,
                    extracted_dimensions,
                );
                // println!("operator:: {:?}", operator);

                match (left, right) {
                    (Value::Integer(lhs), Value::Integer(rhs)) => match operator {
                        "<" => Value::Boolean(lhs < rhs),
                        ">" => Value::Boolean(lhs > rhs),
                        "==" => Value::Boolean(lhs == rhs),
                        "!=" => Value::Boolean(lhs != rhs),
                        ">=" => Value::Boolean(lhs >= rhs),
                        "<=" => Value::Boolean(lhs <= rhs),
                        _ => panic!("Invalid comparison operator"),
                    },
                    (Value::String(lhs), Value::String(rhs)) => match operator {
                        "<" => Value::Boolean(lhs < rhs),
                        ">" => Value::Boolean(lhs > rhs),
                        "==" => Value::Boolean(lhs == rhs),
                        "!=" => Value::Boolean(lhs != rhs),
                        ">=" => Value::Boolean(lhs >= rhs),
                        "<=" => Value::Boolean(lhs <= rhs),
                        _ => panic!("Invalid comparison operator"),
                    },
                    (Value::Float(lhs), Value::Float(rhs)) => match operator {
                        "<" => Value::Boolean(lhs < rhs),
                        ">" => Value::Boolean(lhs > rhs),
                        "==" => Value::Boolean(lhs == rhs),
                        "!=" => Value::Boolean(lhs != rhs),
                        ">=" => Value::Boolean(lhs >= rhs),
                        "<=" => Value::Boolean(lhs <= rhs),
                        _ => panic!("Invalid comparison operator"),
                    },
                    (Value::String(lhs), Value::Integer(rhs)) => {
                        let converted_lhs = lhs.parse::<i64>().unwrap_or_default();
                        match operator {
                            "<" => Value::Boolean(converted_lhs < rhs),
                            ">" => Value::Boolean(converted_lhs > rhs),
                            "==" => Value::Boolean(converted_lhs == rhs),
                            "!=" => Value::Boolean(converted_lhs != rhs),
                            ">=" => Value::Boolean(converted_lhs >= rhs),
                            "<=" => Value::Boolean(converted_lhs <= rhs),
                            _ => panic!("Invalid comparison operator"),
                        }
                    }
                    (Value::String(lhs), Value::Float(rhs)) => {
                        let converted_lhs = lhs.parse::<f64>().unwrap_or_default();
                        match operator {
                            "<" => Value::Boolean(converted_lhs < rhs),
                            ">" => Value::Boolean(converted_lhs > rhs),
                            "==" => Value::Boolean(converted_lhs == rhs),
                            "!=" => Value::Boolean(converted_lhs != rhs),
                            ">=" => Value::Boolean(converted_lhs >= rhs),
                            "<=" => Value::Boolean(converted_lhs <= rhs),
                            _ => panic!("Invalid comparison operator"),
                        }
                    }
                    (Value::Boolean(false), _) => Value::Boolean(false),
                    (Value::Boolean(true), _) => Value::Boolean(true),
                    _ => panic!("Comparison between non-numeric values"),
                }
            } else {
                left
            }
        }
        Rule::dimension => {
            let dimension = pair.as_str();
            let dont_care = &Value::Boolean(false);
            extracted_dimensions.push((dimension[1..]).to_string());
            dimensions.get(&dimension[1..]).unwrap_or(dont_care).clone()
        }
        Rule::term => evaluate_context_expression(
            pair.into_inner().next().unwrap(),
            dimensions,
            extracted_dimensions,
        ),
        Rule::bool_literal => {
            let bool_literal = pair.as_str();
            match bool_literal {
                "true" => Value::Boolean(true),
                "false" => Value::Boolean(false),
                _ => panic!("Unknown identifier: {}", bool_literal),
            }
        }
        Rule::string_literal => {
            let string_literal = String::from(pair.as_str());
            let len = string_literal.len();
            Value::String(string_literal[1..(len - 1)].to_string())
        }
        Rule::integer => Value::Integer(pair.as_str().parse().unwrap()),
        Rule::float => Value::Float(pair.as_str().parse().unwrap()),
        _ => panic!("Unexpected rule: {:?}", pair.as_rule()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fixture_path(name: &str) -> String {
        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        format!("{}/tests/fixtures/{}", manifest_dir, name)
    }

    #[test]
    fn test_parse_without_import() {
        let cac = ContextAwareConfig::parse(&fixture_path("base.cac.toml"));
        assert!(cac.is_ok());
        let cac = cac.unwrap();
        assert!(cac.default_config.contains_key("per_km_rate"));
        assert!(cac.dimension_priority.contains_key("city"));
        assert!(cac.dimension_priority.contains_key("vehicle_type"));
    }

    #[test]
    fn test_parse_with_import() {
        let cac = ContextAwareConfig::parse(&fixture_path("main_with_import.cac.toml"));
        assert!(cac.is_ok());
        let cac = cac.unwrap();

        // Keys from main file
        assert!(cac.default_config.contains_key("per_km_rate"));
        assert!(cac.dimension_priority.contains_key("city"));
        assert!(cac.dimension_priority.contains_key("vehicle_type"));

        // Keys from imported file
        assert!(cac.default_config.contains_key("surge_factor"));
        assert!(cac.dimension_priority.contains_key("hour_of_day"));
    }

    #[test]
    fn test_imported_config_resolution() {
        let cac =
            ContextAwareConfig::parse(&fixture_path("main_with_import.cac.toml")).unwrap();

        let mut dims: HashMap<String, Value> = HashMap::new();
        dims.insert("city".to_string(), Value::String("Delhi".to_string()));
        dims.insert(
            "vehicle_type".to_string(),
            Value::String("cab".to_string()),
        );
        dims.insert("hour_of_day".to_string(), Value::Integer(20));

        let resolved = cac.get_resolved_config(&dims);
        // per_km_rate overridden by $vehicle_type == 'cab' context
        assert_eq!(resolved.get("per_km_rate"), Some(&Value::Float(25.0)));
        // surge_factor overridden by imported context
        assert_eq!(resolved.get("surge_factor"), Some(&Value::Float(5.0)));
    }

    #[test]
    fn test_duplicate_default_config_key() {
        let result =
            ContextAwareConfig::parse(&fixture_path("duplicate_default_config.cac.toml"));
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.message.contains("Duplicate key 'surge_factor'"),
            "Expected duplicate key error, got: {}",
            err.message
        );
    }

    #[test]
    fn test_duplicate_dimension_key() {
        let result =
            ContextAwareConfig::parse(&fixture_path("duplicate_dimension.cac.toml"));
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.message.contains("Duplicate key 'hour_of_day'"),
            "Expected duplicate key error, got: {}",
            err.message
        );
    }

    #[test]
    fn test_duplicate_context_key() {
        let result =
            ContextAwareConfig::parse(&fixture_path("duplicate_context.cac.toml"));
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.message.contains("Duplicate key"),
            "Expected duplicate key error, got: {}",
            err.message
        );
    }

    #[test]
    fn test_circular_import() {
        let result = ContextAwareConfig::parse(&fixture_path("circular_a.cac.toml"));
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.message.contains("Circular import"),
            "Expected circular import error, got: {}",
            err.message
        );
    }
}
