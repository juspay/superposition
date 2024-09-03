use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap};
use std::fmt;
use std::fs;
use std::path::Path;
use std::string::String;
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
pub struct CACParseError;

impl fmt::Display for CACParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Unable to parse CAC TOML file.")
    }
}

impl ContextAwareConfig {
    pub fn parse(file: &str) -> Result<ContextAwareConfig, CACParseError> {
        let toml_file_path = Path::new(file);

        // Read the content of the TOML file
        let toml_content =
            fs::read_to_string(toml_file_path).expect("Failed to read the TOML file");

        // Parse the TOML content
        let toml_value =
            toml::from_str(&toml_content).expect("Failed to parse the TOML file");

        let mut cac: ContextAwareConfig = ContextAwareConfig {
            file: String::from(file),
            dimension_priority: HashMap::new(),
            default_config: HashMap::new(),
            toml_value,
        };

        match cac.check() {
            true => Ok(cac),
            false => Err(CACParseError),
        }
    }

    fn check(&mut self) -> bool {
        if let Some(default_config) = self.toml_value.get("default-config") {
            // Check if it's a Table type
            if let Value::Table(table) = default_config {
                // Iterate over the table
                for (key, value) in table {
                    // println!("{:?}, {:?}", key, value);
                    if value.get("value").is_none() {
                        eprintln!(
                            "configuration: {:?} does not have default value set",
                            key
                        );
                        return false;
                    }
                    if value.get("schema").is_none() {
                        eprintln!("configuration: {:?} does not have schema set", key);
                        return false;
                    }

                    self.default_config
                        .insert(key.to_string(), value.get("value").unwrap().clone());
                }
            } else {
                eprintln!("'default-config' is not a section in file:{}", self.file);
                return false;
            }
        } else {
            eprintln!("No 'default-config' section found in file:{}", self.file);
            return false;
        }

        // check sanity of dimensions
        if let Some(dimensions) = self.toml_value.get("dimensions") {
            // Check if it's a Table type
            if let Value::Table(table) = dimensions {
                // Iterate over the table
                let mut index = 1;
                for (key, value) in table {
                    // println!("{:?}, {:?}", key, value);
                    if value.get("schema").is_none() {
                        eprintln!("dimension: {:?} does not have schema set", key);
                        return false;
                    }

                    self.dimension_priority.insert(key.to_string(), index);
                    index += 1;
                }
            } else {
                eprintln!("'dimensions' is not a section");
                return false;
            }
        } else {
            eprintln!("No 'dimensions' section found in file:{}", self.file);
            return false;
        }

        // check sanity of overrides
        if let Some(overrides) = self.toml_value.get("context") {
            // Check if it's a Table type
            if let Value::Table(table) = overrides {
                // Iterate over the table
                for (context_expression, _override) in table {
                    let parsed = CACParser::parse(Rule::expression, context_expression);
                    // println!("{:?}", parsed);
                    match parsed {
                        Ok(_parsed) => {
                            // nothing to do CAC override expressions parsed correctly
                        }
                        Err(e) => {
                            eprintln!(
                                "Could not parse expression for override: {}, Error: {}",
                                context_expression, e
                            );
                            return false;
                        }
                    }
                    if let Some(contextual_overrides) = _override.as_table() {
                        for (key, _value) in contextual_overrides {
                            match self.default_config.get(key) {
                                None => {
                                    eprintln!(
                                        "key:'{}' not present in default config",
                                        key
                                    );
                                    return false;
                                }
                                _ => {
                                    // do nothing
                                }
                            }
                        }
                    }
                }
            } else {
                eprintln!("'overrides' is not a table in file:{}", self.file);
                return false;
            }
        } else {
            eprintln!("No 'overrides' table found in file:{}", self.file);
            return false;
        }

        true
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
