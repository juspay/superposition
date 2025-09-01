use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap};
use std::fmt;
use std::fs;
use std::path::Path;
use std::string::String;
use superposition_types::{Condition, Context, Overrides};
use toml::Value;

#[derive(Debug)]
struct ContextualOverride {
    context: HashMap<String, Value>,
    overrides: Value,
    priority: i64,
}

impl PartialEq for ContextualOverride {
    fn eq(&self, other: &Self) -> bool {
        self.context == other.context
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
    contexts: Vec<Context>,
    overrides: HashMap<String, Overrides>,
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
            contexts: Vec::new(),
            overrides: HashMap::new(),
            default_config: HashMap::new(),
            toml_value,
        };

        match cac.parse_and_load() {
            true => Ok(cac),
            false => Err(CACParseError),
        }
    }

    fn parse_and_load(&mut self) -> bool {
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

        // check sanity of contexts, overrides and load them
        if let Some(overrides) = self.toml_value.get("context") {
            // Check if it's a Table type
            if let Value::Table(table) = overrides {
                // Iterate over the table
                for (context_expression, _override) in table {
                    let parsed = self.parse_string_to_condition(&context_expression);
                    // println!("{:?}", parsed);
                    match parsed {
                        Ok(context) => {
                            // pub struct Context {
                            //     pub id: String,
                            //     pub condition: Condition,
                            //     pub priority: i32,
                            //     pub weight: i32,
                            //     pub override_with_keys: OverrideWithKeys,
                            // }

                            let priority = self.compute_priority(context);
                            self.contexts.push(Context {
                                condition: context,
                                id: String::from("123"),
                                priority,
                                weight: priority,
                                override_with_keys: [],
                            });
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
                    //
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

    fn parse_string_to_condition(
        &self,
        input: &str,
    ) -> Result<Condition, Box<dyn std::error::Error>> {
        let mut map = HashMap::new();

        // Split by semicolon and process each key-value pair
        for pair in input.split(';') {
            let trimmed = pair.trim();
            if trimmed.is_empty() {
                continue;
            }

            // Split by '=' to get key and value
            let parts: Vec<&str> = trimmed.splitn(2, '=').collect();
            if parts.len() != 2 {
                return Err(format!("Invalid key-value pair: '{}'", trimmed).into());
            }

            let key = parts[0].trim().to_string();
            let value_str = parts[1].trim();

            // Try to parse as different types and convert to toml::Value
            let value = if let Ok(int_val) = value_str.parse::<i64>() {
                Value::Integer(int_val)
            } else if let Ok(float_val) = value_str.parse::<f64>() {
                Value::Float(float_val)
            } else if let Ok(bool_val) = value_str.parse::<bool>() {
                Value::Boolean(bool_val)
            } else {
                Value::String(value_str.to_string())
            };

            if self.dimension_priority.contains_key(&key) {
                map.insert(key, value);
            } else {
                return Err(format!("un-declared dimension: {}", key).into()); // &str
            }
        }

        Ok(map)
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
                    let parsed = self.parse_string_to_hashmap(&context_expression);
                    // println!("{:?}", parsed);
                    match parsed {
                        Ok(context) => {
                            let result = evaluate_context(context, dimensions);
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
                                        context,
                                        overrides: overrides.clone(),
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

    fn compute_priority(&self, dimensions: &[String]) -> i64 {
        let mut priority = 0;

        for dimension in dimensions.iter() {
            priority += self.dimension_priority.get(dimension).unwrap();
        }

        priority
    }
}

fn evaluate_context(
    context: &HashMap<String, Value>,
    dimensions: &HashMap<String, Value>,
) -> bool {
    return true;
}
