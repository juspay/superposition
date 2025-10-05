use itertools::{self, Itertools};
use serde_json::{Map, Value as SerdeValue};
use std::collections::HashMap;
use std::fmt;
use std::fs;
use std::path::Path;
use std::string::String;
use superposition_core::eval_config;
use superposition_types::{
    database::models::cac::{DependencyGraph, DimensionType},
    Cac, Condition, Context, DimensionInfo, ExtendedMap, OverrideWithKeys, Overrides,
};
use toml::{Table, Value as TomlValue};

#[derive(Clone, Debug)]
pub struct SuperpositionToml {
    file: String,
    supported_dimensions: HashMap<String, DimensionInfo>,
    default_config: Map<String, SerdeValue>,
    contexts: Vec<Context>,
    overrides: HashMap<String, Overrides>,
    toml_value: TomlValue,
}

#[derive(Debug, Clone)]
pub struct SuperpositionTomlParseError;

impl fmt::Display for SuperpositionTomlParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Unable to parse CAC TOML file.")
    }
}

impl SuperpositionToml {
    pub fn parse(file: &str) -> Result<SuperpositionToml, SuperpositionTomlParseError> {
        let toml_file_path = Path::new(file);

        // Read the content of the TOML file
        let toml_content =
            fs::read_to_string(toml_file_path).expect("Failed to read the TOML file");

        // Parse the TOML content
        let toml_value =
            toml::from_str(&toml_content).expect("Failed to parse the TOML file");

        let mut superposition_toml: SuperpositionToml = SuperpositionToml {
            file: String::from(file),
            supported_dimensions: HashMap::new(),
            contexts: Vec::new(),
            overrides: HashMap::new(),
            default_config: Map::new(),
            toml_value,
        };

        match superposition_toml.parse_and_load() {
            true => Ok(superposition_toml),
            false => Err(SuperpositionTomlParseError),
        }
    }

    fn parse_and_load(&mut self) -> bool {
        if let Some(default_config) = self.toml_value.get("default-config") {
            // Check if it's a Table type
            if let TomlValue::Table(table) = default_config {
                // Iterate over the table
                for (key, value) in table {
                    // println!("default_config: {:?}, {:?}", key, value);
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

                    self.default_config.insert(
                        key.to_string(),
                        toml_value_to_serde_value(value.get("value").unwrap().clone()),
                    );
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
            if let TomlValue::Table(table) = dimensions {
                // Iterate over the table
                let mut index = 1;
                for (key, value) in table {
                    // println!("dimension: {:?}, {:?}", key, value);
                    if value.get("schema").is_none() {
                        eprintln!("dimension: {:?} does not have schema set", key);
                        return false;
                    }

                    self.supported_dimensions.insert(
                        key.to_string(),
                        DimensionInfo {
                            position: index,
                            schema: ExtendedMap::try_from(toml_value_to_serde_value(
                                value.get("schema").unwrap().clone(),
                            ))
                            .unwrap(), // TODO
                            dimension_type: DimensionType::Regular {},
                            dependency_graph: DependencyGraph(HashMap::new()),
                        },
                    );
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
            if let TomlValue::Table(table) = overrides {
                // Iterate over the table
                for (context_expression, overrides) in table {
                    let parsed = self.parse_string_to_condition(&context_expression);
                    // println!("context: {:?}", parsed);
                    match parsed {
                        Err(e) => {
                            eprintln!(
                                "Could not parse expression for override: {}, Error: {}",
                                context_expression, e
                            );
                            return false;
                        }
                        _ => {}
                    }
                    if let Some(contextual_overrides) = overrides.as_table() {
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

                    let override_hash = hash(&toml_value_to_serde_value(
                        TomlValue::Table(overrides.as_table().unwrap().clone()),
                    ));
                    let condition = parsed.unwrap();

                    let priority = self.compute_priority(condition.clone()) as i32;
                    // println!(
                    //     "adding context with priority: {}, override_hash: {}",
                    //     priority, override_hash
                    // );
                    self.contexts.push(Context {
                        condition: condition.clone(),
                        id: hash(&SerdeValue::Object(condition.get_map())),
                        priority,
                        weight: priority,
                        override_with_keys: OverrideWithKeys::new(override_hash.clone()),
                    });
                    self.overrides.insert(
                        override_hash,
                        Overrides::from(
                            toml_table_to_serde_map(
                                overrides.as_table().unwrap().clone(),
                            )
                            .clone(),
                        ),
                    );
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
        let mut map: Map<String, SerdeValue> = Map::new();

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

            // Try to parse as different types and convert to serde::Value
            let value = if let Ok(int_val) = value_str.parse::<i64>() {
                SerdeValue::from(int_val)
            } else if let Ok(float_val) = value_str.parse::<f64>() {
                SerdeValue::from(float_val)
            } else if let Ok(bool_val) = value_str.parse::<bool>() {
                SerdeValue::from(bool_val)
            } else {
                SerdeValue::from(value_str.to_string())
            };

            if self.supported_dimensions.contains_key(&key) {
                map.insert(key, value);
            } else {
                return Err(format!("un-declared dimension: {}", key).into()); // &str
            }
        }

        Cac::<Condition>::try_from(map)
            .or_else(|e| {
                return Err(
                    format!("un-parseable string: {} with error: {}", input, e).into()
                ); // &str
            })
            .map(|a| a.into_inner())
    }

    pub fn get_resolved_config(
        &self,
        input_dimensions: &Map<String, SerdeValue>,
    ) -> Result<Map<String, SerdeValue>, String> {
        eval_config(
            self.default_config.clone(),
            &self.contexts,
            &self.overrides,
            &self.supported_dimensions,
            &input_dimensions,
            superposition_core::MergeStrategy::MERGE,
            None,
        )
    }

    fn compute_priority(&self, condition: Condition) -> i32 {
        let mut priority = 0;

        for dimension in condition.iter() {
            priority +=
                1_i32 << self.supported_dimensions.get(dimension.0).unwrap().position;
        }

        priority
    }
}

pub fn toml_table_to_serde_map(table: Table) -> Map<String, SerdeValue> {
    table
        .into_iter()
        .map(|(key, toml_value)| (key, toml_value_to_serde_value(toml_value)))
        .collect()
}

/// Converts a toml::Value to a serde_json::Value
pub fn toml_value_to_serde_value(toml_value: toml::Value) -> serde_json::Value {
    match toml_value {
        toml::Value::String(s) => serde_json::Value::String(s),
        toml::Value::Integer(i) => serde_json::Value::Number(serde_json::Number::from(i)),
        toml::Value::Float(f) => {
            // Handle potential NaN/Infinity cases that JSON doesn't support
            if f.is_finite() {
                serde_json::Number::from_f64(f)
                    .map(serde_json::Value::Number)
                    .unwrap_or(serde_json::Value::Null)
            } else {
                serde_json::Value::Null
            }
        }
        toml::Value::Boolean(b) => serde_json::Value::Bool(b),
        toml::Value::Datetime(dt) => serde_json::Value::String(dt.to_string()),
        toml::Value::Array(arr) => {
            let json_array: Vec<serde_json::Value> =
                arr.into_iter().map(toml_value_to_serde_value).collect();
            serde_json::Value::Array(json_array)
        }
        toml::Value::Table(table) => {
            let mut json_object = serde_json::Map::new();
            for (key, value) in table {
                json_object.insert(key, toml_value_to_serde_value(value));
            }
            serde_json::Value::Object(json_object)
        }
    }
}

pub fn hash(val: &SerdeValue) -> String {
    let sorted_str: String = json_to_sorted_string(val);
    blake3::hash(sorted_str.as_bytes()).to_string()
}

pub fn json_to_sorted_string(v: &SerdeValue) -> String {
    match v {
        SerdeValue::Object(m) => {
            let mut new_str: String = String::from("");
            for (i, val) in m.iter().sorted_by_key(|item| item.0) {
                let p: String = json_to_sorted_string(val);
                new_str.push_str(i);
                new_str.push_str(&String::from(":"));
                new_str.push_str(&p);
                new_str.push_str(&String::from("$"));
            }
            new_str
        }
        SerdeValue::String(m) => m.to_string(),
        SerdeValue::Number(m) => m.to_string(),
        SerdeValue::Bool(m) => m.to_string(),
        SerdeValue::Null => String::from("null"),
        SerdeValue::Array(m) => {
            let mut new_vec =
                m.iter().map(json_to_sorted_string).collect::<Vec<String>>();
            new_vec.sort();
            new_vec.join(",")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_types() {
        // Test string
        let toml_str = toml::Value::String("hello".to_string());
        let json_str = toml_value_to_serde_value(toml_str);
        assert_eq!(json_str, serde_json::Value::String("hello".to_string()));

        // Test integer
        let toml_int = toml::Value::Integer(42);
        let json_int = toml_value_to_serde_value(toml_int);
        assert_eq!(
            json_int,
            serde_json::Value::Number(serde_json::Number::from(42))
        );

        // Test boolean
        let toml_bool = toml::Value::Boolean(true);
        let json_bool = toml_value_to_serde_value(toml_bool);
        assert_eq!(json_bool, serde_json::Value::Bool(true));
    }

    #[test]
    fn test_complex_types() {
        // Test array
        let toml_array = toml::Value::Array(vec![
            toml::Value::String("item1".to_string()),
            toml::Value::Integer(123),
        ]);
        let json_array = toml_value_to_serde_value(toml_array);
        let expected = serde_json::Value::Array(vec![
            serde_json::Value::String("item1".to_string()),
            serde_json::Value::Number(serde_json::Number::from(123)),
        ]);
        assert_eq!(json_array, expected);

        // Test table
        let mut toml_table = toml::value::Table::new();
        toml_table.insert("name".to_string(), toml::Value::String("test".to_string()));
        toml_table.insert("count".to_string(), toml::Value::Integer(5));

        let toml_value = toml::Value::Table(toml_table);
        let json_value = toml_value_to_serde_value(toml_value);

        let mut expected_map = serde_json::Map::new();
        expected_map.insert(
            "name".to_string(),
            serde_json::Value::String("test".to_string()),
        );
        expected_map.insert(
            "count".to_string(),
            serde_json::Value::Number(serde_json::Number::from(5)),
        );
        let expected = serde_json::Value::Object(expected_map);

        assert_eq!(json_value, expected);
    }
}
