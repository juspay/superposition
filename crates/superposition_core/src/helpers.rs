//! Helper functions for configuration calculations

use std::collections::{HashMap, HashSet};

use bigdecimal::{BigDecimal, Num, ToPrimitive};
use itertools::Itertools;
use num_bigint::BigUint;
use serde_json::{Map, Value};
use superposition_types::{
    Condition, Context, DimensionInfo, OverrideWithKeys, Overrides,
};

/// Calculate weight from a position index using 2^index formula
///
/// This function computes 2 raised to the power of the given index,
/// returning the result as a BigDecimal. This is used for calculating
/// context weights and priorities based on dimension positions.
///
/// # Arguments
/// * `index` - The position index to calculate 2^index for
///
/// # Returns
/// * `Ok(BigDecimal)` - The calculated weight (2^index)
/// * `Err(String)` - Error message if parsing fails
///
/// # Examples
/// ```
/// use superposition_core::helpers::calculate_weight_from_index;
///
/// // 2^0 = 1
/// assert_eq!(calculate_weight_from_index(0).unwrap().to_string(), "1");
///
/// // 2^1 = 2
/// assert_eq!(calculate_weight_from_index(1).unwrap().to_string(), "2");
///
/// // 2^10 = 1024
/// assert_eq!(calculate_weight_from_index(10).unwrap().to_string(), "1024");
/// ```
pub fn calculate_weight_from_index(index: u32) -> Result<BigDecimal, String> {
    let base = BigUint::from(2u32);
    let result = base.pow(index);
    let biguint_str = &result.to_str_radix(10);
    BigDecimal::from_str_radix(biguint_str, 10).map_err(|err| {
        log::error!("failed to parse bigdecimal with error: {}", err.to_string());
        String::from("failed to parse bigdecimal with error")
    })
}

pub fn calculate_context_weight(
    context: &Map<String, Value>,
    dimensions_info: &HashMap<String, DimensionInfo>,
) -> Result<BigDecimal, String> {
    let dimensions: HashSet<String> = context.keys().cloned().collect();

    let mut weight = BigDecimal::from(0);
    for dimension in dimensions {
        let position = dimensions_info
            .get(&dimension)
            .map(|x| x.position)
            .ok_or_else(|| {
                let msg =
                    format!("Dimension:{} not found in Dimension schema map", dimension);
                log::error!("{}", msg);
                msg
            })?;
        weight += calculate_weight_from_index(position as u32)?;
    }
    Ok(weight)
}

fn json_to_sorted_string(v: &Value) -> String {
    match v {
        Value::Object(m) => {
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
        Value::String(m) => m.to_string(),
        Value::Number(m) => m.to_string(),
        Value::Bool(m) => m.to_string(),
        Value::Null => String::from("null"),
        Value::Array(m) => {
            let mut new_vec =
                m.iter().map(json_to_sorted_string).collect::<Vec<String>>();
            new_vec.sort();
            new_vec.join(",")
        }
    }
}

/// Hash a serde_json Value using BLAKE3
pub fn hash(val: &Value) -> String {
    let sorted_str: String = json_to_sorted_string(val);
    blake3::hash(sorted_str.as_bytes()).to_string()
}

pub fn create_connections_with_dependents(
    cohorted_dimension: &str,
    dimension_name: &str,
    dimensions: &mut HashMap<String, DimensionInfo>,
) {
    for (dim, dim_info) in dimensions.iter_mut() {
        if dim == cohorted_dimension
            && !dim_info.dependency_graph.contains_key(cohorted_dimension)
        {
            dim_info
                .dependency_graph
                .insert(cohorted_dimension.to_string(), vec![]);
        }
        if let Some(current_deps) = dim_info.dependency_graph.get_mut(cohorted_dimension)
        {
            current_deps.push(dimension_name.to_string());
            dim_info
                .dependency_graph
                .insert(dimension_name.to_string(), vec![]);
        }
    }
}

pub fn build_context(
    condition: Condition,
    overrides: Overrides,
    dimensions: &HashMap<String, DimensionInfo>,
) -> Result<(Context, String, Overrides), String> {
    let override_hash = hash(&Value::Object(
        overrides
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect(),
    ));
    let condition_hash = hash(&Value::Object(
        condition
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect(),
    ));

    let priority = calculate_context_weight(&condition, dimensions)
        .map_err(|e| e.to_string())?
        .to_i32()
        .ok_or_else(|| "Failed to convert context weight to i32".to_string())?;

    let context = Context {
        condition,
        id: condition_hash,
        priority,
        override_with_keys: OverrideWithKeys::new(override_hash.clone()),
        weight: 0,
    };

    Ok((context, override_hash, overrides))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_calculate_weight_from_index() {
        let number_2_100_str = "1267650600228229401496703205376";
        // test 2^100
        let big_decimal = BigDecimal::from_str_radix(number_2_100_str, 10)
            .expect("Invalid string format");

        let number_2_200_str =
            "1606938044258990275541962092341162602522202993782792835301376";
        // test 2^200
        let big_decimal_200 = BigDecimal::from_str_radix(number_2_200_str, 10)
            .expect("Invalid string format");

        assert_eq!(Some(big_decimal), calculate_weight_from_index(100).ok());
        assert_eq!(Some(big_decimal_200), calculate_weight_from_index(200).ok());
    }

    #[test]
    fn test_calculate_weight_small_indices() {
        // 2^0 = 1
        assert_eq!(calculate_weight_from_index(0).unwrap().to_string(), "1");
        // 2^1 = 2
        assert_eq!(calculate_weight_from_index(1).unwrap().to_string(), "2");
        // 2^2 = 4
        assert_eq!(calculate_weight_from_index(2).unwrap().to_string(), "4");
        // 2^3 = 8
        assert_eq!(calculate_weight_from_index(3).unwrap().to_string(), "8");
    }
}
