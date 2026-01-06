//! Helper functions for configuration calculations

use bigdecimal::{BigDecimal, Num};
use num_bigint::BigUint;

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_calculate_weight_from_index() {
        let number_2_100_str = "1267650600228229401496703205376";
        // test 2^100
        let big_decimal =
            BigDecimal::from_str_radix(number_2_100_str, 10).expect("Invalid string format");

        let number_2_200_str =
            "1606938044258990275541962092341162602522202993782792835301376";
        // test 2^200
        let big_decimal_200 =
            BigDecimal::from_str_radix(number_2_200_str, 10).expect("Invalid string format");

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