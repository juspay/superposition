use crate::types::{Variant, VariantType};
use serde_json::{Map, Value};

pub fn gen_variant_rows(variants: &[Variant]) -> Result<Vec<Map<String, Value>>, String> {
    let rows = variants
        .iter()
        .enumerate()
        .map(|(i, variant)| {
            let variant_name = match variant.variant_type {
                VariantType::CONTROL => "Control".into(),
                VariantType::EXPERIMENTAL => format!("Variant-{i}"),
            };
            let mut row_data = variant
                .overrides
                .iter()
                .map(|(key, value)| (key.clone(), value.clone()))
                .collect::<Vec<(String, Value)>>();
            row_data.extend_from_slice(&[
                (String::from("Variant"), variant_name.into()),
                (String::from("variant_id"), variant.id.clone().into()),
            ]);
            Map::from_iter(row_data)
        })
        .collect::<Vec<Map<String, Value>>>();
    Ok(rows)
}
