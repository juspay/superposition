use std::collections::HashMap;

use crate::{
    components::table::types::Column,
    types::{Variant, VariantType},
};
use leptos::{view, IntoView};
use serde_json::{Map, Value};

pub fn gen_variant_table(
    variants: &[Variant],
) -> Result<(Vec<Map<String, Value>>, Vec<Column>), String> {
    let mut columns = vec![Column::default("Config Key".into())];
    let mut row_map: HashMap<&String, Map<String, Value>> = HashMap::new();
    for (i, variant) in variants.iter().enumerate() {
        let name = match variant.variant_type {
            VariantType::CONTROL => format!("{}", variant.variant_type),
            VariantType::EXPERIMENTAL => format!("Variant-{}", i),
        };
        columns.push(Column::new(name.clone(), None, |value: &str, _| {
            view! { <span>{value.to_string()}</span> }.into_view()
        }));
        for (config, value) in variant.overrides.iter() {
            match row_map.get_mut(config) {
                Some(c) => {
                    c.insert(name.clone(), value.clone());
                }
                None => {
                    let mut m = Map::new();
                    m.insert("Config Key".into(), Value::String(config.clone()));
                    m.insert(name.clone(), value.clone());
                    row_map.insert(config, m);
                }
            }
        }
    }
    let rows = row_map.into_values().collect();
    Ok((rows, columns))
}
