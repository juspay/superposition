use core::fmt;
use std::fmt::Display;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct DefaultConfigFilters {
    pub name: Option<String>,
}

impl Display for DefaultConfigFilters {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut query_params = vec![];
        if let Some(key_name) = &self.name {
            query_params.push(format!("name={}", key_name));
        }
        write!(f, "{}", query_params.join("&"))
    }
}
