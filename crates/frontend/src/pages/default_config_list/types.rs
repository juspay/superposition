use std::fmt::{self, Display};

use serde::{Deserialize, Deserializer};
use superposition_derives::IsEmpty;
use superposition_types::IsEmpty;

#[derive(PartialEq, Clone, IsEmpty)]
pub struct PageParams {
    pub grouped: bool,
    pub prefix: Option<String>,
}

impl Display for PageParams {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut parts = vec![];

        parts.push(format!("grouped={}", self.grouped));

        if let Some(ref prefix) = self.prefix {
            parts.push(format!("prefix={}", prefix));
        }

        write!(f, "{}", parts.join("&"))
    }
}

impl Default for PageParams {
    fn default() -> Self {
        Self {
            grouped: true,
            prefix: None,
        }
    }
}

impl<'de> Deserialize<'de> for PageParams {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct PageParamsHelper {
            pub grouped: Option<bool>,
            pub prefix: Option<String>,
        }
        let helper = PageParamsHelper::deserialize(deserializer)?;
        Ok(Self {
            grouped: helper.prefix.is_some() || helper.grouped.unwrap_or(true),
            prefix: helper.prefix,
        })
    }
}
