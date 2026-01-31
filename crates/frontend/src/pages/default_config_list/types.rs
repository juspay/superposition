use serde::{Deserialize, Deserializer};
use superposition_derives::{IsEmpty, QueryParam};
use superposition_types::{IsEmpty, custom_query::QueryParam};

use crate::pages::default_config::CreatePageParams;

#[derive(PartialEq, Clone, IsEmpty, QueryParam)]
pub struct PageParams {
    pub grouped: bool,
    #[query_param(skip_if_empty)]
    pub prefix: Option<String>,
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

impl From<PageParams> for CreatePageParams {
    fn from(params: PageParams) -> Self {
        Self {
            prefix: params.prefix,
        }
    }
}
