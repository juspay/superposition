use std::collections::{HashMap, HashSet};

use derive_more::{Deref, DerefMut, Into};
#[cfg(feature = "diesel_derives")]
use diesel::{deserialize::FromSqlRow, expression::AsExpression, sql_types::Json};
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::{json, Map, Value};
#[cfg(feature = "diesel_derives")]
use superposition_derives::{JsonFromSql, JsonToSql};

use crate::{overridden::filter_config_keys_by_prefix, Cac, Contextual, Exp};

macro_rules! impl_try_from_map {
    ($wrapper:ident, $type:ident, $validate:expr) => {
        impl TryFrom<Map<String, Value>> for $wrapper<$type> {
            type Error = String;

            fn try_from(map: Map<String, Value>) -> Result<Self, Self::Error> {
                Ok(Self($validate(map)?))
            }
        }

        impl<'de> Deserialize<'de> for $wrapper<$type> {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                let map = Map::<String, Value>::deserialize(deserializer)?;
                Self::try_from(map).map_err(serde::de::Error::custom)
            }
        }

        impl $wrapper<$type> {
            pub fn validate_db_data(map: Map<String, Value>) -> Result<Self, String> {
                #[cfg(feature = "disable_db_data_validation")]
                return Ok(Self($type(map)));
                #[cfg(not(feature = "disable_db_data_validation"))]
                return Self::try_from(map);
            }
        }
    };
}

#[derive(Deserialize, Serialize, Clone, Deref, DerefMut, Debug, PartialEq, Into)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(AsExpression, FromSqlRow, JsonFromSql, JsonToSql)
)]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = Json))]
pub struct Overrides(Map<String, Value>);

impl Overrides {
    fn validate_data(override_map: Map<String, Value>) -> Result<Self, String> {
        if override_map.is_empty() {
            log::error!("Override validation error: Override is empty");
            return Err("Override should not be empty".to_owned());
        }
        Ok(Self(override_map))
    }
}

impl IntoIterator for Overrides {
    type Item = (String, Value);
    type IntoIter = <Map<String, Value> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl_try_from_map!(Cac, Overrides, Overrides::validate_data);
impl_try_from_map!(Exp, Overrides, Overrides::validate_data);

#[derive(Deserialize, Serialize, Clone, Deref, Debug, PartialEq, Into)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(AsExpression, FromSqlRow, JsonFromSql, JsonToSql)
)]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = Json))]
pub struct Condition(Map<String, Value>);

impl Condition {
    fn validate_data_for_cac(condition_map: Map<String, Value>) -> Result<Self, String> {
        if condition_map.is_empty() {
            log::error!("Condition validation error: Context is empty");
            return Err("Context should not be empty".to_owned());
        }
        jsonlogic::expression::Expression::from_json(&json!(condition_map)).map_err(
            |msg| {
                log::error!("Condition validation error: {}", msg);
                msg
            },
        )?;
        Ok(Self(condition_map))
    }

    fn validate_data_for_exp(condition_map: Map<String, Value>) -> Result<Self, String> {
        let condition_val = json!(condition_map);
        let ast = jsonlogic::expression::Expression::from_json(&condition_val).map_err(
            |msg| {
                log::error!("Condition validation error: {}", msg);
                msg
            },
        )?;
        let dimensions = ast.get_variable_names().map_err(|msg| {
            log::error!("Error while parsing variable names : {}", msg);
            msg
        })?;
        if dimensions.contains("variantIds") {
            log::error!("experiment's context should not contain variantIds dimension");
            return Err(
                "experiment's context should not contain variantIds dimension"
                    .to_string(),
            );
        }
        Ok(Self(condition_map))
    }
}

impl_try_from_map!(Cac, Condition, Condition::validate_data_for_cac);
impl_try_from_map!(Exp, Condition, Condition::validate_data_for_exp);

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Context {
    pub id: String,
    pub condition: Condition,
    pub priority: i32,
    pub override_with_keys: [String; 1],
}

impl Contextual for Context {
    fn get_condition(&self) -> Condition {
        self.condition.clone()
    }
}

#[repr(C)]
#[derive(Serialize, Deserialize, Clone, Debug, Default)]
pub struct Config {
    pub contexts: Vec<Context>,
    pub overrides: HashMap<String, Overrides>,
    pub default_configs: Map<String, Value>,
}

impl Config {
    pub fn filter_by_dimensions(&self, dimension_data: &Map<String, Value>) -> Self {
        let filtered_context =
            Context::filter_by_eval(self.contexts.clone(), dimension_data);

        let filtered_overrides: HashMap<String, Overrides> = filtered_context
            .iter()
            .flat_map(|ele| {
                let override_with_key = &ele.override_with_keys[0];
                self.overrides
                    .get(override_with_key)
                    .map(|value| (override_with_key.to_string(), value.clone()))
            })
            .collect();

        Self {
            contexts: filtered_context,
            overrides: filtered_overrides,
            default_configs: self.default_configs.clone(),
        }
    }

    pub fn filter_default_by_prefix(
        &self,
        prefix_list: &HashSet<String>,
    ) -> Map<String, Value> {
        filter_config_keys_by_prefix(self.default_configs.clone(), prefix_list)
    }

    fn filter_by_prefix_internal(
        &self,
        prefix_list: &HashSet<String>,
        break_on_validate: bool,
    ) -> Result<Self, String> {
        let mut filtered_overrides: HashMap<String, Overrides> = HashMap::new();

        let filtered_default_config = self.filter_default_by_prefix(prefix_list);

        for (key, overrides) in &self.overrides {
            let filtered_overrides_map: Map<String, Value> = overrides
                .clone()
                .into_iter()
                .filter(|(key, _)| prefix_list.contains(key))
                .collect();

            if break_on_validate {
                let filtered_override_map = Cac::<Overrides>::validate_db_data(
                    filtered_overrides_map,
                )
                .map_err(|err| {
                    format!("failed to decode overrides from db with error {}", err)
                })?;
                filtered_overrides
                    .insert(key.clone(), filtered_override_map.into_inner());
            } else {
                let _ = Cac::<Overrides>::try_from(filtered_overrides_map).map(
                    |filtered_overrides_map| {
                        filtered_overrides
                            .insert(key.clone(), filtered_overrides_map.into_inner())
                    },
                );
            }
        }

        let filtered_context: Vec<Context> = self
            .contexts
            .iter()
            .filter(|context| {
                filtered_overrides.contains_key(&context.override_with_keys[0])
            })
            .cloned()
            .collect();

        let filtered_config = Self {
            contexts: filtered_context,
            overrides: filtered_overrides,
            default_configs: filtered_default_config,
        };

        Ok(filtered_config)
    }

    /// To be used while filtering Config which is being `read from DB`
    /// Meant mostly for `server side usage`
    pub fn try_filter_by_prefix(
        &self,
        prefix_list: &HashSet<String>,
    ) -> Result<Self, String> {
        self.filter_by_prefix_internal(prefix_list, true)
    }

    /// To be used while filtering Config which is `not read from DB`
    /// Meant mostly for `client side usage`
    pub fn filter_by_prefix(&self, prefix_list: &HashSet<String>) -> Self {
        self.filter_by_prefix_internal(prefix_list, false).unwrap()
    }
}
