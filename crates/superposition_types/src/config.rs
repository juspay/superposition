use std::collections::{HashMap, HashSet};

use derive_more::{AsRef, Deref, DerefMut, Into};
#[cfg(feature = "diesel_derives")]
use diesel::{deserialize::FromSqlRow, expression::AsExpression, sql_types::Json};
#[cfg(feature = "jsonlogic")]
use jsonlogic::validation as logic_validation;
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::{Map, Value};
#[cfg(feature = "diesel_derives")]
use superposition_derives::{JsonFromSql, JsonToSql};
use uniffi::deps::anyhow;

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
uniffi::custom_type!(Overrides, HashMap<String, String>, {
    lower: |v| {
        v.iter().map(|(k, v)| (
            k.clone(), serde_json::to_string(v).unwrap()
        )).collect::<HashMap<String, String>>()
    },
    try_lift: |v| {
        // Deserialize the JSON string back into a map
        v.iter().map(|(k, s)| {
            serde_json::from_str::<Value>(s)
                .map(|v| (k.clone(), v))
                .map_err(|err| anyhow::anyhow!(err.to_string()))
        }).collect::<Result<Map<String, Value>, anyhow::Error>>().map(Overrides)
    }
});

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

impl From<Cac<Overrides>> for Exp<Overrides> {
    fn from(cac: Cac<Overrides>) -> Self {
        Self(cac.into_inner())
    }
}

impl From<Exp<Overrides>> for Cac<Overrides> {
    fn from(exp: Exp<Overrides>) -> Self {
        Self(exp.into_inner())
    }
}

impl TryFrom<Overrides> for Exp<Overrides> {
    type Error = std::io::Error;
    fn try_from(value: Overrides) -> Result<Self, Self::Error> {
        Exp::<Overrides>::try_from(Into::<serde_json::Map<String, Value>>::into(value))
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))
    }
}
impl From<Exp<Overrides>> for Overrides {
    fn from(value: Exp<Overrides>) -> Self {
        value.into_inner()
    }
}

type VariantOverrides = Exp<Overrides>;
uniffi::custom_type!(VariantOverrides, Overrides);

#[derive(Deserialize, Serialize, Clone, Deref, Debug, PartialEq, Into, AsRef)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(AsExpression, FromSqlRow, JsonFromSql, JsonToSql)
)]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = Json))]
pub struct Condition(Map<String, Value>);
uniffi::custom_type!(Condition, HashMap<String, String>, {
    lower: |v| {
        v.iter().map(|(k, v)| (
            k.clone(), serde_json::to_string(v).unwrap()
        )).collect::<HashMap<String, String>>()
    },
    try_lift: |v| {
        // Deserialize the JSON string back into a map
        v.iter().map(|(k, s)| {
            serde_json::from_str::<Value>(s)
                .map(|v| (k.clone(), v))
                .map_err(|err| anyhow::anyhow!(err.to_string()))
        }).collect::<Result<Map<String, Value>, anyhow::Error>>().map(Condition)
    }
});

#[cfg(feature = "jsonlogic")]
static CAC_CONDITION_VALIDATION_CONFIG: logic_validation::ValidationConfig =
    logic_validation::ValidationConfig {
        require_and_wrapper: Some(logic_validation::RequireAndWrapper {
            allow_empty: false,
        }),
    };

#[cfg(feature = "jsonlogic")]
static EXP_CONDITION_VALIDATION_CONFIG: logic_validation::ValidationConfig =
    logic_validation::ValidationConfig {
        require_and_wrapper: Some(logic_validation::RequireAndWrapper {
            allow_empty: true,
        }),
    };

impl Condition {
    #[cfg(feature = "jsonlogic")]
    fn validate_data_for_cac(condition_map: Map<String, Value>) -> Result<Self, String> {
        let condition_val = serde_json::json!(condition_map);
        logic_validation::validate(&condition_val, &CAC_CONDITION_VALIDATION_CONFIG)
            .map_err(|err| {
                let msg = format!("Condition validation error: {}", err.message);
                log::error!("{}", msg);
                msg
            })?;
        jsonlogic::expression::Expression::from_json(&condition_val).map_err(|msg| {
            log::error!("Condition validation error: {}", msg);
            msg
        })?;
        Ok(Self(condition_map))
    }

    #[cfg(not(feature = "jsonlogic"))]
    fn validate_data_for_cac(condition_map: Map<String, Value>) -> Result<Self, String> {
        if condition_map.is_empty() {
            log::error!("Condition validation error: Context is empty");
            return Err("Context should not be empty".to_owned());
        }
        Ok(Self(condition_map))
    }

    #[cfg(feature = "jsonlogic")]
    fn validate_data_for_exp(condition_map: Map<String, Value>) -> Result<Self, String> {
        let condition_val = serde_json::json!(condition_map);
        logic_validation::validate(&condition_val, &EXP_CONDITION_VALIDATION_CONFIG)
            .map_err(|err| {
                let msg = format!("Condition validation error: {}", err.message);
                log::error!("{}", msg);
                msg
            })?;

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

    #[cfg(not(feature = "jsonlogic"))]
    fn validate_data_for_exp(condition_map: Map<String, Value>) -> Result<Self, String> {
        if condition_map.contains_key("variantIds") {
            return Err(
                "experiment's context should not contain variantIds dimension"
                    .to_string(),
            );
        }
        Ok(Self(condition_map))
    }

    #[cfg(feature = "jsonlogic")]
    pub fn contains(&self, other_condition: &Condition) -> Result<bool, String> {
        let condition1 = Value::Object(self.0.clone());
        let condition1 = jsonlogic::expression::Expression::from_json(&condition1)?
            .get_variable_names_and_values()?;
        let condition2 = Value::Object(other_condition.0.clone());
        let condition2 = jsonlogic::expression::Expression::from_json(&condition2)?
            .get_variable_names_and_values()?;
        Ok(condition1.is_superset(&condition2))
    }

    #[cfg(not(feature = "jsonlogic"))]
    pub fn contains(&self, other_condition: &Condition) -> Result<bool, String> {
        for (key, value) in &other_condition.0 {
            if let Some(val) = self.0.get(key) {
                if *val != *value {
                    return Ok(false);
                }
            } else {
                return Ok(false);
            }
        }
        Ok(true)
    }
}

impl_try_from_map!(Cac, Condition, Condition::validate_data_for_cac);
impl_try_from_map!(Exp, Condition, Condition::validate_data_for_exp);

impl From<Cac<Condition>> for Exp<Condition> {
    fn from(cac: Cac<Condition>) -> Self {
        Self(cac.into_inner())
    }
}

impl TryFrom<Exp<Condition>> for Cac<Condition> {
    type Error = String;

    fn try_from(exp: Exp<Condition>) -> Result<Self, Self::Error> {
        Self::try_from(exp.into_inner().0)
    }
}

impl TryFrom<Condition> for Exp<Condition> {
    type Error = String;

    fn try_from(value: Condition) -> Result<Self, Self::Error> {
        let map: Map<String, Value> = value.into();
        Self::try_from(map)
    }
}

impl TryFrom<Condition> for Cac<Condition> {
    type Error = String;

    fn try_from(value: Condition) -> Result<Self, Self::Error> {
        let map: Map<String, Value> = value.into();
        Self::try_from(map)
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, uniffi::Record)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Context {
    pub id: String,
    pub condition: Condition,
    pub priority: i32,
    pub weight: i32,
    pub override_with_keys: OverrideWithKeys,
}

impl Contextual for Context {
    fn get_condition(&self) -> Condition {
        self.condition.clone()
    }
}

#[derive(Serialize, Deserialize, PartialEq, Clone, Debug, Deref, DerefMut)]
#[serde(try_from = "Vec<String>")]
pub struct OverrideWithKeys([String; 1]);

impl OverrideWithKeys {
    pub fn new(key: String) -> Self {
        Self([key])
    }

    pub fn get_key(&self) -> &String {
        &self.0[0]
    }
}

impl TryFrom<Vec<String>> for OverrideWithKeys {
    type Error = std::io::Error;

    fn try_from(value: Vec<String>) -> Result<Self, Self::Error> {
        let size = value.len();
        value.try_into().map(Self).map_err(|_| {
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!("OverrideWithKeys must contain exactly 1 element, got {size}"),
            )
        })
    }
}

impl From<OverrideWithKeys> for Vec<String> {
    fn from(value: OverrideWithKeys) -> Self {
        value.0.to_vec()
    }
}

uniffi::custom_type!(OverrideWithKeys, Vec<String>);

#[repr(C)]
#[derive(Serialize, Deserialize, Clone, Debug, Default)]
#[cfg_attr(test, derive(PartialEq))]
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
                let override_with_key = ele.override_with_keys.get_key();
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
        filter_config_keys_by_prefix(&self.default_configs, prefix_list)
    }

    pub fn filter_by_prefix(&self, prefix_list: &HashSet<String>) -> Self {
        let mut filtered_overrides: HashMap<String, Overrides> = HashMap::new();

        let filtered_default_config = self.filter_default_by_prefix(prefix_list);

        for (key, overrides) in &self.overrides {
            let filtered_overrides_map =
                filter_config_keys_by_prefix(overrides, prefix_list);

            let _ = Cac::<Overrides>::try_from(filtered_overrides_map).map(
                |filtered_overrides_map| {
                    filtered_overrides
                        .insert(key.clone(), filtered_overrides_map.into_inner())
                },
            );
        }

        let filtered_context: Vec<Context> = self
            .contexts
            .iter()
            .filter(|context| {
                filtered_overrides.contains_key(context.override_with_keys.get_key())
            })
            .cloned()
            .collect();

        Self {
            contexts: filtered_context,
            overrides: filtered_overrides,
            default_configs: filtered_default_config,
        }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use std::collections::{HashMap, HashSet};

    use serde_json::{from_value, json, Map, Number, Value};

    use super::Config;

    #[cfg(feature = "jsonlogic")]
    pub(crate) fn get_config() -> Config {
        let config_json = json!({
            "contexts": [
                {
                    "id": "40c2564c114e1a2036bc6ce0e730289d05e117b051f2d286d6e7c68960f3bc7d",
                    "condition": {
                        "==": [
                            {
                                "var": "test3"
                            },
                            true
                        ]
                    },
                    "priority": 0,
                    "weight": 0,
                    "override_with_keys": [
                        "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef"
                    ]
                },
                {
                    "id": "691ed369369ac3facdd07e5dd388e07ed682a7e212a04b7bcd0186e6f2d0d097",
                    "condition": {
                        "==": [
                            {
                                "var": "test2"
                            },
                            123
                        ]
                    },
                    "priority": 1,
                    "weight": 1,
                    "override_with_keys": [
                        "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9"
                    ]
                },
                {
                    "id": "9fbf3b9fa10caaaf31f6003cbd20ed36d40efe73b5c6b238288c0a96e6933500",
                    "condition": {
                        "and": [
                            {
                                "==": [
                                    {
                                        "var": "test3"
                                    },
                                    false
                                ]
                            },
                            {
                                "==": [
                                    {
                                        "var": "test"
                                    },
                                    "test"
                                ]
                            }
                        ]
                    },
                    "priority": 2,
                    "weight": 2,
                    "override_with_keys": [
                        "e2fa5b38c3a1448cf0e27f9d555fdb8964a686d8ae41b70b55e6ee30359b87c8"
                    ]
                }
            ],
            "overrides": {
                "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef": {
                    "test.test1": 5,
                    "test2.test": "testval"
                },
                "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9": {
                    "test2.key": true,
                    "test2.test": "value"
                },
                "e2fa5b38c3a1448cf0e27f9d555fdb8964a686d8ae41b70b55e6ee30359b87c8": {
                    "key1": true
                }
            },
            "default_configs": {
                "key1": false,
                "test.test.test1": 1,
                "test.test1": 12,
                "test2.key": false,
                "test2.test": "def_val"
            }
        });

        from_value(config_json).unwrap()
    }

    #[cfg(not(feature = "jsonlogic"))]
    pub(crate) fn get_config() -> Config {
        let config_json = json!({
            "contexts": [
                {
                    "id": "40c2564c114e1a2036bc6ce0e730289d05e117b051f2d286d6e7c68960f3bc7d",
                    "condition": {
                        "test3": true
                    },
                    "priority": 0,
                    "weight": 0,
                    "override_with_keys": [
                        "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef"
                    ]
                },
                {
                    "id": "691ed369369ac3facdd07e5dd388e07ed682a7e212a04b7bcd0186e6f2d0d097",
                    "condition": {
                       "test2": 123
                    },
                    "priority": 1,
                    "weight": 1,
                    "override_with_keys": [
                        "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9"
                    ]
                },
                {
                    "id": "9fbf3b9fa10caaaf31f6003cbd20ed36d40efe73b5c6b238288c0a96e6933500",
                    "condition": {
                        "test3": false,
                        "test": "test"
                    },
                    "priority": 2,
                    "weight": 2,
                    "override_with_keys": [
                        "e2fa5b38c3a1448cf0e27f9d555fdb8964a686d8ae41b70b55e6ee30359b87c8"
                    ]
                }
            ],
            "overrides": {
                "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef": {
                    "test.test1": 5,
                    "test2.test": "testval"
                },
                "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9": {
                    "test2.key": true,
                    "test2.test": "value"
                },
                "e2fa5b38c3a1448cf0e27f9d555fdb8964a686d8ae41b70b55e6ee30359b87c8": {
                    "key1": true
                }
            },
            "default_configs": {
                "key1": false,
                "test.test.test1": 1,
                "test.test1": 12,
                "test2.key": false,
                "test2.test": "def_val"
            }
        });

        from_value(config_json).unwrap()
    }

    pub(crate) fn get_dimension_data1() -> Map<String, Value> {
        Map::from_iter(vec![(String::from("test3"), Value::Bool(true))])
    }

    pub(crate) fn get_dimension_data2() -> Map<String, Value> {
        Map::from_iter(vec![
            (String::from("test3"), Value::Bool(false)),
            (String::from("test"), Value::String(String::from("key"))),
        ])
    }

    pub(crate) fn get_dimension_data3() -> Map<String, Value> {
        Map::from_iter(vec![
            (String::from("test3"), Value::Bool(false)),
            (String::from("test"), Value::String(String::from("key"))),
            (String::from("test2"), Value::Number(Number::from(12))),
        ])
    }

    #[cfg(feature = "jsonlogic")]
    pub(crate) fn get_dimension_filtered_config1() -> Config {
        let config_json = json!({
            "contexts": [
                {
                    "id": "40c2564c114e1a2036bc6ce0e730289d05e117b051f2d286d6e7c68960f3bc7d",
                    "condition": {
                        "==": [
                            {
                                "var": "test3"
                            },
                            true
                        ]
                    },
                    "priority": 0,
                    "weight": 0,
                    "override_with_keys": [
                        "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef"
                    ]
                },
                {
                    "id": "691ed369369ac3facdd07e5dd388e07ed682a7e212a04b7bcd0186e6f2d0d097",
                    "condition": {
                        "==": [
                            {
                                "var": "test2"
                            },
                            123
                        ]
                    },
                    "priority": 1,
                    "weight": 1,
                    "override_with_keys": [
                        "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9"
                    ]
                }
            ],
            "overrides": {
                "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9": {
                    "test2.key": true,
                    "test2.test": "value"
                },
                "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef": {
                    "test.test1": 5,
                    "test2.test": "testval"
                }
            },
            "default_configs": {
                "key1": false,
                "test.test.test1": 1,
                "test.test1": 12,
                "test2.key": false,
                "test2.test": "def_val"
            }
        });

        from_value(config_json).unwrap()
    }

    #[cfg(not(feature = "jsonlogic"))]
    pub(crate) fn get_dimension_filtered_config1() -> Config {
        let config_json = json!({
            "contexts": [
                {
                    "id": "40c2564c114e1a2036bc6ce0e730289d05e117b051f2d286d6e7c68960f3bc7d",
                    "condition": {
                        "test3": true
                    },
                    "priority": 0,
                    "weight": 0,
                    "override_with_keys": [
                        "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef"
                    ]
                },
                {
                    "id": "691ed369369ac3facdd07e5dd388e07ed682a7e212a04b7bcd0186e6f2d0d097",
                    "condition": {
                        "test2": 123
                    },
                    "priority": 1,
                    "weight": 1,
                    "override_with_keys": [
                        "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9"
                    ]
                }
            ],
            "overrides": {
                "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9": {
                    "test2.key": true,
                    "test2.test": "value"
                },
                "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef": {
                    "test.test1": 5,
                    "test2.test": "testval"
                }
            },
            "default_configs": {
                "key1": false,
                "test.test.test1": 1,
                "test.test1": 12,
                "test2.key": false,
                "test2.test": "def_val"
            }
        });

        from_value(config_json).unwrap()
    }

    #[cfg(feature = "jsonlogic")]
    pub(crate) fn get_dimension_filtered_config2() -> Config {
        let config_json = json!({
            "contexts": [
                {
                    "id": "691ed369369ac3facdd07e5dd388e07ed682a7e212a04b7bcd0186e6f2d0d097",
                    "condition": {
                        "==": [
                            {
                                "var": "test2"
                            },
                            123
                        ]
                    },
                    "priority": 1,
                    "weight": 1,
                    "override_with_keys": [
                        "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9"
                    ]
                }
            ],
            "overrides": {
                "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9": {
                    "test2.key": true,
                    "test2.test": "value"
                }
            },
            "default_configs": {
                "key1": false,
                "test.test.test1": 1,
                "test.test1": 12,
                "test2.key": false,
                "test2.test": "def_val"
            }
        });

        from_value(config_json).unwrap()
    }

    #[cfg(not(feature = "jsonlogic"))]
    pub(crate) fn get_dimension_filtered_config2() -> Config {
        let config_json = json!({
            "contexts": [
                {
                    "id": "691ed369369ac3facdd07e5dd388e07ed682a7e212a04b7bcd0186e6f2d0d097",
                    "condition": {
                        "test2": 123
                    },
                    "priority": 1,
                    "weight": 1,
                    "override_with_keys": [
                        "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9"
                    ]
                }
            ],
            "overrides": {
                "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9": {
                    "test2.key": true,
                    "test2.test": "value"
                }
            },
            "default_configs": {
                "key1": false,
                "test.test.test1": 1,
                "test.test1": 12,
                "test2.key": false,
                "test2.test": "def_val"
            }
        });

        from_value(config_json).unwrap()
    }

    pub(crate) fn get_dimension_filtered_config3() -> Config {
        let config_json = json!(  {
            "contexts": [],
            "overrides": {},
            "default_configs": {
                "key1": false,
                "test.test.test1": 1,
                "test.test1": 12,
                "test2.key": false,
                "test2.test": "def_val"
            }
        });

        from_value(config_json).unwrap()
    }

    #[cfg(feature = "jsonlogic")]
    pub(crate) fn get_prefix_filtered_config1() -> Config {
        let config_json = json!({
            "contexts": [
                {
                    "id": "40c2564c114e1a2036bc6ce0e730289d05e117b051f2d286d6e7c68960f3bc7d",
                    "condition": {
                        "==": [
                            {
                                "var": "test3"
                            },
                            true
                        ]
                    },
                    "priority": 0,
                    "weight": 0,
                    "override_with_keys": [
                        "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef"
                    ]
                }
            ],
            "overrides": {
                "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef": {
                    "test.test1": 5
                }
            },
            "default_configs": {
                "test.test.test1": 1,
                "test.test1": 12
            }
        });
        from_value(config_json).unwrap()
    }

    #[cfg(not(feature = "jsonlogic"))]
    pub(crate) fn get_prefix_filtered_config1() -> Config {
        let config_json = json!({
            "contexts": [
                {
                    "id": "40c2564c114e1a2036bc6ce0e730289d05e117b051f2d286d6e7c68960f3bc7d",
                    "condition": {
                        "test3": true
                    },
                    "priority": 0,
                    "weight": 0,
                    "override_with_keys": [
                        "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef"
                    ]
                }
            ],
            "overrides": {
                "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef": {
                    "test.test1": 5
                }
            },
            "default_configs": {
                "test.test.test1": 1,
                "test.test1": 12
            }
        });
        from_value(config_json).unwrap()
    }

    #[cfg(feature = "jsonlogic")]
    pub(crate) fn get_prefix_filtered_config2() -> Config {
        let config_json = json!({
            "contexts": [
                {
                    "id": "40c2564c114e1a2036bc6ce0e730289d05e117b051f2d286d6e7c68960f3bc7d",
                    "condition": {
                        "==": [
                            {
                                "var": "test3"
                            },
                            true
                        ]
                    },
                    "priority": 0,
                    "weight": 0,
                    "override_with_keys": [
                        "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef"
                    ]
                },
                {
                    "id": "691ed369369ac3facdd07e5dd388e07ed682a7e212a04b7bcd0186e6f2d0d097",
                    "condition": {
                        "==": [
                            {
                                "var": "test2"
                            },
                            123
                        ]
                    },
                    "priority": 1,
                    "weight": 1,
                    "override_with_keys": [
                        "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9"
                    ]
                }
            ],
            "overrides": {
                "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9": {
                    "test2.key": true,
                    "test2.test": "value"
                },
                "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef": {
                    "test.test1": 5,
                    "test2.test": "testval"
                }
            },
            "default_configs": {
                "test.test.test1": 1,
                "test.test1": 12,
                "test2.key": false,
                "test2.test": "def_val"
            }
        });
        from_value(config_json).unwrap()
    }

    #[cfg(not(feature = "jsonlogic"))]
    pub(crate) fn get_prefix_filtered_config2() -> Config {
        let config_json = json!({
            "contexts": [
                {
                    "id": "40c2564c114e1a2036bc6ce0e730289d05e117b051f2d286d6e7c68960f3bc7d",
                    "condition": {
                        "test3": true
                    },
                    "priority": 0,
                    "weight": 0,
                    "override_with_keys": [
                        "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef"
                    ]
                },
                {
                    "id": "691ed369369ac3facdd07e5dd388e07ed682a7e212a04b7bcd0186e6f2d0d097",
                    "condition": {
                        "test2": 123
                    },
                    "priority": 1,
                    "weight": 1,
                    "override_with_keys": [
                        "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9"
                    ]
                }
            ],
            "overrides": {
                "2b96b6e8c6475d40d0dc92a05360828a304b9c2ed58abbe03958b178b431a5f9": {
                    "test2.key": true,
                    "test2.test": "value"
                },
                "0e72cf409a9eba53446dc858191751accf9f8ad3e6195413933145a497feb0ef": {
                    "test.test1": 5,
                    "test2.test": "testval"
                }
            },
            "default_configs": {
                "test.test.test1": 1,
                "test.test1": 12,
                "test2.key": false,
                "test2.test": "def_val"
            }
        });
        from_value(config_json).unwrap()
    }

    #[test]
    fn filter_by_dimensions() {
        let config = get_config();

        assert_eq!(
            config.filter_by_dimensions(&get_dimension_data1()),
            get_dimension_filtered_config1()
        );

        assert_eq!(
            config.filter_by_dimensions(&get_dimension_data2()),
            get_dimension_filtered_config2()
        );

        assert_eq!(
            config.filter_by_dimensions(&get_dimension_data3()),
            get_dimension_filtered_config3()
        );
    }

    #[test]
    fn filter_default_by_prefix() {
        let config = get_config();

        let prefix_list = HashSet::from_iter(vec![String::from("test.")]);

        assert_eq!(
            config.filter_default_by_prefix(&prefix_list),
            json!({
                "test.test.test1": 1,
                "test.test1": 12,
            })
            .as_object()
            .unwrap()
            .clone()
        );

        let prefix_list = HashSet::from_iter(vec![String::from("test3")]);

        assert_eq!(config.filter_default_by_prefix(&prefix_list), Map::new());
    }

    #[test]
    fn filter_by_prefix() {
        let config = get_config();

        let prefix_list = HashSet::from_iter(vec![String::from("test.")]);

        assert_eq!(
            config.filter_by_prefix(&prefix_list),
            get_prefix_filtered_config1()
        );

        let prefix_list =
            HashSet::from_iter(vec![String::from("test."), String::from("test2.")]);

        assert_eq!(
            config.filter_by_prefix(&prefix_list),
            get_prefix_filtered_config2()
        );

        let prefix_list = HashSet::from_iter(vec![String::from("abcd")]);

        assert_eq!(
            config.filter_by_prefix(&prefix_list),
            Config {
                contexts: Vec::new(),
                overrides: HashMap::new(),
                default_configs: Map::new(),
            }
        );
    }
}
