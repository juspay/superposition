#[cfg(feature = "diesel_derives")]
use diesel::{
    expression::AsExpression,
    pg::Pg,
    serialize::ToSql,
    sql_types::{BigInt, Nullable},
};
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::Value;

pub mod config;
pub mod context;
pub mod default_config;
pub mod dimension;
#[cfg(feature = "experimentation")]
pub mod experiment_groups;
#[cfg(feature = "experimentation")]
pub mod experiments;
pub mod functions;
pub mod type_templates;
pub mod webhook;
pub mod workspace;

pub fn default_true() -> bool {
    true
}

#[derive(
    Deserialize,
    Serialize,
    PartialEq,
    Copy,
    Clone,
    strum_macros::EnumIter,
    strum_macros::Display,
    Default,
)]
#[serde(rename_all = "lowercase")]
#[strum(serialize_all = "lowercase")]
pub enum DimensionMatchStrategy {
    Exact,
    #[default]
    Subset,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "diesel_derives", derive(AsExpression))]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = Nullable<BigInt>))]
pub enum I64Update {
    Add(i64),
    Remove,
}

impl I64Update {
    pub fn get_value(&self) -> Option<i64> {
        match self {
            I64Update::Add(value) => Some(*value),
            I64Update::Remove => None,
        }
    }
}

pub fn deserialize_option_i64<'de, D>(
    deserializer: D,
) -> Result<Option<I64Update>, D::Error>
where
    D: Deserializer<'de>,
{
    let opt: Value = Deserialize::deserialize(deserializer)?;
    match opt {
        Value::String(val) => {
            if &val == "null" {
                Ok(Some(I64Update::Remove))
            } else {
                match val.parse::<i64>() {
                    Ok(config_version) => Ok(Some(I64Update::Add(config_version))),
                    Err(_) => {
                        log::error!("Expected a bigint string as the value.");
                        Err(serde::de::Error::custom(
                            "Expected a bigint string as the value.",
                        ))
                    }
                }
            }
        }
        Value::Null => Ok(Some(I64Update::Remove)),
        _ => {
            log::error!("Expected a bigint string or null literal as the value.");
            Err(serde::de::Error::custom(
                "Expected a bigint string or null literal as the value.",
            ))
        }
    }
}

impl<'de> Deserialize<'de> for I64Update {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserialize_option_i64(deserializer)?.ok_or_else(|| {
            serde::de::Error::custom("Some i64 field is missing or invalid.")
        })
    }
}

impl Serialize for I64Update {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Self::Add(name) => serializer.serialize_str(&name.to_string()),
            Self::Remove => serializer.serialize_str("null"),
        }
    }
}

#[cfg(feature = "diesel_derives")]
impl ToSql<Nullable<BigInt>, Pg> for I64Update {
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, Pg>,
    ) -> diesel::serialize::Result {
        let num = match self {
            Self::Add(name) => Some(*name),
            Self::Remove => None,
        };
        <Option<i64> as ToSql<Nullable<BigInt>, Pg>>::to_sql(&num, &mut out.reborrow())
    }
}

pub fn option_i64_from_value(value: Value) -> Result<Option<i64>, String> {
    match value {
        Value::String(val) => match val.parse::<i64>() {
            Ok(num) => Ok(Some(num)),
            Err(_) => {
                log::error!("Expected a bigint string as the value.");
                Err("Expected a bigint string as the value.".to_string())
            }
        },
        Value::Null => Ok(None),
        _ => {
            log::error!("Expected a bigint string or null literal as the value.");
            Err("Expected a bigint string or null literal as the value.".to_string())
        }
    }
}

pub mod i64_option_formatter {
    use serde::{self, Deserialize, Deserializer, Serializer};
    use serde_json::Value;

    use super::option_i64_from_value;

    pub fn serialize<S>(value: &Option<i64>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match value {
            Some(v) => serializer.serialize_str(&v.to_string()),
            None => serializer.serialize_none(),
        }
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Option<i64>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = Value::deserialize(deserializer)?;
        option_i64_from_value(s).map_err(serde::de::Error::custom)
    }
}
