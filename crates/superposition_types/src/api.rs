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
#[cfg(feature = "experimentation")]
pub mod experiment_groups;
#[cfg(feature = "experimentation")]
pub mod experiments;
pub mod functions;
pub mod webhook;
pub mod workspace;

#[derive(Debug, Clone)]
#[cfg_attr(feature = "diesel_derives", derive(AsExpression))]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = Nullable<BigInt>))]
pub enum I64Update {
    Add(i64),
    Remove,
}

pub fn deserialize_option_i64<'de, D>(
    deserializer: D,
) -> Result<Option<I64Update>, D::Error>
where
    D: Deserializer<'de>,
{
    let opt: Value = Deserialize::deserialize(deserializer)?;
    match opt {
        Value::Number(config_version) => {
            if let Some(config_version) = config_version.as_i64() {
                Ok(Some(I64Update::Add(config_version)))
            } else {
                log::error!("Expected a bigint as the value.");
                Err(serde::de::Error::custom("Expected a bigint as the value."))
            }
        }
        Value::String(val) => {
            if &val == "null" {
                Ok(Some(I64Update::Remove))
            } else {
                match val.parse::<i64>() {
                    Ok(config_version) => Ok(Some(I64Update::Add(config_version))),
                    Err(_) => {
                        log::error!("Expected a bigint or bigint string as the value.");
                        Err(serde::de::Error::custom(
                            "Expected a bigint or bigint string as the value.",
                        ))
                    }
                }
            }
        }
        Value::Null => Ok(Some(I64Update::Remove)),
        _ => {
            log::error!("Expected a bigint, bigint string or null literal as the value.");
            Err(serde::de::Error::custom(
                "Expected a bigint, bigint string or null literal as the value.",
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
            Self::Add(name) => serializer.serialize_i64(*name),
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
