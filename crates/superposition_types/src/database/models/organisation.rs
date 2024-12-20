#[cfg(feature = "diesel_derives")]
use super::super::schema::*;
use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};

#[cfg(feature = "diesel_derives")]
use diesel::{AsChangeset, Insertable, QueryId, Queryable, Selectable};

#[derive(
    Debug, Clone, Copy, PartialEq, Deserialize, Serialize, strum_macros::Display,
)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
#[cfg_attr(
    feature = "diesel_derives",
    derive(diesel_derive_enum::DbEnum, QueryId)
)]
#[cfg_attr(feature = "diesel_derives", DbValueStyle = "SCREAMING_SNAKE_CASE")]
#[cfg_attr(
    feature = "diesel_derives",
    ExistingTypePath = "crate::database::schema::sql_types::OrgStatus"
)]
pub enum OrgStatus {
    Active,
    Inactive,
    PendingKyb,
}

#[derive(Clone, Serialize, Deserialize, Debug)]
#[cfg_attr(
    feature = "diesel_derives",
    derive(Queryable, Selectable, Insertable, AsChangeset)
)]
#[cfg_attr(feature = "diesel_derives", diesel(check_for_backend(diesel::pg::Pg)))]
#[cfg_attr(feature = "diesel_derives", diesel(primary_key(id)))]
#[cfg_attr(feature = "diesel_derives", diesel(treat_none_as_null = true))]
pub struct Organisation {
    pub id: String,
    pub name: String,
    pub country_code: Option<String>,
    pub contact_email: Option<String>,
    pub contact_phone: Option<String>,
    pub created_by: String,
    pub admin_email: String,
    pub status: OrgStatus,
    pub sector: Option<String>,
    pub created_at: NaiveDateTime,
    pub updated_at: NaiveDateTime,
    pub updated_by: String,
}
