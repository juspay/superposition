#[cfg(feature = "diesel_derives")]
use diesel::AsChangeset;
use serde::{Deserialize, Serialize};

use crate::database::models::{NonEmptyString, OrgStatus};
#[cfg(feature = "diesel_derives")]
use crate::database::superposition_schema::superposition::organisations;

#[derive(Deserialize)]
pub struct CreateRequest {
    pub country_code: Option<String>,
    pub contact_email: Option<String>,
    pub contact_phone: Option<String>,
    pub admin_email: String,
    pub name: NonEmptyString,
    pub sector: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[cfg_attr(feature = "diesel_derives", derive(AsChangeset))]
#[cfg_attr(feature = "diesel_derives", diesel(table_name = organisations))]
pub struct UpdateRequest {
    pub country_code: Option<String>,
    pub contact_email: Option<String>,
    pub contact_phone: Option<String>,
    pub admin_email: Option<String>,
    pub sector: Option<String>,
    pub status: Option<OrgStatus>,
}
