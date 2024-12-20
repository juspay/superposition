use serde::{Deserialize, Serialize};
use superposition_types::database::models::OrgStatus;

// Request payload for creating an organisation
#[derive(Deserialize)]
pub struct CreateOrganisationRequest {
    pub country_code: Option<String>,
    pub contact_email: Option<String>,
    pub contact_phone: Option<String>,
    pub admin_email: String,
    pub name: String,
    pub sector: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct UpdateOrganisationRequest {
    pub country_code: Option<String>,
    pub contact_email: Option<String>,
    pub contact_phone: Option<String>,
    pub admin_email: Option<String>,
    pub sector: Option<String>,
    pub status: Option<OrgStatus>,
}
