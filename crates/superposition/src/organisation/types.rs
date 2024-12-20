use serde::{Deserialize, Serialize};

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

// Response type to include `org_id`
#[derive(Serialize)]
pub struct CreateOrganisationResponse {
    pub org_id: String,
}
