use diesel::query_builder::AsChangeset;
use superposition_types::database::{
    models::{ChangeReason, Description},
    schema::secrets,
};

#[derive(Debug, Clone, AsChangeset)]
#[diesel(table_name = secrets)]
pub struct UpdateSecretChangeset {
    pub encrypted_value: Option<String>,
    pub description: Option<Description>,
    pub change_reason: ChangeReason,
}
