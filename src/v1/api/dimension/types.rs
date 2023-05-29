use serde::Deserialize;

use crate::v1::db::models::DimensionType;

#[derive(Deserialize)]
pub struct CreateReq {
    pub dimension: String,
    pub priority: u16,
    pub r#type: DimensionType,
}
