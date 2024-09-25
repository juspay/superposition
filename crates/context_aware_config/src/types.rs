use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Deserialize)]
pub struct QueryFilters {
    pub count: Option<i64>,
    pub page: Option<i64>,
}

#[derive(Serialize, Debug, Clone, Deserialize)]
pub struct PaginatedResponse<T> {
    pub total_pages: i64,
    pub total_items: i64,
    pub data: Vec<T>,
}
