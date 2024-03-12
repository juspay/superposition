use serde::Serialize;

#[derive(Serialize)]
pub struct FunctionCreateRequest {
    pub function_name: String,
    pub function: String,
    pub runtime_version: String,
    pub description: String,
}

#[derive(Serialize)]
pub struct FunctionUpdateRequest {
    pub function: String,
    pub runtime_version: String,
    pub description: String,
}
