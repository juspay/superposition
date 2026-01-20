use diesel::{prelude::Queryable, Selectable};
use superposition_types::database::{
    models::cac::{FunctionCode, FunctionRuntimeVersion, FunctionType},
    schema::functions,
};

#[derive(Clone, Selectable, Queryable)]
#[diesel(table_name = functions)]
pub struct FunctionInfo {
    pub function_name: String,
    pub function_type: FunctionType,
    pub published_code: Option<FunctionCode>,
    pub published_runtime_version: Option<FunctionRuntimeVersion>,
}
