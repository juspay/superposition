pub mod models;
#[cfg(feature = "diesel_derives")]
pub mod schema;
#[cfg(feature = "diesel_derives")]
pub mod superposition_schema;
pub mod types;

#[cfg(feature = "disable_db_data_validation")]
pub trait DisableDBValidation {
    type From;
    fn from_db_unvalidated(data: Self::From) -> Self;
}
