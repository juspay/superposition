use diesel::{
    r2d2::{ConnectionManager, Pool},
    PgConnection,
};

pub mod utils;
pub mod types;

pub type PgSchemaConnectionPool = Pool<ConnectionManager<PgConnection>>;
