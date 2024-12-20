use diesel::{
    r2d2::{ConnectionManager, Pool},
    PgConnection,
};

pub mod utils;

pub type PgSchemaConnectionPool = Pool<ConnectionManager<PgConnection>>;
