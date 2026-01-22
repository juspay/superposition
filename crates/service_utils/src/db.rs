use diesel::{
    PgConnection,
    r2d2::{ConnectionManager, Pool},
};

pub mod utils;

pub type PgSchemaConnectionPool = Pool<ConnectionManager<PgConnection>>;
