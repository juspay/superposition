extern crate derive_more;
use derive_more::{Deref, DerefMut, Display};
use std::collections::HashMap;

use anyhow::anyhow;
use diesel::{
    r2d2::{ConnectionManager, Pool, PooledConnection},
    PgConnection,
};

pub type PgSchemaConnectionPool = Pool<ConnectionManager<PgConnection>>;
pub type PgSchemaConnection = PooledConnection<ConnectionManager<PgConnection>>;

#[derive(Debug, Display)]
#[display(fmt = "connection config {:?}", self)]
pub struct ConnectionConfig {
    name: String,
    database_url: String,
    schema: String,
    count: u32,
}

impl ConnectionConfig {
    pub fn new(name: String, database_url: String, schema: String, count: u32) -> Self {
        ConnectionConfig {
            name,
            database_url,
            schema,
            count,
        }
    }

    pub fn conn_url(&self) -> String {
        if self.database_url.contains("?") {
            format!(
                "{}&options=-c%20search_path%3D{},$user,public",
                self.database_url, self.schema
            )
        } else {
            format!(
                "{}?options=-c%20search_path%3D{},$user,public",
                self.database_url, self.schema
            )
        }
    }
}

#[derive(Deref, DerefMut, Clone)]
pub struct PgSchemaManager(HashMap<String, PgSchemaConnectionPool>);

impl From<Vec<ConnectionConfig>> for PgSchemaManager {
    fn from(value: Vec<ConnectionConfig>) -> Self {
        let mut schema_manager: PgSchemaManager = PgSchemaManager(HashMap::new());
        for config in value.into_iter() {
            let manager = ConnectionManager::<PgConnection>::new(config.conn_url());
            schema_manager.insert(
                config.name.clone(),
                Pool::builder()
                    .max_size(config.count)
                    .build(manager)
                    .expect(format!("Invalid config provided, {}", config.name).as_str()),
            );
        }
        schema_manager
    }
}

impl PgSchemaManager {
    pub fn get_conn(&self, name: String) -> anyhow::Result<PgSchemaConnection> {
        let conn = self
            .get(&name) // gets the pool for the given namespace
            .ok_or_else(|| anyhow!("Invalid connection name provided: {}", name))?
            .get()?; // fetches the connection from the pool
        Ok(conn)
    }
}
