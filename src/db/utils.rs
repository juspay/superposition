use actix::{Actor, Addr, SyncContext};
use diesel::{
    r2d2::{ConnectionManager, Pool},
    PgConnection,
};

//TODOP separate out AppState from DB
pub struct AppState {
    pub db: Addr<DbActor>,
    pub db_pool: Pool<ConnectionManager<PgConnection>>,
}

pub struct DbActor(pub Pool<ConnectionManager<PgConnection>>);

impl Actor for DbActor {
    type Context = SyncContext<Self>;
}

pub fn get_pool(db_url: &str) -> Pool<ConnectionManager<PgConnection>> {
    let manager: ConnectionManager<PgConnection> = ConnectionManager::<PgConnection>::new(db_url);
    Pool::builder()
        .build(manager)
        .expect("Error building a connection pool")
}
