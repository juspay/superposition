use diesel::QueryResult;

use crate::db::models::{db_models::Overrides, insertables::overrides::NewOverride};
use crate::db::utils::DbActor;

use crate::db::messages::overrides::{
    CreateOverride, DeleteOverride, FetchAllOverrides, FetchOverride,
};
use crate::db::schema::overrides::dsl::*;
use actix::Handler;
use diesel::{self, prelude::*};

impl Handler<CreateOverride> for DbActor {
    type Result = QueryResult<Overrides>;

    fn handle(&mut self, msg: CreateOverride, _: &mut Self::Context) -> Self::Result {
        let mut conn = self
            .0
            .get()
            .expect("Error on making DB connection for creating override");

        diesel::insert_into(overrides)
            .values(NewOverride {
                key: msg.key,
                value: msg.value,
            })
            .get_result::<Overrides>(&mut conn)
    }
}

impl Handler<FetchOverride> for DbActor {
    type Result = QueryResult<Overrides>;

    fn handle(&mut self, msg: FetchOverride, _: &mut Self::Context) -> Self::Result {
        let mut conn = self
            .0
            .get()
            .expect("Error on making DB connection for fetching override");

        overrides
            .filter(key.eq(msg.key))
            .get_result::<Overrides>(&mut conn)
    }
}

impl Handler<FetchAllOverrides> for DbActor {
    type Result = QueryResult<Vec<Overrides>>;

    fn handle(&mut self, _msg: FetchAllOverrides, _: &mut Self::Context) -> Self::Result {
        let mut conn = self
            .0
            .get()
            .expect("Error on making DB connection for fetching override");
        overrides.get_results::<Overrides>(&mut conn)
    }
}

impl Handler<DeleteOverride> for DbActor {
    type Result = QueryResult<Overrides>;

    fn handle(&mut self, msg: DeleteOverride, _: &mut Self::Context) -> Self::Result {
        let mut conn = self
            .0
            .get()
            .expect("Error on making DB connection for fetching override");

        diesel::delete(overrides)
            .filter(key.eq(msg.key))
            .get_result::<Overrides>(&mut conn)
    }
}
