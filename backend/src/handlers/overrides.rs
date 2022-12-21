use diesel::QueryResult;

use crate::db::utils::DbActor;
use crate::models::db_models::Overrides;

use crate::db::schema::overrides::dsl::*;
use crate::messages::overrides::{CreateOverride, DeleteOverride, FetchOverride};
use actix::Handler;
use diesel::{self, prelude::*};

use crate::models::insertables::overrides::OverrideInsertion;

impl Handler<CreateOverride> for DbActor {
    type Result = QueryResult<Overrides>;

    fn handle(&mut self, msg: CreateOverride, _: &mut Self::Context) -> Self::Result {
        let mut conn = self.0.get().expect("Error on making DB connection for creating override");

        diesel::insert_into(overrides)
            .values(OverrideInsertion {
                key: msg.key,
                value: msg.value,
            })
            .get_result::<Overrides>(&mut conn)
    }
}

impl Handler<FetchOverride> for DbActor {
    type Result = QueryResult<Overrides>;

    fn handle(&mut self, msg: FetchOverride, _: &mut Self::Context) -> Self::Result {
        let mut conn = self.0.get().expect("Error on making DB connection for fetching override");

        overrides
            .filter(key.eq(msg.key))
            .get_result::<Overrides>(&mut conn)
    }
}

impl Handler<DeleteOverride> for DbActor {
    type Result = QueryResult<Overrides>;

    fn handle(&mut self, msg: DeleteOverride, _: &mut Self::Context) -> Self::Result {
        let mut conn = self.0.get().expect("Error on making DB connection for fetching override");

        diesel::delete(overrides)
            .filter(key.eq(msg.key))
            .get_result::<Overrides>(&mut conn)
    }
}