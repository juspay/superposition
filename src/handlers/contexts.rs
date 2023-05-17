use diesel::QueryResult;

use crate::db::utils::DbActor;
use crate::models::db_models::Contexts;

use crate::db::schema::contexts::dsl::*;
use crate::messages::contexts::{CreateContext, DeleteContext, FetchContext};
use actix::Handler;
use diesel::{self, prelude::*};

use crate::models::insertables::contexts::NewContext;

impl Handler<CreateContext> for DbActor {
    type Result = QueryResult<Contexts>;

    fn handle(&mut self, msg: CreateContext, _: &mut Self::Context) -> Self::Result {
        let mut conn = self.0.get().expect("Error on making DB connection for creating override");

        diesel::insert_into(contexts)
            .values(NewContext {
                key: msg.key,
                value: msg.value,
            })
            .get_result::<Contexts>(&mut conn)
    }
}

impl Handler<FetchContext> for DbActor {
    type Result = QueryResult<Contexts>;

    fn handle(&mut self, msg: FetchContext, _: &mut Self::Context) -> Self::Result {
        let mut conn = self.0.get().expect("Error on making DB connection for fetching override");

        contexts
            .filter(key.eq(msg.key))
            .get_result::<Contexts>(&mut conn)
    }
}

impl Handler<DeleteContext> for DbActor {
    type Result = QueryResult<Contexts>;

    fn handle(&mut self, msg: DeleteContext, _: &mut Self::Context) -> Self::Result {
        let mut conn = self.0.get().expect("Error on making DB connection for fetching override");

        diesel::delete(contexts)
            .filter(key.eq(msg.key))
            .get_result::<Contexts>(&mut conn)
    }
}
