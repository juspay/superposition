use diesel::QueryResult;

use crate::db::models::{
    db_models::CtxOverrides, insertables::context_overrides::CtxOverrideInsertion,
};
use crate::db::utils::DbActor;

use crate::db::messages::context_overrides::{
    CreateCtxOverrides, DeleteCtxOverrides, FetchAllCtxOverrides, FetchCtxOverrides,
};
use crate::db::schema::ctxoverrides::dsl::*;
use actix::Handler;
use diesel::{self, prelude::*};

impl Handler<CreateCtxOverrides> for DbActor {
    type Result = QueryResult<CtxOverrides>;

    fn handle(&mut self, msg: CreateCtxOverrides, _: &mut Self::Context) -> Self::Result {
        let mut conn = self
            .0
            .get()
            .expect("Error on making DB connection for creating context override");

        diesel::insert_into(ctxoverrides)
            .values(CtxOverrideInsertion {
                context_id: msg.context_id,
                override_id: msg.override_id,
            })
            .get_result::<CtxOverrides>(&mut conn)
    }
}

impl Handler<FetchCtxOverrides> for DbActor {
    type Result = QueryResult<CtxOverrides>;

    fn handle(&mut self, msg: FetchCtxOverrides, _: &mut Self::Context) -> Self::Result {
        let mut conn = self
            .0
            .get()
            .expect("Error on making DB connection for fetching context override");

        ctxoverrides
            .filter(context_id.eq(msg.context_id))
            .get_result::<CtxOverrides>(&mut conn)
    }
}

impl Handler<FetchAllCtxOverrides> for DbActor {
    type Result = QueryResult<Vec<CtxOverrides>>;

    fn handle(&mut self, _msg: FetchAllCtxOverrides, _: &mut Self::Context) -> Self::Result {
        let mut conn = self
            .0
            .get()
            .expect("Error on making DB connection for fetching context override");
        ctxoverrides.get_results::<CtxOverrides>(&mut conn)
    }
}

impl Handler<DeleteCtxOverrides> for DbActor {
    type Result = QueryResult<CtxOverrides>;

    fn handle(&mut self, msg: DeleteCtxOverrides, _: &mut Self::Context) -> Self::Result {
        let mut conn = self
            .0
            .get()
            .expect("Error on making DB connection for fetching context override");

        diesel::delete(ctxoverrides)
            .filter(context_id.eq(msg.context_id))
            .get_result::<CtxOverrides>(&mut conn)
    }
}
