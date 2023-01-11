use diesel::QueryResult;

use crate::db::utils::DbActor;
use crate::models::db_models::NewContexts;

use crate::db::schema::newcontexts::dsl::*;
use crate::messages::new_contexts::{CreateNewContext, FetchAllNewContexts, FetchNewContext, DeleteNewContext};
use actix::Handler;
use diesel::{self, prelude::*};

use crate::models::insertables::new_contexts::NewContextInsertion;

impl Handler<CreateNewContext> for DbActor {
    type Result = QueryResult<NewContexts>;

    fn handle(&mut self, msg: CreateNewContext, _: &mut Self::Context) -> Self::Result {
        let mut conn = self.0.get().expect("Error on making DB connection for creating context override");

        diesel::insert_into(newcontexts)
            .values(NewContextInsertion {
                key : msg.key,
                value : msg.value,
                column1: msg.column1,
                column2: msg.column2,
                column3: msg.column3,
                column4: msg.column4,
            })
            .get_result::<NewContexts>(&mut conn)
    }
}

impl Handler<FetchNewContext> for DbActor {
    type Result = QueryResult<Vec<NewContexts>>;

    fn handle(&mut self, _msg: FetchNewContext, _: &mut Self::Context) -> Self::Result {
        let mut conn = self.0.get().expect("Error on making DB connection for fetching context override");

        // ! TODO :: Move filter directly over here (DB scanning)
        newcontexts.get_results::<NewContexts>(&mut conn)
            // .filter(

            //     (column1
            //         .eq(msg.column1)
            //         .or(column1.is_null())
            //     )

            //     .and(
            //         column2
            //         .eq(msg.column2)
            //         .or(column2.is_null())
            //     )
            // )
    }
}

impl Handler<FetchAllNewContexts> for DbActor {
    type Result = QueryResult<Vec<NewContexts>>;

    fn handle(&mut self, _msg: FetchAllNewContexts, _: &mut Self::Context) -> Self::Result {
        let mut conn = self.0.get().expect("Error on making DB connection for fetching context override");
        newcontexts.get_results::<NewContexts>(&mut conn)
    }
}

impl Handler<DeleteNewContext> for DbActor {
    type Result = QueryResult<NewContexts>;

    fn handle(&mut self, msg: DeleteNewContext, _: &mut Self::Context) -> Self::Result {
        let mut conn = self.0.get().expect("Error on making DB connection for fetching context override");

        diesel::delete(newcontexts)
            .filter(key.eq(msg.key))
            .get_result::<NewContexts>(&mut conn)
    }
}