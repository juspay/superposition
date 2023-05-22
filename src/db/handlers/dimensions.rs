use crate::db::models::db_models::Dimension;
use crate::db::utils::DbActor;

use crate::db::messages::dimensions::{CreateDimension, FetchDimension, FetchDimensions};
use crate::db::models::insertables::dimensions::NewDimension;
use crate::db::schema::dimensions::dsl::*;
use actix::Handler;
use diesel::{self, prelude::*};

impl Handler<FetchDimensions> for DbActor {
    type Result = QueryResult<Vec<Dimension>>;

    fn handle(&mut self, _msg: FetchDimensions, _ctx: &mut Self::Context) -> Self::Result {
        let mut conn = self
            .0
            .get()
            .expect("Fetch Dimensions: Unable to establish connection");

        dimensions.get_results::<Dimension>(&mut conn)
    }
}

impl Handler<FetchDimension> for DbActor {
    type Result = QueryResult<Dimension>;

    fn handle(&mut self, msg: FetchDimension, _ctx: &mut Self::Context) -> Self::Result {
        let mut conn = self
            .0
            .get()
            .expect("Fetch Dimension: Unable to establish connection");

        dimensions
            .filter(dimension.eq(msg.dimension))
            .get_result::<Dimension>(&mut conn)
    }
}

impl Handler<CreateDimension> for DbActor {
    type Result = QueryResult<Dimension>;

    fn handle(&mut self, msg: CreateDimension, _ctx: &mut Self::Context) -> Self::Result {
        let mut conn = self
            .0
            .get()
            .expect("Create Dimension: Unable to establish connection");

        let new_dimension = NewDimension {
            dimension: msg.dimension,
            priority: msg.priority,
        };

        diesel::insert_into(dimensions)
            .values(new_dimension)
            .get_result::<Dimension>(&mut conn)
    }
}
