use std::collections::HashMap;

use crate::db::{models::Dimension, schema::dimensions::dsl::*};
use diesel::RunQueryDsl;
use diesel::{
    r2d2::{ConnectionManager, PooledConnection},
    PgConnection,
};
use jsonschema::{Draft, JSONSchema};

pub fn get_all_dimension_schema_map(
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> anyhow::Result<HashMap<String, (JSONSchema, i32)>> {
    let dimensions_vec = dimensions.load::<Dimension>(conn)?;

    let dimension_schema_map = dimensions_vec
        .into_iter()
        .filter_map(|item| {
            let compiled_schema = JSONSchema::options()
                .with_draft(Draft::Draft7)
                .compile(&item.schema)
                .ok()?;

            Some((item.dimension, (compiled_schema, i32::from(item.priority))))
        })
        .collect();

    Ok(dimension_schema_map)
}
