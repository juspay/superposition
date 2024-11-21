use crate::db::{
    models::{Context, Dimension},
    schema::{contexts::dsl::contexts, dimensions::dsl::*},
};
use crate::helpers::DimensionData;
use diesel::{
    r2d2::{ConnectionManager, PooledConnection},
    PgConnection, RunQueryDsl,
};
use jsonschema::{Draft, JSONSchema};
use service_utils::helpers::extract_dimensions;
use std::collections::HashMap;
use superposition_macros::{db_error, unexpected_error};
use superposition_types::{
    cac::{
        models::{Context, Dimension},
        schema::{contexts::dsl::contexts, dimensions::dsl::*},
    },
    result as superposition, Cac, Condition,
};

pub fn get_dimension_data(
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<Vec<Dimension>> {
    Ok(dimensions.load::<Dimension>(conn)?)
}

pub fn get_dimension_data_map(
    dimensions_vec: &Vec<Dimension>,
) -> superposition::Result<HashMap<String, DimensionData>> {
    let dimension_schema_map = dimensions_vec
        .into_iter()
        .filter_map(|item| {
            let compiled_schema = JSONSchema::options()
                .with_draft(Draft::Draft7)
                .compile(&item.schema)
                .ok()?;

            Some((
                item.dimension.clone(),
                DimensionData {
                    schema: compiled_schema,
                    priority: item.priority,
                    position: item.position,
                },
            ))
        })
        .collect();

    Ok(dimension_schema_map)
}

pub fn get_dimension_usage_context_ids(
    key: &str,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<Vec<String>> {
    let result: Vec<Context> = contexts.load(conn).map_err(|err| {
        log::error!("failed to fetch contexts with error: {}", err);
        db_error!(err)
    })?;

    let mut context_ids = vec![];
    for context in result.iter() {
        let condition = Cac::<Condition>::validate_db_data(context.value.clone().into())
            .map_err(|err| {
                log::error!("generate_cac : failed to decode context from db {}", err);
                unexpected_error!(err)
            })?
            .into_inner();

        extract_dimensions(&condition)?
            .get(key)
            .map(|_| context_ids.push(context.id.to_owned()));
    }
    Ok(context_ids)
}
