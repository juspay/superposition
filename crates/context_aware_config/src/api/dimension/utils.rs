use diesel::{
    r2d2::{ConnectionManager, PooledConnection},
    PgConnection, RunQueryDsl,
};
use jsonschema::{Draft, JSONSchema};
use service_utils::helpers::extract_dimensions;
use std::collections::HashMap;
use superposition_macros::{bad_argument, db_error, unexpected_error};
use superposition_types::{
    cac::{
        models::{Context, Dimension},
        schema::{contexts::dsl::contexts, dimensions::dsl::*},
    },
    result as superposition, Cac, Condition,
};

use super::types::{DimensionName, Position};

pub fn get_dimension_data(
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<HashMap<String, (JSONSchema, i32)>> {
    let dimensions_vec = dimensions.load::<Dimension>(conn)?;

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

pub fn validate_dimension_position(
    dimension_name: DimensionName,
    dimension_position: Position,
    max_allowed: i64,
) -> superposition::Result<()> {
    let dimension_name: String = dimension_name.into();
    let dimension_position: i32 = dimension_position.into();
    match (dimension_name.as_str(), dimension_position) {
        ("variantIds", 0) => Ok(()),
        ("variantIds", d_position) => {
            log::error!("invalid position: {d_position} for dimension: variantIds",);
            Err(bad_argument!("variantIds' position should be equal to 0"))
        }
        (_, 0) => {
            log::error!("invalid position: 0 for dimension: {dimension_name}",);
            Err(bad_argument!("Oth position is reserved for variantIds"))
        }
        (_, d_position) if d_position as i64 > max_allowed => {
            log::error!("position {d_position} value exceeds total number of dimensions {max_allowed}");
            Err(bad_argument!(
                "position value exceeds total number of dimensions"
            ))
        }
        _ => Ok(()),
    }
}
