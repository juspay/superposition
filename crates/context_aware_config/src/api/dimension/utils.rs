use std::collections::HashMap;

use chrono::Utc;
use diesel::{query_dsl::methods::SchemaNameDsl, ExpressionMethods, RunQueryDsl};
use jsonschema::{Draft, JSONSchema};
#[cfg(not(feature = "jsonlogic"))]
use serde_json::{Map, Value};
#[cfg(feature = "jsonlogic")]
use service_utils::helpers::extract_dimensions;
use service_utils::service::types::SchemaName;
use superposition_macros::{bad_argument, db_error, unexpected_error};
use superposition_types::{
    api::dimension::DimensionName,
    database::{
        models::{
            cac::{Context, Dimension, Position},
            ChangeReason,
        },
        schema::{contexts::dsl::contexts, dimensions::dsl::*},
    },
    result as superposition, Cac, Condition, DBConnection,
};

use crate::helpers::DimensionData;

pub fn get_dimension_data(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<Vec<Dimension>> {
    Ok(dimensions
        .schema_name(schema_name)
        .load::<Dimension>(conn)?)
}

pub fn get_dimension_data_map(
    dimensions_vec: &[Dimension],
) -> superposition::Result<HashMap<String, DimensionData>> {
    let dimension_schema_map = dimensions_vec
        .iter()
        .filter_map(|item| {
            let compiled_schema = JSONSchema::options()
                .with_draft(Draft::Draft7)
                .compile(&item.schema)
                .ok()?;

            Some((
                item.dimension.clone(),
                DimensionData {
                    schema: compiled_schema,
                    position: item.position.into(),
                },
            ))
        })
        .collect();

    Ok(dimension_schema_map)
}

pub fn get_dimension_usage_context_ids(
    key: &str,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<Vec<String>> {
    let result: Vec<Context> =
        contexts
            .schema_name(schema_name)
            .load(conn)
            .map_err(|err| {
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

        cfg_if::cfg_if! {
            if #[cfg(feature = "jsonlogic")] {
                let map = extract_dimensions(&condition)?;
            } else {
                let map: Map<String, Value> = condition.into();
            }
        }

        if map.get(key).is_some() {
            context_ids.push(context.id.to_owned())
        }
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

/// Update the dependency graph of the cohorted dimension
/// Follow its parents and update their graphs as well
pub fn create_connections_with_dependents(
    cohorted_dimension: &str,
    cohort_dimension: &str,
    user_email: &str,
    schema_name: &SchemaName,
    conn: &mut DBConnection,
) -> superposition::Result<()> {
    let mut dimensions_vector = get_dimension_data(conn, schema_name)?;
    let reason = format!(
        "System Auto updated the dependency graph due to the creation of {}",
        cohort_dimension
    );
    for dim in dimensions_vector.iter_mut() {
        if let Some(current_deps) = dim.dependency_graph.get_mut(cohorted_dimension) {
            current_deps.push(cohort_dimension.to_string());
            dim.dependency_graph
                .insert(cohort_dimension.to_string(), vec![]);
            update_dimensions_in_db(dim, &reason, user_email, schema_name, conn)?;
        } else if dim.dimension == cohorted_dimension {
            dim.dependency_graph.insert(
                cohorted_dimension.to_string(),
                vec![cohort_dimension.to_string()],
            );
            dim.dependency_graph
                .insert(cohort_dimension.to_string(), vec![]);
            update_dimensions_in_db(dim, &reason, user_email, schema_name, conn)?;
        }
    }
    Ok(())
}

/// Update the dependency graph of the cohorted dimension
/// Follow its parents and update their graphs as well
pub fn remove_connections_with_dependents(
    deleted_dimension_name: &str,
    cohorted_dimension: &str,
    user_email: &str,
    schema_name: &SchemaName,
    conn: &mut DBConnection,
) -> superposition::Result<()> {
    let mut dimensions_vector = get_dimension_data(conn, schema_name)?;
    let reason = format!(
        "System Auto updated the dependency graph due to the removal of {}",
        deleted_dimension_name
    );
    for dim in dimensions_vector.iter_mut() {
        let mut to_be_updated = dim.dimension == cohorted_dimension;
        dim.dependency_graph.remove(deleted_dimension_name);
        if let Some(current_deps) = dim.dependency_graph.get_mut(cohorted_dimension) {
            current_deps.retain(|d| d != deleted_dimension_name);
            to_be_updated = true;
        }
        if to_be_updated {
            update_dimensions_in_db(dim, &reason, user_email, schema_name, conn)?;
        }
    }
    Ok(())
}

fn update_dimensions_in_db(
    dimension_data: &mut Dimension,
    reason: &str,
    user_email: &str,
    schema_name: &SchemaName,
    conn: &mut DBConnection,
) -> superposition::Result<()> {
    let reason =
        ChangeReason::try_from(reason.to_string()).map_err(|e| unexpected_error!(e))?;
    dimension_data.change_reason = reason.clone();
    dimension_data.last_modified_by = user_email.to_string();
    dimension_data.last_modified_at = Utc::now();
    diesel::update(dimensions)
        .filter(dimension.eq(&dimension_data.dimension))
        .set(dimension_data.clone())
        .schema_name(schema_name)
        .execute(conn)?;
    Ok(())
}
