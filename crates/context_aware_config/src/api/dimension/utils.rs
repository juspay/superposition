use std::collections::HashMap;

use chrono::Utc;
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use service_utils::service::types::SchemaName;
use superposition_macros::unexpected_error;
use superposition_types::{
    database::{
        models::{
            cac::{Context, DependencyGraph, Dimension, DimensionType},
            ChangeReason,
        },
        schema::{contexts::dsl::contexts, dimensions::dsl::*},
    },
    result as superposition, Cac, Condition, DBConnection, DimensionInfo, ExtendedMap,
};

pub fn get_dimensions_data(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<Vec<Dimension>> {
    Ok(dimensions
        .schema_name(schema_name)
        .load::<Dimension>(conn)?)
}

pub fn get_dimension_usage_context_ids(
    key: &str,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<Vec<String>> {
    let result: Vec<Context> = contexts.schema_name(schema_name).load(conn)?;

    let mut context_ids = vec![];
    for context in result.iter() {
        let condition = Cac::<Condition>::validate_db_data(context.value.clone().into())
            .map_err(|err| {
                log::error!("generate_cac : failed to decode context from db {}", err);
                unexpected_error!(err)
            })?
            .into_inner();

        if condition.get(key).is_some() {
            context_ids.push(context.id.to_owned())
        }
    }
    Ok(context_ids)
}

/// Update the dependency graph of the cohorted dimension
/// Follow its parents and update their graphs as well
pub fn create_connections_with_dependents(
    cohorted_dimension: &str,
    dimension_name: &str,
    user_email: &str,
    schema_name: &SchemaName,
    conn: &mut DBConnection,
) -> superposition::Result<()> {
    let mut dimensions_vector = get_dimensions_data(conn, schema_name)?;
    let reason = format!(
        "System Auto updated the dependency graph due to the creation of {}",
        dimension_name
    );
    for dim in dimensions_vector.iter_mut() {
        if dim.dimension == cohorted_dimension
            && !dim.dependency_graph.contains_key(cohorted_dimension)
        {
            dim.dependency_graph
                .insert(cohorted_dimension.to_string(), vec![]);
        }
        if let Some(current_deps) = dim.dependency_graph.get_mut(cohorted_dimension) {
            current_deps.push(dimension_name.to_string());
            dim.dependency_graph
                .insert(dimension_name.to_string(), vec![]);
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
    let mut dimensions_vector = get_dimensions_data(conn, schema_name)?;
    let reason = format!(
        "System Auto updated the dependency graph due to the removal of {}",
        deleted_dimension_name
    );
    for dim in dimensions_vector.iter_mut() {
        let mut to_be_updated = dim.dimension == cohorted_dimension;
        dim.dependency_graph.remove(deleted_dimension_name);
        if let Some(current_deps) = dim.dependency_graph.get_mut(cohorted_dimension) {
            current_deps.retain(|d| d != deleted_dimension_name);
            if current_deps.is_empty() && dim.dimension == cohorted_dimension {
                dim.dependency_graph.remove(cohorted_dimension);
            }
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

pub fn fetch_dimensions_info_map(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<HashMap<String, DimensionInfo>> {
    let dimensions_map = dimensions
        .select((
            dimension,
            schema,
            position,
            dimension_type,
            dependency_graph,
            value_compute_function_name,
        ))
        .order_by(position.asc())
        .schema_name(schema_name)
        .load::<(
            String,
            ExtendedMap,
            i32,
            DimensionType,
            DependencyGraph,
            Option<String>,
        )>(conn)?
        .into_iter()
        .map(
            |(key, schema_value, pos, dim_type, dep_graph, value_compute_fn)| {
                (
                    key,
                    DimensionInfo {
                        schema: schema_value,
                        position: pos,
                        dimension_type: dim_type,
                        dependency_graph: dep_graph,
                        value_compute_function_name: value_compute_fn,
                    },
                )
            },
        )
        .collect();

    Ok(dimensions_map)
}
