use std::collections::HashMap;

use chrono::Utc;
use diesel::{query_dsl::methods::SchemaNameDsl, ExpressionMethods, RunQueryDsl};
use jsonschema::{Draft, JSONSchema};
#[cfg(not(feature = "jsonlogic"))]
use serde_json::{Map, Value};
#[cfg(feature = "jsonlogic")]
use service_utils::helpers::extract_dimensions;
use service_utils::service::types::SchemaName;
use superposition_macros::{bad_argument, db_error, not_found, unexpected_error};
use superposition_types::{
    api::dimension::DimensionName,
    database::{
        models::{
            cac::{Context, DependencyGraph, Dimension, Position},
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

// Dependent dimensions related functions

pub fn validate_and_initialize_dimension_hierarchy(
    dimension_name: &str,
    dependent_dimensions: &[String],
    user_email: &str,
    schema_name: &SchemaName,
    conn: &mut DBConnection,
) -> superposition::Result<DependencyGraph> {
    let mut dependency_map = DependencyGraph::new();

    if dependent_dimensions.is_empty() {
        return Ok(dependency_map);
    }

    detect_self_loop(dimension_name, dependent_dimensions)?;

    let mut dimensions_map = fetch_dimensions_map(conn, schema_name)?;

    // fetch all dependent_dimensions from dimensions_map at once
    let dependent_dimensions_data = dependent_dimensions
        .iter()
        .filter_map(|d| dimensions_map.get(d))
        .map(|(dimension_data, _)| dimension_data.clone())
        .collect::<Vec<Dimension>>();

    for dependent_data in dependent_dimensions_data {
        // Update dependencies' dependents list
        update_dependents(&dependent_data, dimension_name, &mut dimensions_map)?;

        dependency_map.insert_dependents(&dependent_data);
    }

    if !dependent_dimensions.is_empty() {
        dependency_map.insert(
            dimension_name.to_string(),
            dependent_dimensions
                .iter()
                .map(String::from)
                .collect::<Vec<_>>(),
        );
    }

    update_dimensions_in_db(
        &mut dimensions_map,
        user_email,
        &format!(
            "Auto-updated: dependency added to dimension '{dimension_name}' at runtime"
        ),
        schema_name,
        conn,
    )?;

    Ok(dependency_map)
}

pub fn validate_and_update_dimension_hierarchy(
    dimension_data: &Dimension,
    dependent_dimensions: &[String],
    user_email: &str,
    schema_name: &SchemaName,
    conn: &mut DBConnection,
) -> superposition::Result<()> {
    // Return Ok if the dependent_dimension is same as the current dependencies (no change)
    if *dimension_data.dependencies == *dependent_dimensions {
        return Ok(());
    }

    let dimension_name = &dimension_data.dimension;

    detect_self_loop(dimension_name, dependent_dimensions)?;

    // Distinguish between the dependencies that are removed and the ones that are being added
    let (dependencies_to_remove, dependencies_to_add) =
        compute_dependencies_diff(&dimension_data.dependencies, dependent_dimensions);

    let mut dimensions_map = fetch_dimensions_map(conn, schema_name)?;

    // Validate no cycles will be introduced
    validate_no_cycles(&dependencies_to_add, dimension_name, &mut dimensions_map)?;

    // Recompute dimension's dependency graph and update added dependencies
    let new_dependency_graph = build_dependency_graph_and_update_dependencies(
        dimension_name,
        dependent_dimensions,
        &mut dimensions_map,
    )?;

    // Update removed dependencies
    update_dependents_for_removed_dependencies(
        dimension_name,
        &dependencies_to_remove,
        &mut dimensions_map,
    )?;

    // Update the dimension's dependency graph in dimensions_map
    update_dependency_graph(dimension_name, new_dependency_graph, &mut dimensions_map)?;

    // Update the ancestor's dimension_graphs of the dimension
    update_dependents_dependency_graphs_dfs(
        &dimension_data.dependents,
        &mut dimensions_map,
    )?;

    update_dimensions_in_db(
        &mut dimensions_map,
        user_email,
        &format!(
            "Auto-updated: dependency added to dimension '{dimension_name}' at runtime"
        ),
        schema_name,
        conn,
    )?;

    Ok(())
}

pub fn validate_dimension_deletability(
    dimension_name: &str,
    dimension_data: &Dimension,
    user_email: &str,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<()> {
    // If someone is dependent on this i.e. check the dependents, then don't let it be deleted
    if !dimension_data.dependents.is_empty() {
        let dependents_list = dimension_data.dependents.clone().join(", ");
        log::error!(
            "Cannot delete dimension `{}`: it is used as a dependency by: {}",
            dimension_name,
            dependents_list
        );
        return Err(bad_argument!(
            "Cannot delete dimension `{}`: it is used as a dependency by: {}",
            dimension_name,
            dependents_list
        ));
    }

    let dependencies_list = dimension_data.dependencies.clone();

    if !dependencies_list.is_empty() {
        // Remove the dimension from the dependencies' dependents
        let mut dimensions_map = fetch_dimensions_map(conn, schema_name)?;
        update_dependents_for_removed_dependencies(
            dimension_name,
            &dependencies_list,
            &mut dimensions_map,
        )?;
        update_dimensions_in_db(
            &mut dimensions_map,
            user_email,
            &format!("Auto-updated: Dependents adjusted due to deletion of dimension '{dimension_name}'"),
            schema_name,
            conn,
        )?;
    }

    Ok(())
}

fn update_dependents_dependency_graphs_dfs(
    dependents_list: &[String],
    dimensions_map: &mut HashMap<String, (Dimension, bool)>,
) -> superposition::Result<()> {
    // Early return if no dependents
    if dependents_list.is_empty() {
        return Ok(());
    };

    // fetch all dependents from the dimension_map at once
    let dependents_dimension_data: Vec<Dimension> = dependents_list
        .iter()
        .filter_map(|d| dimensions_map.get(d))
        .map(|(dimension_data, _)| dimension_data.clone())
        .collect();

    for dependents_dimension in dependents_dimension_data {
        let dependencies_list = dependents_dimension.dependencies.clone();

        // Recompute the dimension's dependency graph
        let new_dependency_graph = build_dependency_graph_and_update_dependencies(
            &dependents_dimension.dimension,
            &dependencies_list,
            dimensions_map,
        )?;

        // Update the dimension's dependency graph
        update_dependency_graph(
            &dependents_dimension.dimension,
            new_dependency_graph,
            dimensions_map,
        )?;

        // Recursively update dependents' dependency graphs
        update_dependents_dependency_graphs_dfs(
            &dependents_dimension.dependents,
            dimensions_map,
        )?;
    }

    Ok(())
}

fn compute_dependencies_diff(
    current_dependencies: &[String],
    new_dependencies: &[String],
) -> (Vec<String>, Vec<String>) {
    let dependencies_to_remove: Vec<String> = current_dependencies
        .iter()
        .filter(|d| !new_dependencies.contains(d))
        .cloned()
        .collect();

    let dependencies_to_add: Vec<String> = new_dependencies
        .iter()
        .filter(|d| !current_dependencies.contains(d))
        .cloned()
        .collect();

    (dependencies_to_remove, dependencies_to_add)
}

fn validate_no_cycles(
    dependencies_to_add: &[String],
    dimension_name: &str,
    dimensions_map: &mut HashMap<String, (Dimension, bool)>,
) -> superposition::Result<()> {
    // fetch all dependencies_to_add dimensions from dimensions_map at once
    let dependencies_to_add_data = dependencies_to_add
        .iter()
        .filter_map(|d| dimensions_map.get(d))
        .map(|(dimension_data, _)| dimension_data.clone())
        .collect::<Vec<Dimension>>();

    for dependencies_data in dependencies_to_add_data {
        if dependencies_data
            .dependency_graph
            .contains_key(dimension_name)
        {
            log::error!(
                "Failed to update dependencies: found cycle while adding dimension {}",
                dependencies_data.dimension
            );
            return Err(bad_argument!(
                "Failed to update dependencies: found cycle while adding dimension {}",
                dependencies_data.dimension
            ));
        }
    }
    Ok(())
}

fn build_dependency_graph_and_update_dependencies(
    dimension_name: &str,
    dependent_dimensions: &[String],
    dimensions_map: &mut HashMap<String, (Dimension, bool)>,
) -> superposition::Result<DependencyGraph> {
    let mut new_dependency_graph = DependencyGraph::new();

    // If no dependencies, return empty dependency graph
    if dependent_dimensions.is_empty() {
        return Ok(new_dependency_graph);
    }

    // fetch all dependent_dimensions from dimensions_map at once
    let dependent_dimensions_data = dependent_dimensions
        .iter()
        .filter_map(|d| dimensions_map.get(d))
        .map(|(dimension_data, _)| dimension_data.clone())
        .collect::<Vec<Dimension>>();

    // Add dependencies to the new dependency graph and update the dependencies
    for dependent_dimension in dependent_dimensions_data {
        new_dependency_graph.insert_dependents(&dependent_dimension);

        // If needed Update dependecies' depedents
        update_dependents(&dependent_dimension, dimension_name, dimensions_map)?;
    }

    // Add the dependencies to the new dependency graph
    new_dependency_graph.insert(
        dimension_name.to_string(),
        dependent_dimensions
            .iter()
            .map(String::from)
            .collect::<Vec<_>>(),
    );

    Ok(new_dependency_graph)
}

fn update_dependents_for_removed_dependencies(
    dimension_name: &str,
    dependencies_to_remove: &[String],
    dimensions_map: &mut HashMap<String, (Dimension, bool)>,
) -> superposition::Result<()> {
    // fetch all dependencies_to_remove dimensions from the dimensions_map at once
    let dependencies_to_remove_data = dependencies_to_remove
        .iter()
        .filter_map(|d| dimensions_map.get(d))
        .map(|(dimension_data, _)| dimension_data.clone())
        .collect::<Vec<Dimension>>();

    for dependencies_data in dependencies_to_remove_data {
        let mut dependents_list = dependencies_data.dependents.clone();
        dependents_list.retain(|dependent| dependent != dimension_name);
        update_dependents_in_dimensions_map(
            &dependencies_data.dimension,
            dependents_list,
            dimensions_map,
        )?;
    }
    Ok(())
}

fn update_dependents(
    dependent_dimension: &Dimension,
    dimension_name: &str,
    dimensions_map: &mut HashMap<String, (Dimension, bool)>,
) -> superposition::Result<()> {
    let mut dependents_list = dependent_dimension.dependents.clone();

    // Add dependents if not already present
    if !dependents_list.contains(&dimension_name.to_string()) {
        dependents_list.push(dimension_name.to_string());
        update_dependents_in_dimensions_map(
            &dependent_dimension.dimension,
            dependents_list,
            dimensions_map,
        )?;
    }
    Ok(())
}

fn update_dependents_in_dimensions_map(
    dimension_name: &str,
    dependents_list: Vec<String>,
    dimensions_map: &mut HashMap<String, (Dimension, bool)>,
) -> superposition::Result<()> {
    // Update the dependents of the dimension in dimensions_map and make is_updated true
    if let Some(dimension_data) = dimensions_map.get_mut(dimension_name) {
        dimension_data.0.dependents.clone_from(&dependents_list);
        dimension_data.1 = true;
        Ok(())
    } else {
        log::error!("Dimension {} not found in dimensions_map", dimension_name);
        Err(not_found!("Dimension {} not found", dimension_name))
    }
}

fn update_dependency_graph(
    dimension_name: &str,
    new_dependency_graph: DependencyGraph,
    dimensions_map: &mut HashMap<String, (Dimension, bool)>,
) -> superposition::Result<()> {
    // Update the dependency_graph of the dimension in dimensions_map and make is_updated true
    if let Some(dimension_data) = dimensions_map.get_mut(dimension_name) {
        dimension_data.0.dependency_graph = new_dependency_graph;
        dimension_data.1 = true;
        Ok(())
    } else {
        log::error!("Dimension {} not found in dimensions_map", dimension_name);
        Err(not_found!("Dimension {} not found", dimension_name))
    }
}

fn detect_self_loop(
    dimension_name: &str,
    dependent_dimensions: &[String],
) -> superposition::Result<()> {
    if dependent_dimensions.contains(&dimension_name.to_string()) {
        log::error!("Failed to update dependencies: found self cycle while dependent dimension for {}", dimension_name);
        return Err(bad_argument!(
                "Failed to update dependencies: found self cycle while updating dependent dimension for {}", dimension_name
            ));
    }
    Ok(())
}

fn fetch_dimensions_map(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<HashMap<String, (Dimension, bool)>> {
    let dimensions_data: Vec<Dimension> = get_dimension_data(conn, schema_name)?;
    let dimensions_map: HashMap<String, (Dimension, bool)> = dimensions_data
        .into_iter()
        .map(|d| (d.dimension.clone(), (d, false)))
        .collect();
    Ok(dimensions_map)
}

fn update_dimensions_in_db(
    dimensions_map: &mut HashMap<String, (Dimension, bool)>,
    user_email: &str,
    reason: &str,
    schema_name: &SchemaName,
    conn: &mut DBConnection,
) -> superposition::Result<()> {
    let reason =
        ChangeReason::try_from(reason.to_string()).map_err(|e| unexpected_error!(e))?;
    for (dimension_name, (dimension_data, is_updated)) in dimensions_map.iter_mut() {
        if *is_updated {
            dimension_data.change_reason = reason.clone();
            dimension_data.last_modified_by = user_email.to_string();
            dimension_data.last_modified_at = Utc::now();
            diesel::update(dimensions)
                .filter(dimension.eq(dimension_name.to_string()))
                .set(dimension_data.clone())
                .schema_name(schema_name)
                .execute(conn)?;
        }
    }
    Ok(())
}
