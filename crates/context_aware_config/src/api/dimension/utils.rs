use super::types::DimensionName;
use crate::helpers::DimensionData;
use chrono::Utc;
use diesel::ExpressionMethods;
use diesel::{query_dsl::methods::SchemaNameDsl, RunQueryDsl};
use jsonschema::{Draft, JSONSchema};
use serde_json::Value;
use service_utils::{helpers::extract_dimensions, service::types::SchemaName};
use std::collections::HashMap;
use superposition_macros::{bad_argument, db_error, not_found, unexpected_error};
use superposition_types::{
    database::{
        models::{
            cac::{Context, Dimension, Position},
            DependencyGraph,
        },
        schema::{contexts::dsl::contexts, dimensions::dsl::*},
    },
    result as superposition, Cac, Condition, DBConnection,
};

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

        if extract_dimensions(&condition)?.get(key).is_some() {
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
    dependent_dimensions: &Vec<String>,
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
        .into_iter()
        .filter_map(|d| dimensions_map.get(d))
        .map(|(dimension_data, _)| dimension_data.clone())
        .collect::<Vec<Dimension>>();

    for dependent_data in dependent_dimensions_data {
        // Update immediate_dependency_list's parent list
        update_parent_relationships(
            &dependent_data,
            dimension_name,
            &mut dimensions_map,
        )?;

        // Insert the dependent's graph into the parent's graph
        dependency_map.insert_dependents(&dependent_data);
    }

    // If dependent_dimensions not empty, Add parent dimension with dependent dimensions to its dependency graph
    if !dependent_dimensions.is_empty() {
        dependency_map.insert(
            dimension_name.to_string(),
            Value::Array(
                dependent_dimensions
                    .iter()
                    .map(|d| Value::String(d.clone()))
                    .collect(),
            ),
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
    dependent_dimensions: &Vec<String>,
    user_email: &str,
    schema_name: &SchemaName,
    conn: &mut DBConnection,
) -> superposition::Result<()> {
    // Return Ok if the dependent_dimension is same as the current immediate_dependency_list (no change)
    if &dimension_data.immediate_childrens == dependent_dimensions {
        return Ok(());
    }

    let dimension_name = &dimension_data.dimension;

    detect_self_loop(dimension_name, dependent_dimensions)?;

    // Distinguish between the children that are removed and the ones that are being added
    let (children_to_remove, children_to_add) =
        compute_children_diff(&dimension_data.immediate_childrens, dependent_dimensions);

    let mut dimensions_map = fetch_dimensions_map(conn, schema_name)?;

    // Validate no cycles will be introduced
    validate_no_cycles(&children_to_add, dimension_name, &mut dimensions_map)?;

    // Recompute parent's dependency graph and update added children's dependents
    let new_parent_dependency_list = build_dependency_graph_and_update_childrens_parent(
        dimension_name,
        dependent_dimensions,
        &mut dimensions_map,
    )?;

    // Update removed childrens' dependents
    update_parent_references_for_removed_children(
        dimension_name,
        &children_to_remove,
        &mut dimensions_map,
    )?;

    // Update the parent's dependency graph in the db
    update_dependency_graph(
        dimension_name,
        new_parent_dependency_list,
        &mut dimensions_map,
    )?;

    // Update the ancestor's dimension_graphs of the parent dimension
    update_parent_dependency_graphs_dfs(&dimension_data.dependents, &mut dimensions_map)?;

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
    // If someone is dependent on this i.e. check the parents, then don't let it be deleted
    if !dimension_data.dependents.is_empty() {
        let parent_dimensions = dimension_data.dependents.clone();
        let parent_list = parent_dimensions.join(", ");

        return Err(bad_argument!(
            "Cannot delete dimension `{}`: it is used as a dependency by: {}",
            dimension_name,
            parent_list
        ));
    }

    // If this is dependent on someone i.e. check the children, then clean up the immediate_childrens
    let immediate_dependency_list = dimension_data.immediate_childrens.clone();

    if !immediate_dependency_list.is_empty() {
        // Remove the dimension from the children's dependents
        let mut dimensions_map = fetch_dimensions_map(conn, schema_name)?;
        update_parent_references_for_removed_children(
            dimension_name,
            &immediate_dependency_list,
            &mut dimensions_map,
        )?;
        // No need to Remove the dimension's immediate_childrens to [] and dependency_graph to {} and Recompute the dimension's dependency graph as we are already deleting it 🥲
        // No need to update the parent's dependency graph as there shouldn't be any dependents, if allowed till here
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

fn update_parent_dependency_graphs_dfs(
    parents: &Vec<String>,
    dimensions_map: &mut HashMap<String, (Dimension, bool)>,
) -> superposition::Result<()> {
    // Early return if no parents
    if parents.is_empty() {
        return Ok(());
    };

    // fetch all parent dimensions from the dimension_map at once
    let parent_dimensions_data: Vec<Dimension> = parents
        .iter()
        .filter_map(|d| dimensions_map.get(d))
        .map(|(dimension_data, _)| dimension_data.clone())
        .collect();

    for parent_dimension in parent_dimensions_data {
        let immediate_dependency_list = parent_dimension.immediate_childrens.clone();

        // Recompute the parent's dependency list
        let new_parent_dependency_list =
            build_dependency_graph_and_update_childrens_parent(
                &parent_dimension.dimension,
                &immediate_dependency_list,
                dimensions_map,
            )?;

        // Update the parent's dependency graph
        update_dependency_graph(
            &parent_dimension.dimension,
            new_parent_dependency_list,
            dimensions_map,
        )?;

        // Recursively update ancestors' dependency graphs
        update_parent_dependency_graphs_dfs(
            &parent_dimension.dependents,
            dimensions_map,
        )?;
    }

    Ok(())
}

fn compute_children_diff(
    current_children: &[String],
    new_children: &[String],
) -> (Vec<String>, Vec<String>) {
    let remove_children: Vec<String> = current_children
        .iter()
        .filter(|child| !new_children.contains(child))
        .cloned()
        .collect();

    let add_children: Vec<String> = new_children
        .iter()
        .filter(|child| !current_children.contains(child))
        .cloned()
        .collect();

    (remove_children, add_children)
}

fn validate_no_cycles(
    children_to_add: &Vec<String>,
    dimension_name: &str,
    dimensions_map: &mut HashMap<String, (Dimension, bool)>,
) -> superposition::Result<()> {
    // fetch all children_to_add dimensions from dimensions_map at once
    let children_to_add_dimensions = children_to_add
        .into_iter()
        .filter_map(|d| dimensions_map.get(d))
        .map(|(dimension_data, _)| dimension_data.clone())
        .collect::<Vec<Dimension>>();

    for child_dimension in children_to_add_dimensions {
        if child_dimension
            .dependency_graph
            .contains_key(dimension_name)
        {
            log::error!("Failed to update dependent dimensions: found cycle while adding dimension {}", child_dimension.dimension);
            return Err(bad_argument!(
                "Failed to update dependent dimensions: found cycle while adding dimension {}", 
                child_dimension.dimension
            ));
        }
    }
    Ok(())
}

fn build_dependency_graph_and_update_childrens_parent(
    dimension_name: &str,
    dependent_dimensions: &Vec<String>,
    dimensions_map: &mut HashMap<String, (Dimension, bool)>,
) -> superposition::Result<DependencyGraph> {
    let mut new_parent_dependency_graph = DependencyGraph::new();

    // If no dependent dimensions, return empty dependency graph
    if dependent_dimensions.is_empty() {
        return Ok(new_parent_dependency_graph);
    }

    // fetch all dependent_dimensions from dimensions_map at once
    let dependent_dimensions_data = dependent_dimensions
        .into_iter()
        .filter_map(|d| dimensions_map.get(d))
        .map(|(dimension_data, _)| dimension_data.clone())
        .collect::<Vec<Dimension>>();

    // Add child immediate_childrens to the new parent dependency list and update the dependents of the child
    for dependent_dimension in dependent_dimensions_data {
        new_parent_dependency_graph.insert_dependents(&dependent_dimension);

        // If needed Update immediate_dependency_list's parent list
        update_parent_relationships(
            &dependent_dimension,
            dimension_name,
            dimensions_map,
        )?;
    }

    // Add the dependent_dimensions to the parent dependency list
    new_parent_dependency_graph.insert(
        dimension_name.to_string(),
        Value::Array(
            dependent_dimensions
                .into_iter()
                .map(|s| Value::String(s.to_string()))
                .collect(),
        ),
    );

    Ok(new_parent_dependency_graph)
}

fn update_parent_references_for_removed_children(
    dimension_name: &str,
    children_to_remove: &Vec<String>,
    dimensions_map: &mut HashMap<String, (Dimension, bool)>,
) -> superposition::Result<()> {
    // fetch all children_to_remove dimensions from the dimensions_map at once
    let children_to_remove_data = children_to_remove
        .into_iter()
        .filter_map(|d| dimensions_map.get(d))
        .map(|(dimension_data, _)| dimension_data.clone())
        .collect::<Vec<Dimension>>();

    for children_data in children_to_remove_data {
        let mut child_parents = children_data.dependents.clone();
        child_parents.retain(|parent| parent != dimension_name);
        update_dependents(&children_data.dimension, child_parents, dimensions_map)?;
    }
    Ok(())
}

fn update_parent_relationships(
    dependent_dimension: &Dimension,
    parent_dimension: &str,
    dimensions_map: &mut HashMap<String, (Dimension, bool)>,
) -> superposition::Result<()> {
    let mut parents = dependent_dimension.dependents.clone();

    // Add parent if not already present
    if !parents.contains(&parent_dimension.to_string()) {
        parents.push(parent_dimension.to_string());
        update_dependents(&dependent_dimension.dimension, parents, dimensions_map)?;
    }
    Ok(())
}

fn update_dependents(
    dimension_name: &str,
    parents: Vec<String>,
    dimensions_map: &mut HashMap<String, (Dimension, bool)>,
) -> superposition::Result<()> {
    // Update the dependents of the dimension in dimensions_map and make is_updated true
    if let Some(dimension_data) = dimensions_map.get_mut(dimension_name) {
        dimension_data.0.dependents = parents.clone();
        dimension_data.1 = true;
        Ok(())
    } else {
        log::error!("Dimension {} not found in dimensions_map", dimension_name);
        return Err(not_found!("Dimension {} not found", dimension_name));
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
        return Err(not_found!("Dimension {} not found", dimension_name));
    }
}

fn detect_self_loop(
    dimension_name: &str,
    dependent_dimensions: &Vec<String>,
) -> superposition::Result<()> {
    if dependent_dimensions.contains(&dimension_name.to_string()) {
        log::error!("Failed to update dependent dimensions: found self cycle while dependent dimension for {}", dimension_name);
        return Err(bad_argument!(
                "Failed to update dependent dimensions: found self cycle while updating dependent dimension for {}", dimension_name
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
    for (dimension_name, (dimension_data, is_updated)) in dimensions_map.iter_mut() {
        if *is_updated {
            dimension_data.change_reason = reason.to_string();
            dimension_data.last_modified_by = user_email.to_string();
            dimension_data.last_modified_at = Utc::now().naive_utc();
            diesel::update(dimensions)
                .filter(dimension.eq(dimension_name.to_string()))
                .set(dimension_data.clone())
                .schema_name(schema_name)
                .execute(conn)?;
        }
    }
    Ok(())
}
