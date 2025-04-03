use actix_web::{
    delete, get, post, put,
    web::{self, Data, Json, Path, Query},
    HttpResponse, Scope,
};
use chrono::Utc;
use diesel::{
    delete,
    r2d2::{ConnectionManager, PooledConnection},
    Connection, ExpressionMethods, PgConnection, QueryDsl, RunQueryDsl, SelectableHelper,
};
use serde_json::Value;
use service_utils::service::types::{AppState, DbConnection, SchemaName};
use superposition_macros::{bad_argument, db_error, not_found, unexpected_error};
use superposition_types::{
    custom_query::PaginationParams,
    database::{
        models::{cac::Dimension, Workspace},
        schema::dimensions::{self, dsl::*},
        types::DimensionWithMandatory,
    },
    result as superposition, DependencyGraph, PaginatedResponse, User,
};

use crate::{
    api::dimension::{
        types::CreateReq,
        utils::{get_dimension_usage_context_ids, validate_dimension_position},
    },
    helpers::{get_workspace, validate_jsonschema},
};

use super::types::{DeleteReq, DimensionName, UpdateReq};

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(create)
        .service(update)
        .service(get)
        .service(delete_dimension)
}

#[post("")]
async fn create(
    state: Data<AppState>,
    req: web::Json<CreateReq>,
    user: User,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let create_req = req.into_inner();
    let schema_value = create_req.schema;

    let num_rows = dimensions
        .count()
        .schema_name(&schema_name)
        .get_result::<i64>(&mut conn)
        .map_err(|err| {
            log::error!("failed to fetch number of dimension with error: {}", err);
            db_error!(err)
        })?;

    validate_dimension_position(
        create_req.dimension.clone(),
        create_req.position,
        num_rows,
    )?;
    validate_jsonschema(&state.meta_schema, &schema_value)?;

    let mut dimension_data = Dimension {
        dimension: create_req.dimension.into(),
        position: create_req.position,
        schema: schema_value,
        created_by: user.get_email(),
        created_at: Utc::now(),
        function_name: create_req.function_name.clone(),
        last_modified_at: Utc::now().naive_utc(),
        last_modified_by: user.get_email(),
        description: create_req.description,
        change_reason: create_req.change_reason,
        dependency_graph: DependencyGraph::default(),
        immediate_parents: Vec::new(),
        immediate_childrens: create_req.dependent_dimensions.unwrap_or_default(),
    };

    conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
        diesel::update(dimensions::table)
            .filter(dimensions::position.ge(dimension_data.position))
            .set((
                last_modified_at.eq(Utc::now().naive_utc()),
                last_modified_by.eq(user.get_email()),
                dimensions::position.eq(dimensions::position + 1),
            ))
            .returning(Dimension::as_returning())
            .schema_name(&schema_name)
            .execute(transaction_conn)?;

        dimension_data.dependency_graph = validate_and_initialize_dimension_hierarchy(
            &dimension_data.dimension,
            &dimension_data.immediate_childrens,
            &schema_name,
            transaction_conn,
        )?;

        let insert_resp = diesel::insert_into(dimensions::table)
            .values(&dimension_data)
            .returning(Dimension::as_returning())
            .schema_name(&schema_name)
            .get_result(transaction_conn);

        match insert_resp {
            Ok(inserted_dimension) => {
                let workspace_settings: Workspace =
                    get_workspace(&schema_name, transaction_conn)?;
                let is_mandatory = workspace_settings
                    .mandatory_dimensions
                    .unwrap_or_default()
                    .contains(&inserted_dimension.dimension);
                Ok(HttpResponse::Created().json(DimensionWithMandatory::new(
                    inserted_dimension,
                    is_mandatory,
                )))
            }
            Err(diesel::result::Error::DatabaseError(
                diesel::result::DatabaseErrorKind::ForeignKeyViolation,
                e,
            )) => {
                let fun_name = create_req.function_name.clone();
                log::error!("{fun_name:?} function not found with error: {e:?}");
                Err(bad_argument!(
                    "Function {} doesn't exists",
                    Into::<Option<String>>::into(create_req.function_name.clone())
                        .unwrap_or_default()
                ))
            }
            Err(e) => {
                log::error!("Dimension create failed with error: {e}");
                Err(db_error!(e))
            }
        }
    })
}

#[put("/{name}")]
async fn update(
    path: Path<DimensionName>,
    state: Data<AppState>,
    req: web::Json<UpdateReq>,
    user: User,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let name: String = path.clone().into();
    use dimensions::dsl;
    let DbConnection(mut conn) = db_conn;

    let dimension_data: Dimension = dimensions::dsl::dimensions
        .filter(dimensions::dimension.eq(name.clone()))
        .schema_name(&schema_name)
        .get_result::<Dimension>(&mut conn)?;

    let num_rows = dimensions
        .count()
        .schema_name(&schema_name)
        .get_result::<i64>(&mut conn)
        .map_err(|err| {
            log::error!("failed to fetch number of dimension with error: {}", err);
            db_error!(err)
        })?;

    let update_req = req.into_inner();

    if let Some(schema_value) = update_req.schema.clone() {
        validate_jsonschema(&state.meta_schema, &schema_value)?;
    }

    let result =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            if let Some(position_val) = update_req.position {
                let new_position = position_val;
                validate_dimension_position(
                    path.into_inner(),
                    position_val,
                    num_rows - 1,
                )?;
                let previous_position = dimension_data.position;

                diesel::update(dimensions)
                    .filter(dsl::dimension.eq(&name))
                    .set((
                        dsl::last_modified_at.eq(Utc::now().naive_utc()),
                        dsl::last_modified_by.eq(user.get_email()),
                        dimensions::position.eq((num_rows + 100) as i32),
                    ))
                    .returning(Dimension::as_returning())
                    .schema_name(&schema_name)
                    .get_result::<Dimension>(transaction_conn)?;

                if previous_position < new_position {
                    diesel::update(dsl::dimensions)
                        .filter(dimensions::position.gt(previous_position))
                        .filter(dimensions::position.le(&new_position))
                        .set((
                            dsl::last_modified_at.eq(Utc::now().naive_utc()),
                            dsl::last_modified_by.eq(user.get_email()),
                            dimensions::position.eq(dimensions::position - 1),
                        ))
                        .returning(Dimension::as_returning())
                        .schema_name(&schema_name)
                        .execute(transaction_conn)?
                } else {
                    diesel::update(dsl::dimensions)
                        .filter(dimensions::position.lt(previous_position))
                        .filter(dimensions::position.ge(&new_position))
                        .set((
                            dsl::last_modified_at.eq(Utc::now().naive_utc()),
                            dsl::last_modified_by.eq(user.get_email()),
                            dimensions::position.eq(dimensions::position + 1),
                        ))
                        .returning(Dimension::as_returning())
                        .schema_name(&schema_name)
                        .execute(transaction_conn)?
                };
            }

            if let Some(dependent_dimension) = &update_req.dependent_dimensions {
                validate_and_update_dimension_hierarchy(
                    &dimension_data,
                    dependent_dimension,
                    &schema_name,
                    transaction_conn,
                )?;
            }

            diesel::update(dimensions)
                .filter(dsl::dimension.eq(name))
                .set((
                    update_req,
                    dimensions::last_modified_at.eq(Utc::now().naive_utc()),
                    dimensions::last_modified_by.eq(user.get_email()),
                ))
                .returning(Dimension::as_returning())
                .schema_name(&schema_name)
                .get_result::<Dimension>(transaction_conn)
                .map_err(|err| db_error!(err))
        })?;

    let workspace_settings = get_workspace(&schema_name, &mut conn)?;
    let is_mandatory = workspace_settings
        .mandatory_dimensions
        .unwrap_or_default()
        .contains(&result.dimension);

    Ok(HttpResponse::Ok().json(DimensionWithMandatory::new(result, is_mandatory)))
}

#[get("")]
async fn get(
    db_conn: DbConnection,
    filters: Query<PaginationParams>,
    schema_name: SchemaName,
) -> superposition::Result<Json<PaginatedResponse<DimensionWithMandatory>>> {
    let DbConnection(mut conn) = db_conn;

    let (total_pages, total_items, result) = match filters.all {
        Some(true) => {
            let result: Vec<Dimension> = dimensions
                .schema_name(&schema_name)
                .get_results(&mut conn)?;
            (1, result.len() as i64, result)
        }
        _ => {
            let n_dimensions: i64 = dimensions
                .count()
                .schema_name(&schema_name)
                .get_result(&mut conn)?;
            let limit = filters.count.unwrap_or(10);
            let mut builder = dimensions
                .schema_name(&schema_name)
                .order(created_at.desc())
                .limit(limit)
                .into_boxed();
            if let Some(page) = filters.page {
                let offset = (page - 1) * limit;
                builder = builder.offset(offset);
            }
            let result: Vec<Dimension> = builder.load(&mut conn)?;
            let total_pages = (n_dimensions as f64 / limit as f64).ceil() as i64;
            (total_pages, n_dimensions, result)
        }
    };

    let workspace_settings = get_workspace(&schema_name, &mut conn)?;

    let mandatory_dimensions =
        workspace_settings.mandatory_dimensions.unwrap_or_default();

    let dimensions_with_mandatory: Vec<DimensionWithMandatory> = result
        .into_iter()
        .map(|ele| {
            let is_mandatory = mandatory_dimensions.contains(&ele.dimension);
            DimensionWithMandatory::new(ele, is_mandatory)
        })
        .collect();

    Ok(Json(PaginatedResponse {
        total_pages,
        total_items,
        data: dimensions_with_mandatory,
    }))
}

#[delete("/{name}")]
async fn delete_dimension(
    path: Path<DeleteReq>,
    user: User,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let name: String = path.into_inner().into();
    let DbConnection(mut conn) = db_conn;
    let dimension_data: Dimension = dimensions::dsl::dimensions
        .filter(dimensions::dimension.eq(&name))
        .select(Dimension::as_select())
        .schema_name(&schema_name)
        .get_result(&mut conn)?;

    let context_ids = get_dimension_usage_context_ids(&name, &mut conn, &schema_name)
        .map_err(|_| unexpected_error!("Something went wrong"))?;
    if context_ids.is_empty() {
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            use dimensions::dsl;

            validate_dimension_deletability(
                &name,
                &dimension_data,
                transaction_conn,
                &schema_name,
            )?;

            diesel::update(dsl::dimensions)
                .filter(dsl::dimension.eq(&name))
                .set((
                    dsl::last_modified_at.eq(Utc::now().naive_utc()),
                    dsl::last_modified_by.eq(user.get_email()),
                ))
                .returning(Dimension::as_returning())
                .schema_name(&schema_name)
                .execute(transaction_conn)?;
            diesel::update(dimensions::dsl::dimensions)
                .filter(dimensions::position.gt(dimension_data.position))
                .set(dimensions::position.eq(dimensions::position - 1))
                .returning(Dimension::as_returning())
                .schema_name(&schema_name)
                .execute(transaction_conn)?;
            let deleted_row = delete(dsl::dimensions.filter(dsl::dimension.eq(&name)))
                .schema_name(&schema_name)
                .execute(transaction_conn);
            match deleted_row {
                Ok(0) => Err(not_found!("Dimension `{}` doesn't exists", name)),
                Ok(_) => Ok(HttpResponse::NoContent().finish()),
                Err(e) => {
                    log::error!("dimension delete query failed with error: {e}");
                    Err(unexpected_error!("Something went wrong."))
                }
            }
        })
    } else {
        Err(bad_argument!(
            "Given key already in use in contexts: {}",
            context_ids.join(",")
        ))
    }
}

pub fn validate_dimension_deletability(
    dimension_name: &str,
    dimension_data: &Dimension,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    schema_name: &SchemaName,
) -> superposition::Result<()> {
    // If someone is dependent on this i.e. check the parents, then don't let it be deleted
    if !dimension_data.immediate_parents.is_empty() {
        let parent_dimensions = dimension_data.immediate_parents.clone();
        let parent_list = parent_dimensions.join(", ");

        return Err(bad_argument!(
            "Cannot delete dimension `{}`: it is used as a dependency by: {}",
            dimension_name,
            parent_list
        ));
    }

    // If this is dependent on someone i.e. check the children, then clean up the dependencies
    let immediate_children_list = dimension_data.immediate_childrens.clone();

    if !immediate_children_list.is_empty() {
        // Remove the dimension from the children's immediate_parents
        update_parent_references_for_removed_children(
            dimension_name,
            &immediate_children_list,
            schema_name,
            conn,
        )?;
        // No need to Remove the dimension's immediate_childrens to [] and dependency_graph to {} and Recompute the dimension's dependency graph as we are already deleting it 🥲
        // No need to update the parent's dependency graph as there shouldn't be any immediate_parents, if allowed till here
    }

    Ok(())
}

pub fn validate_and_update_dimension_hierarchy(
    dimension_data: &Dimension,
    dependent_dimensions: &Vec<String>,
    schema_name: &SchemaName,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<()> {
    // Return Ok if the dependent_dimension is same as the current immediate_children_list (no change)
    if &dimension_data.immediate_childrens == dependent_dimensions {
        return Ok(());
    }

    let dimension_name = &dimension_data.dimension;

    // If self loop return error
    if dependent_dimensions.contains(dimension_name) {
        log::error!("Failed to update dependent dimensions: found self cycle while dependent dimension for {}", dimension_name);
        return Err(bad_argument!(
                "Failed to update dependent dimensions: found self cycle while updating dependent dimension for {}", dimension_name
            ));
    }

    // Distinguish between the children that are removed and the ones that are being added
    let (children_to_remove, children_to_add) =
        compute_children_diff(&dimension_data.immediate_childrens, dependent_dimensions);

    // Validate no cycles will be introduced
    validate_no_cycles(&children_to_add, dimension_name, schema_name, conn)?;

    // Recompute parent's dependency graph and update added children's immediate_parents
    let new_parent_dependency_list = build_dependency_graph_and_update_childrens_parent(
        dimension_name,
        dependent_dimensions,
        schema_name,
        conn,
    )?;

    // Update removed childrens' immediate_parents
    update_parent_references_for_removed_children(
        dimension_name,
        &children_to_remove,
        schema_name,
        conn,
    )?;

    // Update the parent's dependency graph in the db
    update_dependency_graph(
        dimension_name,
        new_parent_dependency_list,
        schema_name,
        conn,
    )?;

    // Update the ancestor's dimension_graphs of the parent dimension
    update_parent_dependency_graphs_dfs(
        &dimension_data.immediate_parents,
        schema_name,
        conn,
    )?;

    Ok(())
}

pub fn validate_and_initialize_dimension_hierarchy(
    parent_dimension: &str,
    dependent_dimensions: &Vec<String>,
    schema_name: &SchemaName,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<DependencyGraph> {
    let mut dependency_map = DependencyGraph::new();

    // Execute a single query to fetch all dependent_dimensions at once
    let dependent_dimension: Vec<Dimension> = dimensions::dsl::dimensions
        .filter(dimensions::dimension.eq_any(dependent_dimensions))
        .schema_name(schema_name)
        .load::<Dimension>(conn)?;

    for dependent_data in dependent_dimension {
        // Update immediate_children_list's parent list
        update_parent_relationships(
            &dependent_data,
            parent_dimension,
            schema_name,
            conn,
        )?;

        // Merge the dependent's graph into the parent's graph
        merge_dependency_graph(
            &mut dependency_map,
            &dependent_data,
            &dependent_data.dimension,
        );
    }

    // If dependent_dimensions not empty, Add parent dimension with dependent dimensions to its dependency graph
    if !dependent_dimensions.is_empty() {
        dependency_map.insert(
            parent_dimension.to_string(),
            Value::Array(
                dependent_dimensions
                    .iter()
                    .map(|d| Value::String(d.clone()))
                    .collect(),
            ),
        );
    }

    Ok(dependency_map)
}

pub fn update_parent_dependency_graphs_dfs(
    parents: &Vec<String>,
    schema_name: &SchemaName,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<()> {
    // Early return if no parents
    if parents.is_empty() {
        return Ok(());
    };

    // Execute a single query to fetch all parent dimensions at once
    let parent_dimensions: Vec<Dimension> = dimensions::dsl::dimensions
        .filter(dimensions::dimension.eq_any(parents))
        .schema_name(schema_name)
        .load::<Dimension>(conn)?;

    for parent_dimension in parent_dimensions {
        let immediate_children_list = parent_dimension.immediate_childrens.clone();

        // Recompute the parent's dependency list
        let new_parent_dependency_list =
            build_dependency_graph_and_update_childrens_parent(
                &parent_dimension.dimension,
                &immediate_children_list,
                schema_name,
                conn,
            )?;

        // Update the parent's dependency graph
        update_dependency_graph(
            &parent_dimension.dimension,
            new_parent_dependency_list,
            schema_name,
            conn,
        )?;

        // Recursively update ancestors' dependency graphs
        update_parent_dependency_graphs_dfs(
            &parent_dimension.immediate_parents,
            schema_name,
            conn,
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
    schema_name: &SchemaName,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<()> {
    // Execute a single query to fetch all children_to_add dimensions at once
    let children_to_add_dimensions: Vec<Dimension> = dimensions::dsl::dimensions
        .filter(dimensions::dimension.eq_any(children_to_add))
        .schema_name(schema_name)
        .load::<Dimension>(conn)?;

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
    schema_name: &SchemaName,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<DependencyGraph> {
    let mut new_parent_dependency_list = DependencyGraph::new();

    // Execute a single query to fetch all dimensions at once
    let dependent_dimensions_data: Vec<Dimension> = dimensions::dsl::dimensions
        .filter(dimensions::dimension.eq_any(dependent_dimensions))
        .schema_name(schema_name)
        .load::<Dimension>(conn)?;

    // Add child dependencies to the new parent dependency list and update the immediate_parents of the child
    for dependent_dimension in dependent_dimensions_data {
        merge_dependency_graph(
            &mut new_parent_dependency_list,
            &dependent_dimension,
            &dependent_dimension.dimension,
        );

        let mut child_dependency_list: DependencyGraph =
            dependent_dimension.dependency_graph.clone();
        if child_dependency_list.is_empty() {
            child_dependency_list
                .insert(dependent_dimension.dimension.clone(), Value::Array(vec![]));
        }

        child_dependency_list.iter().for_each(|(key, value)| {
            new_parent_dependency_list.insert(key.clone(), value.clone());
        });

        // If needed Update immediate_children_list's parent list
        update_parent_relationships(
            &dependent_dimension,
            dimension_name,
            schema_name,
            conn,
        )?;
    }

    // If dependent_dimensions not empty, Add the it to the parent dependency list
    if !dependent_dimensions.is_empty() {
        new_parent_dependency_list.insert(
            dimension_name.to_string(),
            Value::Array(
                dependent_dimensions
                    .clone()
                    .into_iter()
                    .map(Value::String)
                    .collect(),
            ),
        );
    }
    Ok(new_parent_dependency_list)
}

fn update_parent_references_for_removed_children(
    dimension_name: &str,
    children_to_remove: &Vec<String>,
    schema_name: &SchemaName,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<()> {
    // Execute a single query to fetch all children_to_remove dimensions at once
    let children_to_remove_data: Vec<Dimension> = dimensions::dsl::dimensions
        .filter(dimensions::dimension.eq_any(children_to_remove))
        .schema_name(schema_name)
        .load::<Dimension>(conn)?;

    for children_data in children_to_remove_data {
        let mut child_parents = children_data.immediate_parents.clone();

        child_parents.retain(|parent| parent != dimension_name);

        diesel::update(dimensions::dsl::dimensions)
            .filter(dimensions::dimension.eq(children_data.dimension.clone()))
            .set(dimensions::immediate_parents.eq(child_parents))
            .schema_name(schema_name)
            .execute(conn)?;
    }
    Ok(())
}

fn update_dependency_graph(
    dimension_name: &str,
    new_parent_dependency_list: DependencyGraph,
    schema_name: &SchemaName,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<()> {
    diesel::update(dimensions::dsl::dimensions)
        .filter(dimensions::dimension.eq(dimension_name))
        .set(dimensions::dependency_graph.eq(new_parent_dependency_list))
        .schema_name(schema_name)
        .execute(conn)?;

    Ok(())
}

fn update_parent_relationships(
    dependent_dimension: &Dimension,
    parent_dimension: &str,
    schema_name: &SchemaName,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<()> {
    let mut parents = dependent_dimension.immediate_parents.clone();

    // Add parent if not already present
    if !parents.contains(&parent_dimension.to_string()) {
        parents.push(parent_dimension.to_string());

        diesel::update(dimensions)
            .filter(dimension.eq(&dependent_dimension.dimension))
            .set(immediate_parents.eq(parents))
            .schema_name(schema_name)
            .get_result::<Dimension>(conn)?;
    }
    Ok(())
}

fn merge_dependency_graph(
    dependency_map: &mut DependencyGraph,
    dependent_dimension: &Dimension,
    dependent_name: &str,
) {
    let mut dependent_graph: DependencyGraph =
        dependent_dimension.dependency_graph.clone();
    if dependent_graph.is_empty() {
        dependent_graph.insert(dependent_name.to_string(), Value::Array(vec![]));
    }

    dependent_graph.iter().for_each(|(key, value)| {
        dependency_map.insert(key, value.clone());
    });
}
