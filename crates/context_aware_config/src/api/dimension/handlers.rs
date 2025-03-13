use std::collections::HashMap;

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
use serde_json::{Map, Value};
use service_utils::service::types::{AppState, DbConnection, SchemaName};
use superposition_macros::{bad_argument, db_error, not_found, unexpected_error};
use superposition_types::{
    custom_query::PaginationParams,
    database::{
        models::{
            cac::{Dimension, Position},
            Workspace,
        },
        schema::dimensions::{self, dsl::*},
        types::DimensionWithMandatory,
    },
    result as superposition, PaginatedResponse, User,
};

use crate::{
    api::dimension::{
        types::CreateReq,
        utils::{get_dimension_usage_context_ids, validate_dimension_position},
    },
    helpers::{get_workspace, validate_jsonschema},
};

use super::types::{DeleteReq, DimensionName, ListDependentDimensions, UpdateReq};

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(create)
        .service(update)
        .service(get)
        .service(delete_dimension)
        .service(list_dependent_dimensions)
}

pub fn validate_and_create_dependent_dimensions(
    dim: &String,
    dependent_dimensions: &Option<Vec<String>>,
    schema_name: &SchemaName,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<Option<Value>> {
    let dependency_list: Option<Value> =
        if let Some(dependent_dimensions) = dependent_dimensions {
            let mut dependency_list = Map::new();
            for dependent in dependent_dimensions {
                let dependent_dimension = dimensions::dsl::dimensions
                    .filter(dimensions::dimension.eq(dependent.clone()))
                    .schema_name(schema_name)
                    .get_result::<Dimension>(conn)?;
                let mut dependent_dimension_parents = dependent_dimension
                    .immediate_parents
                    .clone()
                    .unwrap_or_default();
                let dependent_dimension_graph = dependent_dimension
                    .dependency_graph
                    .clone()
                    .unwrap_or_default()
                    .as_object()
                    .cloned()
                    .unwrap_or_else(|| {
                        [(dependent.clone(), Value::Array(vec![]))]
                            .iter()
                            .cloned()
                            .collect::<Map<String, Value>>()
                    });
                if !dependent_dimension_parents.contains(&dim.to_string()) {
                    dependent_dimension_parents.push(dim.to_string());
                }

                dependent_dimension_graph.iter().for_each(|(key, value)| {
                    dependency_list.insert(key.clone(), value.clone());
                });

                diesel::update(dimensions)
                    .filter(dimension.eq(&dependent))
                    .set(immediate_parents.eq(dependent_dimension_parents))
                    .schema_name(schema_name)
                    .get_result::<Dimension>(conn)?;
            }
            dependency_list.insert(
                dim.to_string(),
                Value::Array(
                    dependent_dimensions
                        .clone()
                        .into_iter()
                        .map(Value::String)
                        .collect(),
                ),
            );
            Some(Value::Object(dependency_list))
        } else {
            None
        };

    Ok(dependency_list)
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
        dependency_graph: None,
        immediate_parents: None,
        immediate_childrens: create_req.immediate_childrens,
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

        dimension_data.dependency_graph = validate_and_create_dependent_dimensions(
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

pub fn update_parent_dependency_graphs_dfs(
    parents: Option<Vec<String>>,
    schema_name: &SchemaName,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<()> {
    println!("<<>> parents: {:?}", parents);
    if let Some(parents) = parents {
        for parent in parents {
            println!("<<>> parent: {:?}", parent);
            let parent_dimension: Dimension = dimensions::dsl::dimensions
                .filter(dimensions::dimension.eq(parent.clone()))
                .schema_name(schema_name)
                .get_result::<Dimension>(conn)?;

            let immediate_children = parent_dimension
                .immediate_childrens
                .clone()
                .unwrap_or_default();

            println!("<<>> immediate_children: {:?}", immediate_children);

            // Recompute the parent's dependency list
            let mut new_parent_dependency_list: Map<String, Value> = Map::new();
            for child in &immediate_children {
                let child_dimension: Dimension = dimensions::dsl::dimensions
                    .filter(dimensions::dimension.eq(child.clone()))
                    .schema_name(schema_name)
                    .get_result::<Dimension>(conn)?;

                let child_dependency_list = child_dimension
                    .dependency_graph
                    .clone()
                    .unwrap_or_default()
                    .as_object()
                    .cloned()
                    .unwrap_or_else(|| {
                        [(child.clone(), Value::Array(vec![]))]
                            .iter()
                            .cloned()
                            .collect::<Map<String, Value>>()
                    });
                for (key, value) in &child_dependency_list {
                    new_parent_dependency_list.insert(key.clone(), value.clone());
                }
            }

            // Add the dependent_dimensions(immediate_children) to the parent dependency list
            new_parent_dependency_list.insert(
                parent.to_string(),
                Value::Array(
                    immediate_children
                        .clone()
                        .into_iter()
                        .map(Value::String)
                        .collect(),
                ),
            );

            println!(
                "<<>> new_parent_dependency_list: {:?}",
                new_parent_dependency_list
            );

            // Update the parent's dependency graph
            diesel::update(dimensions::dsl::dimensions)
                .filter(dimensions::dimension.eq(parent.clone()))
                .set(
                    dimensions::dependency_graph
                        .eq(Value::Object(new_parent_dependency_list)),
                )
                .schema_name(schema_name)
                .execute(conn)?;

            // Recursion to update the ancestors' dependency graphs
            update_parent_dependency_graphs_dfs(
                parent_dimension.immediate_parents,
                schema_name,
                conn,
            )?;
        }
    }
    Ok(())
}

pub fn validate_dependent_dimensions(
    dimension_name: &str,
    dependent_dimensions: &Option<Vec<String>>,
    schema_name: &SchemaName,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<()> {
    if let Some(dependent_dimensions) = dependent_dimensions {
        // If self loop return error
        if dependent_dimensions.contains(&dimension_name.to_string()) {
            log::error!("Failed to update dependent dimensions: found self cycle while dependent dimension for {}", dimension_name);
            return Err(unexpected_error!(
                "Failed to update dependent dimensions: found self cycle while updating dependent dimension for {}", dimension_name
            ));
        }

        let dimension_data: Dimension = dimensions::dsl::dimensions
            .filter(dimensions::dimension.eq(dimension_name))
            .schema_name(schema_name)
            .get_result::<Dimension>(conn)?;

        // Return Ok if the dependent_dimension is same as the current immediate_children
        if let Some(children) = &dimension_data.immediate_childrens {
            if children == dependent_dimensions {
                return Ok(());
            }
        }

        // Distinguish between the children that are removed and the ones that are being added
        let (remove_children, add_children) = match &dimension_data.immediate_childrens {
            Some(children) => {
                let remove_children: Vec<String> = children
                    .iter()
                    .filter(|child| !dependent_dimensions.contains(child))
                    .cloned()
                    .collect();
                let add_children: Vec<String> = dependent_dimensions
                    .iter()
                    .filter(|child| !children.contains(child))
                    .cloned()
                    .collect();
                (remove_children, add_children)
            }
            None => (vec![], dependent_dimensions.clone()),
        };

        // Check for cycles and cache data for the added children
        let mut added_dimension_map: HashMap<String, Dimension> = HashMap::new();
        for child in &add_children {
            let child_dimension: Dimension = dimensions::dsl::dimensions
                .filter(dimensions::dimension.eq(child.clone()))
                .schema_name(schema_name)
                .get_result::<Dimension>(conn)?;

            if let Some(dependency_list_val) = &child_dimension.dependency_graph {
                let dependency_object = dependency_list_val.as_object().ok_or_else(|| {
                        log::error!("Could not convert dependency list to object");
                        unexpected_error!(
                            "Something went wrong, failed to update the dependent dimensions"
                        )
                    })?
                    .clone();
                if dependency_object.contains_key(dimension_name) {
                    log::error!("Failed to update dependent dimensions: found cycle while adding dimension {}", child);
                    return Err(unexpected_error!(
                            "Failed to update dependent dimensions: found cycle while adding dimension {}", child
                        ));
                };
            }
            added_dimension_map.insert(child.clone(), child_dimension);
        }

        // Update parent's dependency graph and update added children's immediate_parents
        let mut new_parent_dependency_list: Map<String, Value> = Map::new();
        for child in dependent_dimensions {
            let child_dimension: Dimension = dimensions::dsl::dimensions
                .filter(dimensions::dimension.eq(child.clone()))
                .schema_name(schema_name)
                .get_result::<Dimension>(conn)?;

            let child_dependency_list = child_dimension
                .dependency_graph
                .clone()
                .unwrap_or_default()
                .as_object()
                .cloned()
                .unwrap_or_else(|| {
                    [(child.clone(), Value::Array(vec![]))]
                        .iter()
                        .cloned()
                        .collect::<Map<String, Value>>()
                });
            for (key, value) in &child_dependency_list {
                new_parent_dependency_list.insert(key.clone(), value.clone());
            }

            let mut child_parents = child_dimension
                .immediate_parents
                .clone()
                .unwrap_or_default();
            if !child_parents.contains(&dimension_name.to_string()) {
                child_parents.push(dimension_name.to_string());

                diesel::update(dimensions::dsl::dimensions)
                    .filter(dimensions::dimension.eq(child.clone()))
                    .set(dimensions::immediate_parents.eq(child_parents))
                    .schema_name(schema_name)
                    .execute(conn)?;
            }
        }

        // Add the dependent_dimensions(immediate_children) to the parent dependency list
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

        // Update removed childrens' immediate_parents
        for child in &remove_children {
            let child_dimension: Dimension = dimensions::dsl::dimensions
                .filter(dimensions::dimension.eq(child.clone()))
                .schema_name(schema_name)
                .get_result::<Dimension>(conn)?;

            let mut child_parents = child_dimension
                .immediate_parents
                .clone()
                .unwrap_or_default();
            child_parents.retain(|parent| parent != dimension_name);
            diesel::update(dimensions::dsl::dimensions)
                .filter(dimensions::dimension.eq(child.clone()))
                .set(dimensions::immediate_parents.eq(child_parents))
                .schema_name(schema_name)
                .execute(conn)?;
        }

        let parent_dependency_list = if dependent_dimensions.is_empty() {
            Value::Null
        } else {
            Value::Object(new_parent_dependency_list)
        };
        // Update the parent's dependency graph in the database
        diesel::update(dimensions::dsl::dimensions)
            .filter(dimensions::dimension.eq(dimension_name))
            .set(dimensions::dependency_graph.eq(parent_dependency_list))
            .schema_name(schema_name)
            .execute(conn)?;

        // Update the ancestor's dimension_graphs of the parent dimension
        update_parent_dependency_graphs_dfs(
            dimension_data.immediate_parents,
            schema_name,
            conn,
        )?;

        // If after updating the parent's dimension_graphs, the parent dimension has no childrens, then return None
        // Remove this as already updated the parent's dependency graph
    };

    Ok(())
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

    let existing_position: Position = dsl::dimensions
        .select(dimensions::position)
        .filter(dimensions::dimension.eq(name.clone()))
        .schema_name(&schema_name)
        .get_result::<Position>(&mut conn)?;

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
                let previous_position = existing_position;

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

            validate_dependent_dimensions(
                &name,
                &update_req.immediate_childrens,
                &schema_name,
                transaction_conn,
            )?;

            // Refactor this
            if let Some(_immediate_children) = update_req.immediate_childrens.clone() {
                // This doesn't work, need to refactor
                // let updated_req = UpdateReq {
                //     immediate_childrens: if immediate_children.is_empty() {
                //         None
                //     } else {
                //         Some(immediate_children)
                //     },
                //     ..update_req
                // };
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
            } else {
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
            }
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

pub fn validate_dependency(
    name: &str,
    dimension_data: &Dimension,
) -> superposition::Result<()> {
    let has_dependencies = dimension_data
        .immediate_parents
        .as_ref()
        .map_or(false, |parents| !parents.is_empty())
        || dimension_data
            .immediate_childrens
            .as_ref()
            .map_or(false, |children| !children.is_empty());

    if has_dependencies {
        Err(bad_argument!("Dimension `{}` has dependencies", name))
    } else {
        Ok(())
    }
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

    validate_dependency(&name, &dimension_data)?;

    let context_ids = get_dimension_usage_context_ids(&name, &mut conn, &schema_name)
        .map_err(|_| unexpected_error!("Something went wrong"))?;
    if context_ids.is_empty() {
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            use dimensions::dsl;
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

#[get("/{name}/dependent_dimensions")]
async fn list_dependent_dimensions(
    path: Path<String>,
    db_conn: DbConnection,
    filters: Query<ListDependentDimensions>,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let dimension_name = path.into_inner();
    let list: bool = filters.list.clone().unwrap_or_default();
    let dimension_data = dimensions::dsl::dimensions
        .filter(dimensions::dimension.eq(dimension_name.clone()))
        .select(dimensions::dependency_graph)
        .schema_name(&schema_name)
        .get_result::<Option<Value>>(&mut conn)?;
    if list {
        let dependent_dimensions_list: Vec<String> = dimension_data
            .iter()
            .filter_map(|val| val.as_object())
            .flat_map(|val| val.keys().cloned())
            .filter(|val| val != &dimension_name)
            .collect();
        Ok(HttpResponse::Ok().json(dependent_dimensions_list))
    } else {
        Ok(HttpResponse::Ok().json(dimension_data))
    }
}
