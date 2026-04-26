use chrono::Utc;
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper};
use serde_json::Value;
use service_utils::service::types::WorkspaceContext;
use superposition_core::validations::validate_schema;
use superposition_macros::{bad_argument, db_error, validation_error};
use superposition_types::{
    DBConnection, User,
    api::dimension::{CreateRequest, DimensionName, UpdateRequest},
    database::{
        models::cac::{DependencyGraph, Dimension, DimensionType},
        schema::dimensions::{self, dsl::*},
    },
    result as superposition,
};

use crate::api::dimension::{
    utils::create_connections_with_dependents,
    validations::{
        allow_primitive_types, does_dimension_exist_for_cohorting,
        validate_cohort_position, validate_cohort_schema,
        validate_dimension_position, validate_position_wrt_dependency,
        validate_validation_function, validate_value_compute_function,
    },
};

pub fn is_mandatory_dimension(
    workspace_context: &WorkspaceContext,
    dimension_name: &str,
) -> bool {
    workspace_context
        .settings
        .mandatory_dimensions
        .clone()
        .unwrap_or_default()
        .contains(&dimension_name.to_string())
}

pub fn create_dimension_entry(
    conn: &mut DBConnection,
    workspace_context: &WorkspaceContext,
    user: &User,
    create_req: CreateRequest,
) -> superposition::Result<Dimension> {
    let schema_value = Value::from(&create_req.schema);
    let num_rows = dimensions
        .count()
        .schema_name(&workspace_context.schema_name)
        .get_result::<i64>(conn)
        .map_err(|err| {
            log::error!("failed to fetch number of dimension with error: {}", err);
            db_error!(err)
        })?;

    validate_dimension_position(
        create_req.dimension.clone(),
        create_req.position,
        num_rows,
    )?;

    match create_req.dimension_type {
        DimensionType::Regular {} => {
            allow_primitive_types(&create_req.schema)?;
            validate_schema(&schema_value).map_err(|err| {
                validation_error!(
                    "JSON Schema's schema is broken - this is unexpected {}",
                    err.join("")
                )
            })?;
        }
        DimensionType::RemoteCohort(ref cohort_based_on) => {
            allow_primitive_types(&create_req.schema)?;
            validate_schema(&schema_value).map_err(|err| {
                validation_error!(
                    "JSON Schema's schema is broken - this is unexpected {}",
                    err.join("")
                )
            })?;
            let based_on_dimension = does_dimension_exist_for_cohorting(
                cohort_based_on,
                &workspace_context.schema_name,
                conn,
            )?;
            validate_cohort_position(&create_req.position, &based_on_dimension, true)?;
        }
        DimensionType::LocalCohort(ref cohort_based_on) => {
            let based_on_dimension = validate_cohort_schema(
                &schema_value,
                cohort_based_on,
                &workspace_context.schema_name,
                conn,
            )?;
            validate_cohort_position(&create_req.position, &based_on_dimension, true)?;
        }
    }

    validate_validation_function(
        &create_req.value_validation_function_name,
        conn,
        &workspace_context.schema_name,
    )?;

    validate_value_compute_function(
        &create_req.dimension_type,
        &create_req.value_compute_function_name,
        conn,
        &workspace_context.schema_name,
    )?;

    let dimension_data = Dimension {
        dimension: create_req.dimension.clone().into(),
        position: create_req.position,
        schema: create_req.schema,
        created_by: user.get_email(),
        created_at: Utc::now(),
        value_validation_function_name: create_req.value_validation_function_name.clone(),
        last_modified_at: Utc::now(),
        last_modified_by: user.get_email(),
        description: create_req.description,
        change_reason: create_req.change_reason,
        dependency_graph: DependencyGraph::default(),
        value_compute_function_name: create_req.value_compute_function_name,
        dimension_type: create_req.dimension_type,
    };

    diesel::update(dimensions::table)
        .filter(dimensions::position.ge(dimension_data.position))
        .set((
            last_modified_at.eq(Utc::now()),
            last_modified_by.eq(user.get_email()),
            dimensions::position.eq(dimensions::position + 1),
        ))
        .schema_name(&workspace_context.schema_name)
        .execute(conn)
        .map_err(|err| {
            log::error!("failed to shift dimensions with error: {err}");
            db_error!(err)
        })?;

    match dimension_data.dimension_type {
        DimensionType::LocalCohort(ref cohort_based_on)
        | DimensionType::RemoteCohort(ref cohort_based_on) => {
            create_connections_with_dependents(
                cohort_based_on,
                &dimension_data.dimension,
                &user.get_email(),
                &workspace_context.schema_name,
                conn,
            )?
        }
        DimensionType::Regular {} => (),
    }

    diesel::insert_into(dimensions::table)
        .values(&dimension_data)
        .returning(Dimension::as_returning())
        .schema_name(&workspace_context.schema_name)
        .get_result(conn)
        .map_err(|err| match err {
            diesel::result::Error::DatabaseError(
                diesel::result::DatabaseErrorKind::ForeignKeyViolation,
                _,
            ) => bad_argument!(
                "Function {} doesn't exists",
                create_req.value_validation_function_name.unwrap_or_default()
            ),
            _ => {
                log::error!("Dimension create failed with error: {err}");
                db_error!(err)
            }
        })
}

pub fn update_dimension_entry(
    conn: &mut DBConnection,
    workspace_context: &WorkspaceContext,
    user: &User,
    name: String,
    update_req: UpdateRequest,
) -> superposition::Result<Dimension> {
    use dimensions::dsl;

    let dimension_data: Dimension = dimensions::dsl::dimensions
        .filter(dimensions::dimension.eq(name.clone()))
        .schema_name(&workspace_context.schema_name)
        .get_result::<Dimension>(conn)?;

    let num_rows = dimensions
        .count()
        .schema_name(&workspace_context.schema_name)
        .get_result::<i64>(conn)
        .map_err(|err| {
            log::error!("failed to fetch number of dimension with error: {}", err);
            db_error!(err)
        })?;

    if let Some(ref new_schema) = update_req.schema {
        let schema_value = Value::from(new_schema);
        match dimension_data.dimension_type {
            DimensionType::Regular {} | DimensionType::RemoteCohort(_) => {
                allow_primitive_types(new_schema)?;
                validate_schema(&schema_value).map_err(|err| {
                    validation_error!(
                        "JSON Schema's schema is broken - this is unexpected {}",
                        err.join("")
                    )
                })?;
            }
            DimensionType::LocalCohort(ref cohort_based_on) => {
                validate_cohort_schema(
                    &schema_value,
                    cohort_based_on,
                    &workspace_context.schema_name,
                    conn,
                )?;
            }
        }
    }

    if let Some(ref new_position) = update_req.position {
        match dimension_data.dimension_type {
            DimensionType::Regular {} => (),
            DimensionType::RemoteCohort(ref cohort_based_on)
            | DimensionType::LocalCohort(ref cohort_based_on) => {
                let based_on_dimension = does_dimension_exist_for_cohorting(
                    cohort_based_on,
                    &workspace_context.schema_name,
                    conn,
                )?;
                validate_cohort_position(new_position, &based_on_dimension, false)?;
            }
        }
    }

    if let Some(ref fn_name) = update_req.value_validation_function_name {
        validate_validation_function(fn_name, conn, &workspace_context.schema_name)?;
    }

    if let Some(ref value_compute_function_name_) = update_req.value_compute_function_name {
        validate_value_compute_function(
            &dimension_data.dimension_type,
            value_compute_function_name_,
            conn,
            &workspace_context.schema_name,
        )?;
    }

    if let Some(position_val) = update_req.position {
        let new_position = position_val;
        validate_dimension_position(
            DimensionName::try_from(name.clone()).map_err(|err| bad_argument!(err))?,
            position_val,
            num_rows - 1,
        )?;
        validate_position_wrt_dependency(
            &name,
            &position_val,
            conn,
            &workspace_context.schema_name,
        )?;
        let previous_position = dimension_data.position;

        diesel::update(dimensions)
            .filter(dsl::dimension.eq(&name))
            .set((
                dsl::last_modified_at.eq(Utc::now()),
                dsl::last_modified_by.eq(user.get_email()),
                dimensions::position.eq((num_rows + 100) as i32),
            ))
            .schema_name(&workspace_context.schema_name)
            .execute(conn)?;

        if previous_position < new_position {
            diesel::update(dsl::dimensions)
                .filter(dimensions::position.gt(previous_position))
                .filter(dimensions::position.le(&new_position))
                .set((
                    dsl::last_modified_at.eq(Utc::now()),
                    dsl::last_modified_by.eq(user.get_email()),
                    dimensions::position.eq(dimensions::position - 1),
                ))
                .schema_name(&workspace_context.schema_name)
                .execute(conn)?;
        } else {
            diesel::update(dsl::dimensions)
                .filter(dimensions::position.lt(previous_position))
                .filter(dimensions::position.ge(&new_position))
                .set((
                    dsl::last_modified_at.eq(Utc::now()),
                    dsl::last_modified_by.eq(user.get_email()),
                    dimensions::position.eq(dimensions::position + 1),
                ))
                .schema_name(&workspace_context.schema_name)
                .execute(conn)?;
        };
    }

    diesel::update(dimensions)
        .filter(dsl::dimension.eq(name))
        .set((
            update_req,
            dimensions::last_modified_at.eq(Utc::now()),
            dimensions::last_modified_by.eq(user.get_email()),
        ))
        .returning(Dimension::as_returning())
        .schema_name(&workspace_context.schema_name)
        .get_result::<Dimension>(conn)
        .map_err(|err| db_error!(err))
}
