use std::collections::HashMap;

use diesel::{
    ExpressionMethods, OptionalExtension, QueryDsl, RunQueryDsl, SelectableHelper,
};
use service_utils::{helpers::fetch_dimensions_info_map, service::types::SchemaName};
use superposition_core::helpers::create_connections_with_dependents;
use superposition_types::{
    database::{
        models::cac::{Dimension, DimensionType},
        schema::dimensions::{self, dsl::*},
    },
    result as superposition, DBConnection,
};

pub(crate) fn upsert_dimension(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    row: Dimension,
) -> superposition::Result<(bool, Dimension)> {
    let existing = dimensions
        .filter(dimension.eq(&row.dimension))
        .schema_name(schema_name)
        .get_result::<Dimension>(conn)
        .optional()?;

    persist_dimension(conn, schema_name, row, existing.as_ref())
}

pub(crate) fn persist_dimension(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    row: Dimension,
    existing: Option<&Dimension>,
) -> superposition::Result<(bool, Dimension)> {
    if let Some(existing) = existing {
        let validation_function = row
            .value_validation_function_name
            .as_ref()
            .or(existing.value_validation_function_name.as_ref());
        let compute_function = row
            .value_compute_function_name
            .as_ref()
            .or(existing.value_compute_function_name.as_ref());

        let updated = diesel::update(dimensions.filter(dimension.eq(&row.dimension)))
            .set((
                schema.eq(&row.schema),
                position.eq(row.position),
                dimension_type.eq(&row.dimension_type),
                dependency_graph.eq(&row.dependency_graph),
                value_validation_function_name.eq(validation_function),
                value_compute_function_name.eq(compute_function),
                last_modified_at.eq(row.last_modified_at),
                last_modified_by.eq(&row.last_modified_by),
                description.eq(&row.description),
                change_reason.eq(&row.change_reason),
            ))
            .returning(Dimension::as_returning())
            .schema_name(schema_name)
            .get_result(conn)?;
        Ok((false, updated))
    } else {
        let created = diesel::insert_into(dimensions)
            .values(&row)
            .returning(Dimension::as_returning())
            .schema_name(schema_name)
            .get_result(conn)?;
        Ok((true, created))
    }
}

pub(crate) fn refresh_dependency_graphs(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<()> {
    let mut dimension_info = fetch_dimensions_info_map(conn, schema_name)?;
    let previous_graphs: HashMap<_, _> = dimension_info
        .iter_mut()
        .map(|(name, info)| (name.clone(), std::mem::take(&mut info.dependency_graph)))
        .collect();
    let cohort_relations: Vec<_> = dimension_info
        .iter()
        .filter_map(|(name, info)| match &info.dimension_type {
            DimensionType::LocalCohort(parent) | DimensionType::RemoteCohort(parent) => {
                Some((name.clone(), parent.clone()))
            }
            DimensionType::Regular {} => None,
        })
        .collect();

    for (name, parent) in cohort_relations {
        create_connections_with_dependents(&parent, &name, &mut dimension_info);
    }

    for (name, info) in dimension_info {
        if previous_graphs.get(&name) != Some(&info.dependency_graph) {
            diesel::update(dimensions::dsl::dimensions.filter(dimension.eq(&name)))
                .set(dependency_graph.eq(info.dependency_graph))
                .schema_name(schema_name)
                .execute(conn)?;
        }
    }
    Ok(())
}
