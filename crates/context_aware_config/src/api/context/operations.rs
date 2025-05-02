use actix_web::web::Json;
use chrono::Utc;
use diesel::{
    r2d2::{ConnectionManager, PooledConnection},
    result::{DatabaseErrorKind::*, Error::DatabaseError},
    Connection, ExpressionMethods, PgConnection, QueryDsl, RunQueryDsl, SelectableHelper,
};
use serde_json::Value;
use service_utils::{helpers::extract_dimensions, service::types::SchemaName};
use superposition_macros::{db_error, not_found, unexpected_error};
use superposition_types::{
    database::{models::cac::Context, schema::contexts},
    result, DBConnection, User,
};

use crate::{
    api::{
        context::{
            helpers::{
                create_ctx_from_put_req, hash, replace_override_of_existing_ctx,
                update_override_of_existing_ctx,
                validate_condition_with_mandatory_dimensions,
                validate_condition_with_strict_mode,
            },
            validations::validate_dimensions,
        },
        dimension::{get_dimension_data, get_dimension_data_map},
    },
    helpers::{calculate_context_weight, get_workspace},
};

use super::{
    types::{MoveReq, PutResp},
    PutReq,
};

pub fn put(
    req: Json<PutReq>,
    description: String,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    already_under_txn: bool,
    user: &User,
    schema_name: &SchemaName,
    replace: bool,
) -> result::Result<PutResp> {
    use contexts::dsl::contexts;
    let new_ctx = create_ctx_from_put_req(req, description, conn, user, schema_name)?;

    if already_under_txn {
        diesel::sql_query("SAVEPOINT put_ctx_savepoint").execute(conn)?;
    }
    let insert = diesel::insert_into(contexts)
        .values(&new_ctx)
        .returning(Context::as_returning())
        .schema_name(schema_name)
        .execute(conn);

    match insert {
        Ok(_) => Ok(new_ctx.into()),
        Err(DatabaseError(UniqueViolation, _)) => {
            if already_under_txn {
                diesel::sql_query("ROLLBACK TO put_ctx_savepoint").execute(conn)?;
            }
            if replace {
                replace_override_of_existing_ctx(conn, new_ctx, user, schema_name)
            } else {
                update_override_of_existing_ctx(conn, new_ctx, user, schema_name)
            }
        }
        Err(e) => {
            log::error!("failed to update context with db error: {:?}", e);
            Err(db_error!(e))
        }
    }
}

pub fn r#move(
    old_ctx_id: String,
    req: Json<MoveReq>,
    req_description: String,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    already_under_txn: bool,
    user: &User,
    schema_name: &SchemaName,
) -> result::Result<PutResp> {
    use contexts::dsl;
    let req = req.into_inner();
    let ctx_condition = req.context.to_owned().into_inner();
    let ctx_condition_value = Value::Object(ctx_condition.clone().into());
    let change_reason = req.change_reason.clone();

    let new_ctx_id = hash(&ctx_condition_value);

    let dimension_data = get_dimension_data(conn, schema_name)?;
    let dimension_data_map = get_dimension_data_map(&dimension_data)?;
    validate_dimensions("context", &ctx_condition_value, &dimension_data_map)?;
    let weight = calculate_context_weight(&ctx_condition_value, &dimension_data_map)
        .map_err(|_| unexpected_error!("Something Went Wrong"))?;

    let workspace_settings = get_workspace(schema_name, conn)?;

    validate_condition_with_strict_mode(&ctx_condition, workspace_settings.strict_mode)?;

    let context_map = extract_dimensions(&req.context.into_inner())?;
    validate_condition_with_mandatory_dimensions(
        &context_map,
        &workspace_settings.mandatory_dimensions.unwrap_or_default(),
    )?;

    if already_under_txn {
        diesel::sql_query("SAVEPOINT update_ctx_savepoint").execute(conn)?;
    }

    let context = diesel::update(dsl::contexts)
        .filter(dsl::id.eq(&old_ctx_id))
        .set((
            dsl::id.eq(&new_ctx_id),
            dsl::value.eq(&ctx_condition_value),
            dsl::weight.eq(&weight),
            dsl::last_modified_at.eq(Utc::now()),
            dsl::last_modified_by.eq(user.get_email()),
            dsl::description.eq(req_description.clone()),
        ))
        .returning(Context::as_returning())
        .schema_name(schema_name)
        .get_result::<Context>(conn);

    let contruct_new_ctx_with_old_overrides = |ctx: Context| Context {
        id: new_ctx_id,
        value: ctx_condition,
        created_at: Utc::now(),
        created_by: user.get_email(),
        override_id: ctx.override_id,
        override_: ctx.override_,
        last_modified_at: Utc::now(),
        last_modified_by: user.get_email(),
        weight,
        description: req_description,
        change_reason,
    };

    let handle_unique_violation =
        |db_conn: &mut DBConnection, already_under_txn: bool| {
            if already_under_txn {
                let deleted_ctxt = diesel::delete(dsl::contexts)
                    .filter(dsl::id.eq(&old_ctx_id))
                    .schema_name(schema_name)
                    .get_result(db_conn)?;

                let ctx = contruct_new_ctx_with_old_overrides(deleted_ctxt);
                update_override_of_existing_ctx(db_conn, ctx, user, schema_name)
            } else {
                db_conn.transaction(|conn| {
                    let deleted_ctxt = diesel::delete(dsl::contexts)
                        .filter(dsl::id.eq(&old_ctx_id))
                        .schema_name(schema_name)
                        .get_result(conn)?;
                    let ctx = contruct_new_ctx_with_old_overrides(deleted_ctxt);
                    update_override_of_existing_ctx(conn, ctx, user, schema_name)
                })
            }
        };

    match context {
        Ok(ctx) => Ok(ctx.into()),
        Err(DatabaseError(UniqueViolation, _)) => {
            if already_under_txn {
                diesel::sql_query("ROLLBACK TO update_ctx_savepoint").execute(conn)?;
            }
            handle_unique_violation(conn, already_under_txn)
        }
        Err(e) => {
            log::error!("failed to move context with db error: {:?}", e);
            Err(db_error!(e))
        }
    }
}

pub fn delete(
    ctx_id: String,
    user: &User,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> result::Result<()> {
    use contexts::dsl;
    diesel::update(dsl::contexts)
        .filter(dsl::id.eq(&ctx_id))
        .set((
            dsl::last_modified_at.eq(Utc::now()),
            dsl::last_modified_by.eq(user.get_email()),
        ))
        .returning(Context::as_returning())
        .schema_name(schema_name)
        .execute(conn)?;
    let deleted_row = diesel::delete(dsl::contexts.filter(dsl::id.eq(&ctx_id)))
        .schema_name(schema_name)
        .execute(conn);
    match deleted_row {
        Ok(0) => Err(not_found!("Context Id `{}` doesn't exists", ctx_id)),
        Ok(_) => {
            log::info!("{ctx_id} context deleted by {}", user.get_email());
            Ok(())
        }
        Err(e) => {
            log::error!("context delete query failed with error: {e}");
            Err(unexpected_error!("Something went wrong."))
        }
    }
}
