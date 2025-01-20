use actix_web::web::Json;
use chrono::Utc;
use diesel::{
    r2d2::{ConnectionManager, PooledConnection},
    result::{DatabaseErrorKind::*, Error::DatabaseError},
    Connection, ExpressionMethods, PgConnection, QueryDsl, RunQueryDsl, SelectableHelper,
};
use serde_json::Value;
use service_utils::service::types::Tenant;
use superposition_macros::{bad_argument, db_error, not_found, unexpected_error};
use superposition_types::{
    database::{models::cac::Context, schema::contexts},
    result, DBConnection, User,
};

use crate::{
    api::{
        context::{
            helpers::{
                create_ctx_from_put_req, ensure_description, hash,
                replace_override_of_existing_ctx, update_override_of_existing_ctx,
                validate_condition_with_mandatory_dimensions,
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
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    already_under_txn: bool,
    user: &User,
    tenant: &Tenant,
    replace: bool,
) -> result::Result<PutResp> {
    use contexts::dsl::contexts;
    let new_ctx = create_ctx_from_put_req(req, conn, user, &tenant)?;

    if already_under_txn {
        diesel::sql_query("SAVEPOINT put_ctx_savepoint").execute(conn)?;
    }
    let insert = diesel::insert_into(contexts)
        .values(&new_ctx)
        .returning(Context::as_returning())
        .schema_name(&tenant)
        .execute(conn);

    match insert {
        Ok(_) => Ok(new_ctx.into()),
        Err(DatabaseError(UniqueViolation, _)) => {
            if already_under_txn {
                diesel::sql_query("ROLLBACK TO put_ctx_savepoint").execute(conn)?;
            }
            if replace {
                replace_override_of_existing_ctx(conn, new_ctx, user, tenant)
            } else {
                update_override_of_existing_ctx(conn, new_ctx, user, tenant)
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
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    already_under_txn: bool,
    user: &User,
    tenant: &Tenant,
) -> result::Result<PutResp> {
    use contexts::dsl;
    let req = req.into_inner();
    let ctx_condition = req.context.to_owned().into_inner();
    let ctx_condition_value = Value::Object(ctx_condition.clone().into());
    let description = if req.description.is_none() {
        ensure_description(ctx_condition_value.clone(), conn, tenant)?
    } else {
        req.description
            .ok_or_else(|| bad_argument!("Description should not be empty"))?
    };
    let change_reason = req.change_reason.clone();

    let new_ctx_id = hash(&ctx_condition_value);

    let dimension_data = get_dimension_data(conn, &tenant)?;
    let dimension_data_map = get_dimension_data_map(&dimension_data)?;
    validate_dimensions("context", &ctx_condition_value, &dimension_data_map)?;
    let weight = calculate_context_weight(&ctx_condition_value, &dimension_data_map)
        .map_err(|_| unexpected_error!("Something Went Wrong"))?;

    let workspace_settings = get_workspace(&tenant, conn)?;

    validate_condition_with_mandatory_dimensions(
        &req.context.into_inner(),
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
            dsl::last_modified_at.eq(Utc::now().naive_utc()),
            dsl::last_modified_by.eq(user.get_email()),
        ))
        .returning(Context::as_returning())
        .schema_name(&tenant)
        .get_result::<Context>(conn);

    let contruct_new_ctx_with_old_overrides = |ctx: Context| Context {
        id: new_ctx_id,
        value: ctx_condition,
        created_at: Utc::now(),
        created_by: user.get_email(),
        override_id: ctx.override_id,
        override_: ctx.override_,
        last_modified_at: Utc::now().naive_utc(),
        last_modified_by: user.get_email(),
        weight,
        description,
        change_reason,
    };

    let handle_unique_violation =
        |db_conn: &mut DBConnection, already_under_txn: bool| {
            if already_under_txn {
                let deleted_ctxt = diesel::delete(dsl::contexts)
                    .filter(dsl::id.eq(&old_ctx_id))
                    .schema_name(&tenant)
                    .get_result(db_conn)?;

                let ctx = contruct_new_ctx_with_old_overrides(deleted_ctxt);
                update_override_of_existing_ctx(db_conn, ctx, user, tenant)
            } else {
                db_conn.transaction(|conn| {
                    let deleted_ctxt = diesel::delete(dsl::contexts)
                        .filter(dsl::id.eq(&old_ctx_id))
                        .schema_name(&tenant)
                        .get_result(conn)?;
                    let ctx = contruct_new_ctx_with_old_overrides(deleted_ctxt);
                    update_override_of_existing_ctx(conn, ctx, user, tenant)
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
    user: User,
    conn: &mut DBConnection,
    tenant: &Tenant,
) -> result::Result<()> {
    use contexts::dsl;
    diesel::update(dsl::contexts)
        .filter(dsl::id.eq(&ctx_id))
        .set((
            dsl::last_modified_at.eq(Utc::now().naive_utc()),
            dsl::last_modified_by.eq(user.get_email()),
        ))
        .returning(Context::as_returning())
        .schema_name(&tenant)
        .execute(conn)?;
    let deleted_row = diesel::delete(dsl::contexts.filter(dsl::id.eq(&ctx_id)))
        .schema_name(&tenant)
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
