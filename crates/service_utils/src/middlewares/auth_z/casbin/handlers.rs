use std::collections::HashMap;

use actix_web::{
    HttpResponse, Scope, delete, get, post,
    web::{Data, Json, Path},
};
use casbin::{MgmtApi, RbacApi};
use diesel::{
    ExpressionMethods, RunQueryDsl,
    query_dsl::methods::{FilterDsl, OrderDsl, SelectDsl},
};
use superposition_derives::{authorized, declare_resource};
use superposition_macros::{bad_argument, unexpected_error};
use superposition_types::{
    Resource,
    api::authz::{
        ResourceActionType,
        casbin::{
            ActionGroupPolicyRequest, ActionResponse, GroupingPolicyRequest,
            PolicyRequest,
        },
    },
    database::superposition_schema::superposition::{organisations, workspaces},
    result as superposition,
};

use crate::{
    middlewares::auth_z::{
        AuthZDomain, AuthZManager,
        casbin::{CasbinPolicyEngine, domain_matcher},
    },
    service::types::{DbConnection, OrganisationId},
};

declare_resource!(Auth);

pub fn workspace_endpoints() -> Scope {
    Scope::new("/casbin")
        .service(add_policy_handler)
        .service(delete_policy_handler)
        .service(list_policy_handler)
        .service(add_roles_handler)
        .service(delete_roles_handler)
        .service(list_roles_handler)
        .service(add_domain_action_group_handler)
        .service(delete_domain_action_group_handler)
        .service(list_domain_action_group_handler)
        .service(list_resource_action_map_handler)
    // .service(get_roles_for_user_handler)
    // .service(get_users_for_role_handler)
}

pub fn org_endpoints() -> Scope {
    Scope::new("/casbin")
        .service(add_policy_handler)
        .service(delete_policy_handler)
        .service(list_policy_handler)
        .service(add_roles_handler)
        .service(delete_roles_handler)
        .service(list_roles_handler)
        .service(add_domain_action_group_handler)
        .service(delete_domain_action_group_handler)
        .service(list_domain_action_group_handler)
        .service(backfill_workspaces_handler)
        .service(list_resource_action_map_handler)
    // .service(get_roles_for_user_handler)
    // .service(get_users_for_role_handler)
}

pub fn admin_endpoints() -> Scope {
    Scope::new("/casbin")
        .service(add_policy_handler)
        .service(delete_policy_handler)
        .service(list_policy_handler)
        .service(add_roles_handler)
        .service(delete_roles_handler)
        .service(list_roles_handler)
        .service(add_action_group_handler)
        .service(delete_action_group_handler)
        .service(list_action_group_handler)
        .service(backfill_orgs_handler)
        .service(list_resource_action_map_handler)
    // .service(get_roles_for_user_handler)
    // .service(get_users_for_role_handler)
}

#[authorized]
#[post("/policy")]
async fn add_policy_handler(
    data: Data<AuthZManager>,
    body: Json<PolicyRequest>,
    domain: AuthZDomain,
) -> superposition::Result<Json<ActionResponse>> {
    let data = data.try_get_casbin_policy_engine()?;
    let body = body.into_inner();
    let added = data
        .enforcer_mut(async |enforcer| {
            let action_map = data.get_resource_action_map(enforcer, domain.clone());
            let act = body.act.to_string();

            action_map
                .get(&body.obj)
                .and_then(|actions| actions.iter().find(|a| a.get_name() == act))
                .ok_or_else(|| {
                    bad_argument!(
                        "The action '{}' is not valid for resource '{}'",
                        act,
                        body.obj
                    )
                })?;

            enforcer
                .add_policy(vec![
                    body.sub.into_inner(),
                    domain.to_string(),
                    body.obj.to_string(),
                    body.act.to_string(),
                    body.attr.map(|a| a.into_inner()).unwrap_or("*".to_string()),
                ])
                .await
                .map_err(|e| unexpected_error!("{}", e))
        })
        .await
        .map_err(|e| unexpected_error!(e))?;

    Ok(Json(ActionResponse {
        success: added,
        message: if added {
            "Policy added".to_string()
        } else {
            "Policy already exists".to_string()
        },
    }))
}

#[authorized]
#[delete("/policy")]
async fn delete_policy_handler(
    data: Data<AuthZManager>,
    body: Json<PolicyRequest>,
    domain: AuthZDomain,
) -> superposition::Result<Json<ActionResponse>> {
    let data = data.try_get_casbin_policy_engine()?;
    let body = body.into_inner();
    let removed = data
        .enforcer_mut(async |enforcer| {
            enforcer
                .remove_policy(vec![
                    body.sub.into_inner(),
                    domain.to_string(),
                    body.obj.to_string(),
                    body.act.to_string(),
                    body.attr.map(|a| a.into_inner()).unwrap_or("*".to_string()),
                ])
                .await
                .map_err(|e| unexpected_error!("{}", e))
        })
        .await
        .map_err(|e| unexpected_error!(e))?;

    Ok(Json(ActionResponse {
        success: removed,
        message: if removed {
            "Policy removed".to_string()
        } else {
            "Policy does not exist".to_string()
        },
    }))
}

#[authorized]
#[get("/policy")]
async fn list_policy_handler(
    data: Data<AuthZManager>,
    domain: AuthZDomain,
) -> superposition::Result<Json<Vec<Vec<String>>>> {
    let data = data.try_get_casbin_policy_engine()?;
    let dom = domain.to_string();
    let policies = data
        .enforcer(async |enforcer| {
            let resp = enforcer
                .get_policy()
                .into_iter()
                .filter(|rule| rule.get(1).is_some_and(|d| domain_matcher(&dom, d)))
                .collect();
            Ok(resp)
        })
        .await
        .map_err(|e| unexpected_error!(e))?;

    Ok(Json(policies))
}

#[authorized]
#[post("/roles")]
async fn add_roles_handler(
    data: Data<AuthZManager>,
    body: Json<GroupingPolicyRequest>,
    domain: AuthZDomain,
) -> superposition::Result<Json<ActionResponse>> {
    let data = data.try_get_casbin_policy_engine()?;
    let body = body.into_inner();
    let added = data
        .enforcer_mut(async |enforcer| {
            enforcer
                // g = user, role, dom
                .add_grouping_policy(vec![
                    body.user.into_inner(),
                    body.role.into_inner(),
                    domain.to_string(),
                ])
                .await
                .map_err(|e| unexpected_error!("{}", e))
        })
        .await
        .map_err(|e| unexpected_error!(e))?;

    Ok(Json(ActionResponse {
        success: added,
        message: if added {
            "Grouping policy added".to_string()
        } else {
            "Grouping policy already exists".to_string()
        },
    }))
}

#[authorized]
#[delete("/roles")]
async fn delete_roles_handler(
    data: Data<AuthZManager>,
    body: Json<GroupingPolicyRequest>,
    domain: AuthZDomain,
) -> superposition::Result<Json<ActionResponse>> {
    let data = data.try_get_casbin_policy_engine()?;
    let body = body.into_inner();
    let removed = data
        .enforcer_mut(async |enforcer| {
            enforcer
                .remove_grouping_policy(vec![
                    body.user.into_inner(),
                    body.role.into_inner(),
                    domain.to_string(),
                ])
                .await
                .map_err(|e| unexpected_error!("{}", e))
        })
        .await
        .map_err(|e| unexpected_error!(e))?;

    Ok(Json(ActionResponse {
        success: removed,
        message: if removed {
            "Grouping policy removed".to_string()
        } else {
            "Grouping policy does not exist".to_string()
        },
    }))
}

#[authorized]
#[get("/roles")]
async fn list_roles_handler(
    data: Data<AuthZManager>,
    domain: AuthZDomain,
) -> superposition::Result<Json<Vec<Vec<String>>>> {
    let data = data.try_get_casbin_policy_engine()?;
    // g = user, role, dom. Filter by dom.
    let dom = domain.to_string();
    let policies = data
        .enforcer(async |enforcer| {
            let resp = enforcer
                .get_grouping_policy()
                .into_iter()
                .filter(|rule| rule.get(2).is_some_and(|d| domain_matcher(&dom, d)))
                .collect::<Vec<_>>();

            Ok(resp)
        })
        .await
        .map_err(|e| unexpected_error!(e))?;
    Ok(Json(policies))
}

#[authorized]
#[post("/domain-action-groups")]
async fn add_domain_action_group_handler(
    data: Data<AuthZManager>,
    body: Json<ActionGroupPolicyRequest>,
    domain: AuthZDomain,
) -> superposition::Result<Json<ActionResponse>> {
    let data = data.try_get_casbin_policy_engine()?;
    let body = body.into_inner();
    let added = data
        .enforcer_mut(async |enforcer| {
            enforcer
                .add_named_grouping_policy(
                    "g2",
                    vec![
                        format!("{}:{}", body.resource, body.action.into_inner()),
                        domain.to_string(),
                        body.action_group.into_inner(),
                    ],
                )
                .await
                .map_err(|e| unexpected_error!("{}", e))
        })
        .await
        .map_err(|e| unexpected_error!(e))?;

    Ok(Json(ActionResponse {
        success: added,
        message: if added {
            "Action-group policy added".to_string()
        } else {
            "Action-group policy already exists".to_string()
        },
    }))
}

#[authorized]
#[delete("/domain-action-groups")]
async fn delete_domain_action_group_handler(
    data: Data<AuthZManager>,
    body: Json<ActionGroupPolicyRequest>,
    domain: AuthZDomain,
) -> superposition::Result<Json<ActionResponse>> {
    let data = data.try_get_casbin_policy_engine()?;
    let body = body.into_inner();
    let removed = data
        .enforcer_mut(async |enforcer| {
            enforcer
                .remove_named_grouping_policy(
                    "g2",
                    vec![
                        format!("{}:{}", body.resource, body.action.into_inner()),
                        domain.to_string(),
                        body.action_group.into_inner(),
                    ],
                )
                .await
                .map_err(|e| unexpected_error!("{}", e))
        })
        .await
        .map_err(|e| unexpected_error!(e))?;

    Ok(Json(ActionResponse {
        success: removed,
        message: if removed {
            "Action-group policy removed".to_string()
        } else {
            "Action-group policy does not exist".to_string()
        },
    }))
}

#[authorized]
#[get("/domain-action-groups")]
async fn list_domain_action_group_handler(
    data: Data<AuthZManager>,
    domain: AuthZDomain,
) -> superposition::Result<Json<Vec<Vec<String>>>> {
    let data = data.try_get_casbin_policy_engine()?;
    let dom = domain.to_string();
    let policies = data
        .enforcer(async |enforcer| {
            let resp = CasbinPolicyEngine::list_domain_action_groups(enforcer, &dom);
            Ok(resp)
        })
        .await
        .map_err(|e| unexpected_error!(e))?;
    Ok(Json(policies))
}

#[authorized]
#[post("/action-groups")]
async fn add_action_group_handler(
    data: Data<AuthZManager>,
    body: Json<ActionGroupPolicyRequest>,
) -> superposition::Result<Json<ActionResponse>> {
    let data = data.try_get_casbin_policy_engine()?;
    let body = body.into_inner();
    let added = data
        .enforcer_mut(async |enforcer| {
            enforcer
                .add_named_grouping_policy(
                    "g3",
                    vec![
                        format!("{}:{}", body.resource, body.action.into_inner()),
                        body.action_group.into_inner(),
                    ],
                )
                .await
                .map_err(|e| unexpected_error!("{}", e))
        })
        .await
        .map_err(|e| unexpected_error!(e))?;

    Ok(Json(ActionResponse {
        success: added,
        message: if added {
            "Action-group policy added".to_string()
        } else {
            "Action-group policy already exists".to_string()
        },
    }))
}

#[authorized]
#[delete("/action-groups")]
async fn delete_action_group_handler(
    data: Data<AuthZManager>,
    body: Json<ActionGroupPolicyRequest>,
) -> superposition::Result<Json<ActionResponse>> {
    let data = data.try_get_casbin_policy_engine()?;
    let body = body.into_inner();
    let removed = data
        .enforcer_mut(async |enforcer| {
            enforcer
                .remove_named_grouping_policy(
                    "g3",
                    vec![
                        format!("{}:{}", body.resource, body.action.into_inner()),
                        body.action_group.into_inner(),
                    ],
                )
                .await
                .map_err(|e| unexpected_error!("{}", e))
        })
        .await
        .map_err(|e| unexpected_error!(e))?;

    Ok(Json(ActionResponse {
        success: removed,
        message: if removed {
            "Action-group policy removed".to_string()
        } else {
            "Action-group policy does not exist".to_string()
        },
    }))
}

#[authorized]
#[get("/action-groups")]
async fn list_action_group_handler(
    data: Data<AuthZManager>,
) -> superposition::Result<Json<Vec<Vec<String>>>> {
    let data = data.try_get_casbin_policy_engine()?;
    let policies = data
        .enforcer(async |enforcer| {
            Ok(CasbinPolicyEngine::list_root_action_groups(enforcer))
        })
        .await
        .map_err(|e| unexpected_error!(e))?;
    Ok(Json(policies))
}

#[authorized]
#[get("/resource-action-map")]
async fn list_resource_action_map_handler(
    data: Data<AuthZManager>,
    domain: AuthZDomain,
) -> superposition::Result<Json<HashMap<Resource, Vec<ResourceActionType>>>> {
    let data = data.try_get_casbin_policy_engine()?;
    let map = data
        .enforcer(async |enforcer| {
            Ok(data
                .get_resource_action_map(enforcer, domain)
                .into_iter()
                .map(|(k, v)| {
                    let mut list = v.into_iter().collect::<Vec<_>>();
                    list.sort();
                    (k, list)
                })
                .collect::<HashMap<_, _>>())
        })
        .await
        .map_err(|e| unexpected_error!(e))?;
    Ok(Json(map))
}

#[authorized]
#[get("/users/{user}/roles")]
async fn get_roles_for_user_handler(
    data: Data<AuthZManager>,
    path: Path<String>,
) -> superposition::Result<HttpResponse> {
    let data = data.try_get_casbin_policy_engine()?;
    let user = path.into_inner();
    let roles = data
        .enforcer(async |enforcer| Ok(enforcer.get_roles_for_user(&user, None)))
        .await
        .map_err(|e| unexpected_error!(e))?;
    Ok(HttpResponse::Ok().json(roles))
}

#[authorized]
#[get("/roles/{role}/users")]
async fn get_users_for_role_handler(
    data: Data<AuthZManager>,
    path: Path<String>,
) -> superposition::Result<HttpResponse> {
    let data = data.try_get_casbin_policy_engine()?;
    let role = path.into_inner();
    let users = data
        .enforcer(async |enforcer| Ok(enforcer.get_users_for_role(&role, None)))
        .await
        .map_err(|e| unexpected_error!(e))?;
    Ok(HttpResponse::Ok().json(users))
}

#[authorized]
#[post("/backfill")]
async fn backfill_orgs_handler(
    data: Data<AuthZManager>,
    db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let data = data.try_get_casbin_policy_engine()?;

    let orgs = organisations::table
        .order(organisations::created_at.desc())
        .select((organisations::id, organisations::admin_email))
        .get_results::<(String, String)>(&mut conn)?;

    data.enforcer_mut(async |enforcer| {
        for (org_id, admin_email) in orgs {
            CasbinPolicyEngine::add_org_policy(enforcer, org_id, admin_email)
                .await
                .map_err(|e| unexpected_error!("{}", e))?;
        }
        Ok(())
    })
    .await
    .map_err(|e| unexpected_error!(e))?;

    Ok(HttpResponse::Ok().body("Org backfill completed"))
}

#[authorized]
#[post("/backfill")]
async fn backfill_workspaces_handler(
    data: Data<AuthZManager>,
    org_id: OrganisationId,
    db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let data = data.try_get_casbin_policy_engine()?;

    let workspaces = workspaces::table
        .filter(workspaces::organisation_id.eq(org_id.to_string()))
        .select((
            workspaces::workspace_schema_name,
            workspaces::workspace_admin_email,
        ))
        .get_results::<(String, String)>(&mut conn)?;

    data.enforcer_mut(async |enforcer| {
        for (schema_name, admin_email) in workspaces {
            CasbinPolicyEngine::add_workspace_admin(enforcer, &schema_name, admin_email)
                .await
                .map_err(|e| unexpected_error!("{}", e))?;
        }
        Ok(())
    })
    .await
    .map_err(|e| unexpected_error!(e))?;

    Ok(HttpResponse::Ok().body("Workspace backfill completed"))
}
