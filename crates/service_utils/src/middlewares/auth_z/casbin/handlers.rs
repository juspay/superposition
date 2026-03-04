use actix_web::{
    HttpResponse, Scope, delete, get, post,
    web::{Data, Json, Path},
};
use casbin::{MgmtApi, RbacApi};
use superposition_derives::authorized;
use superposition_macros::unexpected_error;
use superposition_types::{
    api::authz::casbin::{
        ActionGroupPolicyRequest, GroupingPolicyRequest, PolicyRequest,
    },
    result as superposition,
};

use crate::middlewares::auth_z::{AuthZDomain, AuthZManager};

pub fn workspace_endpoints() -> Scope {
    Scope::new("/casbin")
        .service(add_policy_handler)
        .service(remove_policy_handler)
        .service(list_policy_handler)
        .service(add_grouping_policy_handler)
        .service(remove_grouping_policy_handler)
        .service(add_action_group_policy_handler)
        .service(remove_action_group_policy_handler)
        .service(list_action_group_policies_handler)
        .service(list_roles_handler)
    // .service(get_roles_for_user_handler)
    // .service(get_users_for_role_handler)
}

pub fn admin_endpoints() -> Scope {
    Scope::new("/casbin")
        .service(add_policy_handler)
        .service(remove_policy_handler)
        .service(list_policy_handler)
        .service(add_grouping_policy_handler)
        .service(remove_grouping_policy_handler)
        .service(add_action_group_policy_handler)
        .service(remove_action_group_policy_handler)
        .service(list_action_group_policies_handler)
        .service(list_roles_handler)
    // .service(get_roles_for_user_handler)
    // .service(get_users_for_role_handler)
}

pub fn org_endpoints() -> Scope {
    Scope::new("/casbin")
        .service(add_policy_handler)
        .service(remove_policy_handler)
        .service(list_policy_handler)
        .service(add_grouping_policy_handler)
        .service(remove_grouping_policy_handler)
        .service(add_action_group_policy_handler)
        .service(remove_action_group_policy_handler)
        .service(list_action_group_policies_handler)
        .service(list_roles_handler)
    // .service(get_roles_for_user_handler)
    // .service(get_users_for_role_handler)
}

// based on this condition: (r.dom == p.dom || p.dom == "*" || globMatch(r.dom, p.dom))
fn domain_matcher(request_domain: &str, policy_domain: &str) -> bool {
    // Exact match or wildcard match (*) or glob pattern match (o1_*)
    request_domain == policy_domain // exact match
        || policy_domain == "*" // full wildcard match
    || (policy_domain.ends_with('*') // glob pattern match
        && request_domain.starts_with(&policy_domain[..policy_domain.len() - 1]))
}

#[authorized]
#[post("/policy")]
async fn add_policy_handler(
    data: Data<AuthZManager>,
    body: Json<PolicyRequest>,
    domain: AuthZDomain,
) -> superposition::Result<HttpResponse> {
    let data = data.try_get_casbin_policy_engine()?;
    let body = body.into_inner();
    let mut enforcer = data.enforcer().await.map_err(|e| unexpected_error!(e))?;
    let added = enforcer
        .add_policy(vec![
            body.sub,
            domain.to_string(),
            body.obj.to_string(),
            body.act,
            body.attr.unwrap_or("*".to_string()),
        ])
        .await
        .map_err(|e| unexpected_error!(e))?;

    if added {
        Ok(HttpResponse::Ok().body("Policy added"))
    } else {
        Ok(HttpResponse::Ok().body("Policy already exists"))
    }
}

#[authorized]
#[delete("/policy")]
async fn remove_policy_handler(
    data: Data<AuthZManager>,
    body: Json<PolicyRequest>,
    domain: AuthZDomain,
) -> superposition::Result<HttpResponse> {
    let data = data.try_get_casbin_policy_engine()?;
    let body = body.into_inner();
    let mut enforcer = data.enforcer().await.map_err(|e| unexpected_error!(e))?;
    let removed = enforcer
        .remove_policy(vec![
            body.sub,
            domain.to_string(),
            body.obj.to_string(),
            body.act,
            body.attr.unwrap_or("*".to_string()),
        ])
        .await
        .map_err(|e| unexpected_error!(e))?;

    if removed {
        Ok(HttpResponse::Ok().body("Policy removed"))
    } else {
        Ok(HttpResponse::Ok().body("Policy does not exist"))
    }
}

#[authorized]
#[get("/policy")]
async fn list_policy_handler(
    data: Data<AuthZManager>,
    domain: AuthZDomain,
) -> superposition::Result<Json<Vec<Vec<String>>>> {
    let data = data.try_get_casbin_policy_engine()?;
    let enforcer = data.enforcer().await.map_err(|e| unexpected_error!(e))?;
    let dom = domain.to_string();
    let policies = enforcer
        .get_policy()
        .into_iter()
        .filter(|rule| rule.get(1).is_some_and(|d| domain_matcher(&dom, d)))
        .collect();
    Ok(Json(policies))
}

#[authorized]
#[post("/roles")]
async fn add_grouping_policy_handler(
    data: Data<AuthZManager>,
    body: Json<GroupingPolicyRequest>,
    domain: AuthZDomain,
) -> superposition::Result<HttpResponse> {
    let data = data.try_get_casbin_policy_engine()?;
    let body = body.into_inner();
    let mut enforcer = data.enforcer().await.map_err(|e| unexpected_error!(e))?;
    let added = enforcer
        // g = user, role, dom
        .add_grouping_policy(vec![body.user, body.role, domain.to_string()])
        .await
        .map_err(|e| unexpected_error!(e))?;

    if added {
        Ok(HttpResponse::Ok().body("Grouping policy added"))
    } else {
        Ok(HttpResponse::Ok().body("Grouping policy already exists"))
    }
}

#[authorized]
#[delete("/roles")]
async fn remove_grouping_policy_handler(
    data: Data<AuthZManager>,
    body: Json<GroupingPolicyRequest>,
    domain: AuthZDomain,
) -> superposition::Result<HttpResponse> {
    let data = data.try_get_casbin_policy_engine()?;
    let body = body.into_inner();
    let mut enforcer = data.enforcer().await.map_err(|e| unexpected_error!(e))?;
    let removed = enforcer
        .remove_grouping_policy(vec![body.user, body.role, domain.to_string()])
        .await
        .map_err(|e| unexpected_error!(e))?;

    if removed {
        Ok(HttpResponse::Ok().body("Grouping policy removed"))
    } else {
        Ok(HttpResponse::Ok().body("Grouping policy does not exist"))
    }
}

#[authorized]
#[get("/roles")]
async fn list_roles_handler(
    data: Data<AuthZManager>,
    domain: AuthZDomain,
) -> superposition::Result<Json<Vec<Vec<String>>>> {
    let data = data.try_get_casbin_policy_engine()?;
    let enforcer = data.enforcer().await.map_err(|e| unexpected_error!(e))?;
    // g = user, role, dom. Filter by dom.
    let dom = domain.to_string();
    let policies = enforcer
        .get_grouping_policy()
        .into_iter()
        .filter(|rule| rule.get(2).is_some_and(|d| domain_matcher(&dom, d)))
        .collect::<Vec<_>>();
    Ok(Json(policies))
}

#[authorized]
#[post("/action-groups")]
async fn add_action_group_policy_handler(
    data: Data<AuthZManager>,
    body: Json<ActionGroupPolicyRequest>,
    domain: AuthZDomain,
) -> superposition::Result<HttpResponse> {
    let data = data.try_get_casbin_policy_engine()?;
    let body = body.into_inner();
    let mut enforcer = data.enforcer().await.map_err(|e| unexpected_error!(e))?;
    let added = enforcer
        .add_named_grouping_policy(
            "g2",
            vec![
                format!("{}:{}", body.resource, body.action),
                domain.to_string(),
                body.action_group,
            ],
        )
        .await
        .map_err(|e| unexpected_error!(e))?;

    if added {
        Ok(HttpResponse::Ok().body("Action-group policy added"))
    } else {
        Ok(HttpResponse::Ok().body("Action-group policy already exists"))
    }
}

#[authorized]
#[delete("/action-groups")]
async fn remove_action_group_policy_handler(
    data: Data<AuthZManager>,
    body: Json<ActionGroupPolicyRequest>,
    domain: AuthZDomain,
) -> superposition::Result<HttpResponse> {
    let data = data.try_get_casbin_policy_engine()?;
    let body = body.into_inner();
    let mut enforcer = data.enforcer().await.map_err(|e| unexpected_error!(e))?;
    let removed = enforcer
        .remove_named_grouping_policy(
            "g2",
            vec![
                format!("{}:{}", body.resource, body.action),
                domain.to_string(),
                body.action_group,
            ],
        )
        .await
        .map_err(|e| unexpected_error!(e))?;

    if removed {
        Ok(HttpResponse::Ok().body("Action-group policy removed"))
    } else {
        Ok(HttpResponse::Ok().body("Action-group policy does not exist"))
    }
}

#[authorized]
#[get("/action-groups")]
async fn list_action_group_policies_handler(
    data: Data<AuthZManager>,
    domain: AuthZDomain,
) -> superposition::Result<Json<Vec<Vec<String>>>> {
    let data = data.try_get_casbin_policy_engine()?;
    let enforcer = data.enforcer().await.map_err(|e| unexpected_error!(e))?;
    let dom = domain.to_string();
    let policies = enforcer
        .get_named_grouping_policy("g2")
        .into_iter()
        .filter(|rule| rule.get(1).is_some_and(|d| domain_matcher(&dom, d)))
        .collect::<Vec<_>>();
    Ok(Json(policies))
}

#[authorized]
#[get("/users/{user}/roles")]
async fn get_roles_for_user_handler(
    data: Data<AuthZManager>,
    path: Path<String>,
) -> superposition::Result<HttpResponse> {
    let data = data.try_get_casbin_policy_engine()?;
    let user = path.into_inner();
    let enforcer = data.enforcer().await.map_err(|e| unexpected_error!(e))?;
    let roles = enforcer.get_roles_for_user(&user, None);
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
    let enforcer = data.enforcer().await.map_err(|e| unexpected_error!(e))?;
    let users = enforcer.get_users_for_role(&role, None);
    Ok(HttpResponse::Ok().json(users))
}
