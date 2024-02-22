use dashboard_auth::types::AuthenticatedRoute;

pub fn fill_service_prefix(
    routes: Vec<(&'static str, AuthenticatedRoute)>,
    service_prefix: &str,
) -> Vec<(String, AuthenticatedRoute)> {
    routes
        .into_iter()
        .map(|route| {
            (
                route.0.replace("{service_prefix}", service_prefix),
                route.1.clone(),
            )
        })
        .collect()
}

pub mod experiments {
    use dashboard_auth::types::AuthenticatedRoute;

    pub fn authenticated_routes() -> Vec<(&'static str, AuthenticatedRoute)> {
        Vec::from([
            (
                "POST::{service_prefix}/experiments",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
            (
                "PATCH::{service_prefix}/experiments/{experiment_id}/conclude",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
            (
                "PATCH::{service_prefix}/experiments/{id}/ramp",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
            (
                "PATCH::{service_prefix}/experiments/{id}/stabilize",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
            (
                "PATCH::{service_prefix}/experiments/{id}/revert",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
            (
                "PUT::{service_prefix}/experiments/{id}/overrides",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
        ])
    }
}

pub mod contexts {
    use dashboard_auth::types::AuthenticatedRoute;
    pub fn authenticated_routes() -> Vec<(&'static str, AuthenticatedRoute)> {
        Vec::from([
            (
                "PUT::{service_prefix}/context",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
            (
                "PUT::{service_prefix}/context/move/{ctx_id}",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
            (
                "DELETE::{service_prefix}/context/{ctx_id}",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
            (
                "PUT::{service_prefix}/context/{ctx_id}",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
            (
                "PUT::{service_prefix}/context/bulk-operations",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
        ])
    }
}

pub mod default_config {
    use dashboard_auth::types::AuthenticatedRoute;
    pub fn authenticated_routes() -> Vec<(&'static str, AuthenticatedRoute)> {
        Vec::from([(
            "PUT::{service_prefix}/default-config/{key}",
            AuthenticatedRoute {
                api_tag: "MANAGER".into(),
                user_permissions: ("manager".into(), "RW".into()),
            },
        )])
    }
}

pub mod dimension {
    use dashboard_auth::types::AuthenticatedRoute;
    pub fn authenticated_routes() -> Vec<(&'static str, AuthenticatedRoute)> {
        Vec::from([(
            "PUT::{service_prefix}/dimension",
            AuthenticatedRoute {
                api_tag: "MANAGER".into(),
                user_permissions: ("manager".into(), "RW".into()),
            },
        )])
    }
}

pub mod functions {
    use dashboard_auth::types::AuthenticatedRoute;
    pub fn authenticated_routes() -> Vec<(&'static str, AuthenticatedRoute)> {
        Vec::from([
            (
                "POST::/function",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
            (
                "PATCH::/function/{function_name}",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
            (
                "GET::/function/{function_name}",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
            (
                "DELETE::/function/{function_name}",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
            (
                "GET::/function",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
        ])
    }
}
