pub mod experiments {
    use dashboard_auth::types::AuthenticatedRoute;

    pub fn authenticated_routes() -> Vec<(&'static str, AuthenticatedRoute)> {
        Vec::from([
            (
                "POST::/experiments",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
            (
                "PATCH::/experiments/{experiment_id}/conclude",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
            (
                "PATCH::/experiments/{id}/ramp",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
            (
                "PATCH::/experiments/{id}/stabilize",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
            (
                "PATCH::/experiments/{id}/revert",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
            (
                "PUT::/experiments/{id}/overrides",
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
                "PUT::/context",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
            (
                "PUT::/context/move/{ctx_id}",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
            (
                "DELETE::/context/{ctx_id}",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
            (
                "PUT::/context/{ctx_id}",
                AuthenticatedRoute {
                    api_tag: "MANAGER".into(),
                    user_permissions: ("manager".into(), "RW".into()),
                },
            ),
            (
                "PUT::/context/bulk-operations",
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
            "PUT::/default-config/{key}",
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
            "PUT::/dimension",
            AuthenticatedRoute {
                api_tag: "MANAGER".into(),
                user_permissions: ("manager".into(), "RW".into()),
            },
        )])
    }
}
