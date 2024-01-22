use std::env::VarError;
use std::str::FromStr;

use leptos::*;
use leptos_meta::*;
use leptos_router::*;

use crate::hoc::layout::layout::Layout;
use crate::pages::Dimensions::Dimensions::Dimensions;
use crate::pages::ExperimentList::ExperimentList::ExperimentList;
use crate::pages::{
    ContextOverride::ContextOverride::ContextOverride,
    DefaultConfig::DefaultConfig::DefaultConfig, Experiment::ExperimentPage,
    Home::Home::Home, NotFound::NotFound::NotFound,
};
use crate::types::{AppEnv, Envs};

fn get_from_env_unsafe<F>(name: &str) -> Result<F, VarError>
where
    F: FromStr,
    <F as FromStr>::Err: std::fmt::Debug,
{
    std::env::var(name)
        .map(|val| val.parse().unwrap())
        .map_err(|e| {
            return e;
        })
}

async fn load_envs() -> Envs {
    let app_env = get_from_env_unsafe::<AppEnv>("APP_ENV").unwrap_or(AppEnv::DEV);

    let tenants = get_from_env_unsafe::<String>("TENANTS")
        .unwrap_or("".into())
        .split(",")
        .map(|tenant| tenant.to_string())
        .collect::<Vec<String>>();

    let host = get_from_env_unsafe::<String>("API_HOSTNAME")
        .unwrap_or(String::from("http://localhost:8080"));

    Envs {
        host,
        app_env,
        tenants,
    }
}

#[component]
pub fn App() -> impl IntoView {
    // Provides context that manages stylesheets, titles, meta tags, etc.
    provide_meta_context();

    let envs_resource = create_blocking_resource(|| (), |_| load_envs());
    provide_context(envs_resource);
    view! {
        <html data-theme="light">
            <Stylesheet id="leptos" href="/pkg/style.css"/>
            <Link rel="shortcut icon" type_="image/ico" href="/assets/favicon.ico"/>
            <Link
                href="https://cdn.jsdelivr.net/npm/remixicon@3.5.0/fonts/remixicon.css"
                rel="stylesheet"
            />
            // sets the document title
            <Title text="Welcome to Context Aware Config"/>
            // content for this welcome page
            <Router>
                <body class="m-0 min-h-screen bg-gray-50 font-mono">
                    <Layout>
                        <Routes>
                            <Route
                                ssr=SsrMode::PartiallyBlocked
                                path="/admin/:tenant/dimensions"
                                view=Dimensions
                            />
                            <Route
                                ssr=SsrMode::PartiallyBlocked
                                path="/admin/:tenant/experiments"
                                view=ExperimentList
                            />
                            <Route
                                ssr=SsrMode::PartiallyBlocked
                                path="/admin/:tenant/experiments/:id"
                                view=ExperimentPage
                            />
                            <Route
                                ssr=SsrMode::PartiallyBlocked
                                path="/admin/:tenant/default-config"
                                view=DefaultConfig
                            />
                            <Route
                                ssr=SsrMode::PartiallyBlocked
                                path="/admin/:tenant/overrides"
                                view=ContextOverride
                            />
                            <Route
                                ssr=SsrMode::PartiallyBlocked
                                path="/admin/:tenant/resolve"
                                view=Home
                            />
                            <Route path="/*any" view=NotFound/>
                        </Routes>
                    </Layout>
                </body>
            </Router>
        </html>
    }
}
