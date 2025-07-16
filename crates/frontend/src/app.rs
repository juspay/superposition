use leptos::*;
use leptos_meta::*;
use leptos_router::*;
use serde_json::json;

use crate::hoc::layout::{use_org, CommonLayout, Layout};
use crate::pages::compare_overrides::CompareOverrides;
use crate::pages::config_version::ConfigVersion;
use crate::pages::config_version_list::ConfigVersionList;
use crate::pages::dimensions::Dimensions;
use crate::pages::experiment_group_listing::ExperimentGroupListing;
use crate::pages::experiment_groups::ExperimentGroups;
use crate::pages::experiment_list::ExperimentList;
use crate::pages::function::{
    function_create::CreateFunctionView, function_list::FunctionList, FunctionPage,
};
use crate::pages::{
    context_override::ContextOverride, custom_types::TypesPage,
    default_config::DefaultConfig, experiment::ExperimentPage, home::Home,
    organisations::Organisations, webhooks::Webhooks, workspace::Workspace,
};
use crate::providers::alert_provider::AlertProvider;
use crate::types::Envs;

#[component]
pub fn app(app_envs: Envs) -> impl IntoView {
    // Provides context that manages stylesheets, titles, meta tags, etc.
    provide_meta_context();
    let service_prefix = app_envs.service_prefix;
    provide_context(app_envs.clone());
    view! {
        <Html {..} attr:data-theme="light" />
        {move || {
            let base = match service_prefix {
                "" | "/" => "".to_owned(),
                prefix => "/".to_owned() + prefix,
            };
            let styles_href = base.to_owned() + "/pkg/style.css";
            let favicon_href = base.to_owned() + "/assets/favicon.ico";

            view! {
                <Stylesheet id="leptos" href=styles_href />
                <Link rel="shortcut icon" type_="image/ico" href=favicon_href />
                <Link
                    href="https://cdn.jsdelivr.net/npm/remixicon/fonts/remixicon.css"
                    rel="stylesheet"
                />
                <Script
                    type_="text/javascript"
                    src="https://cdn.jsdelivr.net/npm/@andypf/json-viewer@2.1.5/dist/iife/index.min.js"
                />
                {move || {
                    if base.is_empty() {
                        view! {}.into_view()
                    } else {
                        let wasm_href = base.to_owned() + "/pkg/frontend_bg.wasm";
                        let js_href = base.to_owned() + "/pkg/frontend.js";
                        view! {
                            <Link
                                rel="preload"
                                href=wasm_href.clone()
                                as_="fetch"
                                type_="application/wasm"
                                crossorigin=""
                            />
                            <Link as_="script" rel="modulepreload" href=js_href.clone() />
                            <Script type_="module">
                                {format!(
                                    r#"
                                    function idle(c) {{
                                        if ('requestIdleCallback' in window) {{
                                            window.requestIdleCallback(c);
                                        }} else {{
                                            c();
                                        }}
                                    }}
                                    idle(() => {{
                                        import('{js_href}')
                                            .then(mod => {{
                                                mod.default('{wasm_href}').then(() => mod.hydrate());
                                            }})
                                    }});
                                    "#,
                                )}
                            </Script>
                        }
                            .into_view()
                    }
                }}
            }
        }}
        <Title text="Welcome to Superposition" />
        <script type_="text/javascript">"__APP_ENVS=" {json!(app_envs).to_string()}</script>
        <Body class="h-screen m-0 flex bg-gray-50 overflow-y-hidden" />
        <Router base=service_prefix>
            <AlertProvider>
                <Routes base=service_prefix.to_string()>
                    <Route
                        ssr=SsrMode::InOrder
                        path="/admin/organisations"
                        view=move || {
                            view! {
                                <CommonLayout>
                                    <Organisations />
                                </CommonLayout>
                            }
                        }
                    />

                    <Route
                        ssr=SsrMode::Async
                        path="/admin/:org_id/workspaces"
                        view=move || {
                            provide_context(use_org());

                            view! {
                                <CommonLayout>
                                    <Workspace />
                                </CommonLayout>
                            }
                        }
                    />

                    <Route ssr=SsrMode::Async path="/admin/:org_id/:tenant" view=Layout>
                        <Route ssr=SsrMode::Async path="dimensions" view=Dimensions />

                        <Route ssr=SsrMode::Async path="function" view=FunctionList />

                        <Route ssr=SsrMode::Async path="function/create" view=CreateFunctionView />

                        <Route
                            ssr=SsrMode::Async
                            path="function/:function_name"
                            view=FunctionPage
                        />

                        <Route ssr=SsrMode::Async path="experiments" view=ExperimentList />

                        <Route ssr=SsrMode::Async path="experiments/:id" view=ExperimentPage />

                        <Route
                            ssr=SsrMode::Async
                            path="experiment-groups"
                            view=ExperimentGroupListing
                        />

                        <Route
                            ssr=SsrMode::Async
                            path="experiment-groups/:id"
                            view=ExperimentGroups
                        />

                        <Route ssr=SsrMode::Async path="default-config" view=DefaultConfig />

                        <Route ssr=SsrMode::Async path="overrides" view=ContextOverride />

                        <Route ssr=SsrMode::Async path="resolve" view=Home />

                        <Route ssr=SsrMode::Async path="types" view=TypesPage />

                        <Route ssr=SsrMode::Async path="config/versions" view=ConfigVersionList />

                        <Route
                            ssr=SsrMode::Async
                            path="config/versions/:version"
                            view=ConfigVersion
                        />

                        <Route ssr=SsrMode::Async path="compare" view=CompareOverrides />

                        <Route ssr=SsrMode::Async path="webhooks" view=Webhooks />
                    </Route>
                // <Route
                // path="/*any"
                // view=move || {
                // view! {
                // <Layout>
                // <NotFound/>
                // </Layout>
                // }
                // }
                // />
                </Routes>
            </AlertProvider>
        </Router>
    }
}
