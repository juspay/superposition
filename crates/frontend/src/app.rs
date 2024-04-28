use leptos::*;
use leptos_meta::*;
use leptos_router::*;
use serde_json::json;

use crate::hoc::layout::layout::Layout;
use crate::pages::experiment_list::experiment_list::ExperimentList;
use crate::pages::function::{
    function::FunctionPage, function_create::CreateFunctionView,
    function_list::FunctionList,
};
use crate::pages::Dimensions::Dimensions::Dimensions;
use crate::pages::{
    default_config::default_config::DefaultConfig,
    ContextOverride::context_override::ContextOverride, Experiment::ExperimentPage,
    Home::Home::Home,
};
use crate::providers::alert_provider::AlertProvider;
use crate::types::Envs;

#[component]
pub fn App(app_envs: Envs) -> impl IntoView {
    // Provides context that manages stylesheets, titles, meta tags, etc.
    provide_meta_context();

    let service_prefix = app_envs.service_prefix;
    provide_context(app_envs.clone());
    view! {
        <html data-theme="light">
            {move || {
                let base = match service_prefix {
                    "" | "/" => "".to_owned(),
                    prefix => "/".to_owned() + prefix,
                };
                let styles_href = base.to_owned() + "/pkg/style.css";
                let favicon_href = base.to_owned() + "/assets/favicon.ico";
                let wasm_href = base.to_owned() + "/pkg/frontend_bg.wasm";
                let js_href = base.to_owned() + "/pkg/frontend.js";
                let import_callback = "() => mod.hydrate()";
                view! {
                    <Stylesheet id="leptos" href=styles_href/>
                    <Link rel="shortcut icon" type_="image/ico" href=favicon_href/>
                    <Link
                        href="https://cdn.jsdelivr.net/npm/remixicon@3.5.0/fonts/remixicon.css"
                        rel="stylesheet"
                    />
                    {move || {
                        if base == "" {
                            view! {}.into_view()
                        } else {
                            view! {
                                <link
                                    rel="preload"
                                    href=wasm_href.clone()
                                    as_="fetch"
                                    type_="application/wasm"
                                    crossorigin=""
                                />
                                <link rel="modulepreload" href=js_href.clone()/>
                                <script type_="module">
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
                                                mod.default('{wasm_href}').then({import_callback});
                                            }})
                                    }});
                                    "#,
                                    )}

                                </script>
                            }
                                .into_view()
                        }
                    }}
                }
            }}
            // sets the document title
            <Title text="Welcome to Context Aware Config"/>
            <script type_="text/javascript">"__APP_ENVS=" {json!(app_envs).to_string()}</script>
            <Router base=service_prefix>
                <body class="m-0 min-h-screen bg-gray-50 font-mono">
                    <AlertProvider>
                        <Routes base=service_prefix.to_string()>
                            <Route
                                ssr=SsrMode::Async
                                path="/admin/:tenant/dimensions"
                                view=move || {
                                    view! {
                                        <Layout>
                                            <Dimensions/>
                                        </Layout>
                                    }
                                }
                            />

                            <Route
                                ssr=SsrMode::Async
                                path="/admin/:tenant/function"
                                view=move || {
                                    view! {
                                        <Layout>
                                            <FunctionList/>
                                        </Layout>
                                    }
                                }
                            />

                            <Route
                                ssr=SsrMode::Async
                                path="/admin/:tenant/function/create"
                                view=move || {
                                    view! {
                                        <Layout>
                                            <CreateFunctionView/>
                                        </Layout>
                                    }
                                }
                            />

                            <Route
                                ssr=SsrMode::Async
                                path="/admin/:tenant/function/:function_name"
                                view=move || {
                                    view! {
                                        <Layout>
                                            <FunctionPage/>
                                        </Layout>
                                    }
                                }
                            />

                            <Route
                                ssr=SsrMode::Async
                                path="/admin/:tenant/experiments"
                                view=move || {
                                    view! {
                                        <Layout>
                                            <ExperimentList/>
                                        </Layout>
                                    }
                                }
                            />

                            <Route
                                ssr=SsrMode::Async
                                path="/admin/:tenant/experiments/:id"
                                view=move || {
                                    view! {
                                        <Layout>
                                            <ExperimentPage/>
                                        </Layout>
                                    }
                                }
                            />

                            <Route
                                ssr=SsrMode::Async
                                path="/admin/:tenant/default-config"
                                view=move || {
                                    view! {
                                        <Layout>
                                            <DefaultConfig/>
                                        </Layout>
                                    }
                                }
                            />

                            <Route
                                ssr=SsrMode::Async
                                path="/admin/:tenant/overrides"
                                view=move || {
                                    view! {
                                        <Layout>
                                            <ContextOverride/>
                                        </Layout>
                                    }
                                }
                            />

                            <Route
                                ssr=SsrMode::Async
                                path="/admin/:tenant/resolve"
                                view=move || {
                                    view! {
                                        <Layout>
                                            <Home/>
                                        </Layout>
                                    }
                                }
                            />

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
                </body>
            </Router>

        </html>
    }
}
