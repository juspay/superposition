use leptos::*;
use leptos_meta::*;
use leptos_router::*;
use serde_json::json;

use crate::components::datetime::DatetimeConversionScript;
use crate::hoc::layout::{CommonLayout, Layout, OrgLayout, Providers};
use crate::pages::authz::{AdminAuthz, OrganisationAuthz, WorkspaceAuthz};
use crate::pages::compare_overrides::CompareOverrides;
use crate::pages::config_version::ConfigVersion;
use crate::pages::config_version_list::ConfigVersionList;
use crate::pages::dimension::{CreateDimension, DimensionPage, EditDimension};
use crate::pages::dimensions::Dimensions;
use crate::pages::experiment_group_listing::ExperimentGroupListing;
use crate::pages::experiment_groups::ExperimentGroups;
use crate::pages::experiment_list::ExperimentList;
use crate::pages::function::{
    FunctionPage, function_create::CreateFunctionView, function_list::FunctionList,
};
use crate::pages::secret::Secret;
use crate::pages::secrets_list::SecretsList;
use crate::pages::variable::Variable;
use crate::pages::variables_list::VariablesList;
use crate::pages::{
    audit_log::AuditLog,
    context_override::ContextOverride,
    default_config::{CreateDefaultConfig, DefaultConfig, EditDefaultConfig},
    default_config_list::DefaultConfigList,
    experiment::ExperimentPage,
    home::Home,
    organisations::Organisations,
    override_page::{CreateOverride, EditOverride, OverridePage},
    type_template::TypePage,
    type_templates::TypesPage,
    webhook::Webhook,
    webhooks::Webhooks,
    workspace::Workspace,
};
use crate::types::{Envs, RoutePart, RouteSegment, join_route_parts};

#[component]
pub fn App(app_envs: Envs) -> impl IntoView {
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
                <Script
                    type_="text/javascript"
                    src="https://cdn.jsdelivr.net/npm/sortablejs@1.15.6/Sortable.min.js"
                />
                <DatetimeConversionScript />
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
            <Providers>
                <Routes base=service_prefix.to_string()>
                    <Route
                        ssr=SsrMode::InOrder
                        path=format!(
                            "/{}",
                            join_route_parts([RouteSegment::Admin, RouteSegment::Organisations]),
                        )
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
                        path=format!(
                            "/{}",
                            join_route_parts([
                                RouteSegment::Admin,
                                RouteSegment::Settings,
                                RouteSegment::Authz,
                            ]),
                        )
                        view=move || {
                            view! {
                                <CommonLayout>
                                    <AdminAuthz />
                                </CommonLayout>
                            }
                        }
                    />

                    <Route
                        ssr=SsrMode::Async
                        path=format!(
                            "/{}",
                            join_route_parts([
                                RoutePart::from(RouteSegment::Admin),
                                RoutePart::from("org_id"),
                            ]),
                        )
                        view=OrgLayout
                    >
                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([RouteSegment::Workspaces])
                            view=Workspace
                        />

                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([RouteSegment::OrgAuthz])
                            view=OrganisationAuthz
                        />
                    </Route>

                    <Route
                        ssr=SsrMode::Async
                        path=format!(
                            "/{}",
                            join_route_parts([
                                RoutePart::from(RouteSegment::Admin),
                                RoutePart::from("org_id"),
                                RoutePart::from("workspace"),
                            ]),
                        )
                        view=Layout
                    >
                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([RouteSegment::Dimensions])
                            view=Dimensions
                        />
                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([
                                RouteSegment::Dimensions,
                                RouteSegment::Action,
                                RouteSegment::Create,
                            ])
                            view=CreateDimension
                        />
                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([
                                RoutePart::from(RouteSegment::Dimensions),
                                RoutePart::from("dimension_name"),
                                RoutePart::from(RouteSegment::Edit),
                            ])
                            view=EditDimension
                        />
                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([
                                RoutePart::from(RouteSegment::Dimensions),
                                RoutePart::from("dimension_name"),
                            ])
                            view=DimensionPage
                        />

                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([RouteSegment::Function])
                            view=FunctionList
                        />
                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([
                                RouteSegment::Function,
                                RouteSegment::Action,
                                RouteSegment::Create,
                            ])
                            view=CreateFunctionView
                        />
                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([
                                RoutePart::from(RouteSegment::Function),
                                RoutePart::from("function_name"),
                            ])
                            view=FunctionPage
                        />

                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([RouteSegment::Experiments])
                            view=ExperimentList
                        />
                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([
                                RoutePart::from(RouteSegment::Experiments),
                                RoutePart::from("id"),
                            ])
                            view=ExperimentPage
                        />

                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([RouteSegment::ExperimentGroups])
                            view=ExperimentGroupListing
                        />
                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([
                                RoutePart::from(RouteSegment::ExperimentGroups),
                                RoutePart::from("id"),
                            ])
                            view=ExperimentGroups
                        />

                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([RouteSegment::DefaultConfig])
                            view=DefaultConfigList
                        />
                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([
                                RouteSegment::DefaultConfig,
                                RouteSegment::Action,
                                RouteSegment::Create,
                            ])
                            view=CreateDefaultConfig
                        />
                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([
                                RoutePart::from(RouteSegment::DefaultConfig),
                                RoutePart::from("config_key"),
                                RoutePart::from(RouteSegment::Edit),
                            ])
                            view=EditDefaultConfig
                        />
                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([
                                RoutePart::from(RouteSegment::DefaultConfig),
                                RoutePart::from("config_key"),
                            ])
                            view=DefaultConfig
                        />

                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([RouteSegment::Overrides])
                            view=ContextOverride
                        />

                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([
                                RouteSegment::Overrides,
                                RouteSegment::Action,
                                RouteSegment::Create,
                            ])
                            view=CreateOverride
                        />

                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([
                                RoutePart::from(RouteSegment::Overrides),
                                RoutePart::from("context_id"),
                                RoutePart::from(RouteSegment::Edit),
                            ])
                            view=EditOverride
                        />

                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([
                                RoutePart::from(RouteSegment::Overrides),
                                RoutePart::from("context_id"),
                            ])
                            view=OverridePage
                        />

                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([RouteSegment::Resolve])
                            view=Home
                        />

                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([RouteSegment::Types])
                            view=TypesPage
                        />
                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([
                                RoutePart::from(RouteSegment::Types),
                                RoutePart::from("type_name"),
                            ])
                            view=TypePage
                        />

                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([RouteSegment::Config, RouteSegment::Versions])
                            view=ConfigVersionList
                        />

                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([
                                RoutePart::from(RouteSegment::Config),
                                RoutePart::from(RouteSegment::Versions),
                                RoutePart::from("version"),
                            ])
                            view=ConfigVersion
                        />

                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([RouteSegment::Compare])
                            view=CompareOverrides
                        />

                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([RouteSegment::Webhooks])
                            view=Webhooks
                        />
                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([
                                RoutePart::from(RouteSegment::Webhooks),
                                RoutePart::from("webhook_name"),
                            ])
                            view=Webhook
                        />

                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([RouteSegment::AuditLog])
                            view=AuditLog
                        />

                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([RouteSegment::Authz])
                            view=WorkspaceAuthz
                        />

                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([RouteSegment::Variables])
                            view=VariablesList
                        />
                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([
                                RoutePart::from(RouteSegment::Variables),
                                RoutePart::from("variable_name"),
                            ])
                            view=Variable
                        />

                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([RouteSegment::Secrets])
                            view=SecretsList
                        />
                        <Route
                            ssr=SsrMode::Async
                            path=join_route_parts([
                                RoutePart::from(RouteSegment::Secrets),
                                RoutePart::from("secret_name"),
                            ])
                            view=Secret
                        />
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
            </Providers>
        </Router>
    }
}
