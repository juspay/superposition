use leptos::*;
use leptos_router::{Route, SsrMode};

use crate::{
    hoc::{layout::Layout, nav_breadcrums::WithBreadcrumbs},
    pages::{
        config_version::ConfigVersion,
        config_version_list::ConfigVersionList,
        context_override::ContextOverride,
        default_config::DefaultConfig,
        dimensions::Dimensions,
        experiment::ExperimentPage,
        experiment_list::ExperimentList,
        function::{
            function_create::CreateFunctionView, function_list::FunctionList,
            FunctionPage,
        },
        home::Home,
        new_experiment::NewExperiment,
        new_template_type::NewTemplateType,
        template_types::TypesPage,
        update_template_type::UpdateTemplateType,
    },
};

#[component(transparent)]
pub fn app_routes() -> impl IntoView {
    view! {
        <Route
            ssr=SsrMode::Async
            path="/admin/:tenant"
            view=move || {
                view! { <Layout /> }
            }
        >
            <Route
                ssr=SsrMode::Async
                path="/experiments"
                view=move || {
                    view! {
                        <WithBreadcrumbs>
                            <ExperimentList />
                        </WithBreadcrumbs>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/experiments/new"
                view=move || {
                    view! {
                        <WithBreadcrumbs>
                            <NewExperiment />
                        </WithBreadcrumbs>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/experiments/:id"
                view=move || {
                    view! {
                        <WithBreadcrumbs>
                            <ExperimentPage />
                        </WithBreadcrumbs>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/function"
                view=move || {
                    view! {
                        <WithBreadcrumbs>
                            <FunctionList />
                        </WithBreadcrumbs>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/function/create"
                view=move || {
                    view! {
                        <WithBreadcrumbs>
                            <CreateFunctionView />
                        </WithBreadcrumbs>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/function/:function_name"
                view=move || {
                    view! {
                        <WithBreadcrumbs>
                            <FunctionPage />
                        </WithBreadcrumbs>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/types"
                view=move || {
                    view! {
                        <WithBreadcrumbs>
                            <TypesPage />
                        </WithBreadcrumbs>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/types/new"
                view=move || {
                    view! {
                        <WithBreadcrumbs>
                            <NewTemplateType />
                        </WithBreadcrumbs>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/types/:type_name/update"
                view=move || {
                    view! {
                        <WithBreadcrumbs>
                            <UpdateTemplateType />
                        </WithBreadcrumbs>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/config/version"
                view=move || {
                    view! { <ConfigVersionList /> }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/config/version/:version"
                view=move || {
                    view! { <ConfigVersion /> }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/dimensions"
                view=move || {
                    view! {
                        <WithBreadcrumbs>
                            <Dimensions />
                        </WithBreadcrumbs>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/default-config"
                view=move || {
                    view! {
                        <WithBreadcrumbs>
                            <DefaultConfig />
                        </WithBreadcrumbs>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/overrides"
                view=move || {
                    view! {
                        <WithBreadcrumbs>
                            <ContextOverride />
                        </WithBreadcrumbs>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/resolve"
                view=move || {
                    view! {
                        <WithBreadcrumbs>
                            <Home />
                        </WithBreadcrumbs>
                    }
                }
            />
        </Route>
    }
}
