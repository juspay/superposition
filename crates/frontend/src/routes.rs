use leptos::*;
use leptos_router::{Route, SsrMode};

use crate::{
    hoc::layout::{Layout, Page, UnstyledPage},
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
        new_contextual_override::NewContextualOverride,
        new_default_config::NewDefaultConfig,
        new_dimension::NewDimension,
        new_experiment::NewExperiment,
        new_template_type::NewTemplateType,
        organisations::Organisations,
        template_types::TypesPage,
        update_contextual_override::UpdateContextualOverride,
        update_default_config::UpdateDefaultConfig,
        update_dimension::UpdateDimension,
        update_template_type::UpdateTemplateType,
        workspace::Workspace,
    },
};

#[component(transparent)]
pub fn organisation_routes() -> impl IntoView {
    view! {
        <Route
            ssr=SsrMode::Async
            path="/admin"
            view=move || {
                view! { <Layout show_side_nav=false /> }
            }
        >
            <Route
                ssr=SsrMode::Async
                path="/organisations"
                view=move || {
                    view! {
                        <UnstyledPage>
                            <Organisations />
                        </UnstyledPage>
                    }
                }
            />
        </Route>
    }
}

#[component(transparent)]
pub fn workspace_routes() -> impl IntoView {
    view! {
        <Route
            ssr=SsrMode::Async
            path="/admin/:org_id"
            view=move || {
                view! { <Layout show_side_nav=false /> }
            }
        >
            <Route
                ssr=SsrMode::Async
                path="/workspaces"
                view=move || {
                    view! {
                        <UnstyledPage>
                            <Workspace />
                        </UnstyledPage>
                    }
                }
            />
        </Route>
    }
}

#[component(transparent)]
pub fn app_routes() -> impl IntoView {
    view! {
        <Route
            ssr=SsrMode::Async
            path="/admin/:org_id/:tenant"
            view=move || {
                view! { <Layout /> }
            }
        >
            <Route
                ssr=SsrMode::Async
                path="/experiments"
                view=move || {
                    view! {
                        <Page>
                            <ExperimentList />
                        </Page>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/experiments/new"
                view=move || {
                    view! {
                        <Page>
                            <NewExperiment />
                        </Page>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/experiments/:id"
                view=move || {
                    view! {
                        <Page>
                            <ExperimentPage />
                        </Page>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/function"
                view=move || {
                    view! {
                        <Page>
                            <FunctionList />
                        </Page>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/function/create"
                view=move || {
                    view! {
                        <Page>
                            <CreateFunctionView />
                        </Page>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/function/:function_name"
                view=move || {
                    view! {
                        <Page>
                            <FunctionPage />
                        </Page>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/types"
                view=move || {
                    view! {
                        <Page>
                            <TypesPage />
                        </Page>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/types/new"
                view=move || {
                    view! {
                        <Page>
                            <NewTemplateType />
                        </Page>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/types/:type_name/update"
                view=move || {
                    view! {
                        <Page>
                            <UpdateTemplateType />
                        </Page>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/config/versions"
                view=move || {
                    view! {
                        <Page>
                            <ConfigVersionList />
                        </Page>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/config/versions/:version"
                view=move || {
                    view! {
                        <Page>
                            <ConfigVersion />
                        </Page>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/dimensions"
                view=move || {
                    view! {
                        <Page>
                            <Dimensions />
                        </Page>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/dimensions/new"
                view=move || {
                    view! {
                        <Page>
                            <NewDimension />
                        </Page>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/dimensions/:name/update"
                view=move || {
                    view! {
                        <Page>
                            <UpdateDimension />
                        </Page>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/default-config"
                view=move || {
                    view! {
                        <Page>
                            <DefaultConfig />
                        </Page>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/default-config/new"
                view=move || {
                    view! {
                        <Page>
                            <NewDefaultConfig />
                        </Page>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/default-config/:key/update"
                view=move || {
                    view! {
                        <Page>
                            <UpdateDefaultConfig />
                        </Page>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/overrides"
                view=move || {
                    view! {
                        <Page>
                            <ContextOverride />
                        </Page>
                    }
                }
            />
            <Route
                ssr=SsrMode::Async
                path="/overrides/new"
                view=move || {
                    view! {
                        <Page>
                            <NewContextualOverride />
                        </Page>
                    }
                }
            />
            <Route
                ssr=SsrMode::Async
                path="/overrides/:id/update"
                view=move || {
                    view! {
                        <Page>
                            <UpdateContextualOverride />
                        </Page>
                    }
                }
            />

            <Route
                ssr=SsrMode::Async
                path="/resolve"
                view=move || {
                    view! {
                        <Page>
                            <Home />
                        </Page>
                    }
                }
            />
        </Route>
    }
}
