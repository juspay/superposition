use leptos::*;
use leptos_router::{Route, SsrMode};

use crate::{
    hoc::layout::{Layout, Page},
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
