use futures::join;
use leptos::*;
use leptos_router::{use_params_map, use_query_map};
use serde::{Deserialize, Serialize};
use superposition_types::{
    api::{default_config::DefaultConfigFilters, dimension::DimensionResponse},
    custom_query::PaginationParams,
    database::models::cac::DefaultConfig,
};

use crate::{
    api::{default_configs, dimensions, fetch_experiment, get_context},
    components::skeleton::{Skeleton, SkeletonVariant},
    logic::Conditions,
    types::{OrganisationId, VariantFormTs, Workspace},
};

use crate::components::experiment_form::ExperimentFormType;

#[derive(Serialize, Deserialize, Clone, Debug, Default)]
struct FormPageResource {
    dimensions: Vec<DimensionResponse>,
    default_config: Vec<DefaultConfig>,
}

#[component]
pub fn EditExperiment() -> impl IntoView {
    let path_params = use_params_map();
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let experiment_id = Memo::new(move |_| {
        path_params.with(|params| params.get("id").cloned().unwrap_or("1".into()))
    });

    let experiment_resource = create_blocking_resource(
        move || (experiment_id.get(), workspace.get().0, org.get().0),
        |(experiment_id, workspace, org_id)| async move {
            fetch_experiment(experiment_id, &workspace, &org_id)
                .await
                .ok()
        },
    );

    let page_resource: Resource<(String, String), FormPageResource> =
        create_blocking_resource(
            move || (workspace.get().0, org.get().0),
            |(workspace, org_id)| async move {
                let empty_list_filters = PaginationParams::all_entries();
                let default_config_filters = DefaultConfigFilters::default();
                let (dimensions_result, default_config_result) = join!(
                    dimensions::list(&empty_list_filters, &workspace, &org_id),
                    default_configs::list(
                        &empty_list_filters,
                        &default_config_filters,
                        &workspace,
                        &org_id
                    ),
                );
                FormPageResource {
                    dimensions: dimensions_result.unwrap_or_default().data,
                    default_config: default_config_result.unwrap_or_default().data,
                }
            },
        );

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::DetailPage /> }
        }>
            {move || {
                let experiment = match experiment_resource.get() {
                    Some(Some(exp)) => exp,
                    _ => return view! { <h1>"Error fetching experiment"</h1> }.into_view(),
                };
                let exp_id = experiment.id.clone();
                let conditions = Conditions::from_iter(experiment.context.clone().into_inner());
                let variants = VariantFormTs::from_iter(experiment.variants.clone());
                let description = experiment.description.to_string();
                let metrics = experiment.metrics.clone();
                let experiment_group_id = experiment.experiment_group_id.clone();
                let experiment_form_type = ExperimentFormType::from(experiment.experiment_type);
                let FormPageResource { dimensions, default_config } = page_resource
                    .get()
                    .unwrap_or_default();
                let redirect_url = format!(
                    "/admin/{}/{}/experiments/{}",
                    org.get().0,
                    workspace.get().0,
                    exp_id,
                );

                view! {
                    <crate::components::experiment_form_page::ExperimentFormPage
                        edit=true
                        edit_id=exp_id
                        name=experiment.name
                        context=conditions
                        variants=variants
                        experiment_form_type=experiment_form_type
                        default_config=default_config
                        dimensions=dimensions
                        description=description
                        metrics=metrics
                        experiment_group_id=experiment_group_id
                        redirect_url_cancel=redirect_url.clone()
                        redirect_url_success=redirect_url
                    />
                }
                    .into_view()
            }}
        </Suspense>
    }
}

#[component]
pub fn CreateExperiment() -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let query = use_query_map();
    let context_id = Memo::new(move |_| query.with(|q| q.get("context_id").cloned()));
    let is_delete = Memo::new(move |_| {
        query.with(|q| q.get("delete").cloned().unwrap_or_default() == "true")
    });

    let context_resource = create_blocking_resource(
        move || (context_id.get(), workspace.get().0, org.get().0),
        |(context_id, workspace, org_id)| async move {
            let id = context_id?;
            get_context(&id, &workspace, &org_id).await.ok()
        },
    );

    let page_resource: Resource<(String, String), FormPageResource> =
        create_blocking_resource(
            move || (workspace.get().0, org.get().0),
            |(workspace, org_id)| async move {
                let empty_list_filters = PaginationParams::all_entries();
                let default_config_filters = DefaultConfigFilters::default();
                let (dimensions_result, default_config_result) = join!(
                    dimensions::list(&empty_list_filters, &workspace, &org_id),
                    default_configs::list(
                        &empty_list_filters,
                        &default_config_filters,
                        &workspace,
                        &org_id
                    ),
                );
                FormPageResource {
                    dimensions: dimensions_result.unwrap_or_default().data,
                    default_config: default_config_result.unwrap_or_default().data,
                }
            },
        );

    let redirect_url = store_value(format!(
        "/admin/{}/{}/experiments",
        org.get().0,
        workspace.get().0
    ));

    let workspace_settings = use_context::<
        StoredValue<superposition_types::api::workspace::WorkspaceResponse>,
    >()
    .unwrap();

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::DetailPage /> }
        }>
            {move || {
                let FormPageResource { dimensions, default_config } = page_resource
                    .get()
                    .unwrap_or_default();
                let cloned_context = context_resource.get().flatten();
                let (context, variants, experiment_form_type) = match cloned_context {
                    Some(ctx) => {
                        let conditions = Conditions::from_iter(ctx.value.clone().into_inner());
                        let overrides = ctx.override_.clone().into_iter().collect::<Vec<_>>();
                        let form_type = if is_delete.get() {
                            ExperimentFormType::Delete(Some((ctx.id, ctx.override_.into())))
                        } else {
                            ExperimentFormType::Default
                        };
                        (conditions, VariantFormTs::default_with_overrides(overrides), form_type)
                    }
                    None => {
                        (
                            Conditions::default(),
                            VariantFormTs::default(),
                            ExperimentFormType::Default,
                        )
                    }
                };

                view! {
                    <crate::components::experiment_form_page::ExperimentFormPage
                        edit=false
                        context=context
                        variants=variants
                        experiment_form_type=experiment_form_type
                        dimensions=dimensions
                        default_config=default_config
                        redirect_url_cancel=redirect_url.get_value()
                        redirect_url_success=redirect_url.get_value()
                        metrics=workspace_settings.with_value(|w| w.metrics.clone())
                    />
                }
                    .into_view()
            }}
        </Suspense>
    }
}
