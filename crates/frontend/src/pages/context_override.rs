pub mod filter;
pub mod utils;

use std::ops::Deref;

use filter::{ContextFilterDrawer, ContextFilterSummary};
use futures::join;
use leptos::*;
use leptos_router::use_navigate;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use strum::IntoEnumIterator;
use superposition_macros::box_params;
use superposition_types::{
    api::{
        context::{ContextListFilters, SortOn, UpdateRequest},
        default_config::DefaultConfigFilters,
        dimension::DimensionResponse,
        workspace::WorkspaceResponse,
    },
    custom_query::{CustomQuery, DimensionQuery, PaginationParams, Query, QueryMap},
    database::models::{
        cac::{Context, DefaultConfig},
        experimentation::ExperimentType,
    },
    PaginatedResponse, SortBy,
};
use utils::{create_context, try_update_context_payload, update_context};
use wasm_bindgen::JsCast;
use web_sys::{Element, Event};

use crate::{
    api::{
        delete_context, fetch_context, fetch_default_config, fetch_dimensions,
        get_context,
    },
    components::{
        alert::AlertType,
        button::{Button, ButtonStyle},
        change_form::ChangeForm,
        change_summary::{ChangeLogPopup, ChangeSummary},
        context_card::ContextCard,
        context_form::ContextForm,
        drawer::{close_drawer, open_drawer, Drawer, DrawerBtn},
        dropdown::{Dropdown, DropdownBtnType},
        experiment_form::{ExperimentForm, ExperimentFormType},
        override_form::OverrideForm,
        pagination::Pagination,
        skeleton::{Skeleton, SkeletonVariant},
        stat::Stat,
    },
    logic::Conditions,
    providers::{
        alert_provider::enqueue_alert,
        condition_collapse_provider::ConditionCollapseProvider,
        editor_provider::EditorProvider,
    },
    query_updater::{use_param_updater, use_signal_from_query},
    types::{OrganisationId, Tenant, VariantFormTs},
};

#[derive(Clone, Debug, Default)]
pub struct Data {
    pub context_id: String,
    pub context: Conditions,
    pub overrides: Vec<(String, Value)>,
    pub description: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, Default)]
struct PageResource {
    contexts: PaginatedResponse<Context>,
    dimensions: Vec<DimensionResponse>,
    default_config: Vec<DefaultConfig>,
}

#[derive(Debug, Clone)]
enum FormMode {
    Edit(String),
    Clone(String),
    Create,
    Experiment(ExperimentType, String),
    Delete(String),
}

enum ResponseType {
    Response,
    UpdatePrecheck,
}

#[component]
fn form(
    context: Conditions,
    overrides: Vec<(String, Value)>,
    dimensions: Vec<DimensionResponse>,
    #[prop(optional)] edit_id: Option<String>,
    default_config: Vec<DefaultConfig>,
    #[prop(into)] handle_submit: Callback<bool, ()>,
    #[prop(default = String::new())] description: String,
) -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let (context_rs, context_ws) = create_signal(context);
    let (overrides_rs, overrides_ws) = create_signal(overrides);
    let dimensions = StoredValue::new(dimensions);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);
    let edit_id = StoredValue::new(edit_id);

    let (description_rs, description_ws) = create_signal(description);
    let (change_reason_rs, change_reason_ws) = create_signal(String::new());
    let update_request_rws = RwSignal::new(None);

    let fn_environment = create_memo(move |_| {
        let context = context_rs.get();
        let overrides = overrides_rs.get();
        json!({
            "context": context,
            "overrides": overrides,
        })
    });

    let on_submit = move || {
        req_inprogress_ws.set(true);
        spawn_local(async move {
            let f_overrides = overrides_rs.get_untracked();
            let result = match (edit_id.get_value(), update_request_rws.get_untracked()) {
                (Some(_), Some((_, payload))) => {
                    let future = update_context(
                        payload,
                        workspace.get_untracked().0,
                        org.get_untracked().0,
                    );
                    update_request_rws.set(None);
                    future.await.map(|_| ResponseType::Response)
                }
                (Some(context_id), None) => {
                    let request_payload = try_update_context_payload(
                        context_id.clone(),
                        Map::from_iter(f_overrides),
                        description_rs.get_untracked(),
                        change_reason_rs.get_untracked(),
                    );
                    match request_payload {
                        Ok(payload) => {
                            update_request_rws.set(Some((context_id, payload)));
                            Ok(ResponseType::UpdatePrecheck)
                        }
                        Err(e) => Err(e),
                    }
                }
                _ => create_context(
                    workspace.get_untracked().0,
                    Map::from_iter(f_overrides),
                    context_rs.get_untracked(),
                    description_rs.get_untracked(),
                    change_reason_rs.get_untracked(),
                    org.get_untracked().0,
                )
                .await
                .map(|_| ResponseType::Response),
            };

            let edit = edit_id.get_value().is_some();

            req_inprogress_ws.set(false);
            match result {
                Ok(ResponseType::UpdatePrecheck) => (),
                Ok(ResponseType::Response) => {
                    logging::log!("Context and overrides submitted successfully");
                    handle_submit.call(edit);
                    let success_message = if edit {
                        "Context and overrides updated successfully!"
                    } else {
                        "Context and overrides created successfully!"
                    };
                    enqueue_alert(
                        String::from(success_message),
                        AlertType::Success,
                        5000,
                    );
                }
                Err(e) => {
                    logging::log!("Error submitting context and overrides: {:?}", e);
                    enqueue_alert(e, AlertType::Error, 5000);
                }
            }
        });
    };

    view! {
        <div class="flex flex-col gap-5">
            <ContextForm
                dimensions=dimensions.get_value()
                context=context_rs.get_untracked()
                on_context_change=move |new_context| context_ws.set(new_context)
                fn_environment
                disabled=edit_id.get_value().is_some()
            />

            <ChangeForm
                title="Description".to_string()
                placeholder="Enter a description".to_string()
                value=description_rs.get_untracked()
                on_change=move |new_description| description_ws.set(new_description)
            />
            <ChangeForm
                title="Reason for Change".to_string()
                placeholder="Enter a reason for this change".to_string()
                value=change_reason_rs.get_untracked()
                on_change=move |new_change_reason| change_reason_ws.set(new_change_reason)
            />

            <OverrideForm
                overrides=overrides_rs.get_untracked()
                default_config=default_config
                handle_change=move |new_overrides| overrides_ws.set(new_overrides)
                fn_environment
            />

            {move || {
                let loading = req_inprogess_rs.get();
                view! {
                    <Button
                        class="self-end h-12 w-48"
                        text="Submit"
                        icon_class="ri-send-plane-line"
                        on_click=move |_| on_submit()
                        loading
                    />
                }
            }}
        </div>
        {move || match update_request_rws.get() {
            None => ().into_view(),
            Some((context_id, update_request)) => {
                view! {
                    <ChangeLogSummary
                        context_id
                        change_type=ChangeType::Update(update_request)
                        on_confirm=move |_| on_submit()
                        on_close=move |_| update_request_rws.set(None)
                    />
                }
            }
        }}
    }
}

#[allow(clippy::type_complexity)]
fn use_context_data(
    context_id: String,
) -> Resource<(String, String, String), Result<(Context, Conditions), String>> {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    create_local_resource(
        move || (workspace.get().0, org.get().0, context_id.clone()),
        |(tenant, org, context_id)| async move {
            get_context(&context_id, &tenant, &org)
                .await
                .and_then(|context| {
                    Conditions::from_context_json(&context.value)
                        .map(|condition| (context, condition))
                        .map_err(String::from)
                })
        },
    )
}

#[component]
fn autofill_form(
    context_id: String,
    #[prop(into)] handle_submit: Callback<bool, ()>,
    dimensions: Vec<DimensionResponse>,
    default_config: Vec<DefaultConfig>,
    #[prop(default = false)] edit: bool,
) -> impl IntoView {
    let default_config = StoredValue::new(default_config);
    let dimensions = StoredValue::new(dimensions);

    let context_data = use_context_data(context_id);

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::Block /> }
        }>
            {move || {
                match context_data.get() {
                    Some(Ok((context, condition))) => {
                        let overrides = context.override_.into_iter().collect::<Vec<_>>();
                        if edit {
                            view! {
                                <Form
                                    context=condition
                                    overrides=overrides
                                    default_config=default_config.get_value()
                                    dimensions=dimensions.get_value()
                                    edit_id=context.id
                                    handle_submit
                                    description=context.description.deref().to_string()
                                />
                            }
                        } else {
                            view! {
                                <Form
                                    context=condition
                                    overrides=overrides
                                    default_config=default_config.get_value()
                                    dimensions=dimensions.get_value()
                                    handle_submit
                                />
                            }
                        }
                            .into_view()
                    }
                    Some(Err(e)) => {
                        logging::error!("Error fetching context: {}", e);
                        view! { <div>Error fetching context</div> }.into_view()
                    }
                    None => view! { <div>Loading...</div> }.into_view(),
                }
            }}
        </Suspense>
    }
}

#[component]
fn autofill_experiment_form(
    context_id: String,
    #[prop(into)] handle_submit: Callback<String, ()>,
    dimensions: Vec<DimensionResponse>,
    default_config: Vec<DefaultConfig>,
    experiment_type: ExperimentType,
) -> impl IntoView {
    let workspace_settings = use_context::<StoredValue<WorkspaceResponse>>().unwrap();
    let default_config = StoredValue::new(default_config);
    let dimensions = StoredValue::new(dimensions);

    let context_data = use_context_data(context_id);

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::Block /> }
        }>
            {move || {
                match context_data.get() {
                    Some(Ok((context, condition))) => {
                        match experiment_type {
                            ExperimentType::Default => {
                                let overrides = context.override_.into_iter().collect::<Vec<_>>();
                                view! {
                                    <ExperimentForm
                                        context=condition
                                        variants=VariantFormTs::default_with_overrides(overrides)
                                        default_config=default_config.get_value()
                                        dimensions=dimensions.get_value()
                                        handle_submit
                                        metrics=workspace_settings.with_value(|w| w.metrics.clone())
                                    />
                                }
                            }
                            ExperimentType::DeleteOverrides => {
                                view! {
                                    <ExperimentForm
                                        experiment_form_type=ExperimentFormType::Delete(
                                            Some((context.id, context.override_.into())),
                                        )
                                        context=condition
                                        default_config=default_config.get_value()
                                        dimensions=dimensions.get_value()
                                        handle_submit
                                        metrics=workspace_settings.with_value(|w| w.metrics.clone())
                                    />
                                }
                            }
                        }
                            .into_view()
                    }
                    Some(Err(e)) => {
                        logging::error!("Error fetching context: {}", e);
                        view! { <div>Error fetching context</div> }.into_view()
                    }
                    None => view! { <div>Loading...</div> }.into_view(),
                }
            }}
        </Suspense>
    }
}

#[component]
pub fn context_override() -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let (form_mode, set_form_mode) = create_signal::<Option<FormMode>>(None);
    let delete_inprogress_rws = RwSignal::new(false);
    let scrolled_to_top_rws = RwSignal::new(false);

    let pagination_params_rws = use_signal_from_query(move |query_string| {
        Query::<PaginationParams>::extract_non_empty(&query_string).into_inner()
    });
    let context_filters_rws = use_signal_from_query(move |query_string| {
        Query::<ContextListFilters>::extract_non_empty(&query_string).into_inner()
    });
    let dimension_params_rws = use_signal_from_query(move |query_string| {
        DimensionQuery::<QueryMap>::extract_non_empty(&query_string)
    });

    use_param_updater(move || {
        box_params![
            context_filters_rws.get(),
            pagination_params_rws.get(),
            dimension_params_rws.get(),
        ]
    });

    let page_resource: Resource<
        (
            String,
            String,
            PaginationParams,
            ContextListFilters,
            DimensionQuery<QueryMap>,
        ),
        PageResource,
    > = create_blocking_resource(
        move || {
            (
                workspace.get().0,
                org.get().0,
                pagination_params_rws.get(),
                context_filters_rws.get(),
                dimension_params_rws.get(),
            )
        },
        |(
            current_tenant,
            org_id,
            pagination_params,
            context_filters,
            dimension_params,
        )| async move {
            let empty_list_filters = PaginationParams::all_entries();
            let default_config_filters = DefaultConfigFilters::default();
            let (contexts_result, dimensions_result, default_config_result) = join!(
                fetch_context(
                    current_tenant.to_string(),
                    org_id.clone(),
                    &pagination_params,
                    &context_filters,
                    &dimension_params
                ),
                fetch_dimensions(
                    &empty_list_filters,
                    current_tenant.to_string(),
                    org_id.clone()
                ),
                fetch_default_config(
                    &empty_list_filters,
                    &default_config_filters,
                    current_tenant.to_string(),
                    org_id.clone()
                ),
            );
            PageResource {
                contexts: contexts_result.unwrap_or_default(),
                dimensions: dimensions_result
                    .unwrap_or_default()
                    .data
                    .into_iter()
                    .filter(|d| d.dimension != "variantIds")
                    .collect(),
                default_config: default_config_result.unwrap_or_default().data,
            }
        },
    );

    let on_create_context_click = Callback::new(move |_| {
        set_form_mode.set(Some(FormMode::Create));
        open_drawer("context_and_override_drawer");
    });

    let on_submit = move |edit: bool| {
        close_drawer("context_and_override_drawer");
        if !edit {
            context_filters_rws.set(ContextListFilters::default());
            dimension_params_rws.set(DimensionQuery::default());
            pagination_params_rws.update(|f| f.reset_page());
        }
        set_form_mode.set(None);
        page_resource.refetch();
    };

    let on_context_edit = move |context_id| {
        set_form_mode.set(Some(FormMode::Edit(context_id)));
        open_drawer("context_and_override_drawer");
    };

    let handle_submit_experiment_form = move |experiment_id: String| {
        page_resource.refetch();
        close_drawer("create_exp_drawer");

        let tenant = workspace.get().0;
        let org = org.get().0;
        let navigate = use_navigate();
        let redirect_url = format!("/admin/{org}/{tenant}/experiments/{experiment_id}");
        navigate(redirect_url.as_str(), Default::default())
    };

    let handle_create_experiment = move |context_id| {
        set_form_mode.set(Some(FormMode::Experiment(
            ExperimentType::Default,
            context_id,
        )));
        open_drawer("context_and_override_drawer");
    };

    let handle_delete_experiment = move |context_id| {
        set_form_mode.set(Some(FormMode::Experiment(
            ExperimentType::DeleteOverrides,
            context_id,
        )));
        open_drawer("context_and_override_drawer");
    };

    let on_context_clone = move |context_id| {
        set_form_mode.set(Some(FormMode::Clone(context_id)));
        open_drawer("context_and_override_drawer");
    };

    let on_context_delete = move |context_id| {
        set_form_mode.set(Some(FormMode::Delete(context_id)));
    };

    let handle_page_change = move |page: i64| {
        pagination_params_rws.update(|f| f.page = Some(page));
    };

    let on_delete_confirm = move |context_id| {
        delete_inprogress_rws.set(true);
        spawn_local(async move {
            let result = delete_context(
                context_id,
                &workspace.get_untracked(),
                &org.get_untracked(),
            )
            .await;
            delete_inprogress_rws.set(false);
            match result {
                Ok(_) => {
                    set_form_mode.set(None);
                    logging::log!("Context and overrides deleted successfully");
                    page_resource.refetch();
                }
                Err(e) => {
                    logging::log!("Error deleting context and overrides: {:?}", e);
                }
            }
        });
    };

    let on_sort_on_select = Callback::new(move |sort_on: SortOn| {
        context_filters_rws.update(|filters| filters.sort_on = Some(sort_on));
    });

    let on_sort_by_select = Callback::new(move |sort_by: SortBy| {
        context_filters_rws.update(|filters| filters.sort_by = Some(sort_by));
    });

    let filter_node_ref = create_node_ref::<html::Div>();
    let on_scroll = move |ev: Event| {
        let filter_element_top = filter_node_ref.get().map(|ele| {
            let element: &Element = (**ele).unchecked_ref();
            element.get_bounding_client_rect().top()
        });

        let scroll_element_top = ev
            .target()
            .and_then(|e| e.dyn_into::<Element>().ok())
            .map(|e| e.get_bounding_client_rect().top());

        if let Some(scroll_top) = scroll_element_top {
            scrolled_to_top_rws.set(
                filter_element_top
                    .map(|p| p <= scroll_top)
                    .unwrap_or_default(),
            );
        }
    };

    view! {
        <Suspense fallback=move || view! { <Skeleton /> }>
            <div class="relative h-full flex flex-col gap-8">
                <div class="flex flex-col gap-6 flex-1 overflow-y-scroll" on:scroll=on_scroll>
                    <div class="flex justify-between">
                        {move || {
                            let total_items = page_resource
                                .get()
                                .map(|v| v.contexts.total_items)
                                .unwrap_or_default()
                                .to_string();
                            let context_filters = context_filters_rws.get();
                            let selected_sort_by = context_filters.sort_by.unwrap_or_default();
                            let sort_by_icon = match selected_sort_by {
                                SortBy::Desc => "ri-sort-desc",
                                SortBy::Asc => "ri-sort-asc",
                            };
                            view! {
                                <div class="flex items-center gap-2">
                                    <Stat
                                        heading="Overrides"
                                        icon="ri-guide-fill"
                                        number=total_items
                                    />
                                    <div class="w-max flex flex-col justify-center">
                                        <Dropdown
                                            class="!w-fit !h-fit".to_string()
                                            dropdown_width="w-max".to_string()
                                            dropdown_options=SortOn::iter().collect()
                                            dropdown_text=format!(
                                                "Sort On: {}",
                                                context_filters.sort_on.unwrap_or_default().label(),
                                            )
                                            dropdown_icon="ri-arrow-up-down-line"
                                            dropdown_btn_type=DropdownBtnType::Link
                                            on_select=on_sort_on_select
                                            searchable=false
                                        />
                                        <Dropdown
                                            class="!w-fit !h-fit".to_string()
                                            dropdown_width="w-max".to_string()
                                            dropdown_options=SortBy::iter().collect()
                                            dropdown_text=format!(
                                                "Sort By: {}",
                                                selected_sort_by.label(),
                                            )
                                            dropdown_icon=sort_by_icon
                                            dropdown_btn_type=DropdownBtnType::Link
                                            on_select=on_sort_by_select
                                            searchable=false
                                        />
                                    </div>
                                </div>
                                <div class="flex items-end gap-4">
                                    <DrawerBtn
                                        drawer_id="context_filter_drawer"
                                        style=ButtonStyle::Outline
                                        class="!h-9 !min-h-[32px] !w-fit px-2"
                                        text="Filters"
                                        icon_class="ri-filter-3-line"
                                    />
                                    <DrawerBtn
                                        class="h-fit"
                                        drawer_id="context_and_override_drawer"
                                        on_click=on_create_context_click
                                        text="Create Override"
                                        icon_class="ri-add-line"
                                    />
                                </div>
                            }
                        }}
                    </div>
                    <ContextFilterSummary
                        context_filters_rws
                        dimension_params_rws
                        scrolled_to_top=scrolled_to_top_rws
                        filter_node_ref
                    />
                    {move || {
                        match page_resource.get().map(|v| v.contexts) {
                            Some(contexts) => {
                                let is_empty = contexts.data.is_empty();
                                view! {
                                    <div class="flex flex-col gap-6">
                                        <Show when=move || is_empty>
                                            <div class="flex flex-col gap-4 justify-center items-center">
                                                <div class="flex justify-center text-gray-400">
                                                    <i class="ri-file-add-line ri-xl"></i>
                                                </div>
                                                <div class="flex font-semibold items-center text-gray-400 text-xl justify-center">
                                                    "Start with creating an override"
                                                </div>
                                            </div>
                                        </Show>
                                        <ConditionCollapseProvider>
                                            {contexts
                                                .data
                                                .into_iter()
                                                .map(|context| {
                                                    view! {
                                                        <ContextCard
                                                            context=context.clone()
                                                            overrides=context.override_.into()
                                                            handle_create_experiment=handle_create_experiment
                                                            handle_edit=on_context_edit
                                                            handle_clone=on_context_clone
                                                            handle_delete=on_context_delete
                                                            handle_delete_experiment
                                                        />
                                                    }
                                                })
                                                .collect_view()}
                                        </ConditionCollapseProvider>
                                    </div>
                                }
                                    .into_view()
                            }
                            None => view! { <div>Loading....</div> }.into_view(),
                        }
                    }}
                </div>
                {move || {
                    view! {
                        <Pagination
                            class="self-end".to_string()
                            current_page=pagination_params_rws.get().page.unwrap_or_default()
                            total_pages=page_resource
                                .get()
                                .map(|d| d.contexts)
                                .unwrap_or_default()
                                .total_pages
                            on_change=handle_page_change
                        />
                    }
                }}
            </div>
            {move || {
                let PageResource { dimensions, default_config, .. } = page_resource
                    .get()
                    .unwrap_or_default();
                if let Some(FormMode::Delete(context_id)) = form_mode.get() {
                    return view! {
                        <ChangeLogSummary
                            context_id=context_id.clone()
                            change_type=ChangeType::Delete
                            on_confirm=move |_| on_delete_confirm(context_id.clone())
                            on_close=move |_| set_form_mode.set(None)
                            inprogress=delete_inprogress_rws
                        />
                    }
                        .into_view();
                }
                let drawer_header = match form_mode.get() {
                    Some(FormMode::Edit(_)) => "Update Overrides",
                    Some(FormMode::Create) | Some(FormMode::Clone(_)) => "Create Overrides",
                    Some(FormMode::Experiment(ExperimentType::Default, _)) => {
                        "Update Override via Experiment"
                    }
                    Some(FormMode::Experiment(ExperimentType::DeleteOverrides, _)) => {
                        "Delete Override via Experiment"
                    }
                    Some(FormMode::Delete(_)) | None => "",
                };
                view! {
                    <Drawer
                        id="context_and_override_drawer"
                        header=drawer_header
                        width_class="max-w-[780px] min-w-[680px] w-[45vw]"
                        handle_close=move || {
                            close_drawer("context_and_override_drawer");
                            set_form_mode.set(None);
                        }
                    >
                        <EditorProvider>
                            {match form_mode.get_untracked() {
                                Some(FormMode::Edit(context_id)) => {
                                    view! {
                                        <AutofillForm
                                            context_id
                                            dimensions=dimensions
                                            default_config=default_config
                                            handle_submit=on_submit
                                            edit=true
                                        />
                                    }
                                        .into_view()
                                }
                                Some(FormMode::Clone(context_id)) => {
                                    view! {
                                        <AutofillForm
                                            context_id
                                            dimensions
                                            default_config
                                            handle_submit=on_submit
                                        />
                                    }
                                        .into_view()
                                }
                                Some(FormMode::Create) => {
                                    view! {
                                        <Form
                                            context=Conditions(vec![])
                                            overrides=vec![]
                                            dimensions
                                            default_config
                                            handle_submit=on_submit
                                        />
                                    }
                                        .into_view()
                                }
                                Some(FormMode::Experiment(experiment_type, context_id)) => {
                                    view! {
                                        <AutofillExperimentForm
                                            experiment_type
                                            context_id
                                            dimensions
                                            default_config
                                            handle_submit=handle_submit_experiment_form
                                        />
                                    }
                                        .into_view()
                                }
                                Some(FormMode::Delete(_)) | None => ().into_view(),
                            }}
                        </EditorProvider>
                    </Drawer>
                }
            }}
            {move || {
                view! {
                    <ContextFilterDrawer
                        dimensions=page_resource.get().map(|r| r.dimensions).unwrap_or_default()
                        pagination_params_rws
                        context_filters_rws
                        dimension_params_rws
                    />
                }
            }}
        </Suspense>
    }
}

#[derive(Clone)]
enum ChangeType {
    Delete,
    Update(UpdateRequest),
}

#[component]
fn change_log_summary(
    context_id: String,
    change_type: ChangeType,
    #[prop(into)] on_confirm: Callback<()>,
    #[prop(into)] on_close: Callback<()>,
    #[prop(into, default = Signal::derive(|| false))] inprogress: Signal<bool>,
) -> impl IntoView {
    let context_data = use_context_data(context_id.clone());

    let disabled_rws = RwSignal::new(true);
    let change_type = StoredValue::new(change_type);

    let (title, description, confirm_text) = match change_type.get_value() {
        ChangeType::Update(_) => (
            "Confirm Update",
            "Are you sure you want to update this context?",
            "Yes, Update",
        ),
        ChangeType::Delete => (
            "Confirm Delete",
            "Are you sure you want to delete this context? Action is irreversible.",
            "Yes, Delete",
        ),
    };

    view! {
        <ChangeLogPopup
            title
            description
            confirm_text
            on_confirm
            on_close
            disabled=disabled_rws
            inprogress
        >
            <Suspense fallback=move || {
                view! { <Skeleton variant=SkeletonVariant::Block style_class="h-10".to_string() /> }
            }>
                {
                    Effect::new(move |_| {
                        let context = context_data.get();
                        if let Some(Ok(_)) = context {
                            disabled_rws.set(false);
                        } else if let Some(Err(e)) = context {
                            logging::error!("Error fetching context: {}", e);
                        }
                    });
                }
                {move || match context_data.get() {
                    Some(Ok((context, _))) => {
                        let (new_overrides, title, description) = match change_type.get_value() {
                            ChangeType::Update(update_request) => {
                                (
                                    update_request.override_.clone().into_inner().into(),
                                    "Override changes",
                                    update_request
                                        .description
                                        .unwrap_or_else(|| context.description.clone()),
                                )
                            }
                            ChangeType::Delete => {
                                (Map::new(), "Overrides to be deleted", context.description.clone())
                            }
                        };
                        view! {
                            <ChangeSummary
                                title
                                old_values=context.override_.clone().into()
                                new_values=new_overrides
                            />
                            <ChangeSummary
                                title="Other changes"
                                key_column="Property"
                                old_values=Map::from_iter(
                                    vec![
                                        (
                                            "Description".to_string(),
                                            Value::String(context.description.deref().to_string()),
                                        ),
                                    ],
                                )
                                new_values=Map::from_iter(
                                    vec![
                                        (
                                            "Description".to_string(),
                                            Value::String(description.deref().to_string()),
                                        ),
                                    ],
                                )
                            />
                        }
                            .into_view()
                    }
                    Some(Err(e)) => {
                        logging::error!("Error fetching context: {}", e);
                        view! { <div>Error fetching context</div> }.into_view()
                    }
                    None => view! { <div>Loading...</div> }.into_view(),
                }}
            </Suspense>
        </ChangeLogPopup>
    }
}
