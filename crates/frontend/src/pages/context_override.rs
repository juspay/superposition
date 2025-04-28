use futures::join;
use leptos::*;
use leptos_router::use_navigate;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use strum::IntoEnumIterator;
use superposition_macros::box_params;
use superposition_types::{
    api::{
        context::{ContextListFilters, SortOn},
        default_config::DefaultConfigFilters,
    },
    custom_query::{CustomQuery, DimensionQuery, PaginationParams, Query, QueryMap},
    database::{
        models::cac::{Context, DefaultConfig},
        types::DimensionWithMandatory,
    },
    PaginatedResponse, SortBy,
};

use crate::api::{delete_context, fetch_context, fetch_default_config, fetch_dimensions};
use crate::components::{
    alert::AlertType,
    button::Button,
    context_card::ContextCard,
    context_form::{
        utils::{create_context, update_context},
        ContextForm,
    },
    delete_modal::DeleteModal,
    drawer::{close_drawer, open_drawer, Drawer, DrawerBtn, DrawerButtonStyle},
    dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
    experiment_form::ExperimentForm,
    override_form::OverrideForm,
    pagination::Pagination,
    skeleton::Skeleton,
    stat::Stat,
};
use crate::logic::{Condition, Conditions, Expression};
use crate::providers::{
    alert_provider::enqueue_alert,
    condition_collapse_provider::ConditionCollapseProvider,
    editor_provider::EditorProvider,
};
use crate::query_updater::{use_param_updater, use_signal_from_query};
use crate::types::{OrganisationId, Tenant, VariantFormTs};

#[derive(Clone, Debug, Default)]
pub struct Data {
    pub context: Conditions,
    pub overrides: Vec<(String, Value)>,
}

#[derive(Serialize, Deserialize, Clone, Debug, Default)]
struct PageResource {
    contexts: PaginatedResponse<Context>,
    dimensions: Vec<DimensionWithMandatory>,
    default_config: Vec<DefaultConfig>,
}

#[derive(Debug, Clone)]
enum FormMode {
    Edit,
    Create,
}

#[component]
fn context_filter_drawer(
    pagination_params_rws: RwSignal<PaginationParams>,
    context_filters_rws: RwSignal<ContextListFilters>,
    dimension_params_rws: RwSignal<DimensionQuery<QueryMap>>,
    dimensions: Vec<DimensionWithMandatory>,
) -> impl IntoView {
    let filters_buffer_rws = RwSignal::new(context_filters_rws.get_untracked());
    let dimension_buffer_rws = RwSignal::new(dimension_params_rws.get_untracked());
    let context = dimension_params_rws
        .get_untracked()
        .into_inner()
        .iter()
        .map(|(k, v)| Condition {
            variable: k.clone(),
            expression: Expression::Is(v.clone()),
        })
        .collect::<Conditions>();

    view! {
        <Drawer
            id="context_filter_drawer".to_string()
            header="Context Filters"
            drawer_width="w-[50vw]"
            handle_close=move || close_drawer("context_filter_drawer")
        >
            <div class="flex flex-col gap-4">
                <ContextForm
                    dimensions
                    context
                    dropdown_direction=DropdownDirection::Down
                    resolve_mode=true
                    handle_change=move |context: Conditions| {
                        let map = context
                            .iter()
                            .filter_map(|condition| {
                                match condition.expression.clone() {
                                    Expression::Is(value) => {
                                        Some((condition.variable.clone(), value))
                                    }
                                    _ => None,
                                }
                            })
                            .collect::<Map<_, _>>();
                        dimension_buffer_rws.set(DimensionQuery::from(map));
                    }
                    heading_sub_text="Search By Context".to_string()
                />
                <div class="form-control">
                    <label class="label">
                        <span class="label-text">{"Key Prefix (Seperate by Comma)"}</span>
                    </label>
                    <input
                        type="text"
                        id="context-key-prefix"
                        class="input input-bordered rounded-md resize-y w-full max-w-md"
                        placeholder="eg: key1,key2"
                        value=move || {
                            context_filters_rws.with(|f| f.prefix.clone().map(|d| d.to_string()))
                        }
                        on:change=move |event| {
                            let prefixes = event_target_value(&event);
                            let prefixes = (!prefixes.is_empty())
                                .then(|| serde_json::from_value(Value::String(prefixes)).ok())
                                .flatten();
                            filters_buffer_rws.update(|filter| filter.prefix = prefixes);
                        }
                    />
                </div>
                <div class="form-control">
                    <label class="label">
                        <span class="label-text">{"Created By (Seperate by Comma)"}</span>
                    </label>
                    <input
                        type="text"
                        id="context-creator-filter"
                        class="input input-bordered rounded-md resize-y w-full max-w-md"
                        placeholder="eg: user@superposition.io"
                        value=move || {
                            context_filters_rws
                                .with(|f| f.created_by.clone().map(|d| d.to_string()))
                        }
                        on:change=move |event| {
                            let user_names = event_target_value(&event);
                            let user_names = (!user_names.is_empty())
                                .then(|| serde_json::from_value(Value::String(user_names)).ok())
                                .flatten();
                            filters_buffer_rws.update(|filter| filter.created_by = user_names);
                        }
                    />
                </div>
                <div class="form-control">
                    <label class="label">
                        <span class="label-text">{"Last Modified By (Seperate by Comma)"}</span>
                    </label>
                    <input
                        type="text"
                        id="context-modifier-filter"
                        class="input input-bordered rounded-md resize-y w-full max-w-md"
                        placeholder="eg: user@superposition.io"
                        value=move || {
                            context_filters_rws
                                .with(|f| f.last_modified_by.clone().map(|d| d.to_string()))
                        }
                        on:change=move |event| {
                            let user_names = event_target_value(&event);
                            let user_names = (!user_names.is_empty())
                                .then(|| serde_json::from_value(Value::String(user_names)).ok())
                                .flatten();
                            filters_buffer_rws
                                .update(|filter| filter.last_modified_by = user_names);
                        }
                    />
                </div>
                <div class="form-control">
                    <label class="label">
                        <span class="label-text">{"Override (Plaintext search)"}</span>
                    </label>
                    {move || {
                        view! {
                            <textarea
                                id="context-plaintext-filter"
                                placeholder="Search overrides with plaintext"
                                class="textarea textarea-bordered w-full max-w-md"
                                on:change=move |event| {
                                    let plaintext = event_target_value(&event);
                                    let plaintext = (!plaintext.is_empty()).then_some(plaintext);
                                    filters_buffer_rws
                                        .update(|filter| filter.plaintext = plaintext);
                                }
                            >
                                {context_filters_rws
                                    .with(|f| f.plaintext.clone())
                                    .unwrap_or_default()}
                            </textarea>
                        }
                    }}
                </div>
                <div class="flex justify-end">
                    <Button
                        class="h-12 w-48".to_string()
                        text="Submit".to_string()
                        on_click=move |event| {
                            event.prevent_default();
                            context_filters_rws.set(filters_buffer_rws.get());
                            dimension_params_rws.set(dimension_buffer_rws.get());
                            pagination_params_rws.update(|f| f.page = None);
                            close_drawer("context_filter_drawer")
                        }
                    />
                    <Button
                        class="h-12 w-48".to_string()
                        text="Reset".to_string()
                        icon_class="ri-restart-line".into()
                        on_click=move |event| {
                            event.prevent_default();
                            context_filters_rws.set(ContextListFilters::default());
                            dimension_params_rws.set(DimensionQuery::default());
                            pagination_params_rws.update(|f| f.page = None);
                            close_drawer("context_filter_drawer")
                        }
                    />

                </div>
            </div>
        </Drawer>
    }
}

#[component]
fn form(
    context: Conditions,
    overrides: Vec<(String, Value)>,
    dimensions: Vec<DimensionWithMandatory>,
    edit: bool,
    default_config: Vec<DefaultConfig>,
    handle_submit: Callback<bool, ()>,
    #[prop(default = String::new())] description: String,
    #[prop(default = String::new())] change_reason: String,
) -> impl IntoView {
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();
    let (context, set_context) = create_signal(context);
    let (overrides, set_overrides) = create_signal(overrides);
    let dimensions = StoredValue::new(dimensions);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);

    let (description_rs, description_ws) = create_signal(description);
    let (change_reason_rs, change_reason_ws) = create_signal(change_reason);

    let on_submit = move |_| {
        req_inprogress_ws.set(true);
        spawn_local(async move {
            let f_context = context.get();
            let f_overrides = overrides.get();
            let result = if edit {
                update_context(
                    tenant_rws.get().0,
                    Map::from_iter(f_overrides),
                    f_context,
                    description_rs.get(),
                    change_reason_rs.get(),
                    org_rws.get().0,
                )
                .await
            } else {
                create_context(
                    tenant_rws.get().0,
                    Map::from_iter(f_overrides),
                    f_context,
                    description_rs.get(),
                    change_reason_rs.get(),
                    org_rws.get().0,
                )
                .await
            };

            match result {
                Ok(_) => {
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
            req_inprogress_ws.set(false);
        });
    };
    view! {
        <ContextForm
            dimensions=dimensions.get_value()
            context=context.get_untracked()
            handle_change=move |new_context| {
                set_context
                    .update(|value| {
                        *value = new_context;
                    });
            }

            disabled=edit
        />

        <div class="form-control">
            <label class="label">
                <span class="label-text">Description</span>
            </label>
            <textarea
                placeholder="Enter description"
                class="textarea textarea-bordered w-full max-w-md"
                value=description_rs.get_untracked()
                on:change=move |ev| {
                    let value = event_target_value(&ev);
                    description_ws.set(value);
                }
            />
        </div>

        <div class="form-control">
            <label class="label">
                <span class="label-text">Reason for Change</span>
            </label>
            <textarea
                placeholder="Enter a reason for this change"
                class="textarea textarea-bordered w-full max-w-md"
                value=change_reason_rs.get_untracked()
                on:change=move |ev| {
                    let value = event_target_value(&ev);
                    change_reason_ws.set(value);
                }
            />
        </div>

        <OverrideForm
            overrides=overrides.get_untracked()
            default_config=default_config
            handle_change=move |new_overrides| {
                set_overrides
                    .update(|value| {
                        *value = new_overrides;
                    });
            }
        />

        <div class="flex justify-start w-full mt-10">
            {move || {
                let loading = req_inprogess_rs.get();
                view! {
                    <Button
                        class="pl-[70px] pr-[70px] w-48 h-12".to_string()
                        text="Submit".to_string()
                        on_click=on_submit
                        loading
                    />
                }
            }}

        </div>
    }
}

#[component]
pub fn context_override() -> impl IntoView {
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();
    let (selected_context_rs, selected_context_ws) = create_signal::<Option<Data>>(None);
    let (form_mode, set_form_mode) = create_signal::<Option<FormMode>>(None);
    let (delete_modal, set_delete_modal) = create_signal(false);
    let (delete_id, set_delete_id) = create_signal::<Option<String>>(None);

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
                tenant_rws.get().0,
                org_rws.get().0,
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
                )
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
        selected_context_ws.set(Some(Data::default()));
        open_drawer("context_and_override_drawer");
    });

    let on_submit = Callback::new(move |edit: bool| {
        close_drawer("context_and_override_drawer");
        if !edit {
            context_filters_rws.set(ContextListFilters::default());
            dimension_params_rws.set(DimensionQuery::default());
            pagination_params_rws.update(|f| f.page = None);
        }
        set_form_mode.set(None);
        selected_context_ws.set(None);
        page_resource.refetch();
    });

    let on_context_edit = Callback::new(move |data: (Context, Map<String, Value>)| {
        let (context, overrides) = data;
        match Conditions::from_context_json(&context.value.into()) {
            Ok(conditions) => {
                selected_context_ws.set(Some(Data {
                    context: conditions,
                    overrides: overrides.into_iter().collect::<Vec<(String, Value)>>(),
                }));
                set_form_mode.set(Some(FormMode::Edit));
                open_drawer("context_and_override_drawer");
            }
            Err(e) => {
                logging::error!("Error parsing context: {}", e);
                enqueue_alert(e.to_string(), AlertType::Error, 5000);
            }
        };
    });
    let handle_submit_experiment_form = move |experiment_id: String| {
        page_resource.refetch();
        close_drawer("create_exp_drawer");

        let tenant = tenant_rws.get().0;
        let org = org_rws.get().0;
        let navigate = use_navigate();
        let redirect_url = format!("/admin/{org}/{tenant}/experiments/{experiment_id}");
        navigate(redirect_url.as_str(), Default::default())
    };

    let handle_create_experiment =
        Callback::new(move |data: (Context, Map<String, Value>)| {
            let (context, overrides) = data;

            match Conditions::from_context_json(&context.value.into()) {
                Ok(conditions) => {
                    selected_context_ws.set(Some(Data {
                        context: conditions,
                        overrides: overrides
                            .into_iter()
                            .collect::<Vec<(String, Value)>>(),
                    }));

                    open_drawer("create_exp_drawer");
                }
                Err(e) => {
                    logging::error!("Error parsing context: {}", e);
                    enqueue_alert(e.to_string(), AlertType::Error, 5000);
                }
            };
        });

    let on_context_clone = Callback::new(move |data: (Context, Map<String, Value>)| {
        let (context, overrides) = data;

        match Conditions::from_context_json(&context.value.into()) {
            Ok(conditions) => {
                selected_context_ws.set(Some(Data {
                    context: conditions,
                    overrides: overrides.into_iter().collect::<Vec<(String, Value)>>(),
                }));
                set_form_mode.set(Some(FormMode::Create));

                open_drawer("context_and_override_drawer");
            }
            Err(e) => {
                logging::error!("Error parsing context: {}", e);
                enqueue_alert(e.to_string(), AlertType::Error, 5000);
            }
        };
    });

    let on_context_delete = Callback::new(move |id: String| {
        set_delete_id.set(Some(id.clone()));
        set_delete_modal.set(true);
    });

    let handle_next_click = Callback::new(move |next_page: i64| {
        pagination_params_rws.update(|f| f.page = Some(next_page));
    });

    let handle_prev_click = Callback::new(move |prev_page: i64| {
        pagination_params_rws.update(|f| f.page = Some(prev_page));
    });

    let on_delete_confirm = Callback::new(move |_| {
        if let Some(id) = delete_id.get().clone() {
            spawn_local(async move {
                let result =
                    delete_context(tenant_rws.get().0, id, org_rws.get().0).await;

                match result {
                    Ok(_) => {
                        logging::log!("Context and overrides deleted successfully");
                        page_resource.refetch();
                    }
                    Err(e) => {
                        logging::log!("Error deleting context and overrides: {:?}", e);
                    }
                }
            });
        }
        set_delete_id.set(None);
        set_delete_modal.set(false);
    });

    let on_sort_on_select = Callback::new(move |sort_on: SortOn| {
        context_filters_rws.update(|filters| filters.sort_on = Some(sort_on));
    });

    let on_sort_by_select = Callback::new(move |sort_by: SortBy| {
        context_filters_rws.update(|filters| filters.sort_by = Some(sort_by));
    });

    view! {
        <Suspense fallback=move || view! { <Skeleton /> }>
            <div class="h-screen p-8 flex flex-col gap-8">
                <div class="flex flex-col gap-6 flex-1 overflow-y-scroll">
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
                                <div class="flex gap-2">
                                    <Stat
                                        heading="Overrides"
                                        icon="ri-guide-fill"
                                        number=total_items
                                    />
                                    <div class="w-max flex flex-col justify-end">
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
                                    <DrawerBtn
                                        drawer_id="context_filter_drawer".into()
                                        style=DrawerButtonStyle::Outline
                                        class="self-end".to_string()
                                    >
                                        "Filters"
                                        <i class="ri-filter-3-line"></i>
                                    </DrawerBtn>
                                </div>
                                <DrawerBtn
                                    class="self-end h-fit".to_string()
                                    drawer_id="context_and_override_drawer".to_string()
                                    on_click=on_create_context_click
                                >
                                    "Create Override"
                                    <i class="ri-edit-2-line ml-2"></i>
                                </DrawerBtn>
                            }
                        }}
                    </div>
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
                            next=handle_next_click
                            previous=handle_prev_click
                        />
                    }
                }}
            </div>
            {move || {
                let PageResource { dimensions, default_config, .. } = page_resource
                    .get()
                    .unwrap_or_default();
                let data = selected_context_rs.get();
                view! {
                    <Drawer
                        id="create_exp_drawer".to_string()
                        header="Create Experiment"
                        handle_close=move || {
                            close_drawer("create_exp_drawer");
                            selected_context_ws.set(None);
                        }
                    >
                        <EditorProvider>
                            {match data {
                                Some(data) => {
                                    view! {
                                        <ExperimentForm
                                            name="".to_string()
                                            context=data.context
                                            variants=VariantFormTs::default_with_overrides(
                                                data.overrides,
                                            )
                                            dimensions=dimensions
                                            default_config=default_config
                                            handle_submit=handle_submit_experiment_form
                                        />
                                    }
                                        .into_view()
                                }
                                None => view! {}.into_view(),
                            }}

                        </EditorProvider>
                    </Drawer>
                }
            }}
            {move || {
                let PageResource { dimensions, default_config, .. } = page_resource
                    .get()
                    .unwrap_or_default();
                let data = selected_context_rs.get();
                let drawer_header = match form_mode.get() {
                    Some(FormMode::Edit) => "Update Overrides",
                    Some(FormMode::Create) => "Create Overrides",
                    None => "",
                };
                view! {
                    <Drawer
                        id="context_and_override_drawer".to_string()
                        header=drawer_header
                        handle_close=move || {
                            close_drawer("context_and_override_drawer");
                            set_form_mode.set(None);
                            selected_context_ws.set(None);
                        }
                    >
                        <EditorProvider>
                            {match (form_mode.get_untracked(), data) {
                                (Some(FormMode::Edit), Some(data)) => {
                                    view! {
                                        <Form
                                            context=data.context
                                            overrides=data.overrides
                                            dimensions=dimensions
                                            default_config=default_config
                                            handle_submit=on_submit
                                            edit=true
                                        />
                                    }
                                        .into_view()
                                }
                                (Some(FormMode::Create), data) => {
                                    let Data { context, overrides } = data.unwrap_or_default();
                                    view! {
                                        <Form
                                            context=context
                                            overrides=overrides
                                            dimensions=dimensions
                                            default_config=default_config
                                            handle_submit=on_submit
                                            edit=false
                                        />
                                    }
                                        .into_view()
                                }
                                (Some(FormMode::Edit), None) => {
                                    enqueue_alert(
                                        String::from("Something went wrong, failed to load form"),
                                        AlertType::Error,
                                        5000,
                                    );
                                    view! {}.into_view()
                                }
                                (None, _) => view! {}.into_view(),
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
            <DeleteModal
                modal_visible=delete_modal
                confirm_delete=on_delete_confirm
                set_modal_visible=set_delete_modal
                header_text="Are you sure you want to delete this context? Action is irreversible."
                    .to_string()
            />
        </Suspense>
    }
}
