use std::{fmt::Display, ops::Deref, str::FromStr};

use chrono::{DateTime, Days, Duration, Utc};
use leptos::*;
use serde_json::{json, Map, Value};
use superposition_types::{
    api::{experiments::ExperimentListFilters, workspace::WorkspaceResponse},
    custom_query::{
        CommaSeparatedQParams, CustomQuery, DimensionQuery, PaginationParams, QueryMap,
    },
};
use web_sys::MouseEvent;

use crate::{
    components::{
        badge::{GlassyPills, GrayPill, ListPills},
        button::{Button, ButtonStyle},
        condition_pills::Condition,
        context_form::ContextForm,
        drawer::{close_drawer, Drawer, DrawerBtn},
        form::label::Label,
        input::{DateInput, Toggle},
    },
    logic::{Condition, Conditions, Expression},
    providers::condition_collapse_provider::ConditionCollapseProvider,
};

use super::CombinedResource;

#[component]
pub(super) fn filter_summary(
    filters_rws: RwSignal<ExperimentListFilters>,
    dimension_params_rws: RwSignal<DimensionQuery<QueryMap>>,
) -> impl IntoView {
    let workspace_settings = use_context::<StoredValue<WorkspaceResponse>>().unwrap();
    let force_open_rws = RwSignal::new(true);
    // let force_open_rws = RwSignal::new(scrolled_to_top.get_untracked());

    fn filter_index<T: Display + FromStr + Clone>(
        items: &Option<CommaSeparatedQParams<T>>,
        index: usize,
    ) -> Option<CommaSeparatedQParams<T>> {
        items.clone().and_then(|mut items| {
            items.0.remove(index);
            (!items.is_empty()).then_some(items)
        })
    }

    view! {
        <Show when=move || {
            let filters_empty = filters_rws
                .with(|f| {
                    f.created_by.is_none() && f.from_date.is_none() && f.to_date.is_none()
                        && f.status.is_none() && f.experiment_name.is_none()
                        && f.experiment_ids.is_none()
                });
            let dimension_params_empty = dimension_params_rws.with(|f| f.is_empty());
            !filters_empty || !dimension_params_empty
        }>
            <div class="flex gap-2">
                <div
                    class="h-max max-w-[1000px] pt-1 px-0.5 border-[1.5px] border-solid border-purple-400 rounded-[10px] cursor-pointer"
                    on:click=move |_| force_open_rws.update(|f| *f = !*f)
                >
                    <i class=move || {
                        format!(
                            "{} ri-xl text-purple-800",
                            if force_open_rws.get() {
                                "ri-filter-2-fill"
                            } else {
                                "ri-filter-2-line"
                            },
                        )
                    } />
                    <i class=move || {
                        format!(
                            "{} ri-xl text-gray-500",
                            if force_open_rws.get() {
                                "ri-arrow-up-s-line"
                            } else {
                                "ri-arrow-down-s-line"
                            },
                        )
                    } />

                </div>
                <div class=move || {
                    format!(
                        "{} flex flex-col gap-1 overflow-hidden",
                        if force_open_rws.get() { "max-h-[1000px]" } else { "max-h-0" },
                    )
                }>
                    {move || {
                        if filters_rws.with(|f| f.global_experiments_only.unwrap_or_default()) {
                            view! {
                                <div class="flex gap-2 items-center">
                                    <span class="text-xs">"Global Experiments Only"</span>
                                    <GrayPill
                                        text="Enabled"
                                        on_delete=move |_| {
                                            filters_rws.update(|f| f.global_experiments_only = None)
                                        }
                                    />
                                </div>
                            }
                                .into_view()
                        } else if !dimension_params_rws.with(|d| d.is_empty()) {
                            let dimension_params = dimension_params_rws.get();
                            let conditions = dimension_params
                                .clone()
                                .into_inner()
                                .iter()
                                .map(|(k, v)| Condition {
                                    variable: k.clone(),
                                    expression: Expression::Is(v.clone()),
                                })
                                .collect::<Conditions>();
                            let condition_id = serde_json::to_string(
                                    dimension_params.clone().into_inner().deref(),
                                )
                                .unwrap_or_else(|_| "[]".to_string());
                            view! {
                                <div class="flex justify-end gap-2">
                                    <div class="min-w-fit pt-1 text-xs">"Context"</div>
                                    <ConditionCollapseProvider>
                                        <Condition
                                            conditions
                                            id=condition_id
                                            grouped_view=false
                                            class="xl:w-[400px] h-fit"
                                            strict_mode=workspace_settings.with_value(|w| w.strict_mode)
                                        />
                                    </ConditionCollapseProvider>
                                </div>
                            }
                                .into_view()
                        } else {
                            ().into_view()
                        }
                    }}
                    {move || {
                        filters_rws
                            .with(|f| f.from_date)
                            .map(|from_date| {
                                view! {
                                    <div class="flex gap-2 items-center">
                                        <span class="text-xs">"Last Modified From"</span>
                                        <GrayPill
                                            text=from_date.format("%Y-%m-%d").to_string()
                                            on_delete=move |_| {
                                                filters_rws.update(|f| f.from_date = None);
                                            }
                                        />
                                    </div>
                                }
                            })
                    }}
                    {move || {
                        filters_rws
                            .with(|f| f.to_date)
                            .map(|to_date| {
                                view! {
                                    <div class="flex gap-2 items-center">
                                        <span class="text-xs">"Last Modified To"</span>
                                        <GrayPill
                                            text=to_date.format("%Y-%m-%d").to_string()
                                            on_delete=move |_| {
                                                filters_rws.update(|f| f.to_date = None);
                                            }
                                        />
                                    </div>
                                }
                            })
                    }}
                    {move || {
                        view! {
                            <ListPills
                                label="Status"
                                items=filters_rws
                                    .with(|f| {
                                        f.status.as_ref().map(|p| p.0.clone()).unwrap_or_default()
                                    })
                                on_delete=move |idx| {
                                    filters_rws.update(|f| f.status = filter_index(&f.status, idx))
                                }
                            />
                        }
                    }}
                    {move || {
                        filters_rws
                            .with(|f| f.experiment_name.clone())
                            .map(|experiment_name| {
                                view! {
                                    <div class="flex gap-2 items-center">
                                        <span class="text-xs">"Name"</span>
                                        <GrayPill
                                            text=experiment_name.clone()
                                            on_delete=move |_| {
                                                filters_rws.update(|f| f.experiment_name = None);
                                            }
                                        />
                                    </div>
                                }
                            })
                    }}
                    {move || {
                        view! {
                            <ListPills
                                label="Created by"
                                items=filters_rws
                                    .with(|f| {
                                        f.created_by
                                            .as_ref()
                                            .map(|p| p.0.clone())
                                            .unwrap_or_default()
                                    })
                                on_delete=move |idx| {
                                    filters_rws
                                        .update(|f| f.created_by = filter_index(&f.created_by, idx))
                                }
                            />
                        }
                    }}
                    {move || {
                        view! {
                            <ListPills
                                label="IDs"
                                items=filters_rws
                                    .with(|f| {
                                        f.experiment_ids
                                            .as_ref()
                                            .map(|p| p.0.clone())
                                            .unwrap_or_default()
                                    })
                                on_delete=move |idx| {
                                    filters_rws
                                        .update(|f| {
                                            f.experiment_ids = filter_index(&f.experiment_ids, idx);
                                        })
                                }
                            />
                        }
                    }}
                </div>
            </div>
        </Show>
    }
}

#[component]
pub(super) fn experiment_table_filter_widget(
    pagination_params_rws: RwSignal<PaginationParams>,
    filters_rws: RwSignal<ExperimentListFilters>,
    dimension_params_rws: RwSignal<DimensionQuery<QueryMap>>,
    combined_resource: CombinedResource,
) -> impl IntoView {
    let filters_buffer_rws = RwSignal::new(filters_rws.get_untracked());
    let dimension_buffer_rws = RwSignal::new(dimension_params_rws.get_untracked());
    let context_rws = RwSignal::new(
        dimension_params_rws
            .get_untracked()
            .into_inner()
            .iter()
            .map(|(k, v)| Condition {
                variable: k.clone(),
                expression: Expression::Is(v.clone()),
            })
            .collect::<Conditions>(),
    );

    let fn_environment = create_memo(move |_| {
        let context = context_rws.get();
        json!({
            "context": context,
            "overrides": [],
        })
    });

    view! {
        <DrawerBtn
            drawer_id="experiment_filter_drawer"
            class="!h-9 !min-h-[32px] !w-fit px-2"
            style=ButtonStyle::Outline
            text="Filters"
            icon_class="ri-filter-3-line"
        />
        <Drawer
            id="experiment_filter_drawer"
            header="Experiment Filters"
            width_class="max-w-[780px] min-w-[560px] w-[45vw]"
            handle_close=move || close_drawer("experiment_filter_drawer")
        >
            <div class="flex flex-col gap-5">
                {move || {
                    view! {
                        <ContextForm
                            dimensions=combined_resource.dimensions.clone()
                            context=context_rws.get_untracked()
                            on_context_change=move |context: Conditions| {
                                context_rws.set(context.clone());
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
                            heading_sub_text="Search By Context"
                            resolve_mode=true
                            fn_environment
                            disabled=filters_buffer_rws
                                .with(|f| f.global_experiments_only.unwrap_or_default())
                        />
                    }
                }} <div class="w-fit flex items-center gap-2">
                    <Toggle
                        name="workspace-strict-mode"
                        value=filters_buffer_rws
                            .with(|f| f.global_experiments_only.unwrap_or_default())
                        on_change=move |flag| {
                            filters_buffer_rws
                                .update(|f| {
                                    f.global_experiments_only = if flag {
                                        Some(true)
                                    } else {
                                        None
                                    };
                                });
                        }
                    />
                    <Label
                        title="Fetch global experiments only"
                        extra_info="Enabling this will disable context filter"
                    />
                </div> <div class="w-full flex flex-row justify-start items-end gap-10">
                    <div class="form-control">
                        <Label title="Last Modified From" />
                        <DateInput
                            id="experiment_from_date_input"
                            name="experiment_from_date"
                            min="2020-01-01"
                            value=filters_buffer_rws
                                .with(|f| f.from_date)
                                .map(|s| s.format("%Y-%m-%d").to_string())
                                .unwrap_or_default()
                            on_change=Callback::new(move |new_date: DateTime<Utc>| {
                                filters_buffer_rws.update(|f| f.from_date = Some(new_date));
                            })
                            on_clear=move |_| {
                                filters_buffer_rws.update(|f| f.from_date = None);
                            }
                        />
                    </div>
                    <i class="ri-arrow-right-line pb-2.5 text-3xl" />
                    <div class="form-control">
                        <Label title="Last Modified To" />
                        <DateInput
                            id="experiment_to_date_input"
                            name="experiment_to_date"
                            value=filters_buffer_rws
                                .with(|f| f.to_date)
                                .map(|s| s.format("%Y-%m-%d").to_string())
                                .unwrap_or_default()
                            on_change=Callback::new(move |new_date: DateTime<Utc>| {
                                filters_buffer_rws
                                    .update(|f| {
                                        f.to_date = Some(
                                            new_date + Days::new(1) - Duration::seconds(1),
                                        );
                                    });
                            })
                            on_clear=move |_| {
                                filters_buffer_rws.update(|f| f.to_date = None);
                            }
                        />
                    </div>
                </div>
                <GlassyPills
                    selected=Signal::derive(move || {
                        filters_buffer_rws
                            .with(|f| f.status.clone().map(|p| p.0).unwrap_or_default())
                    })
                    title="Experiment Status"
                    on_click=move |items| {
                        filters_buffer_rws.update(|f| f.status = Some(CommaSeparatedQParams(items)))
                    }
                /> <div class="form-control">
                    <Label title="Experiment Name" />
                    <input
                        type="text"
                        id="experiment-name-filter"
                        placeholder="eg: city experiment"
                        class="input input-bordered rounded-md resize-y w-full max-w-md"
                        value=move || filters_buffer_rws.with(|f| f.experiment_name.clone())
                        on:change=move |event| {
                            let experiment_name = event_target_value(&event);
                            let experiment_name = if experiment_name.is_empty() {
                                None
                            } else {
                                Some(experiment_name)
                            };
                            filters_buffer_rws.update(|f| f.experiment_name = experiment_name);
                        }
                    />
                </div> <div class="form-control">
                    <Label
                        title="Experiment ID"
                        info="(any of)"
                        description="Separate each ID by a comma"
                    />
                    <input
                        type="text"
                        id="experiment-id-filter"
                        class="input input-bordered rounded-md resize-y w-full max-w-md"
                        value=move || {
                            filters_buffer_rws
                                .with(|f| f.experiment_ids.clone().map(|d| d.to_string()))
                        }
                        placeholder="eg: 7259558160762015744"
                        on:change=move |event| {
                            let ids = event_target_value(&event);
                            let ids = (!ids.is_empty())
                                .then(|| serde_json::from_value(Value::String(ids)).ok())
                                .flatten();
                            filters_buffer_rws.update(|filter| filter.experiment_ids = ids);
                        }
                    />
                </div> <div class="form-control">
                    <Label
                        title="Created By"
                        info="(any of)"
                        description="Separate each user by a comma"
                    />
                    <input
                        type="text"
                        id="experiment-user-filter"
                        class="input input-bordered rounded-md resize-y w-full max-w-md"
                        placeholder="eg: user@superposition.io"
                        value=move || filters_buffer_rws.get().created_by.map(|d| d.to_string())
                        on:change=move |event| {
                            let user_names = event_target_value(&event);
                            let user_names = (!user_names.is_empty())
                                .then(|| serde_json::from_value(Value::String(user_names)).ok())
                                .flatten();
                            filters_buffer_rws.update(|filter| filter.created_by = user_names);
                        }
                    />
                </div> <div class="flex justify-end gap-2">
                    <Button
                        class="h-12 w-48"
                        text="Submit"
                        icon_class="ri-send-plane-line"
                        on_click=move |event: MouseEvent| {
                            event.prevent_default();
                            let filters = filters_buffer_rws.get();
                            let dimension_params = dimension_buffer_rws.get();
                            batch(|| {
                                filters_rws.set(filters);
                                dimension_params_rws.set(dimension_params);
                                pagination_params_rws.update(|f| f.reset_page());
                            });
                            close_drawer("experiment_filter_drawer")
                        }
                    />
                    <Button
                        class="h-12 w-48"
                        text="Reset"
                        icon_class="ri-restart-line"
                        on_click=move |event: MouseEvent| {
                            event.prevent_default();
                            batch(|| {
                                filters_rws.set(ExperimentListFilters::default());
                                dimension_params_rws.set(DimensionQuery::default());
                                pagination_params_rws.update(|f| f.reset_page());
                            });
                            close_drawer("experiment_filter_drawer")
                        }
                    />
                </div>
            </div>
        </Drawer>
    }
}
