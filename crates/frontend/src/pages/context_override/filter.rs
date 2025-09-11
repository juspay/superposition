use std::ops::Deref;

use leptos::*;
use serde_json::{json, Map, Value};
use superposition_types::{
    api::{
        context::ContextListFilters, dimension::DimensionResponse,
        workspace::WorkspaceResponse, DimensionMatchStrategy,
    },
    custom_query::{
        CommaSeparatedStringQParams, CustomQuery, DimensionQuery, PaginationParams,
        QueryMap,
    },
};
use web_sys::MouseEvent;

use crate::{
    components::{
        badge::{GrayPill, ListPills},
        button::Button,
        condition_pills::Condition,
        context_form::ContextForm,
        drawer::{close_drawer, Drawer},
        form::label::Label,
        input::Toggle,
    },
    logic::{Condition, Conditions, Expression},
    providers::condition_collapse_provider::ConditionCollapseProvider,
};

#[component]
pub fn context_filter_summary(
    #[prop(into)] scrolled_to_top: Signal<bool>,
    context_filters_rws: RwSignal<ContextListFilters>,
    dimension_params_rws: RwSignal<DimensionQuery<QueryMap>>,
    filter_node_ref: NodeRef<html::Div>,
) -> impl IntoView {
    let workspace_settings = use_context::<StoredValue<WorkspaceResponse>>().unwrap();
    let force_open_rws = RwSignal::new(true);
    // let force_open_rws = RwSignal::new(scrolled_to_top.get_untracked());

    let summary_expanded =
        Signal::derive(move || force_open_rws.get() || !scrolled_to_top.get());

    let filter_index = |items: &Option<CommaSeparatedStringQParams>, index| {
        items.clone().and_then(|mut items| {
            items.0.remove(index);
            (!items.is_empty()).then_some(items)
        })
    };

    view! {
        <Show when=move || {
            let context_filters_empty = context_filters_rws
                .with(|f| {
                    f.created_by.is_none() && f.last_modified_by.is_none() && f.plaintext.is_none()
                        && f.prefix.is_none()
                });
            let dimension_params_empty = dimension_params_rws.with(|f| f.is_empty());
            !context_filters_empty || !dimension_params_empty
        }>
            <div
                ref_=filter_node_ref
                class=move || {
                    format!(
                        "z-[990] sticky top-0 {} flex gap-2 bg-gray-50",
                        if scrolled_to_top.get() { "pb-4" } else { "" },
                    )
                }
            >
                <div
                    class=format!(
                        "h-max max-w-[1000px] pt-1 px-0.5 border-[1.5px] border-solid border-purple-400 rounded-[10px] ease-in-out duration-300 tranisition-[width] cursor-pointer {}",
                        if scrolled_to_top.get() { "shadow-md" } else { "" },
                    )
                    // on:click=move |_| force_open_rws.set(!summary_expanded.get())
                    on:click=move |_| {
                        if scrolled_to_top.get() {
                            force_open_rws.update(|f| *f = !*f);
                        }
                    }
                >
                    <i class=move || {
                        format!(
                            "{} ri-xl text-purple-800",
                            if summary_expanded.get() {
                                "ri-filter-2-fill"
                            } else {
                                "ri-filter-2-line"
                            },
                        )
                    } />
                    <Show when=move || scrolled_to_top.get()>
                        <i class=move || {
                            format!(
                                "{} ri-xl text-gray-500",
                                if summary_expanded.get() {
                                    "ri-arrow-up-s-line"
                                } else {
                                    "ri-arrow-down-s-line"
                                },
                            )
                        } />
                    </Show>
                </div>
                <div class=move || {
                    format!(
                        "{} flex flex-col gap-1 transition-all ease-in-out duration-300 overflow-hidden",
                        if summary_expanded.get() { "max-h-[1000px]" } else { "max-h-0" },
                    )
                }>
                    {move || {
                        if !dimension_params_rws.with(|d| d.is_empty()) {
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
                                <div class="flex gap-2">
                                    <div class="min-w-fit pt-1 text-xs">{"Context"}</div>
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
                    <Show when=move || {
                        context_filters_rws
                            .with(|f| {
                                f.dimension_match_strategy.unwrap_or_default()
                                    == DimensionMatchStrategy::Exact
                            })
                    }>
                        <div class="flex gap-2 items-center">
                            <span class="text-xs">"Exact match context"</span>
                            <GrayPill
                                text="Enabled"
                                on_delete=move |_| {
                                    context_filters_rws
                                        .update(|f| f.dimension_match_strategy = None)
                                }
                            />
                        </div>
                    </Show>
                    {move || {
                        view! {
                            <ListPills
                                label="Key prefix"
                                items=context_filters_rws
                                    .with(|f| f.prefix.clone().unwrap_or_default())
                                    .0
                                on_delete=move |idx| {
                                    context_filters_rws
                                        .update(|f| f.prefix = filter_index(&f.prefix, idx))
                                }
                            />
                        }
                    }}
                    {move || {
                        context_filters_rws
                            .with(|f| f.plaintext.clone())
                            .map(|plaintext| {
                                view! {
                                    <div class="flex gap-2 items-center">
                                        <span class="text-xs">"Free text"</span>
                                        <GrayPill
                                            text=plaintext.clone()
                                            on_delete=move |_| {
                                                context_filters_rws.update(|f| f.plaintext = None);
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
                                items=context_filters_rws
                                    .with(|f| f.created_by.clone().unwrap_or_default())
                                    .0
                                on_delete=move |idx| {
                                    context_filters_rws
                                        .update(|f| f.created_by = filter_index(&f.created_by, idx))
                                }
                            />
                        }
                    }}
                    {move || {
                        view! {
                            <ListPills
                                label="Last Modified by"
                                items=context_filters_rws
                                    .with(|f| f.last_modified_by.clone().unwrap_or_default())
                                    .0
                                on_delete=move |idx| {
                                    context_filters_rws
                                        .update(|f| {
                                            f.last_modified_by = filter_index(&f.last_modified_by, idx);
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
pub fn context_filter_drawer(
    pagination_params_rws: RwSignal<PaginationParams>,
    context_filters_rws: RwSignal<ContextListFilters>,
    dimension_params_rws: RwSignal<DimensionQuery<QueryMap>>,
    dimensions: Vec<DimensionResponse>,
) -> impl IntoView {
    let filters_buffer_rws = RwSignal::new(context_filters_rws.get_untracked());
    let dimension_buffer_rws = RwSignal::new(dimension_params_rws.get_untracked());
    let (context_rs, context_ws) = create_signal(
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
        let context = context_rs.get();
        json!({
            "context": context,
            "overrides": [],
        })
    });
    view! {
        <Drawer
            id="context_filter_drawer"
            header="Context Filters"
            width_class="max-w-[720px] min-w-[560px] w-[45vw]"
            handle_close=move || close_drawer("context_filter_drawer")
        >
            <div class="flex flex-col gap-5">
                <ContextForm
                    dimensions
                    context=context_rs.get_untracked()
                    fn_environment
                    resolve_mode=true
                    on_context_change=move |context: Conditions| {
                        context_ws.set(context.clone());
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
                    heading_sub_text="Get matching overrides based on the context, this search ensures that the failing conditions are filtered out, and the shown overrides may contain conditions which are not matching but are not in conflict with the context"
                        .to_string()
                />
                {move || {
                    view! {
                        <div class="w-fit flex items-center gap-2">
                            <Toggle
                                name="workspace-strict-mode"
                                disabled=dimension_buffer_rws.with(|d| d.is_empty())
                                value=filters_buffer_rws
                                    .with(|f| {
                                        f.dimension_match_strategy.unwrap_or_default()
                                            == DimensionMatchStrategy::Exact
                                    })
                                on_change=move |flag| {
                                    filters_buffer_rws
                                        .update(|f| {
                                            f.dimension_match_strategy = if flag
                                                && !dimension_buffer_rws.with(|d| d.is_empty())
                                            {
                                                Some(DimensionMatchStrategy::Exact)
                                            } else {
                                                None
                                            };
                                        });
                                }
                            />
                            <Label title="Exact match context" />
                        // extra_info="Enabling this will disable context filter"
                        </div>
                    }
                }}
                <div class="form-control">
                    <Label
                        title="Free text search inside overrides"
                        info="(any of)"
                        description="Searches both keys as well as the values"
                    />
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
                <div class="form-control">
                    <Label
                        title="Created By"
                        info="(any of)"
                        description="Separate each ID by a comma"
                    />
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
                    <Label
                        title="Last Modified By"
                        info="(any of)"
                        description="Separate each ID by a comma"
                    />
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
                <div class="flex justify-end gap-2">
                    <Button
                        class="h-12 w-48"
                        text="Submit"
                        icon_class="ri-send-plane-line"
                        on_click=move |event: MouseEvent| {
                            event.prevent_default();
                            batch(|| {
                                context_filters_rws.set(filters_buffer_rws.get());
                                dimension_params_rws.set(dimension_buffer_rws.get());
                                pagination_params_rws.update(|f| f.reset_page());
                            });
                            close_drawer("context_filter_drawer")
                        }
                    />
                    <Button
                        class="h-12 w-48"
                        text="Reset"
                        icon_class="ri-restart-line"
                        on_click=move |event: MouseEvent| {
                            event.prevent_default();
                            batch(|| {
                                context_filters_rws.set(ContextListFilters::default());
                                dimension_params_rws.set(DimensionQuery::default());
                                pagination_params_rws.update(|f| f.reset_page());
                            });
                            close_drawer("context_filter_drawer")
                        }
                    />
                </div>
            </div>
        </Drawer>
    }
}
