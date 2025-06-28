use std::{fmt::Display, ops::Deref, str::FromStr};

use leptos::*;
use serde_json::{json, Map, Value};
use superposition_types::{
    api::context::ContextListFilters,
    custom_query::{
        CommaSeparatedQParams, CommaSeparatedStringQParams, CustomQuery, DimensionQuery,
        PaginationParams, QueryMap,
    },
    database::types::DimensionWithMandatory,
};

use crate::{
    components::{
        button::Button,
        condition_pills::Condition,
        context_form::ContextForm,
        drawer::{close_drawer, Drawer},
        dropdown::DropdownDirection,
    },
    logic::{Condition, Conditions, Expression},
    providers::condition_collapse_provider::ConditionCollapseProvider,
};

#[component]
fn gray_pill(
    text: String,
    #[prop(into, default = Callback::new(move |_| () ))] on_delete: Callback<()>,
) -> impl IntoView {
    view! {
        <div class="badge badge-sm !h-fit py-[1px] bg-gray-200 flex gap-1">
            {text}
            <i class="ri-close-line cursor-pointer" on:click=move |_| { on_delete.call(()) } />
        </div>
    }
}

#[component]
fn list_pills<T>(
    #[prop(into)] label: String,
    items: CommaSeparatedQParams<T>,
    #[prop(into)] on_delete: Callback<usize>,
) -> impl IntoView
where
    T: Display + FromStr,
{
    if items.is_empty() {
        return ().into_view();
    }

    view! {
        <div class="flex gap-2 items-center">
            <div class="min-w-fit flex gap-[2px] text-xs">
                {label} <span class="text-[10px] text-slate-400">{"(any of)"}</span>
            </div>
            <div class="flex gap-[2px] items-center flex-wrap">
                {items
                    .iter()
                    .enumerate()
                    .map(|(idx, item)| {
                        view! {
                            <GrayPill
                                text=item.to_string()
                                on_delete=move |_| on_delete.call(idx)
                            />
                        }
                    })
                    .collect_view()}
            </div>
        </div>
    }
    .into_view()
}

#[component]
pub fn context_filter_summary(
    #[prop(into)] scrolled_to_top: Signal<bool>,
    context_filters_rws: RwSignal<ContextListFilters>,
    dimension_params_rws: RwSignal<DimensionQuery<QueryMap>>,
    filter_node_ref: NodeRef<html::Div>,
) -> impl IntoView {
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
                        "z-[1000] sticky top-0 {} flex gap-2 bg-gray-50",
                        if scrolled_to_top.get() { "pb-4" } else { "" },
                    )
                }
            >
                <div
                    class=format!("h-max max-w-[1000px] pt-1 px-0.5 border-[1.5px] border-solid border-purple-400 rounded-[10px] ease-in-out duration-300 tranisition-[width] cursor-pointer {}", if scrolled_to_top.get() { "shadow-md" } else { "" })
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
                        view! {
                            <ListPills
                                label="Key prefix"
                                items=context_filters_rws
                                    .with(|f| f.prefix.clone())
                                    .unwrap_or_default()
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
                                        <span class="text-xs">{"Free text"}</span>
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
                                    .with(|f| f.created_by.clone())
                                    .unwrap_or_default()
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
                                    .with(|f| f.last_modified_by.clone())
                                    .unwrap_or_default()
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
    dimensions: Vec<DimensionWithMandatory>,
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
            id="context_filter_drawer".to_string()
            header="Context Filters"
            drawer_width="w-[50vw]"
            handle_close=move || close_drawer("context_filter_drawer")
        >
            <div class="flex flex-col gap-4">
                <ContextForm
                    dimensions
                    context_rs
                    context_ws
                    fn_environment
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
                    heading_sub_text="Get matching overrides based on the context, this search ensures that the failing conditions are filtered out, and the shown overrides may contain conditions which are not matching but are not in conflict with the context"
                        .to_string()
                />
                <div class="form-control">
                    <label class="label flex-col justify-center items-start">
                        <span class="label-text font-semibold text-base">
                            {"Free text search inside overrides"}
                        </span>
                        <span class="label-text text-slate-400">
                            {"Searches both keys as well as the values"}
                        </span>
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
                <div class="form-control">
                    <label class="label flex flex-col items-start justify-center">
                        <div class="flex gap-1 label-text font-semibold text-base">
                            {"Created By"}
                            <span class="text-sm font-normal text-slate-400">"(any of)"</span>
                        </div>
                        <span class="label-text text-slate-400">
                            {"Separate each user by a comma"}
                        </span>
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
                    <label class="label flex flex-col items-start justify-center">
                        <div class="flex gap-1 label-text font-semibold text-base">
                            {"Last Modified By"}
                            <span class="text-sm font-normal text-slate-400">"(any of)"</span>
                        </div>
                        <span class="label-text text-slate-400">
                            {"Separate each user by a comma"}
                        </span>
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
                <div class="flex justify-end">
                    <Button
                        class="h-12 w-48".to_string()
                        text="Submit".to_string()
                        on_click=move |event| {
                            event.prevent_default();
                            pagination_params_rws.update(|f| {
                                context_filters_rws.set_untracked(filters_buffer_rws.get());
                                dimension_params_rws.set_untracked(dimension_buffer_rws.get());
                                f.reset_page()
                            });
                            close_drawer("context_filter_drawer")
                        }
                    />
                    <Button
                        class="h-12 w-48".to_string()
                        text="Reset".to_string()
                        icon_class="ri-restart-line".into()
                        on_click=move |event| {
                            event.prevent_default();
                            pagination_params_rws.update(|f| {
                                context_filters_rws.set_untracked(ContextListFilters::default());
                                dimension_params_rws.set_untracked(DimensionQuery::default());
                                f.reset_page()
                        });
                            close_drawer("context_filter_drawer")
                        }
                    />

                </div>
            </div>
        </Drawer>
    }
}
