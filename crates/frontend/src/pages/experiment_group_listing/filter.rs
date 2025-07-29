use std::{collections::HashSet, fmt::Display, str::FromStr};
use strum::IntoEnumIterator;

use leptos::*;
use superposition_types::{
    api::experiment_groups::ExpGroupFilters,
    custom_query::{CommaSeparatedQParams, PaginationParams},
    database::models::experimentation::GroupType,
};
use web_sys::MouseEvent;

use crate::components::{
    badge::GrayPill,
    button::{Button, ButtonStyle},
    drawer::{close_drawer, Drawer, DrawerBtn},
    form::label::Label,
};

#[component]
pub(super) fn filter_summary(filters_rws: RwSignal<ExpGroupFilters>) -> impl IntoView {
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
                    f.created_by.is_none() && f.name.is_none() && f.last_modified_by.is_none()
                });
            !filters_empty
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
                        filters_rws
                            .with(|f| f.name.clone())
                            .map(|name| {
                                view! {
                                    <div class="flex gap-2 items-center">
                                        <span class="text-xs">"Name"</span>
                                        <GrayPill
                                            text=name
                                            on_delete=move |_| {
                                                filters_rws.update(|f| f.name = None);
                                            }
                                        />
                                    </div>
                                }
                            })
                    }}
                    {move || {
                        filters_rws
                            .with(|f| f.created_by.clone())
                            .map(|created_by| {
                                view! {
                                    <div class="flex gap-2 items-center">
                                        <span class="text-xs">"Created By"</span>
                                        <GrayPill
                                            text=created_by
                                            on_delete=move |_| {
                                                filters_rws.update(|f| f.created_by = None);
                                            }
                                        />
                                    </div>
                                }
                            })
                    }}
                    {move || {
                        filters_rws
                            .with(|f| f.last_modified_by.clone())
                            .map(|last_modified_by| {
                                view! {
                                    <div class="flex gap-2 items-center">
                                        <span class="text-xs">"Last Modified By"</span>
                                        <GrayPill
                                            text=last_modified_by
                                            on_delete=move |_| {
                                                filters_rws.update(|f| f.last_modified_by = None);
                                            }
                                        />
                                    </div>
                                }
                            })
                    }}
                </div>
            </div>
        </Show>
    }
}

#[component]
pub(super) fn experiment_group_filter_widget(
    pagination_params_rws: RwSignal<PaginationParams>,
    filters_rws: RwSignal<ExpGroupFilters>,
) -> impl IntoView {
    let filters_buffer_rws = RwSignal::new(filters_rws.get_untracked());

    let group_type_management = move |checked: bool, group_type: GroupType| {
        filters_buffer_rws.update(|f| {
            let group_types = f.group_type.clone().map(|s| s.0).unwrap_or_default();
            let mut old_group_vector: HashSet<GroupType> =
                HashSet::from_iter(group_types);

            if checked {
                old_group_vector.insert(group_type);
            } else {
                old_group_vector.remove(&group_type);
            }
            let new_group_vector = old_group_vector.into_iter().collect();
            f.group_type = Some(CommaSeparatedQParams(new_group_vector))
        })
    };

    view! {
        <DrawerBtn
            drawer_id="experiment_group_filter_drawer"
            class="!h-9 !min-h-[32px] !w-fit px-2"
            style=ButtonStyle::Outline
            text="Filters"
            icon_class="ri-filter-3-line"
        />
        <Drawer
            id="experiment_group_filter_drawer"
            header="Experiment Group Filters"
            handle_close=move || close_drawer("experiment_group_filter_drawer")
        >
            <div class="flex flex-col gap-5">
                <div class="form-control">
                    <Label title="Experiment Group Name" />
                    <input
                        type="text"
                        id="experiment-group-name-filter"
                        placeholder="eg: city experiment group"
                        class="input input-bordered rounded-md resize-y w-full max-w-md"
                        value=move || filters_buffer_rws.with(|f| f.name.clone())
                        on:change=move |event| {
                            let name = event_target_value(&event);
                            let group_name = if name.trim().is_empty() { None } else { Some(name) };
                            filters_buffer_rws.update(|f| f.name = group_name);
                        }
                    />
                </div>
                <div class="form-control">
                    <Label title="Created By" />
                    <input
                        type="text"
                        id="experiment-group-created-by-filter"
                        class="input input-bordered rounded-md resize-y w-full max-w-md"
                        value=move || filters_buffer_rws.with(|f| f.created_by.clone())
                        placeholder="eg: user@superposition.io"
                        on:change=move |event| {
                            let created_by = event_target_value(&event);
                            let created_by = if created_by.trim().is_empty() {
                                None
                            } else {
                                Some(created_by)
                            };
                            filters_buffer_rws.update(|filter| filter.created_by = created_by);
                        }
                    />
                </div>
                <div class="form-control">
                    <Label title="Last Modified By" />
                    <input
                        type="text"
                        id="experiment-last-modified-filter"
                        class="input input-bordered rounded-md resize-y w-full max-w-md"
                        placeholder="eg: user@superposition.io"
                        value=move || filters_buffer_rws.with(|f| f.last_modified_by.clone())
                        on:change=move |event| {
                            let last_modified = event_target_value(&event);
                            let last_modified_by = if last_modified.trim().is_empty() {
                                None
                            } else {
                                Some(last_modified)
                            };
                            filters_buffer_rws
                                .update(|filter| filter.last_modified_by = last_modified_by);
                        }
                    />
                </div>
                <div class="form-control w-full">
                    <Label title="Group Type" />
                    <div class="flex flex-row flex-wrap justify-start gap-5">
                        {GroupType::iter()
                            .map(|status| {
                                let label = status.to_string();
                                let input_id = format!("{label}-checkbox");

                                view! {
                                    <div>
                                        <input
                                            type="checkbox"
                                            id=&input_id
                                            class="peer hidden"
                                            checked=move || {
                                                filters_buffer_rws
                                                    .with(|f| f.group_type.clone())
                                                    .is_some_and(|s| s.iter().any(|item| *item == status))
                                            }
                                            on:change=move |event| {
                                                let checked = event_target_checked(&event);
                                                group_type_management(checked, status)
                                            }
                                        />
                                        <label
                                            for=&input_id
                                            class="badge h-[30px] px-6 py-2 peer-checked:bg-purple-500 peer-checked:border-purple-500 peer-checked:text-white cursor-pointer transition duration-300 ease-in-out peer-checked:shadow-purple-500 peer-checked:shadow-md shadow-inner shadow-slate-500"
                                        >
                                            {label}
                                        </label>
                                    </div>
                                }
                            })
                            .collect_view()}
                    </div>
                </div>
                <div class="flex justify-end gap-2">
                    <Button
                        class="h-12 w-48"
                        text="Submit"
                        icon_class="ri-send-plane-line"
                        on_click=move |event: MouseEvent| {
                            event.prevent_default();
                            let filter = filters_buffer_rws.get();
                            close_drawer("experiment_group_filter_drawer");
                            batch(|| {
                                pagination_params_rws.update(|f| f.reset_page());
                                filters_rws.set(filter);
                            });
                        }
                    />
                    <Button
                        class="h-12 w-48"
                        text="Reset"
                        icon_class="ri-restart-line"
                        on_click=move |event: MouseEvent| {
                            close_drawer("experiment_group_filter_drawer");
                            event.prevent_default();
                            batch(|| {
                                filters_rws.set(ExpGroupFilters::default());
                                pagination_params_rws.update(|f| f.reset_page());
                                filters_buffer_rws.set(ExpGroupFilters::default());
                            });
                        }
                    />
                </div>
            </div>
        </Drawer>
    }
}
