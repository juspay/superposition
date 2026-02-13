use std::{fmt::Display, str::FromStr};

use leptos::*;
use superposition_types::{
    api::default_config::DefaultConfigFilters,
    custom_query::{CommaSeparatedQParams, PaginationParams},
};
use web_sys::MouseEvent;

use crate::components::{
    badge::{GrayPill, ListPills},
    button::{Button, ButtonStyle},
    drawer::{Drawer, DrawerBtn, close_drawer},
    form::label::Label,
};

#[component]
pub(super) fn FilterSummary(
    filters_rws: RwSignal<DefaultConfigFilters>,
) -> impl IntoView {
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
            let filters_empty = filters_rws.with(|f| f.name.is_none());
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
                        view! {
                            <ListPills
                                label="Config key"
                                items=filters_rws.with(|f| f.name.clone().unwrap_or_default()).0
                                on_delete=move |idx| {
                                    filters_rws.update(|f| f.name = filter_index(&f.name, idx))
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
pub(super) fn DefaultConfigFilterWidget(
    pagination_params_rws: RwSignal<PaginationParams>,
    filters_rws: RwSignal<DefaultConfigFilters>,
) -> impl IntoView {
    let filters_buffer_rws = create_rw_signal(filters_rws.get_untracked());

    Effect::new(move |_| {
        let filters = filters_rws.get();
        if filters_buffer_rws.get_untracked() != filters {
            filters_buffer_rws.set(filters);
        }
    });

    view! {
        <DrawerBtn
            drawer_id="default_config_filter_drawer"
            class="!h-9 !min-h-[32px] !w-fit px-2"
            style=ButtonStyle::Outline
            text="Filters"
            icon_class="ri-filter-3-line"
        />
        <Drawer
            id="default_config_filter_drawer"
            header="Default Config Filters"
            handle_close=move || close_drawer("default_config_filter_drawer")
        >
            <div class="flex flex-col gap-5">
                <div class="form-control">
                    <div class="flex items-center gap-2">
                        <Label
                            title="Config key"
                            info="(any of)"
                            description="Separate each ID by a comma"
                        />
                        {move || {
                            filters_buffer_rws
                                .with(|f| f.prefix.clone())
                                .as_ref()
                                .map(|p| {
                                    view! {
                                        <GrayPill
                                            data=p
                                            deletable=false
                                            on_delete=Callback::new(|_: String| {})
                                            icon_class="ri-map-pin-2-fill"
                                        />
                                    }
                                })
                        }}
                    </div>
                    <input
                        type="text"
                        id="default-config-name-filter"
                        class="input input-bordered rounded-md resize-y w-full max-w-md"
                        placeholder="eg: key1,key2"
                        prop:value=move || {
                            filters_buffer_rws
                                .with(|f| {
                                    f.name.as_ref().map(|d| d.to_string()).unwrap_or_default()
                                })
                        }
                        on:change=move |event| {
                            let names = event_target_value(&event);
                            let names = (!names.is_empty())
                                .then(|| {
                                    let name_list: Vec<String> = names
                                        .split(',')
                                        .map(|s| s.trim().to_string())
                                        .filter(|s| !s.is_empty())
                                        .collect();
                                    (!name_list.is_empty())
                                        .then_some(CommaSeparatedQParams(name_list))
                                })
                                .flatten();
                            filters_buffer_rws.update(|filter| filter.set_name(names));
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
                            let filter = filters_buffer_rws.get();
                            batch(|| {
                                pagination_params_rws.update(|f| f.reset_page());
                                filters_rws.set(filter);
                            });
                            close_drawer("default_config_filter_drawer")
                        }
                    />
                    <Button
                        class="h-12 w-48"
                        text="Reset"
                        on_click=move |event: MouseEvent| {
                            event.prevent_default();
                            batch(|| {
                                let filters = DefaultConfigFilters::default();
                                pagination_params_rws.update(|f| f.reset_page());
                                filters_rws.set(filters);
                            });
                            close_drawer("default_config_filter_drawer")
                        }
                    />

                </div>
            </div>
        </Drawer>
    }
}
