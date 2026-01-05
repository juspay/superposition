use std::{fmt::Display, str::FromStr};

use leptos::*;
use superposition_types::{
    api::default_config::DefaultConfigFilters,
    custom_query::{CommaSeparatedQParams, PaginationParams},
};
use web_sys::MouseEvent;

use crate::components::{
    badge::GrayPill,
    button::{Button, ButtonStyle},
    drawer::{close_drawer, Drawer, DrawerBtn},
    form::label::Label,
};

#[component]
pub(super) fn filter_summary(
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
                        filters_rws
                            .with(|f| f.name.clone())
                            .map(|name| {
                                view! {
                                    <div class="flex gap-2 items-center">
                                        <span class="text-xs">"Prefix"</span>
                                        <GrayPill
                                            data=name
                                            on_delete=move |_: String| {
                                                filters_rws.update(|f| f.name = None);
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
pub(super) fn default_config_filter_widget(
    pagination_params_rws: RwSignal<PaginationParams>,
    filters_rws: RwSignal<DefaultConfigFilters>,
    #[prop(into)] prefix: Option<String>,
) -> impl IntoView {
    let filters = filters_rws.get_untracked();
    let filters_buffer_rws = create_rw_signal(filters.clone());
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
                    <Label title="Configuration Prefix" />
                    <input
                        type="text"
                        id="default-config-name-filter"
                        placeholder="eg: city"
                        class="input input-bordered rounded-md resize-y w-full max-w-md"
                        value=if prefix.is_some()
                            && filters_buffer_rws.get_untracked().name.is_none()
                        {
                            prefix
                        } else {
                            filters_buffer_rws.get_untracked().name
                        }
                        on:change=move |event| {
                            let key_name = event_target_value(&event);
                            let key_name = if key_name.is_empty() { None } else { Some(key_name) };
                            filters_buffer_rws.update(|f| f.name = key_name);
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
                            pagination_params_rws.update(|f| f.reset_page());
                            filters_rws.set(filter);
                            close_drawer("default_config_filter_drawer")
                        }
                    />
                    <Button
                        class="h-12 w-48"
                        text="Reset"
                        on_click=move |event: MouseEvent| {
                            event.prevent_default();
                            let filters = DefaultConfigFilters::default();
                            pagination_params_rws.update(|f| f.reset_page());
                            filters_rws.set(filters);
                            close_drawer("default_config_filter_drawer")
                        }
                    />

                </div>
            </div>
        </Drawer>
    }
}
