use leptos::*;
use serde_json::Value;
use superposition_types::{
    api::secrets::SecretFilters,
    custom_query::{CommaSeparatedStringQParams, PaginationParams},
};
use web_sys::MouseEvent;

use crate::components::{
    badge::ListPills,
    button::{Button, ButtonStyle},
    drawer::{Drawer, DrawerBtn, close_drawer},
    form::label::Label,
};

#[component]
pub fn filter_summary(filters_rws: RwSignal<SecretFilters>) -> impl IntoView {
    let force_open_rws = RwSignal::new(true);

    fn filter_index(
        items: &Option<CommaSeparatedStringQParams>,
        index: usize,
    ) -> Option<CommaSeparatedStringQParams> {
        items.clone().and_then(|mut items| {
            items.0.remove(index);
            (!items.is_empty()).then_some(items)
        })
    }

    view! {
        <Show when=move || {
            let filters_empty = filters_rws
                .with(|f| {
                    f.name.is_none() && f.created_by.is_none() && f.last_modified_by.is_none()
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
                    <ListPills
                        label="Name"
                        items=filters_rws
                            .with(|f| { f.name.as_ref().map(|p| p.0.clone()).unwrap_or_default() })
                        on_delete=move |idx| {
                            filters_rws.update(|f| f.name = filter_index(&f.name, idx))
                        }
                    />
                    <ListPills
                        label="Created By"
                        items=filters_rws
                            .with(|f| {
                                f.created_by.as_ref().map(|p| p.0.clone()).unwrap_or_default()
                            })
                        on_delete=move |idx| {
                            filters_rws.update(|f| f.created_by = filter_index(&f.created_by, idx))
                        }
                    />
                    <ListPills
                        label="Modified By"
                        items=filters_rws
                            .with(|f| {
                                f.last_modified_by.as_ref().map(|p| p.0.clone()).unwrap_or_default()
                            })
                        on_delete=move |idx| {
                            filters_rws
                                .update(|f| {
                                    f.last_modified_by = filter_index(&f.last_modified_by, idx);
                                })
                        }
                    />
                </div>
            </div>
        </Show>
    }
}

#[component]
pub fn secret_filter_widget(
    pagination_params_rws: RwSignal<PaginationParams>,
    filters_rws: RwSignal<SecretFilters>,
) -> impl IntoView {
    let filters_buffer_rws = RwSignal::new(filters_rws.get_untracked());

    view! {
        <DrawerBtn
            drawer_id="secret_filter_drawer"
            class="!h-9 !min-h-[32px] !w-fit px-2"
            style=ButtonStyle::Outline
            text="Filters"
            icon_class="ri-filter-3-line"
        />
        <Drawer
            id="secret_filter_drawer"
            header="Secret Filters"
            handle_close=move || close_drawer("secret_filter_drawer")
        >
            <div class="flex flex-col gap-5">
                <div class="form-control">
                    <Label
                        title="Secret Name"
                        info="(any of)"
                        description="Separate each name by a comma"
                    />
                    <input
                        type="text"
                        id="secret-name-filter"
                        placeholder="eg: API_KEY,DB_PASSWORD,SECRET_TOKEN"
                        class="input input-bordered rounded-md resize-y w-full max-w-md"
                        value=move || {
                            filters_buffer_rws
                                .with(|f| {
                                    f.name
                                        .as_ref()
                                        .map(|p| p.0.clone())
                                        .unwrap_or_default()
                                        .join(", ")
                                })
                        }
                        on:change=move |event| {
                            let names = event_target_value(&event);
                            let names = (!names.is_empty())
                                .then(|| serde_json::from_value(Value::String(names)).ok())
                                .flatten();
                            filters_buffer_rws.update(|filter| filter.name = names);
                        }
                    />
                </div>
                <div class="form-control">
                    <Label
                        title="Created By"
                        info="(any of)"
                        description="Separate each ID by a comma"
                    />
                    <input
                        type="text"
                        id="secret-created-by-filter"
                        placeholder="eg: user@example.com,admin@example.com"
                        class="input input-bordered rounded-md resize-y w-full max-w-md"
                        value=move || {
                            filters_buffer_rws
                                .with(|f| {
                                    f.created_by
                                        .as_ref()
                                        .map(|p| p.0.clone())
                                        .unwrap_or_default()
                                        .join(", ")
                                })
                        }
                        on:change=move |event| {
                            let names = event_target_value(&event);
                            let names = (!names.is_empty())
                                .then(|| serde_json::from_value(Value::String(names)).ok())
                                .flatten();
                            filters_buffer_rws.update(|filter| filter.created_by = names);
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
                        id="secret-last-modified-by-filter"
                        placeholder="eg: user@example.com,admin@example.com"
                        class="input input-bordered rounded-md resize-y w-full max-w-md"
                        value=move || {
                            filters_buffer_rws
                                .with(|f| {
                                    f.last_modified_by
                                        .as_ref()
                                        .map(|p| p.0.clone())
                                        .unwrap_or_default()
                                        .join(", ")
                                })
                        }
                        on:change=move |event| {
                            let names = event_target_value(&event);
                            let names = (!names.is_empty())
                                .then(|| serde_json::from_value(Value::String(names)).ok())
                                .flatten();
                            filters_buffer_rws.update(|filter| filter.last_modified_by = names);
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
                            close_drawer("secret_filter_drawer");
                            batch(|| {
                                filters_rws.set(filter);
                                pagination_params_rws.update(|f| f.reset_page());
                            });
                        }
                    />
                    <Button
                        class="h-12 w-48"
                        text="Reset"
                        icon_class="ri-restart-line"
                        on_click=move |event: MouseEvent| {
                            event.prevent_default();
                            close_drawer("secret_filter_drawer");
                            batch(|| {
                                let default_filters = SecretFilters::default();
                                filters_rws.set(default_filters.clone());
                                filters_buffer_rws.set(default_filters);
                                pagination_params_rws.update(|f| f.reset_page());
                            });
                        }
                    />
                </div>
            </div>
        </Drawer>
    }
}
