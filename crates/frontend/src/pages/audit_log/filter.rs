use std::{fmt::Display, str::FromStr};

use chrono::{DateTime, Days, Duration, TimeZone, Utc};
use leptos::*;
use superposition_types::{
    api::audit_log::AuditQueryFilters,
    custom_query::{CommaSeparatedQParams, PaginationParams},
};

use crate::components::{
    badge::{GlassyPills, GrayPill, ListPills},
    button::{Button, ButtonStyle},
    datetime::{Datetime, DatetimeFormat},
    drawer::{close_drawer, Drawer, DrawerBtn},
    form::label::Label,
    input::DateInput,
};

#[component]
pub fn filter_summary(filters_rws: RwSignal<AuditQueryFilters>) -> impl IntoView {
    let force_open_rws = RwSignal::new(true);

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
                    f.username.is_none() && f.from_date.is_none() && f.to_date.is_none()
                        && f.action.is_none() && f.table.is_none()
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
                            .with(|f| f.from_date)
                            .map(|from_date| {
                                view! {
                                    <div class="flex gap-2 items-center">
                                        <span class="text-xs">"From Date"</span>
                                        <GrayPill
                                            data=from_date
                                            renderer=Some(
                                                Callback::new(move |datetime: DateTime<Utc>| {
                                                    view! { <Datetime datetime format=DatetimeFormat::Date /> }
                                                        .into_view()
                                                }),
                                            )
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
                                        <span class="text-xs">"To Date"</span>
                                        <GrayPill
                                            data=to_date
                                            renderer=Some(
                                                Callback::new(move |datetime: DateTime<Utc>| {
                                                    view! { <Datetime datetime format=DatetimeFormat::Date /> }
                                                        .into_view()
                                                }),
                                            )
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
                                label="Action"
                                items=filters_rws
                                    .with(|f| {
                                        f.action.as_ref().map(|p| p.0.clone()).unwrap_or_default()
                                    })
                                on_delete=move |idx| {
                                    filters_rws.update(|f| f.action = filter_index(&f.action, idx))
                                }
                            />
                        }
                    }}
                    {move || {
                        view! {
                            <ListPills
                                label="Table"
                                items=filters_rws
                                    .with(|f| {
                                        f.table.as_ref().map(|p| p.0.clone()).unwrap_or_default()
                                    })
                                on_delete=move |idx| {
                                    filters_rws.update(|f| f.table = filter_index(&f.table, idx))
                                }
                            />
                        }
                    }}
                    {move || {
                        filters_rws
                            .with(|f| f.username.clone())
                            .map(|username| {
                                view! {
                                    <div class="flex gap-2 items-center">
                                        <span class="text-xs">"Username"</span>
                                        <GrayPill
                                            data=username
                                            on_delete=move |_: String| {
                                                filters_rws.update(|f| f.username = None);
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
pub fn audit_log_filter_widget(
    filters_rws: RwSignal<AuditQueryFilters>,
    pagination_params_rws: RwSignal<PaginationParams>,
) -> impl IntoView {
    let filters_buffer_rws = RwSignal::new(filters_rws.get_untracked());

    view! {
        <DrawerBtn
            drawer_id="audit_log_filter_drawer"
            text="Filters"
            icon_class="ri-filter-3-line"
            class="!h-9 !min-h-[32px] !w-fit px-2"
            style=ButtonStyle::Outline
        />
        <Drawer
            id="audit_log_filter_drawer"
            header="Audit Log Filters"
            width_class="max-w-[780px] min-w-[560px] w-[45vw]"
            handle_close=move || close_drawer("audit_log_filter_drawer")
        >
            <div class="flex flex-col gap-5">
                <div class="w-full flex flex-row justify-start items-end gap-10">
                    <div class="form-control">
                        <Label title="From Date" />
                        <DateInput
                            id="audit_from_date_input"
                            name="audit_from_date"
                            min=Utc
                                .with_ymd_and_hms(2020, 1, 1, 0, 0, 0)
                                .single()
                                .unwrap_or_default()
                            value=filters_buffer_rws.with(|f| f.from_date)
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
                        <Label title="To Date" />
                        <DateInput
                            id="audit_to_date_input"
                            name="audit_to_date"
                            value=filters_buffer_rws.with(|f| f.to_date)
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
                            .with(|f| f.action.clone().map(|p| p.0).unwrap_or_default())
                    })
                    title="Action"
                    on_click=move |items| {
                        filters_buffer_rws.update(|f| f.action = Some(CommaSeparatedQParams(items)))
                    }
                />

                <div class="form-control">
                    <Label title="Username" />
                    <input
                        type="text"
                        id="audit-username-filter"
                        placeholder="eg: user@example.com"
                        class="input input-bordered rounded-md resize-y w-full max-w-md"
                        value=move || filters_buffer_rws.with(|f| f.username.clone())
                        on:change=move |event| {
                            let username = event_target_value(&event);
                            let username = if username.is_empty() { None } else { Some(username) };
                            filters_buffer_rws.update(|f| f.username = username);
                        }
                    />
                </div>

                <div class="form-control">
                    <Label
                        title="Table Name"
                        info="(any of)"
                        description="Separate each table by a comma"
                    />
                    <input
                        type="text"
                        id="audit-table-filter"
                        class="input input-bordered rounded-md resize-y w-full max-w-md"
                        value=move || {
                            filters_buffer_rws.with(|f| f.table.clone().map(|d| d.to_string()))
                        }
                        placeholder="eg: experiments,contexts"
                        on:change=move |event| {
                            let tables = event_target_value(&event);
                            let tables = (!tables.is_empty())
                                .then(|| {
                                    let table_list: Vec<String> = tables
                                        .split(',')
                                        .map(|s| s.trim().to_string())
                                        .filter(|s| !s.is_empty())
                                        .collect();
                                    (!table_list.is_empty())
                                        .then_some(CommaSeparatedQParams(table_list))
                                })
                                .flatten();
                            filters_buffer_rws.update(|filter| filter.table = tables);
                        }
                    />
                </div>

                <div class="flex justify-end gap-2">
                    <Button
                        class="h-12 w-48"
                        text="Submit"
                        icon_class="ri-send-plane-line"
                        on_click=move |event: web_sys::MouseEvent| {
                            event.prevent_default();
                            let filters = filters_buffer_rws.get();
                            close_drawer("audit_log_filter_drawer");
                            batch(|| {
                                filters_rws.set(filters);
                                pagination_params_rws.update(|f| f.reset_page());
                            });
                        }
                    />
                    <Button
                        class="h-12 w-48"
                        text="Reset"
                        icon_class="ri-restart-line"
                        on_click=move |event: web_sys::MouseEvent| {
                            event.prevent_default();
                            close_drawer("audit_log_filter_drawer");
                            batch(|| {
                                let default_filters = AuditQueryFilters::default();
                                filters_rws.set(default_filters.clone());
                                filters_buffer_rws.set(default_filters);
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
