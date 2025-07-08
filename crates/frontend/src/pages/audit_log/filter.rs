use std::{collections::HashSet, fmt::Display, str::FromStr};

use chrono::{DateTime, Days, Duration, Utc};
use leptos::*;
use serde::{Deserialize, Serialize};
use superposition_types::{
    custom_query::{CommaSeparatedQParams, CommaSeparatedStringQParams},
    IsEmpty,
};

use crate::components::{
    badge::{GrayPill, ListPills},
    button::Button,
    drawer::{close_drawer, Drawer, DrawerBtn, DrawerButtonStyle},
    form::label::Label,
    input::DateInput,
};

#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct AuditLogFilters {
    pub from_date: Option<DateTime<Utc>>,
    pub to_date: Option<DateTime<Utc>>,
    pub table: Option<CommaSeparatedStringQParams>,
    pub action: Option<CommaSeparatedStringQParams>,
    pub username: Option<String>,
}

impl Display for AuditLogFilters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut parts = Vec::new();

        if let Some(from_date) = &self.from_date {
            parts.push(format!("from_date={}", from_date));
        }
        if let Some(to_date) = &self.to_date {
            parts.push(format!("to_date={}", to_date));
        }
        if let Some(username) = &self.username {
            parts.push(format!("username={}", username));
        }
        if let Some(table) = &self.table {
            parts.push(format!("table={}", table));
        }
        if let Some(action) = &self.action {
            parts.push(format!("action={}", action));
        }

        write!(f, "{}", parts.join("&"))
    }
}

impl IsEmpty for AuditLogFilters {
    fn is_empty(&self) -> bool {
        self.from_date.is_none()
            && self.to_date.is_none()
            && self.table.is_none()
            && self.action.is_none()
            && self.username.is_none()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum AuditAction {
    INSERT,
    UPDATE,
    DELETE,
}

impl Display for AuditAction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AuditAction::INSERT => write!(f, "INSERT"),
            AuditAction::UPDATE => write!(f, "UPDATE"),
            AuditAction::DELETE => write!(f, "DELETE"),
        }
    }
}

impl FromStr for AuditAction {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "INSERT" => Ok(AuditAction::INSERT),
            "UPDATE" => Ok(AuditAction::UPDATE),
            "DELETE" => Ok(AuditAction::DELETE),
            _ => Err(format!("Unknown audit action: {}", s)),
        }
    }
}

#[component]
pub fn filter_summary(filters_rws: RwSignal<AuditLogFilters>) -> impl IntoView {
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

    fn filter_string_index(
        items: &Option<CommaSeparatedStringQParams>,
        index: usize,
    ) -> Option<CommaSeparatedStringQParams> {
        items.clone().and_then(|mut items| {
            items.0.remove(index);
            (!items.0.is_empty()).then_some(items)
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
                                        <span class="text-xs">"To Date"</span>
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
                                label="Action"
                                items=filters_rws
                                    .with(|f| {
                                        f.action.as_ref().map(|p| p.0.clone()).unwrap_or_default()
                                    })
                                on_delete=move |idx| {
                                    filters_rws.update(|f| f.action = filter_string_index(&f.action, idx))
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
                                    filters_rws.update(|f| f.table = filter_string_index(&f.table, idx))
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
                                            text=username.clone()
                                            on_delete=move |_| {
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
    pagination_params_rws: RwSignal<superposition_types::custom_query::PaginationParams>,
    filters_rws: RwSignal<AuditLogFilters>,
) -> impl IntoView {
    let filters_buffer_rws = RwSignal::new(filters_rws.get_untracked());

    let action_filter_management = move |checked: bool, filter_action: AuditAction| {
        filters_buffer_rws.update(|f| {
            let action_types = f.action.clone().map(|s| s.0).unwrap_or_default();
            let mut old_action_vector: HashSet<String> = HashSet::from_iter(action_types);

            let action_str = filter_action.to_string();
            if checked {
                old_action_vector.insert(action_str);
            } else {
                old_action_vector.remove(&action_str);
            }
            let new_action_vector: Vec<String> = old_action_vector.into_iter().collect();
            f.action = if new_action_vector.is_empty() {
                None
            } else {
                Some(CommaSeparatedQParams(new_action_vector))
            }
        })
    };

    view! {
        <DrawerBtn
            drawer_id="audit_log_filter_drawer"
            class="!h-9 !min-h-[32px] !w-fit px-2"
            style=DrawerButtonStyle::Outline
        >
            Filters
            <i class="ri-filter-3-line"></i>
        </DrawerBtn>
        <Drawer
            id="audit_log_filter_drawer".to_string()
            header="Audit Log Filters"
            drawer_width="w-[50vw]"
            handle_close=move || close_drawer("audit_log_filter_drawer")
        >
            <div class="flex flex-col gap-5">
                <div class="w-full flex flex-row justify-start items-end gap-10">
                    <div class="form-control">
                        <Label title="From Date" />
                        <DateInput
                            id="audit_from_date_input"
                            name="audit_from_date"
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
                        <Label title="To Date" />
                        <DateInput
                            id="audit_to_date_input"
                            name="audit_to_date"
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

                <div class="form-control w-full">
                    <Label title="Action" />
                    <div class="flex flex-row flex-wrap justify-start gap-5">
                        {[AuditAction::INSERT, AuditAction::UPDATE, AuditAction::DELETE].iter()
                            .map(|action| {
                                let label = action.to_string();
                                let input_id = format!("{label}-checkbox");
                                let label_for_check = label.clone();

                                view! {
                                    <div>
                                        <input
                                            type="checkbox"
                                            id=&input_id
                                            class="peer hidden"
                                            checked=move || {
                                                filters_buffer_rws
                                                    .with(|f| f.action.clone())
                                                    .is_some_and(|s| s.0.iter().any(|item| *item == label_for_check))
                                            }
                                            on:change=move |event| {
                                                let checked = event_target_checked(&event);
                                                action_filter_management(checked, *action)
                                            }
                                        />
                                        <label
                                            for=&input_id
                                            class="badge h-[30px] px-6 py-2 peer-checked:bg-purple-500 peer-checked:text-white cursor-pointer transition duration-300 ease-in-out"
                                        >
                                            {label}
                                        </label>
                                    </div>
                                }
                            })
                            .collect_view()}
                    </div>
                </div>

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
                            let username = if username.is_empty() {
                                None
                            } else {
                                Some(username)
                            };
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
                            filters_buffer_rws
                                .with(|f| f.table.clone().map(|d| d.to_string()))
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
                                    (!table_list.is_empty()).then_some(CommaSeparatedQParams(table_list))
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
                        on_click=move |event| {
                            event.prevent_default();
                            let filters = filters_buffer_rws.get();
                            batch(|| {
                                filters_rws.set(filters);
                                pagination_params_rws.update(|f| f.reset_page());
                            });
                            close_drawer("audit_log_filter_drawer");
                        }
                    />
                    <Button
                        class="h-12 w-48"
                        text="Reset"
                        icon_class="ri-restart-line"
                        on_click=move |event| {
                            event.prevent_default();
                            batch(|| {
                                filters_rws.set(AuditLogFilters::default());
                                filters_buffer_rws.set(AuditLogFilters::default());
                                pagination_params_rws.update(|f| f.reset_page());
                            });
                            close_drawer("audit_log_filter_drawer");
                        }
                    />
                </div>
            </div>
        </Drawer>
    }
}

