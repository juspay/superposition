use leptos::logging::*;
use leptos::*;
use leptos_router::*;

use chrono::{
    prelude::{DateTime, Utc},
    TimeZone,
};

use crate::components::{
    pagination::pagination::Pagination,
    table::{table::Table, types::Column},
};

use crate::pages::ExperimentList::types::{
    ExperimentResponse, ExperimentsResponse, ListFilters,
};

use super::utils::fetch_experiments;
use serde_json::{json, Map, Value};

#[component]
pub fn ExperimentList() -> impl IntoView {
    // acquire tenant
    let tenant = "test".to_string();
    let (filters, set_filters) = create_signal(ListFilters {
        status: None,
        from_date: Utc.timestamp_opt(0, 0).single(),
        to_date: Utc.timestamp_opt(4130561031, 0).single(),
        page: Some(1),
        count: Some(1),
    });

    let table_columns = create_memo(move |_| {
        vec![
            Column::default("id".to_string()),
            Column::default("name".to_string()),
            Column::new(
                "status".to_string(),
                None,
                Some(|value: &str, _| {
                    let badge_color = match value {
                        "CREATED" => "badge-info",
                        "INPROGRESS" => "badge-warning",
                        "CONCLUDED" => "badge-success",
                        &_ => "info",
                    };
                    let class = format!("badge {}", badge_color);
                    view! {
                        <div class={class}>
                            <span class="text-white font-semibold text-xs">
                                {value.to_string()}
                            </span>
                        </div>
                    }
                    .into_view()
                }),
            ),
            Column::default("context".to_string()),
        ]
    });

    let experiments = create_blocking_resource(
        move || filters.get(),
        |value| async move {
            match fetch_experiments(value).await {
                Ok(data) => data,
                Err(e) => ExperimentsResponse {
                    total_items: 0,
                    total_pages: 0,
                    data: vec![],
                },
            }
        },
    );

    // TODO: Add filters
    view! {
        <div class="p-8">
            <Suspense fallback=move || view! {<p>"Loading (Suspense Fallback)..."</p> }>
                <div class="pb-4">
                    <div class="stats shadow">
                        <div class="stat">
                            <div class="stat-figure text-primary">
                                <i class="ri-test-tube-fill text-5xl" />
                            </div>
                            <div class="stat-title">Experiments</div>
                                {
                                    move || {
                                        let value = experiments.get();
                                        let total_items = match value {
                                            Some(v) => v.total_items,
                                            _ => 0,
                                        };
                                        view! {
                                            <div class="stat-value">
                                                {total_items}
                                            </div>
                                        }
                                    }
                                }
                        </div>
                    </div>
                </div>
                <div class="card rounded-xl w-full bg-base-100 shadow">
                    <div class="card-body">
                        <h2 class="card-title">Experiments</h2>
                        <div>
                            {
                                move || {
                                    let value = experiments.get();
                                    match value {
                                        Some(v) => {
                                            let data = v
                                                .data
                                                .iter()
                                                .map(|ele| {
                                                    json!(ele)
                                                        .as_object()
                                                        .unwrap()
                                                        .clone()
                                                })
                                                .collect::<Vec<Map<String, Value>>>()
                                                .to_owned();
                                            view! {
                                                <Table
                                                    table_style="abc".to_string()
                                                    rows={data}
                                                    key_column="id".to_string()
                                                    columns={table_columns.get()}
                                                />
                                            }
                                        },
                                        None => {
                                            view! {<div>Loading....</div> }.into_view()
                                        }
                                    }
                                }

                            }
                        </div>
                        <div class="mt-2 flex justify-end">
                            {
                                move || {
                                    let current_page = filters.get().page.unwrap_or(0);
                                    let total_pages = match experiments.get() {
                                        Some(val) => val.total_pages,
                                        None => 0,
                                    };

                                    view! {
                                        <Pagination
                                            current_page={current_page}
                                            total_pages={total_pages}
                                            next={
                                                move || {
                                                    set_filters.update(|f| {
                                                        f.page = match f.page {
                                                            Some(p) if p < total_pages => Some(p + 1),
                                                            Some(p) => Some(p),
                                                            None => None,
                                                        }
                                                    });
                                                }
                                            }
                                            previous={
                                                move || {
                                                    set_filters.update(|f| {
                                                        f.page = match f.page {
                                                            Some(p) if p > 1 => Some(p - 1),
                                                            Some(p) => Some(p),
                                                            None => None,
                                                        }
                                                    });
                                                }
                                            }
                                        />
                                    }
                                }
                            }
                        </div>
                    </div>
                </div>
            </Suspense>
        </div>
    }
}
