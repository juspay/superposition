use leptos::*;
use leptos_router::*;
use leptos::logging::*;

use crate::components::table::{table::Table, types::Column};

use crate::pages::ExperimentList::types::{
    ExperimentResponse, ExperimentsResponse, ListFilters,
};

use super::utils::fetch_experiments;
use serde_json::{json, Map, Value};

// pub struct ListFilters {
//     pub status: Option<StatusTypes>,
//     pub from_date: Option<DateTime<Utc>>,
//     pub to_date: Option<DateTime<Utc>>,
//     pub page: Option<i64>,
//     pub count: Option<i64>,
// }

#[component]
pub fn ExperimentList() -> impl IntoView {
    // acquire tenant
    let tenant = "test".to_string();
    let (filters, set_filters) = create_signal(ListFilters {
        status: None,
        from_date: None,
        to_date: None,
        page: None,
        count: None,
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
        move || filters,
        move |_| fetch_experiments(filters.get()),
    );
    // TODO: Add filters
    view! {
        <div class="p-8">
            <Suspense fallback=move || view! {<p>"Loading (Suspense Fallback)..."</p> }>
                <div class="py-4">
                    <div class="stats shadow">
                        <div class="stat">
                            <div class="stat-figure text-primary">
                                <i class="ri-test-tube-fill text-5xl" />
                            </div>
                            <div class="stat-title">Experiments</div>
                                {
                                    move || {
                                        experiments.with(
                                            move |value| {
                                                match value {
                                                    Some(Ok(val)) => view! {
                                                        <div class="stat-value">
                                                            {val.total_items}
                                                        </div>
                                                    }.into_view(),
                                                    _ => view! {
                                                        <div class="stat-value">
                                                            0
                                                        </div>
                                                    }.into_view()
                                                }
                                            }
                                        )
                                    }
                                }
                        </div>
                    </div>
                </div>
                <div class="card rounded-lg w-full bg-base-100 shadow">
                    <div class="card-body">
                        <h2 class="card-title">Experiments</h2>
                            <div>
                                {
                                    move || experiments.with(move |value| {
                                        log!("{:?}", value);
                                        match value {
                                            Some(Ok(value)) => {
                                                // TODO: Why data.clone() works?
                                                let data = value
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
                                            Some(Err(e)) => view! {<div>{e}</div> }.into_view(),
                                            None => view! {<div>Loading....</div> }.into_view(),
                                        }
                                    })
                                }
                            </div>
                    </div>
                </div>
            </Suspense>
        </div>
    }
}
