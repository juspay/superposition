use leptos::logging::*;
use leptos::*;
use leptos_dom::*;

use chrono::{prelude::Utc, TimeZone};

use crate::components::{
    experiment_form::experiment_form::ExperimentForm,
    pagination::pagination::Pagination,
    stat::stat::Stat,
    table::{
        table::Table,
        types::{Column, TableSettings},
    },
};

use crate::pages::ExperimentList::types::{ExperimentsResponse, ListFilters};

use super::utils::{fetch_default_config, fetch_dimensions, fetch_experiments};
use serde_json::{json, Map, Value};
use wasm_bindgen::JsCast;

#[component]
pub fn ExperimentList() -> impl IntoView {
    // acquire tenant
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let (filters, set_filters) = create_signal(ListFilters {
        status: None,
        from_date: Utc.timestamp_opt(0, 0).single(),
        to_date: Utc.timestamp_opt(4130561031, 0).single(),
        page: Some(1),
        count: Some(10),
    });
    let (open_form_modal, set_open_form_modal) = create_signal(false);

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
        move || (tenant_rs.get(), filters.get()),
        |(current_tenant, value)| async move {
            match fetch_experiments(value, &current_tenant).await {
                Ok(data) => data,
                Err(_) => ExperimentsResponse {
                    total_items: 0,
                    total_pages: 0,
                    data: vec![],
                },
            }
        },
    );

    let dimensions = create_blocking_resource(
        || (),
        |_| async move {
            match fetch_dimensions().await {
                Ok(data) => data,
                Err(_) => vec![],
            }
        },
    );

    let default_config = create_blocking_resource(
        || (),
        |_| async move {
            match fetch_default_config().await {
                Ok(data) => data,
                Err(_) => vec![],
            }
        },
    );

    let handle_submit_experiment_form = move || {
        experiments.refetch();
        set_open_form_modal.set(false);
        if let Some(element) = document().get_element_by_id("create_exp_modal") {
            let dialog_ele = element.dyn_ref::<web_sys::HtmlDialogElement>();
            match dialog_ele {
                Some(ele) => { ele.close(); },
                None => { log!("no modal element"); }
            }
        }
    };

    // TODO: Add filters
    view! {
        <div class="p-8">
            <Suspense fallback=move || view! { <p>"Loading (Suspense Fallback)..."</p> }>
                <div class="pb-4">
                    {
                        move || {
                            let value = experiments.get();
                            let total_items = match value {
                                Some(v) => v.total_items.to_string(),
                                _ => "0".to_string(),
                            };

                            view! {
                                <Stat
                                    heading="Experiments"
                                    icon="ri-test-tube-fill"
                                    number={total_items}
                                />
                            }
                        }
                    }
                </div>
                <div class="card rounded-xl w-full bg-base-100 shadow">
                    <div class="card-body">
                        <div class="flex justify-between">
                            <h2 class="card-title">Experiments</h2>
                            <div>
                                <button
                                    class="text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 hover:bg-gradient-to-br focus:ring-4 focus:outline-none focus:ring-purple-300 dark:focus:ring-purple-800 shadow-lg shadow-purple-500/50 dark:shadow-lg dark:shadow-purple-800/80 font-medium rounded-lg text-sm px-5 py-2.5 text-center me-2 mb-2"
                                    on:click=move |event: web_sys::MouseEvent| {
                                        event.prevent_default();
                                        set_open_form_modal.set(true);
                                        if let Some(element) = document().get_element_by_id("create_exp_modal") {
                                            log!("opening the experiment modal");
                                            let dialog_ele = element.dyn_ref::<web_sys::HtmlDialogElement>();
                                            match dialog_ele {
                                                Some(ele) => { ele.show_modal(); },
                                                None => { log!("no modal element"); }
                                            }
                                        }
                                    }
                                >
                                    Create Experiment
                                    <i class="ri-edit-2-line ml-2"></i>
                                </button>
                            </div>
                        </div>
                        <div>

                            {move || {
                                let current_tenant = tenant_rs.get();
                                let settings = TableSettings {
                                    redirect_prefix: Some(
                                        format!("admin/{current_tenant}/experiments"),
                                    ),
                                };
                                let value = experiments.get();
                                match value {
                                    Some(v) => {
                                        let data = v
                                            .data
                                            .iter()
                                            .map(|ele| { json!(ele).as_object().unwrap().clone() })
                                            .collect::<Vec<Map<String, Value>>>()
                                            .to_owned();
                                        view! {
                                            <Table
                                                table_style="abc".to_string()
                                                rows=data
                                                key_column="id".to_string()
                                                columns=table_columns.get()
                                                settings=settings
                                            />
                                        }
                                    }
                                    None => view! { <div>Loading....</div> }.into_view(),
                                }
                            }}

                        </div>
                        <div class="mt-2 flex justify-end">

                            {move || {
                                let current_page = filters.get().page.unwrap_or(0);
                                let total_pages = match experiments.get() {
                                    Some(val) => val.total_pages,
                                    None => 0,
                                };
                                view! {
                                    <Pagination
                                        current_page=current_page
                                        total_pages=total_pages
                                        next=move || {
                                            set_filters
                                                .update(|f| {
                                                    f
                                                        .page = match f.page {
                                                        Some(p) if p < total_pages => Some(p + 1),
                                                        Some(p) => Some(p),
                                                        None => None,
                                                    }
                                                });
                                        }

                                        previous=move || {
                                            set_filters
                                                .update(|f| {
                                                    f
                                                        .page = match f.page {
                                                        Some(p) if p > 1 => Some(p - 1),
                                                        Some(p) => Some(p),
                                                        None => None,
                                                    }
                                                });
                                        }
                                    />
                                }
                            }}

                        </div>
                    </div>
                </div>

                {move || {
                    let dim = dimensions.get().unwrap_or(vec![]);
                    let def_conf = default_config.get().unwrap_or(vec![]);
                    let open_modal = open_form_modal.get();
                    view! {
                        <Show
                            when=move || { open_form_modal.get() }
                        >
                            <dialog id="create_exp_modal" class="modal">
                                <div class="modal-box w-12/12 max-w-5xl">
                                    <div class="flex justify-between">
                                        <h3 class="font-bold text-lg">Create Experiment</h3>
                                        <div>
                                            <button
                                                on:click=move |_| {
                                                    set_open_form_modal.set(false);
                                                    if let Some(element) = document().get_element_by_id("create_exp_modal") {
                                                        log!("FOUND AND CLOSING THE FORM");
                                                        let dialog_ele = element.dyn_ref::<web_sys::HtmlDialogElement>();
                                                        match dialog_ele {
                                                            Some(ele) => { ele.close(); },
                                                            None => { log!("no modal element"); }
                                                        }
                                                    } else {
                                                        log!("outer close button no modal element");
                                                    }
                                                }
                                            >
                                                <i class="ri-close-fill"></i>
                                            </button>
                                        </div>
                                    </div>
                                    <div class="modal-action flex flex-col">
                                        <ExperimentForm
                                            name="".to_string()
                                            context=vec![]
                                            variants=vec![]
                                            dimensions=dim.clone()
                                            default_config=def_conf.clone()
                                            handle_submit=handle_submit_experiment_form
                                        />
                                    </div>
                                </div>
                            </dialog>
                        </Show>
                    }
                }}

            </Suspense>
        </div>
    }
}