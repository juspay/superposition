use leptos::logging::*;
use leptos::*;
use serde_json::{json, Map, Value};

use crate::components::table::types::TableSettings;
use crate::components::table::{table::Table, types::Column};
use crate::pages::Dimensions::helper::fetch_dimensions;

#[component]
pub fn Dimensions() -> impl IntoView {
    let dimensions = create_blocking_resource(
        move || {},
        |_value| async move {
            match fetch_dimensions().await {
                Ok(data) => data,
                Err(e) => vec![],
            }
        },
    );

    let table_columns = create_memo(move |_| {
        vec![
            Column::default("created_at".to_string()),
            Column::default("created_by".to_string()),
            Column::default("dimension".to_string()),
            Column::default("priority".to_string()),
            Column::default("schema".to_string()),
        ]
    });

    view! {
        <div class="p-8">
            <Suspense fallback=move || view! { <p>"Loading (Suspense Fallback)..."</p> }>
                <div class="pb-4">
                    <div class="stats shadow">
                        <div class="stat">
                            <div class="stat-figure text-primary">
                                <i class="ri-ruler-2-fill text-5xl"></i>
                            </div>
                            <div class="stat-title">Dimensions</div>

                            {move || {
                                let value = dimensions.get();
                                let total_items = match value {
                                    Some(v) => v.len(),
                                    _ => 0,
                                };
                                view! { <div class="stat-value">{total_items}</div> }
                            }}

                        </div>
                    </div>
                </div>

                <div class="card rounded-xl w-full bg-base-100 shadow">
                    <div class="card-body">
                        <h2 class="card-title">Dimensions</h2>
                        <div>

                            {move || {
                                let value = dimensions.get();
                                match value {
                                    Some(v) => {
                                        let data = v
                                            .iter()
                                            .map(|ele| { json!(ele).as_object().unwrap().clone() })
                                            .collect::<Vec<Map<String, Value>>>()
                                            .to_owned();
                                        println!("hello1: {:?}", data);
                                        view! {
                                            <Table
                                                _table_style="abc".to_string()
                                                rows=data
                                                _key_column="id".to_string()
                                                columns=table_columns.get()
                                                settings={TableSettings {redirect_prefix: None}}
                                            />
                                        }
                                    }
                                    None => view! { <div>Loading....</div> }.into_view(),
                                }
                            }}

                        </div>
                    </div>
                </div>
            </Suspense>
        </div>
    }
}
