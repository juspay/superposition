use leptos::*;
use serde_json::{Map, Value};

use crate::{
    components::{
        condition_pills::{types::Condition, ConditionPills, Condition as ConditionComponent},
        table::{types::Column, Table},
    },
    types::Context,
};

#[component]
pub fn context_card(
    context: Context,
    overrides: Map<String, Value>,
    handle_edit: Callback<(Context, Map<String, Value>), ()>,
    handle_clone: Callback<(Context, Map<String, Value>), ()>,
    handle_delete: Callback<String, ()>,
) -> impl IntoView {
    let conditions: Vec<Condition> = (&context).try_into().unwrap_or(vec![]);
    let override_table_rows = overrides
        .clone()
        .into_iter()
        .map(|(k, v)| {
            let k = Value::String(k.trim_matches('"').to_string());
            let v = Value::String(format!("{}", v).trim_matches('"').to_string());
            Map::from_iter(vec![(String::from("KEY"), k), (String::from("VALUE"), v)])
        })
        .collect::<Vec<Map<String, Value>>>();

    let context = StoredValue::new(context);
    let overrides = StoredValue::new(overrides);

    let table_columns = vec![
        Column::default("KEY".to_string()),
        Column::default("VALUE".to_string()),
    ];

    view! {
        <div class="rounded-lg shadow bg-base-100 p-6 shadow">
            <div class="flex justify-between">
                <h3 class="card-title text-base timeline-box text-gray-800 bg-base-100 shadow-md font-mono m-0 w-max">
                    "Condition"
                </h3>
                <div class="h-fit text-right space-x-4">
                    // Action button section
                    <i
                        class="ri-pencil-line ri-lg text-blue-500 cursor-pointer"
                        on:click=move |_| {
                            handle_edit.call((context.get_value(), overrides.get_value()))
                        }
                    >
                    </i>
                    <i
                        class="ri-file-copy-line ri-lg text-blue-500 cursor-pointer"
                        on:click=move |_| {
                            handle_clone.call((context.get_value(), overrides.get_value()))
                        }
                    >
                    </i>
                    <i
                        class="ri-delete-bin-5-line ri-lg text-red-500 cursor-pointer"
                        on:click=move |_| { handle_delete.call(context.get_value().id) }
                    ></i>
                </div>
            </div>

            <div class="xl:flex xl:gap-x-4 xl:justify-between mt-3">
                <div class="xl:w-[400px]">
                    <ConditionComponent conditions=conditions/>
                </div>
                // <div class="xl:block xs:hidden border-r border-slate-200"/>
                <div class="xl:w-2/3 overflow-auto">
                    <Table
                        cell_style="min-w-48 font-mono".to_string()
                        rows=override_table_rows
                        key_column="id".to_string()
                        columns=table_columns
                    />
                </div>
            </div>
        // <div class="flex justify-between">
        // <div class="flex items-center flex-wrap space-x-4">
        // <h3 class="card-title text-base timeline-box text-gray-800 bg-base-100 shadow-md font-mono">
        // "Condition"
        // </h3>
        // <i class="ri-arrow-right-fill ri-xl text-blue-500"></i>
        // <ConditionPills conditions=conditions/>
        // </div>
        // <div class="flex space-x-4">
        // <i
        // class="ri-pencil-line ri-xl text-blue-500 cursor-pointer"
        // on:click=move |_| {
        // handle_edit.call((context.get_value(), overrides.get_value()))
        // }
        // >
        // </i>
        // <i
        // class="ri-file-copy-line ri-xl text-blue-500 cursor-pointer"
        // on:click=move |_| {
        // handle_clone.call((context.get_value(), overrides.get_value()))
        // }
        // >
        // </i>
        // <i
        // class="ri-delete-bin-5-line ri-xl text-blue-500 cursor-pointer"
        // on:click=move |_| { handle_delete.call(context.get_value().id) }
        // ></i>
        // </div>
        // </div>
        // <div class="space-x-4">
        // <Table
        // cell_style="min-w-48 font-mono".to_string()
        // rows=override_table_rows
        // key_column="id".to_string()
        // columns=table_columns
        // />
        // </div>
        </div>
    }
}
