use leptos::*;
use leptos_router::A;
use serde_json::{Map, Value};
use superposition_types::Context;

use crate::{
    components::{
        condition_pills::Condition as ConditionComponent,
        table::{types::Column, Table},
    },
    logic::{Conditions, Expression},
};

#[component]
pub fn context_card(
    context: Context,
    overrides: Map<String, Value>,
    #[prop(default = true)] show_actions: bool,
    #[prop(default=Callback::new(|_| {}))] handle_delete: Callback<String, ()>,
) -> impl IntoView {
    let conditions: Conditions = (&context).try_into().unwrap_or_default();

    let override_table_rows = overrides
        .clone()
        .into_iter()
        .map(|(k, v)| {
            Map::from_iter(vec![
                (String::from("KEY"), Value::String(k.clone())),
                (String::from("VALUE"), v),
            ])
        })
        .collect::<Vec<Map<String, Value>>>();

    let context_id = store_value(context.id.clone());

    let table_columns = vec![
        Column::default("KEY".to_string()),
        Column::default("VALUE".to_string()),
    ];

    let actions_supported =
        show_actions && !conditions.0.iter().any(|c| c.variable == "variantIds");

    let edit_unsupported = conditions
        .0
        .iter()
        .any(|c| matches!(c.expression, Expression::Other(_, _)));

    view! {
        <div class="rounded-lg shadow bg-base-100 p-6 flex flex-col gap-4">
            <div class="flex justify-between">
                <h3 class="card-title text-base timeline-box text-gray-800 bg-base-100 shadow-md font-mono m-0 w-max">
                    "Condition"
                </h3>
                <Show when=move || actions_supported>
                    <div class="h-fit text-right space-x-4">
                        <Show when=move || !edit_unsupported>
                            <A href=format!("{}/update", context_id.get_value())>
                                <i class="ri-pencil-line ri-lg text-blue-500 cursor-pointer"></i>
                            </A>

                            <A href=format!("new?clone_from={}", context_id.get_value())>
                                <i class="ri-file-copy-line ri-lg text-blue-500 cursor-pointer"></i>
                            </A>

                        </Show>
                        <Show when=move || edit_unsupported>
                            <span class="badge badge-warning text-xs ml-2 flex items-center">
                                {"Edit Unsupported"}
                            </span>
                        </Show>
                        <i
                            class="ri-delete-bin-5-line ri-lg text-red-500 cursor-pointer"
                            on:click=move |_| {
                                let context_id = context_id.get_value();
                                handle_delete.call(context_id);
                            }
                        ></i>

                    </div>
                </Show>
                <Show when=move || !actions_supported>
                    <span class="badge badge-warning text-xs ml-2 flex items-center">
                        {"Edit Unsupported"}
                    </span>
                </Show>
            </div>

            <div class="pl-5">
                <ConditionComponent
                    conditions=conditions
                    id=context_id.get_value()
                    class="xl:w-[400px] h-fit"
                />
                <Table
                    cell_class="min-w-48 font-mono".to_string()
                    rows=override_table_rows
                    key_column="KEY".to_string()
                    columns=table_columns
                />
            </div>
        </div>
    }
}
