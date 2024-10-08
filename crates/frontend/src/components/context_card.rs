use leptos::*;
use serde_json::{Map, Value};

use crate::{
    components::{
        condition_pills::Condition as ConditionComponent,
        table::{types::Column, Table},
    },
    logic::{Conditions, Operator},
    types::Context,
};

#[component]
pub fn context_card(
    context: Context,
    overrides: Map<String, Value>,
    #[prop(default = true)] show_actions: bool,
    #[prop(default=Callback::new(|_| {}))] handle_edit: Callback<
        (Context, Map<String, Value>),
        (),
    >,
    #[prop(default=Callback::new(|_| {}))] handle_clone: Callback<
        (Context, Map<String, Value>),
        (),
    >,
    #[prop(default=Callback::new(|_| {}))] handle_delete: Callback<String, ()>,
) -> impl IntoView {
    let conditions: Conditions = (&context).try_into().unwrap_or_default();

    let override_table_rows = overrides
        .clone()
        .into_iter()
        .map(|(k, v)| {
            let k = Value::String(k.trim_matches('"').to_string());
            let v = Value::String(format!("{}", v).trim_matches('"').to_string());
            Map::from_iter(vec![(String::from("KEY"), k), (String::from("VALUE"), v)])
        })
        .collect::<Vec<Map<String, Value>>>();

    // Clone context and overrides for use in event handlers
    let context_id = store_value(context.id.clone());
    let context = store_value(context);
    let overrides = store_value(overrides);

    let table_columns = vec![
        Column::default("KEY".to_string()),
        Column::default("VALUE".to_string()),
    ];

    let actions_supported = show_actions
        && !conditions
            .iter()
            .any(|condition| condition.dimension == "variantIds");

    let edit_unsupported = conditions
        .iter()
        .any(|condition| matches!(condition.operator, Operator::Other(_)));

    view! {
        <div class="rounded-lg shadow bg-base-100 p-6 flex flex-col gap-4">
            <div class="flex justify-between">
                <h3 class="card-title text-base timeline-box text-gray-800 bg-base-100 shadow-md font-mono m-0 w-max">
                    "Condition"
                </h3>
                <Show when=move || actions_supported>
                    <div class="h-fit text-right space-x-4">
                        <Show when=move || !edit_unsupported>
                            <i
                                class="ri-pencil-line ri-lg text-blue-500 cursor-pointer"
                                on:click=move |_| {
                                    handle_edit.call((context.get_value(), overrides.get_value()));
                                }
                            ></i>

                            <i
                                class="ri-file-copy-line ri-lg text-blue-500 cursor-pointer"
                                on:click=move |_| {
                                    handle_clone.call((context.get_value(), overrides.get_value()));
                                }
                            ></i>

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
                    // Clone only once before reusing in multiple closures
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
