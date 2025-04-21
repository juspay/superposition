use leptos::*;
use serde_json::{Map, Value};
use superposition_types::database::models::cac::Context;

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
    #[prop(default=Callback::new(|_| {}))] handle_create_experiment: Callback<
        (Context, Map<String, Value>),
        (),
    >,
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
            Map::from_iter(vec![
                (String::from("KEY"), Value::String(k.clone())),
                (String::from("VALUE"), v),
            ])
        })
        .collect::<Vec<Map<String, Value>>>();

    // Clone context and overrides for use in event handlers
    let context_id = store_value(context.id.clone());
    let context = store_value(context);
    let overrides = store_value(overrides);

    let table_columns = vec![
        Column::default_no_collapse("KEY".to_string()),
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
                <div class="flex gap-4 items-center">
                    <h3 class="card-title text-base timeline-box text-gray-800 bg-base-100 shadow-md font-mono m-0 w-max">
                        "Condition"
                    </h3>
                    <div class="group relative inline-block text-xs text-gray-700 cursor-pointer">
                        <div class="z-[1000] hidden absolute top-full left-1/2 p-2.5 group-hover:flex flex-col gap-4 bg-white rounded shadow-[0_4px_6px_rgba(0,0,0,0.1)] whitespace-normal translate-x-[20px] -translate-y-1/2">
                            <div class="flex flex-col gap-1">
                                <div class="font-bold">"Created"</div>
                                <div class="flex gap-1 items-center">
                                    <i class="ri-user-line text-gray-950" />
                                    <span>{context.get_value().created_by}</span>
                                </div>
                                <div class="flex gap-1 items-center">
                                    <i class="ri-time-line text-gray-950" />
                                    <span>
                                        {context
                                            .get_value()
                                            .created_at
                                            .format("%v %T")
                                            .to_string()}
                                    </span>
                                </div>
                            </div>
                            <div class="flex flex-col gap-1">
                                <div class="font-bold">"Last Modified"</div>
                                <div class="flex gap-1 items-center">
                                    <i class="ri-user-line text-gray-950" />
                                    <span>{context.get_value().last_modified_by}</span>
                                </div>
                                <div class="flex gap-1 items-center">
                                    <i class="ri-time-line text-gray-950" />
                                    <span>
                                        {context.get_value().last_modified_at.format("%v %T").to_string()}
                                    </span>
                                </div>
                            </div>
                        </div>
                        <i class="ri-information-line ri-lg" />
                    </div>
                </div>
                <Show when=move || actions_supported>
                    <div class="h-fit flex gap-4 text-right">
                        <Show when=move || !edit_unsupported>
                            <i
                                class="ri-test-tube-line ri-lg text-blue-500 cursor-pointer"
                                on:click=move |_| {
                                    handle_create_experiment
                                        .call((context.get_value(), overrides.get_value()));
                                }
                            />
                            <i
                                class="ri-pencil-line ri-lg text-blue-500 cursor-pointer"
                                on:click=move |_| {
                                    handle_edit.call((context.get_value(), overrides.get_value()));
                                }
                            />
                            <i
                                class="ri-file-copy-line ri-lg text-blue-500 cursor-pointer"
                                on:click=move |_| {
                                    handle_clone.call((context.get_value(), overrides.get_value()));
                                }
                            />
                            <i
                                class="ri-delete-bin-5-line ri-lg text-red-500 cursor-pointer"
                                on:click=move |_| {
                                    let context_id = context_id.get_value();
                                    handle_delete.call(context_id);
                                }
                            />
                        </Show>
                        <Show when=move || edit_unsupported>
                            <span class="badge badge-warning text-xs ml-2 flex items-center">
                                {"Edit Unsupported"}
                            </span>
                        </Show>
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
                    rows=override_table_rows
                    key_column="KEY".to_string()
                    columns=table_columns
                />
            </div>
        </div>
    }
}
