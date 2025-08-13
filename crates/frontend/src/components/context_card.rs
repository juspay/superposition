use std::ops::Deref;

use leptos::*;
use serde_json::{Map, Value};
use superposition_types::{
    api::workspace::WorkspaceResponse, database::models::cac::Context,
};

use crate::{
    components::{
        condition_pills::Condition as ConditionComponent,
        description_icon::InfoDescription,
        table::{types::Column, Table},
    },
    logic::Conditions,
};

#[component]
fn option(
    label: String,
    icon: String,
    #[prop(into)] on_click: Callback<(), ()>,
    #[prop(default = "text-blue-500".to_string())] icon_color_class: String,
) -> impl IntoView {
    view! {
        <li on:click=move |_| on_click.call(())>
            <div class="flex gap-2">
                <i class=format!("w-fit {icon} ri-lg {icon_color_class}") />
                <span>{label}</span>
            </div>
        </li>
    }
}

#[component]
fn context_options(
    #[prop(into)] handle_create_experiment: Callback<(), ()>,
    #[prop(into)] handle_delete_experiment: Callback<(), ()>,
    #[prop(into)] handle_clone: Callback<(), ()>,
    #[prop(into)] handle_edit: Callback<(), ()>,
    #[prop(into)] handle_delete: Callback<(), ()>,
) -> impl IntoView {
    let node_ref = create_node_ref::<html::Input>();

    view! {
        <div class="w-fit dropdown dropdown-left">
            <label
                tabindex="0"
                class="btn btn-sm text-xs m-1 w-full"
                on:click:undelegated=move |_| {
                    if let Some(element) = node_ref.get() {
                        let _ = element.focus();
                    }
                }
            >
                <i class="ri-more-2-fill" />
            </label>
            <ul
                tabindex="0"
                class="dropdown-content z-[999999] menu w-[350px] flex-nowrap p-2 shadow bg-base-100 rounded-box overflow-x-hidden"
            >
                <Option
                    label="Update Overrides via Experiment".to_string()
                    icon="ri-test-tube-line".to_string()
                    on_click=handle_create_experiment
                />
                <Option
                    label="Delete Overrides via Experiment".to_string()
                    icon="ri-delete-row".to_string()
                    on_click=handle_delete_experiment
                />
                <Option
                    label="Update Overrides".to_string()
                    icon="ri-pencil-line".to_string()
                    on_click=handle_edit
                />
                <Option
                    label="Clone Overrides".to_string()
                    icon="ri-file-copy-line".to_string()
                    on_click=handle_clone
                />
                <Option
                    label="Delete Overrides".to_string()
                    icon="ri-delete-bin-5-line".to_string()
                    icon_color_class="text-red-500".to_string()
                    on_click=handle_delete
                />
            </ul>

        </div>
    }
}

#[component]
pub fn context_card(
    context: Context,
    overrides: Map<String, Value>,
    #[prop(default = true)] show_actions: bool,
    #[prop(into)] handle_create_experiment: Callback<String, ()>,
    #[prop(into)] handle_delete_experiment: Callback<String, ()>,
    #[prop(into)] handle_edit: Callback<String, ()>,
    #[prop(into)] handle_clone: Callback<String, ()>,
    #[prop(into)] handle_delete: Callback<String, ()>,
) -> impl IntoView {
    let workspace_settings = use_context::<StoredValue<WorkspaceResponse>>().unwrap();
    let conditions: Conditions = (&context).try_into().unwrap_or_default();
    let description = context.description.clone();
    let change_reason = context.change_reason.clone();

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

    let context = store_value(context);

    let table_columns = vec![
        Column::default_no_collapse("KEY".to_string()),
        Column::default("VALUE".to_string()),
    ];

    let actions_supported =
        show_actions && !conditions.0.iter().any(|c| c.variable == "variantIds");

    cfg_if::cfg_if! {
        if #[cfg(feature = "jsonlogic")] {
            let edit_unsupported = conditions
                .0
                .iter()
                .any(|c| matches!(c.expression, crate::logic::Expression::Other(_, _)));
        } else {
            let edit_unsupported = false;
        }
    }

    view! {
        <div class="rounded-lg shadow bg-base-100 p-6 flex flex-col gap-4">
            <div class="flex justify-between">
                <div class="flex gap-4 items-center">
                    <h3 class="card-title text-base timeline-box text-gray-800 bg-base-100 shadow-md m-0 w-max">
                        "Condition"
                    </h3>
                    <InfoDescription
                        description=description.deref().to_string()
                        change_reason=change_reason.deref().to_string()
                    />
                    <div class="group relative inline-block text-xs text-gray-700 cursor-pointer">
                        <div class="z-[1000] hidden absolute top-full left-1/2 p-2.5 group-hover:flex flex-col gap-4 bg-white rounded shadow-[0_4px_6px_rgba(0,0,0,0.1)] whitespace-normal translate-x-[20px] -translate-y-1/2">
                            <div class="flex flex-col gap-1">
                                <div class="font-bold">"Created"</div>
                                <div class="flex gap-1 items-center">
                                    <i class="ri-user-line text-gray-950" />
                                    <span>{context.with_value(|c| c.created_by.clone())}</span>
                                </div>
                                <div class="flex gap-1 items-center">
                                    <i class="ri-time-line text-gray-950" />
                                    <span>
                                        {context
                                            .with_value(|c| c.created_at.format("%v %T").to_string())}
                                    </span>
                                </div>
                            </div>
                            <div class="flex flex-col gap-1">
                                <div class="font-bold">"Last Modified"</div>
                                <div class="flex gap-1 items-center">
                                    <i class="ri-user-line text-gray-950" />
                                    <span>
                                        {context.with_value(|c| c.last_modified_by.clone())}
                                    </span>
                                </div>
                                <div class="flex gap-1 items-center">
                                    <i class="ri-time-line text-gray-950" />
                                    <span>
                                        {context
                                            .with_value(|c| {
                                                c.last_modified_at.format("%v %T").to_string()
                                            })}
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
                            <ContextOptions
                                handle_create_experiment=move |_| {
                                    handle_create_experiment
                                        .call(context.with_value(|c| c.id.clone()))
                                }
                                handle_delete_experiment=move |_| {
                                    handle_delete_experiment
                                        .call(context.with_value(|c| c.id.clone()))
                                }
                                handle_clone=move |_| {
                                    handle_clone.call(context.with_value(|c| c.id.clone()))
                                }
                                handle_edit=move |_| {
                                    handle_edit.call(context.with_value(|c| c.id.clone()))
                                }
                                handle_delete=move |_| {
                                    handle_delete.call(context.with_value(|c| c.id.clone()))
                                }
                            />
                        </Show>
                        <Show when=move || edit_unsupported>
                            <span class="badge badge-warning text-xs flex items-center">
                                {"Edit Unsupported"}
                            </span>
                        </Show>
                    </div>
                </Show>
                <Show when=move || !actions_supported>
                    <span class="badge badge-warning text-xs flex items-center">
                        {"Edit Unsupported"}
                    </span>
                </Show>
            </div>

            <div class="pl-5">
                <ConditionComponent
                    // Clone only once before reusing in multiple closures
                    conditions=conditions
                    id=context.with_value(|c| c.id.clone())
                    class="xl:w-[400px] h-fit"
                    strict_mode=workspace_settings.with_value(|w| w.strict_mode)
                />
                <Table rows=override_table_rows key_column="KEY" columns=table_columns />
            </div>
        </div>
    }
}
