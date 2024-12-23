pub mod types;
pub mod utils;

use leptos::*;
use serde_json::{json, Value};
use superposition_types::{
    custom_query::PaginationParams,
    database::models::cac::{Function, TypeTemplate},
};
use types::{DimensionCreateReq, DimensionUpdateReq};
use utils::{create_dimension, update_dimension};
use web_sys::MouseEvent;

use crate::components::{
    dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
    input::{Input, InputType},
};
use crate::providers::editor_provider::EditorProvider;
use crate::schema::{JsonSchemaType, SchemaType};
use crate::types::FunctionsName;
use crate::{api::fetch_functions, components::button::Button};
use crate::{
    api::fetch_types,
    types::{OrganisationId, Tenant},
};

#[component]
pub fn dimension_form<NF>(
    #[prop(default = false)] edit: bool,
    #[prop(default = 0)] position: u32,
    #[prop(default = String::new())] dimension_name: String,
    #[prop(default = String::new())] dimension_type: String,
    #[prop(default = Value::Null)] dimension_schema: Value,
    #[prop(default = None)] function_name: Option<Value>,
    handle_submit: NF,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();

    let (position_rs, position_ws) = create_signal(position);
    let (dimension_name_rs, dimension_name_ws) = create_signal(dimension_name);
    let (dimension_type_rs, dimension_type_ws) = create_signal(dimension_type);
    let (dimension_schema_rs, dimension_schema_ws) = create_signal(dimension_schema);
    let (function_name, set_function_name) = create_signal(function_name);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);
    let functions_resource: Resource<(String, String), Vec<Function>> =
        create_blocking_resource(
            move || (tenant_rws.get().0, org_rws.get().0),
            |(current_tenant, org)| async move {
                fetch_functions(&PaginationParams::all_entries(), current_tenant, org)
                    .await
                    .map_or_else(|_| vec![], |list| list.data)
            },
        );

    let type_template_resource = create_blocking_resource(
        move || (tenant_rws.get().0, org_rws.get().0),
        |(current_tenant, org)| async move {
            fetch_types(&PaginationParams::all_entries(), current_tenant, org)
                .await
                .map_or_else(|_| vec![], |response| response.data)
        },
    );

    let handle_select_dropdown_option =
        Callback::new(move |selected_function: FunctionsName| {
            set_function_name.update(|value| {
                let function_name = selected_function.clone();
                leptos::logging::log!("function selected: {:?}", function_name);
                let fun_name = match function_name.as_str() {
                    "None" => None,
                    _ => Some(json!(function_name)),
                };
                *value = fun_name;
            });
        });

    let (error_message, set_error_message) = create_signal("".to_string());

    let on_submit = move |ev: MouseEvent| {
        req_inprogress_ws.set(true);
        ev.prevent_default();
        let function_position = position_rs.get();
        let dimension_name = dimension_name_rs.get();
        let function_name = function_name.get();
        let function_schema = dimension_schema_rs.get();

        let handle_submit_clone = handle_submit.clone();
        spawn_local({
            let handle_submit = handle_submit_clone;
            async move {
                let result = if edit {
                    let update_payload = DimensionUpdateReq {
                        position: Some(function_position),
                        schema: Some(function_schema),
                        function_name: function_name,
                    };
                    update_dimension(
                        tenant_rws.get().0,
                        dimension_name,
                        update_payload,
                        org_rws.get().0,
                    )
                    .await
                } else {
                    let create_payload = DimensionCreateReq {
                        dimension: dimension_name,
                        position: function_position,
                        schema: function_schema,
                        function_name: function_name,
                    };
                    create_dimension(tenant_rws.get().0, create_payload, org_rws.get().0)
                        .await
                };
                match result {
                    Ok(_) => {
                        handle_submit();
                    }
                    Err(e) => {
                        set_error_message.set(e);
                        // Handle error
                        // Consider logging or displaying the error
                    }
                }
                req_inprogress_ws.set(false);
            }
        });
    };
    view! {
        <form class="form-control w-full space-y-4 bg-white text-gray-700 font-mono">
            <div class="form-control">
                <label class="label">
                    <span class="label-text">Dimension</span>
                </label>
                <input
                    disabled=edit
                    type="text"
                    placeholder="Dimension"
                    class="input input-bordered w-full max-w-md"
                    value=dimension_name_rs.get()
                    on:change=move |ev| {
                        let value = event_target_value(&ev);
                        dimension_name_ws.set(value);
                    }
                />

            </div>

            <div class="divider"></div>

            <Suspense>
                {move || {
                    let options = type_template_resource.get().unwrap_or(vec![]);
                    let dimension_t = if dimension_type_rs.get().is_empty() && edit {
                        "change current type template".into()
                    } else if dimension_type_rs.get().is_empty() && !edit {
                        "choose a type template".into()
                    } else {
                        dimension_type_rs.get()
                    };
                    let dimension_type_schema = SchemaType::Single(JsonSchemaType::from(&dimension_schema_rs.get()));
                    view! {
                        <div class="form-control">
                            <label class="label">
                                <span class="label-text">Set Schema</span>
                            </label>
                            <Dropdown
                                dropdown_width="w-100"
                                dropdown_icon="".to_string()
                                dropdown_text=dimension_t
                                dropdown_direction=DropdownDirection::Down
                                dropdown_btn_type=DropdownBtnType::Select
                                dropdown_options=options
                                on_select=Callback::new(move |selected_item: TypeTemplate| {
                                    logging::log!("selected item {:?}", selected_item);
                                    dimension_type_ws.set(selected_item.type_name);
                                    dimension_schema_ws.set(selected_item.type_schema);
                                })
                            />
                            <EditorProvider>
                                <Input
                                    id="type-schema"
                                    class="mt-5 rounded-md resize-y w-full max-w-md pt-3"
                                    schema_type=dimension_type_schema
                                    value=dimension_schema_rs.get()
                                    on_change=Callback::new(move |new_type_schema| dimension_schema_ws.set(new_type_schema))
                                    r#type=InputType::Monaco
                                />
                            </EditorProvider>
                        </div>
                    }
                }}

            </Suspense>
            <div class="divider"></div>

            {move || {
                view! {
                    <div class="form-control">
                        <label class="label">
                            <span class="label-text">Position</span>
                        </label>
                        <input
                            type="Number"
                            min=0
                            placeholder="Position"
                            class="input input-bordered w-full max-w-md"
                            value=position_rs.get()
                            on:keypress=move |ev| {
                                let char_code = ev.char_code();
                                if char_code != 0 && char_code != 8 && char_code != 13
                                    && !(char_code >= 48 && char_code <= 57)
                                {
                                    ev.prevent_default();
                                }
                            }

                            on:change=move |ev| {
                                logging::log!("{:?}", event_target_value(& ev).parse::< u32 > ());
                                match event_target_value(&ev).parse::<u32>() {
                                    Ok(i_prio) => position_ws.set(i_prio),
                                    Err(e) => {
                                        position_ws.set(0);
                                        logging::log!("{e}");
                                    }
                                };
                            }
                        />

                    </div>
                }
            }}

            <Suspense>
                {move || {
                    let mut functions = functions_resource.get().unwrap_or_default();
                    let mut function_names: Vec<FunctionsName> = vec!["None".to_string()];
                    functions.sort_by(|a, b| a.function_name.cmp(&b.function_name));
                    functions
                        .into_iter()
                        .for_each(|ele| {
                            function_names.push(ele.function_name);
                        });
                    view! {
                        <div class="form-control">
                            <div class="gap-1">
                                <label class="label flex-col justify-center items-start">
                                    <span class="label-text">Function Name</span>
                                    <span class="label-text text-slate-400">
                                        Assign Function validation to your key
                                    </span>
                                </label>
                            </div>

                            <div class="mt-2">
                                <Dropdown
                                    dropdown_width="w-100"
                                    dropdown_icon="".to_string()
                                    dropdown_text=function_name
                                        .get()
                                        .and_then(|v| match v {
                                            Value::String(s) => Some(s),
                                            _ => None,
                                        })
                                        .map_or("Add Function".to_string(), |v| v.to_string())
                                    dropdown_direction=DropdownDirection::Down
                                    dropdown_btn_type=DropdownBtnType::Select
                                    dropdown_options=function_names
                                    on_select=handle_select_dropdown_option
                                />
                            </div>
                        </div>
                    }
                }}

            </Suspense>

            <div class="form-control grid w-full justify-start">
                {move || {
                    let loading = req_inprogess_rs.get();
                    view! {
                        <Button
                            class="pl-[70px] pr-[70px] w-48 h-12".to_string()
                            text="Submit".to_string()
                            on_click=on_submit.clone()
                            loading
                        />
                    }
                }}

            </div>

            {
                view! {
                    <div>
                        <p class="text-red-500">{move || error_message.get()}</p>
                    </div>
                }
            }

        </form>
    }
}
