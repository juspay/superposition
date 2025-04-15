pub mod types;
pub mod utils;

use leptos::*;
use serde_json::{json, Value};
use superposition_types::{
    custom_query::PaginationParams,
    database::models::cac::{Function, TypeTemplate},
};
use web_sys::MouseEvent;

use crate::{
    api::{fetch_functions, fetch_types},
    components::{
        alert::AlertType,
        button::Button,
        dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
        input::{Input, InputType},
    },
    schema::{EnumVariants, HtmlDisplay, JsonSchemaType, SchemaType},
    types::FunctionsName,
};
use crate::{
    providers::{alert_provider::enqueue_alert, editor_provider::EditorProvider},
    types::{OrganisationId, Tenant},
};

use self::{
    types::{DefaultConfigCreateReq, DefaultConfigUpdateReq},
    utils::{create_default_config, update_default_config},
};

#[component]
pub fn default_config_form<NF>(
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] config_key: String,
    #[prop(default = String::new())] config_type: String,
    #[prop(default = Value::Null)] type_schema: Value,
    #[prop(default = Value::Null)] config_value: Value,
    #[prop(default = None)] function_name: Option<Value>,
    #[prop(default = None)] prefix: Option<String>,
    #[prop(default = String::new())] description: String,
    #[prop(default = String::new())] change_reason: String,
    handle_submit: NF,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();

    let (config_key_rs, config_key_ws) = create_signal(config_key);
    let (config_type_rs, config_type_ws) = create_signal(config_type);
    let (config_schema_rs, config_schema_ws) = create_signal(type_schema);
    let (config_value_rs, config_value_ws) = create_signal(config_value);
    let (function_name_rs, function_name_ws) = create_signal(function_name);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);
    let (description_rs, description_ws) = create_signal(description);
    let (change_reason_rs, change_reason_ws) = create_signal(change_reason);

    let schema_type_s = Signal::derive(move || {
        SchemaType::try_from(config_schema_rs.get())
            .map_err(|err| format!("Failed to get schema type: {}", err))
    });
    let enum_variants_s = Signal::derive(move || {
        EnumVariants::try_from(config_schema_rs.get())
            .map_err(|err| format!("Failed to get enum variants: {}", err))
    });

    let functions_resource: Resource<(String, String), Vec<Function>> =
        create_blocking_resource(
            move || (tenant_rws.get().0, org_rws.get().0),
            |(current_tenant, org)| async move {
                fetch_functions(&PaginationParams::all_entries(), current_tenant, org)
                    .await
                    .map_or_else(|_| vec![], |data| data.data)
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
            function_name_ws.update(|value| {
                let function_name = selected_function.clone();
                leptos::logging::log!("function selected: {:?}", function_name);
                let fun_name = match function_name.as_str() {
                    "None" => None,
                    _ => Some(json!(function_name)),
                };
                *value = fun_name;
            });
        });

    let on_submit = move |ev: MouseEvent| {
        req_inprogress_ws.set(true);
        ev.prevent_default();
        let f_name = prefix.clone().map_or_else(
            || config_key_rs.get(),
            |prefix| prefix + &config_key_rs.get(),
        );
        let f_schema = config_schema_rs.get();
        let f_value = config_value_rs.get();

        let fun_name = function_name_rs.get();
        let description = description_rs.get();
        let change_reason = change_reason_rs.get();

        let create_payload = DefaultConfigCreateReq {
            key: config_key_rs.get(),
            schema: f_schema.clone(),
            value: f_value.clone(),
            function_name: fun_name.clone(),
            description: description.clone(),
            change_reason: change_reason.clone(),
        };

        let update_payload = DefaultConfigUpdateReq {
            schema: f_schema,
            value: f_value,
            function_name: fun_name,
            description,
            change_reason,
        };

        let handle_submit_clone = handle_submit.clone();
        let is_edit = edit;
        spawn_local({
            let handle_submit = handle_submit_clone;
            async move {
                let result = if is_edit {
                    // Call update_default_config when edit is true
                    update_default_config(
                        f_name,
                        tenant_rws.get().0,
                        update_payload,
                        org_rws.get().0,
                    )
                    .await
                } else {
                    // Call create_default_config when edit is false
                    create_default_config(
                        tenant_rws.get().0,
                        create_payload,
                        org_rws.get().0,
                    )
                    .await
                };

                match result {
                    Ok(_) => {
                        handle_submit();
                        let success_message = if is_edit {
                            "Default config updated successfully!"
                        } else {
                            "New default config created successfully!"
                        };
                        enqueue_alert(
                            String::from(success_message),
                            AlertType::Success,
                            5000,
                        );
                    }
                    Err(e) => {
                        logging::error!(
                            "An error occurred while trying to {} the default config: {}",
                            if is_edit { "update" } else { "create" },
                            e
                        );
                        enqueue_alert(e, AlertType::Error, 5000);
                    }
                }
                req_inprogress_ws.set(false);
            }
        });
    };
    view! {
        <EditorProvider>
            <form class="form-control w-full space-y-4 bg-white text-gray-700 font-mono">
                <div class="form-control">
                    <label class="label">
                        <span class="label-text">Key Name</span>
                    </label>
                    <input
                        disabled=edit
                        type="text"
                        placeholder="Key"
                        class="input input-bordered w-full max-w-md"
                        value=config_key_rs.get_untracked()
                        on:change=move |ev| {
                            let value = event_target_value(&ev);
                            config_key_ws.set(value);
                        }
                    />

                </div>

                <div class="form-control">
                    <label class="label">
                        <span class="label-text">Description</span>
                    </label>
                    <textarea
                        placeholder="Enter a description"
                        class="textarea textarea-bordered w-full max-w-md"
                        value=description_rs.get_untracked()
                        on:change=move |ev| {
                            let value = event_target_value(&ev);
                            description_ws.set(value);
                        }
                    />
                </div>

                <div class="form-control">
                    <label class="label">
                        <span class="label-text">Reason for Change</span>
                    </label>
                    <textarea
                        placeholder="Enter a reason for this change"
                        class="textarea textarea-bordered w-full max-w-md"
                        value=change_reason_rs.get_untracked()
                        on:change=move |ev| {
                            let value = event_target_value(&ev);
                            change_reason_ws.set(value);
                        }
                    />
                </div>
                <Suspense>
                    {move || {
                        let options = type_template_resource.get().unwrap_or(vec![]);
                        let config_t = if config_type_rs.get().is_empty() && edit {
                            "change current type template".into()
                        } else if config_type_rs.get().is_empty() && !edit {
                            "Choose a type template".into()
                        } else {
                            config_type_rs.get()
                        };
                        let config_type_schema = SchemaType::Single(
                            JsonSchemaType::from(&config_schema_rs.get()),
                        );
                        view! {
                            <div class="form-control">
                                <label class="label">
                                    <span class="label-text">Set Schema</span>
                                </label>
                                <Dropdown
                                    dropdown_width="w-100"
                                    dropdown_icon="".to_string()
                                    dropdown_text=config_t
                                    dropdown_direction=DropdownDirection::Down
                                    dropdown_btn_type=DropdownBtnType::Select
                                    dropdown_options=options
                                    on_select=Callback::new(move |selected_item: TypeTemplate| {
                                        logging::log!("selected item {:?}", selected_item);
                                        let type_schema = selected_item.type_schema.clone();
                                        let parsed_schema_type = SchemaType::try_from(
                                            type_schema.clone(),
                                        );
                                        let parsed_enum_variants = EnumVariants::try_from(
                                            type_schema.clone(),
                                        );
                                        config_type_ws.set(selected_item.type_name);
                                        config_schema_ws.set(type_schema);
                                        if let (Ok(schema_type), Ok(enum_variants)) = (
                                            parsed_schema_type,
                                            parsed_enum_variants,
                                        ) {
                                            if InputType::from((schema_type, enum_variants))
                                                == InputType::Toggle
                                            {
                                                config_value_ws.set(Value::Bool(false));
                                            }
                                        }
                                    })
                                />

                                <Input
                                    id="type-schema"
                                    class="mt-5 rounded-md resize-y w-full max-w-md pt-3"
                                    schema_type=config_type_schema
                                    value=config_schema_rs.get()
                                    on_change=Callback::new(move |new_config_schema| {
                                        config_schema_ws.set(new_config_schema)
                                    })
                                    r#type=InputType::Monaco(vec![])
                                />

                                <Show when=move || {
                                    !config_schema_rs.get().is_null()
                                        && schema_type_s.get().is_err()
                                }>
                                    <span class="flex gap-2 py-2 text-xs font-semibold text-red-600">
                                        <i class="ri-close-circle-line"></i>
                                        {schema_type_s.get().unwrap_err()}
                                    </span>
                                </Show>
                            </div>
                        }
                    }}

                </Suspense>

                {move || {
                    let schema_type = schema_type_s.get();
                    let enum_variants = enum_variants_s.get();
                    if schema_type.is_err() || enum_variants.is_err() {
                        let tooltip_txt = if config_schema_rs.get().is_null() {
                            "Please select a schema type".to_string()
                        } else {
                            schema_type_s.get().unwrap_err()
                        };
                        return view! {
                            <div class="form-control">
                                <label class="label">
                                    <span class="label-text">Default Value</span>
                                </label>
                                <div class="tooltip text-left w-full max-w-md" data-tip=tooltip_txt>
                                    <textarea
                                        type="text"
                                        placeholder="Value"
                                        class="input input-bordered w-full pt-3"
                                        disabled=true
                                    >
                                        {config_value_rs.get().html_display()}
                                    </textarea>
                                </div>
                            </div>
                        }
                            .into_view();
                    }
                    let input_type = InputType::from((
                        schema_type.clone().unwrap(),
                        enum_variants.unwrap(),
                    ));
                    let class = match input_type {
                        InputType::Toggle => String::new(),
                        InputType::Select(_) => "mt-2".into(),
                        InputType::Integer | InputType::Number => "w-full max-w-md".into(),
                        _ => "rounded-md resize-y w-full max-w-md".into(),
                    };
                    view! {
                        <div class="form-control">
                            <label class="label">
                                <span class="label-text">Default Value</span>
                            </label>

                            <Input
                                id="default-config-value-input"
                                class
                                schema_type=schema_type.unwrap()
                                value=config_value_rs.get()
                                on_change=Callback::new(move |new_default_config: Value| {
                                    logging::log!(
                                        "new value entered for default config = {:?}", new_default_config
                                    );
                                    config_value_ws.set(new_default_config);
                                })
                                r#type=input_type
                            />
                        </div>
                    }
                        .into_view()
                }}

                <Suspense>
                    {move || {
                        let functions = functions_resource.get().unwrap_or_default();
                        let mut function_names: Vec<FunctionsName> = vec![];
                        functions
                            .into_iter()
                            .for_each(|ele| {
                                function_names.push(ele.function_name);
                            });
                        function_names.sort();
                        function_names.insert(0, "None".to_string());
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
                                        dropdown_text=function_name_rs
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
            </form>
        </EditorProvider>
    }
}
