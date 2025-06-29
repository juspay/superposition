pub mod utils;

use leptos::{html::Div, *};
use serde_json::Value;
use superposition_types::{
    api::functions::ListFunctionFilters,
    custom_query::PaginationParams,
    database::models::cac::{Function, FunctionType, TypeTemplate},
};
use wasm_bindgen::JsCast;
use web_sys::MouseEvent;

use crate::{
    api::{fetch_functions, fetch_types},
    components::{
        alert::AlertType,
        button::Button,
        change_form::ChangeForm,
        dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
        form::label::Label,
        input::{Input, InputType},
    },
    schema::{EnumVariants, HtmlDisplay, JsonSchemaType, SchemaType},
    types::FunctionsName,
    utils::set_function,
};
use crate::{
    providers::{alert_provider::enqueue_alert, editor_provider::EditorProvider},
    types::{OrganisationId, Tenant},
};

use self::utils::{create_default_config, update_default_config};

#[component]
pub fn default_config_form<NF>(
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] config_key: String,
    #[prop(default = String::new())] config_type: String,
    #[prop(default = Value::Null)] type_schema: Value,
    #[prop(default = Value::Null)] config_value: Value,
    #[prop(default = None)] validation_function_name: Option<String>,
    #[prop(default = None)] autocomplete_function_name: Option<String>,
    #[prop(default = None)] prefix: Option<String>,
    #[prop(default = String::new())] description: String,
    handle_submit: NF,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let (config_key_rs, config_key_ws) = create_signal(
        prefix
            .as_ref()
            .map_or_else(|| config_key.clone(), |p| format!("{p}{config_key}")),
    );
    let (config_type_rs, config_type_ws) = create_signal(config_type);
    let (config_schema_rs, config_schema_ws) = create_signal(type_schema);
    let (config_value_rs, config_value_ws) = create_signal(config_value);
    let (validation_fn_name_rs, validation_fn_name_ws) =
        create_signal(validation_function_name);
    let (autocomplete_fn_name_rs, autocomplete_fn_name_ws) =
        create_signal(autocomplete_function_name);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);
    let (description_rs, description_ws) = create_signal(description);
    let (change_reason_rs, change_reason_ws) = create_signal(String::new());

    let schema_type_s = Signal::derive(move || {
        SchemaType::try_from(config_schema_rs.get())
            .map_err(|err| format!("Failed to get schema type: {}", err))
    });
    let enum_variants_s = Signal::derive(move || {
        EnumVariants::try_from(config_schema_rs.get())
            .map_err(|err| format!("Failed to get enum variants: {}", err))
    });

    let validation_functions_resource: Resource<(String, String), Vec<Function>> =
        create_blocking_resource(
            move || (workspace.get().0, org.get().0),
            |(current_tenant, org)| async move {
                let fn_filters = ListFunctionFilters {
                    function_type: None,
                };
                fetch_functions(
                    &PaginationParams::all_entries(),
                    &fn_filters,
                    current_tenant,
                    org,
                )
                .await
                .map_or_else(|_| vec![], |data| data.data)
            },
        );

    let type_template_resource = create_blocking_resource(
        move || (workspace.get().0, org.get().0),
        |(current_tenant, org)| async move {
            fetch_types(&PaginationParams::all_entries(), current_tenant, org)
                .await
                .map_or_else(|_| vec![], |response| response.data)
        },
    );

    let handle_select_dropdown_option_validation =
        Callback::new(move |selected_function: FunctionsName| {
            validation_fn_name_ws.update(|v| set_function(selected_function, v));
        });

    let handle_select_dropdown_option_autocomplete =
        Callback::new(move |selected_function: FunctionsName| {
            autocomplete_fn_name_ws.update(|v| set_function(selected_function, v));
        });

    let on_submit = move |ev: MouseEvent| {
        req_inprogress_ws.set(true);
        ev.prevent_default();
        let f_name = config_key_rs.get_untracked();
        let f_schema = config_schema_rs.get_untracked();
        let f_value = config_value_rs.get_untracked();

        let fun_name = validation_fn_name_rs.get_untracked();
        let description = description_rs.get_untracked();
        let change_reason = change_reason_rs.get_untracked();

        let handle_submit_clone = handle_submit.clone();
        let is_edit = edit;
        spawn_local({
            let handle_submit = handle_submit_clone;
            async move {
                let result = if is_edit {
                    // Call update_default_config when edit is true
                    update_default_config(
                        f_name,
                        workspace.get_untracked().0,
                        org.get_untracked().0,
                        f_value,
                        f_schema,
                        fun_name,
                        None,
                        description,
                        change_reason,
                    )
                    .await
                } else {
                    // Call create_default_config when edit is false
                    create_default_config(
                        workspace.get_untracked().0,
                        org.get_untracked().0,
                        config_key_rs.get_untracked(),
                        f_value,
                        f_schema,
                        fun_name,
                        None,
                        description,
                        change_reason,
                    )
                    .await
                };

                req_inprogress_ws.set(false);
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
            }
        });
    };

    let scroll_ref = create_node_ref::<Div>();

    // Helper to scroll the div to center
    Effect::new(move |_| {
        if let Some(elem) = scroll_ref.get() {
            if let Some(elem) = elem.dyn_ref::<web_sys::HtmlDivElement>() {
                let scroll_width = elem.scroll_width();
                let client_width = elem.client_width();
                let center = (scroll_width - client_width) / 2;
                elem.set_scroll_left(center);
            }
        }
    });

    view! {
        <EditorProvider>
            <form class="w-full flex flex-col gap-5 text-gray-700 bg-white">
                <div class="form-control">
                    <Label title="Key Name" />
                    <div class="input input-bordered w-full max-w-md p-0 flex" disabled=edit>
                        <div
                            node_ref=scroll_ref
                            class="w-full px-4 py-2 flex items-center overflow-x-scroll whitespace-nowrap"
                            style="scrollbar-gutter: stable;"
                        >
                            {prefix
                                .as_ref()
                                .map(|p| {
                                    view! { <span class="text-gray-500 select-none">{p}</span> }
                                })}
                            <input
                                disabled=edit
                                type="text"
                                placeholder="Enter your key"
                                class="flex-shrink-0 min-w-0 w-kull bg-transparent focus:outline-none"
                                value=config_key_rs
                                    .with_untracked(|k| {
                                        k.strip_prefix(&prefix.clone().unwrap_or_default())
                                            .map(String::from)
                                            .unwrap_or_else(|| k.clone())
                                    })
                                on:input=move |ev| {
                                    let value = event_target_value(&ev);
                                    config_key_ws
                                        .set(
                                            format!("{}{value}", prefix.clone().unwrap_or_default()),
                                        );
                                }
                            />
                        </div>
                    </div>
                </div>
                <Suspense>
                    {move || {
                        let options = type_template_resource.get().unwrap_or_default();
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
                            <ChangeForm
                                title="Description".to_string()
                                placeholder="Enter a description".to_string()
                                value=description_rs.get_untracked()
                                on_change=Callback::new(move |new_description| {
                                    description_ws.set(new_description)
                                })
                            />
                            <ChangeForm
                                title="Reason for Change".to_string()
                                placeholder="Enter a reason for this change".to_string()
                                value=change_reason_rs.get_untracked()
                                on_change=Callback::new(move |new_change_reason| {
                                    change_reason_ws.set(new_change_reason)
                                })
                            />
                            <div class="form-control">
                                <Label title="Set Schema" />
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
                                <Label title="Default Value" />
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
                            <Label title="Default Value" />
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
                        let mut functions = validation_functions_resource.get().unwrap_or_default();
                        let mut validation_function_names: Vec<FunctionsName> = vec![
                            "None".to_string(),
                        ];
                        let mut autocomplete_function_names: Vec<FunctionsName> = vec![
                            "None".to_string(),
                        ];
                        functions.sort_by(|a, b| a.function_name.cmp(&b.function_name));
                        functions
                            .iter()
                            .for_each(|ele| {
                                if ele.function_type == FunctionType::Validation {
                                    validation_function_names.push(ele.function_name.clone());
                                } else {
                                    autocomplete_function_names.push(ele.function_name.clone());
                                }
                            });
                        view! {
                            <div class="form-control">
                                <Label
                                    title="Validation Function"
                                    description="Function to add validation logic to your key"
                                />
                                <Dropdown
                                    dropdown_width="w-100"
                                    dropdown_icon="".to_string()
                                    dropdown_text=validation_fn_name_rs
                                        .get()
                                        .map_or("Add Function".to_string(), |v| v.to_string())
                                    dropdown_direction=DropdownDirection::Down
                                    dropdown_btn_type=DropdownBtnType::Select
                                    dropdown_options=validation_function_names
                                    on_select=handle_select_dropdown_option_validation
                                />

                            </div>

                            <div class="form-control">
                                <Label
                                    title="AutoComplete Function"
                                    description="Function to add auto complete suggestion to your key"
                                />
                                <Dropdown
                                    dropdown_width="w-100"
                                    dropdown_icon="".to_string()
                                    dropdown_text=autocomplete_fn_name_rs
                                        .get()
                                        .map_or("Add Function".to_string(), |v| v.to_string())
                                    dropdown_direction=DropdownDirection::Down
                                    dropdown_btn_type=DropdownBtnType::Select
                                    dropdown_options=autocomplete_function_names
                                    on_select=handle_select_dropdown_option_autocomplete
                                />
                            </div>
                        }
                    }}
                </Suspense>
                {move || {
                    let loading = req_inprogess_rs.get();
                    view! {
                        <Button
                            class="self-end h-12 w-48"
                            text="Submit"
                            icon_class="ri-send-plane-line"
                            on_click=on_submit.clone()
                            loading
                        />
                    }
                }}
            </form>
        </EditorProvider>
    }
}
