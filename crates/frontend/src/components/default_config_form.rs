pub mod utils;

use std::ops::Deref;

use chrono::Utc;
use futures::join;
use leptos::{html::Div, *};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use superposition_types::{
    api::{default_config::DefaultConfigUpdateRequest, functions::ListFunctionFilters},
    custom_query::PaginationParams,
    database::models::{
        cac::{Function, FunctionType, TypeTemplate},
        ChangeReason, Description,
    },
    ExtendedMap,
};
use utils::try_update_payload;
use wasm_bindgen::JsCast;
use web_sys::MouseEvent;

use crate::{
    api::{fetch_functions, fetch_types, get_default_config},
    components::{
        alert::AlertType,
        badge::GrayPill,
        button::Button,
        change_form::ChangeForm,
        change_summary::{ChangeLogPopup, ChangeSummary, JsonChangeSummary},
        dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
        form::label::Label,
        input::{Input, InputType},
        skeleton::{Skeleton, SkeletonVariant},
    },
    schema::{EnumVariants, HtmlDisplay, JsonSchemaType, SchemaType},
    types::FunctionsName,
    utils::set_function,
};
use crate::{
    providers::{alert_provider::enqueue_alert, editor_provider::EditorProvider},
    types::{OrganisationId, Workspace},
};

use self::utils::{create_default_config, update_default_config};

enum ResponseType {
    UpdatePrecheck,
    Response,
}

#[derive(Serialize, Deserialize, Clone, Default)]
pub struct CombinedResource {
    functions: Vec<Function>,
    type_templates: Vec<TypeTemplate>,
}

#[component]
pub fn default_config_form(
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] config_key: String,
    #[prop(default = String::new())] config_type: String,
    #[prop(default = Value::Null)] type_schema: Value,
    #[prop(default = Value::Null)] config_value: Value,
    #[prop(default = None)] validation_function_name: Option<String>,
    #[prop(default = None)] value_compute_function_name: Option<String>,
    #[prop(default = None)] prefix: Option<String>,
    #[prop(default = String::new())] description: String,
    #[prop(into)] handle_submit: Callback<()>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
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
    let (value_compute_function_name_rs, value_compute_function_name_ws) =
        create_signal(value_compute_function_name);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);
    let (description_rs, description_ws) = create_signal(description);
    let (change_reason_rs, change_reason_ws) = create_signal(String::new());
    let update_request_rws = RwSignal::new(None);

    let schema_type_s = Signal::derive(move || {
        SchemaType::try_from(config_schema_rs.get())
            .map_err(|err| format!("Failed to get schema type: {}", err))
    });
    let enum_variants_s = Signal::derive(move || {
        EnumVariants::try_from(config_schema_rs.get())
            .map_err(|err| format!("Failed to get enum variants: {}", err))
    });

    let combined_resources = create_blocking_resource(
        move || (workspace.get().0, org.get().0),
        |(workspace, org_id)| async move {
            let all_entries = PaginationParams::all_entries();
            let list_filters = ListFunctionFilters::default();
            let functions_future =
                fetch_functions(&all_entries, &list_filters, &workspace, &org_id);

            let types_future = fetch_types(&all_entries, &workspace, &org_id);

            let (functions_result, types_result) = join!(functions_future, types_future);

            CombinedResource {
                functions: functions_result.map(|d| d.data).unwrap_or_default(),
                type_templates: types_result.map(|d| d.data).unwrap_or_default(),
            }
        },
    );

    let handle_select_dropdown_option_validation =
        Callback::new(move |selected_function: FunctionsName| {
            validation_fn_name_ws.update(|v| set_function(selected_function, v));
        });

    let handle_select_dropdown_option_value_compute =
        Callback::new(move |selected_function: FunctionsName| {
            value_compute_function_name_ws.update(|v| set_function(selected_function, v));
        });

    let on_submit = Callback::new(move |_| {
        req_inprogress_ws.set(true);
        let key_name = config_key_rs.get_untracked();
        let f_schema = config_schema_rs.get_untracked();
        let f_value = config_value_rs.get_untracked();

        let fun_name = validation_fn_name_rs.get_untracked();
        let value_compute_fn = value_compute_function_name_rs.get_untracked();

        let description = description_rs.get_untracked();
        let change_reason = change_reason_rs.get_untracked();
        let workspace = workspace.get_untracked();
        let org = org.get_untracked();

        let is_edit = edit;

        spawn_local(async move {
            let result = match (is_edit, update_request_rws.get_untracked()) {
                (true, Some((_, payload))) => {
                    let future =
                        update_default_config(key_name, payload, &workspace, &org);
                    update_request_rws.set(None);
                    future.await.map(|_| ResponseType::Response)
                }
                (true, None) => {
                    let request_payload = try_update_payload(
                        f_value,
                        f_schema,
                        fun_name,
                        value_compute_fn,
                        description,
                        change_reason,
                    );
                    match request_payload {
                        Ok(payload) => {
                            update_request_rws.set(Some((key_name, payload)));
                            Ok(ResponseType::UpdatePrecheck)
                        }
                        Err(e) => Err(e),
                    }
                }
                _ => create_default_config(
                    config_key_rs.get_untracked(),
                    f_value,
                    f_schema,
                    fun_name,
                    value_compute_fn,
                    description,
                    change_reason,
                    &workspace,
                    &org,
                )
                .await
                .map(|_| ResponseType::Response),
            };

            req_inprogress_ws.set(false);
            match result {
                Ok(ResponseType::UpdatePrecheck) => (),
                Ok(ResponseType::Response) => {
                    handle_submit.call(());
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
        });
    });

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
                    <div class="flex items-center gap-2">
                        <Label title="Key Name" />
                        {prefix
                            .as_ref()
                            .map(|p| {
                                view! {
                                    <GrayPill
                                        data=p
                                        deletable=false
                                        on_delete=Callback::new(|_: String| {})
                                        icon_class="ri-map-pin-2-fill"
                                    />
                                }
                            })}
                    </div>
                    <input
                        disabled=edit
                        type="text"
                        placeholder=if prefix.is_some() {
                            "Enter your key (prefix will be added)"
                        } else {
                            "Enter your key"
                        }
                        class="input input-bordered w-full max-w-md"
                        value=config_key_rs
                            .with_untracked(|k| {
                                k.strip_prefix(&prefix.clone().unwrap_or_default())
                                    .map(String::from)
                                    .unwrap_or_else(|| k.clone())
                            })
                        on:input=move |ev| {
                            let value = event_target_value(&ev);
                            config_key_ws
                                .set(format!("{}{value}", prefix.clone().unwrap_or_default()));
                        }
                    />
                </div>
                <Suspense>
                    {move || {
                        let mut options = combined_resources
                            .with(|c| c.as_ref().map(|c| c.type_templates.clone()))
                            .unwrap_or_default();
                        options
                            .push(TypeTemplate {
                                type_name: "Custom JSON Schema".to_string(),
                                type_schema: ExtendedMap::from(
                                    Map::from_iter([
                                        ("type".to_string(), Value::String("object".to_string())),
                                    ]),
                                ),
                                created_by: "NA".to_string(),
                                created_at: Utc::now(),
                                last_modified_at: Utc::now(),
                                last_modified_by: "NA".to_string(),
                                description: Description::default(),
                                change_reason: ChangeReason::default(),
                            });
                        let config_type = config_type_rs.get();
                        let config_t = if config_type.is_empty() && edit {
                            "change current type template".into()
                        } else if config_type.is_empty() && !edit {
                            "Choose a type template".into()
                        } else {
                            config_type
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
                                        let type_schema: &Map<String, Value> = &selected_item
                                            .type_schema;
                                        let parsed_schema_type = SchemaType::try_from(type_schema);
                                        let parsed_enum_variants = EnumVariants::try_from(
                                            type_schema,
                                        );
                                        config_type_ws.set(selected_item.type_name);
                                        config_schema_ws
                                            .set(Value::from(selected_item.type_schema));
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
                                    disabled=config_type_rs.get().is_empty() && !edit
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
                        let mut functions = combined_resources
                            .with(|c| c.as_ref().map(|c| c.functions.clone()))
                            .unwrap_or_default();
                        let mut validation_function_names: Vec<FunctionsName> = vec![
                            "None".to_string(),
                        ];
                        let mut value_compute_function_names: Vec<FunctionsName> = vec![
                            "None".to_string(),
                        ];
                        functions.sort_by(|a, b| a.function_name.cmp(&b.function_name));
                        functions
                            .iter()
                            .for_each(|ele| {
                                if ele.function_type == FunctionType::ValueValidation {
                                    validation_function_names.push(ele.function_name.clone());
                                } else {
                                    value_compute_function_names.push(ele.function_name.clone());
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
                                    title="Value Compute Function"
                                    description="Function to add value compute suggestion to your key"
                                />
                                <Dropdown
                                    dropdown_width="w-100"
                                    dropdown_icon="".to_string()
                                    dropdown_text=value_compute_function_name_rs
                                        .get()
                                        .map_or("Add Function".to_string(), |v| v.to_string())
                                    dropdown_direction=DropdownDirection::Down
                                    dropdown_btn_type=DropdownBtnType::Select
                                    dropdown_options=value_compute_function_names
                                    on_select=handle_select_dropdown_option_value_compute
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
                            on_click=move |ev: MouseEvent| {
                                ev.prevent_default();
                                on_submit.call(());
                            }
                            loading
                        />
                    }
                }}
            </form>
        </EditorProvider>
        {move || match update_request_rws.get() {
            None => ().into_view(),
            Some((key_name, update_request)) => {
                view! {
                    <ChangeLogSummary
                        key_name
                        change_type=ChangeType::Update(update_request)
                        on_confirm=on_submit
                        on_close=move |_| update_request_rws.set(None)
                    />
                }
                    .into_view()
            }
        }}
    }
}

#[derive(Clone)]
pub enum ChangeType {
    Delete,
    Update(DefaultConfigUpdateRequest),
}

#[component]
pub fn change_log_summary(
    key_name: String,
    change_type: ChangeType,
    #[prop(into)] on_confirm: Callback<()>,
    #[prop(into)] on_close: Callback<()>,
    #[prop(into, default = Signal::derive(|| false))] inprogress: Signal<bool>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let default_config = create_local_resource(
        move || (key_name.clone(), workspace.get().0, org.get().0),
        |(key_name, workspace, org)| async move {
            get_default_config(&key_name, &workspace, &org).await
        },
    );

    let disabled_rws = RwSignal::new(true);
    let change_type = StoredValue::new(change_type);

    let (title, description, confirm_text) = match change_type.get_value() {
        ChangeType::Update(_) => (
            "Confirm Update",
            "Are you sure you want to update this config?",
            "Yes, Update",
        ),
        ChangeType::Delete => (
            "Confirm Delete",
            "Are you sure you want to delete this config? Action is irreversible.",
            "Yes, Delete",
        ),
    };

    view! {
        <ChangeLogPopup
            title
            description
            confirm_text
            on_confirm
            on_close
            disabled=disabled_rws
            inprogress
        >
            <Suspense fallback=move || {
                view! { <Skeleton variant=SkeletonVariant::Block style_class="h-10".to_string() /> }
            }>
                {
                    Effect::new(move |_| {
                        let default_config = default_config.get();
                        if let Some(Ok(_)) = default_config {
                            disabled_rws.set(false);
                        } else if let Some(Err(e)) = default_config {
                            logging::error!("Error fetching default config: {}", e);
                        }
                    });
                }
                {move || match default_config.get() {
                    Some(Ok(default)) => {
                        let (new_default_value, new_schema, new_values) = match change_type
                            .get_value()
                        {
                            ChangeType::Update(update_request) => {
                                let description = update_request
                                    .description
                                    .unwrap_or_else(|| default.description.clone())
                                    .deref()
                                    .to_string();
                                let valdiate_fn = update_request
                                    .value_validation_function_name
                                    .clone()
                                    .unwrap_or_else(|| {
                                        default.value_validation_function_name.clone()
                                    });
                                let value_compute_fn = update_request
                                    .value_compute_function_name
                                    .clone()
                                    .unwrap_or_else(|| {
                                        default.value_compute_function_name.clone()
                                    });
                                (
                                    Some(
                                        update_request
                                            .value
                                            .unwrap_or_else(|| default.value.clone()),
                                    ),
                                    Some(
                                        update_request
                                            .schema
                                            .unwrap_or_else(|| default.schema.clone()),
                                    ),
                                    Map::from_iter(
                                        vec![
                                            Some((
                                                "Description".to_string(),
                                                Value::String(description),
                                            )),
                                            valdiate_fn
                                                .map(|f| (
                                                    "Value Validation Function".to_string(),
                                                    Value::String(f.clone()),
                                                )),
                                            value_compute_fn
                                                .map(|f| (
                                                    "Value Compute Function".to_string(),
                                                    Value::String(f.clone()),
                                                )),
                                        ]
                                            .into_iter()
                                            .flatten(),
                                    ),
                                )
                            }
                            ChangeType::Delete => (None, None, Map::new()),
                        };
                        view! {
                            <JsonChangeSummary
                                title="Default Value changes"
                                old_values=Some(default.value)
                                new_values=new_default_value
                            />
                            <JsonChangeSummary
                                title="Schema changes"
                                old_values=Some(Value::from(default.schema))
                                new_values=new_schema.map(Value::from)
                            />
                            <ChangeSummary
                                title="Other changes"
                                key_column="Property"
                                old_values=Map::from_iter(
                                    vec![
                                        Some((
                                            "Description".to_string(),
                                            Value::String(default.description.deref().to_string()),
                                        )),
                                        default
                                            .value_validation_function_name
                                            .map(|f| (
                                                "Value Validation Function".to_string(),
                                                Value::String(f.clone()),
                                            )),
                                        default
                                            .value_compute_function_name
                                            .map(|f| (
                                                "Value Compute Function".to_string(),
                                                Value::String(f.clone()),
                                            )),
                                    ]
                                        .into_iter()
                                        .flatten(),
                                )
                                new_values
                            />
                        }
                            .into_view()
                    }
                    Some(Err(e)) => {
                        logging::error!("Error fetching default config: {}", e);
                        view! { <div>Error fetching default config</div> }.into_view()
                    }
                    None => view! { <div>Loading...</div> }.into_view(),
                }}
            </Suspense>
        </ChangeLogPopup>
    }
}
