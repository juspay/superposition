pub mod utils;

use std::ops::Deref;

use futures::join;
use leptos::*;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Number, Value};
use strum::IntoEnumIterator;
use superposition_types::{
    api::{
        dimension::{DimensionResponse, UpdateRequest},
        functions::ListFunctionFilters,
    },
    custom_query::PaginationParams,
    database::models::cac::{DimensionType, Function, FunctionType, TypeTemplate},
};
use utils::try_update_payload;
use web_sys::MouseEvent;

use crate::components::{
    alert::AlertType,
    button::Button,
    change_form::ChangeForm,
    change_summary::{ChangeLogPopup, ChangeSummary, JsonChangeSummary},
    dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
    form::label::Label,
    input::{Input, InputType},
    skeleton::{Skeleton, SkeletonVariant},
};
use crate::providers::alert_provider::enqueue_alert;
use crate::providers::editor_provider::EditorProvider;
use crate::schema::{JsonSchemaType, SchemaType};
use crate::types::{FunctionsName, OrganisationId, Tenant};
use crate::utils::set_function;
use crate::{
    api::{dimensions, fetch_functions, fetch_types},
    types::DimensionTypeOptions,
};

enum ResponseType {
    UpdatePrecheck,
    Response,
}

#[derive(Serialize, Deserialize, Clone, Default)]
pub struct CombinedResource {
    dimensions: Vec<DimensionResponse>,
    functions: Vec<Function>,
    type_templates: Vec<TypeTemplate>,
}

#[component]
pub fn dimension_form(
    #[prop(default = false)] edit: bool,
    #[prop(default = 0)] position: u32,
    #[prop(default = String::new())] dimension_name: String,
    #[prop(default = String::new())] dimension_type_template: String,
    #[prop(default = Value::Null)] dimension_schema: Value,
    #[prop(default = None)] validation_function_name: Option<String>,
    #[prop(default = None)] autocomplete_function_name: Option<String>,
    #[prop(default = String::new())] description: String,
    #[prop(default = DimensionType::Regular)] dimension_type: DimensionType,
    #[prop(optional)] dimensions: Option<Vec<DimensionResponse>>,
    #[prop(into)] handle_submit: Callback<()>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let (position_rs, position_ws) = create_signal(position);
    let (dimension_name_rs, dimension_name_ws) = create_signal(dimension_name);
    let (dimension_type_template_rs, dimension_type_template_ws) =
        create_signal(dimension_type_template);
    let (dimension_type_rs, dimension_type_ws) =
        create_signal(DimensionTypeOptions::from_dimension_type(&dimension_type));
    let (dimension_schema_rs, dimension_schema_ws) = create_signal(dimension_schema);
    let (validation_fn_name_rs, validation_fn_name_ws) =
        create_signal(validation_function_name);
    let (autocomplete_fn_name_rs, autocomplete_fn_name_ws) =
        create_signal(autocomplete_function_name);
    let (description_rs, description_ws) = create_signal(description);
    let (change_reason_rs, change_reason_ws) = create_signal(String::new());
    let (cohort_based_on_rs, cohort_based_on_ws) = match dimension_type {
        DimensionType::Regular => create_signal(String::new()),
        DimensionType::LocalCohort(cohort_based_on)
        | DimensionType::RemoteCohort(cohort_based_on) => create_signal(cohort_based_on),
    };
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);
    let update_request_rws = RwSignal::new(None);
    let combined_resources = create_blocking_resource(
        move || (dimensions.clone(), workspace.get().0, org.get().0),
        |(dimensions, tenant, org_id)| async move {
            let dimensions_future = async {
                match dimensions {
                    None => dimensions::fetch(
                        &PaginationParams::all_entries(),
                        tenant.clone(),
                        org_id.clone(),
                    )
                    .await
                    .map(|d| d.data)
                    .unwrap_or_default(),
                    Some(data) => data,
                }
            };

            let all_entries = PaginationParams::all_entries();
            let list_filters = ListFunctionFilters::default();
            let functions_future = fetch_functions(
                &all_entries,
                &list_filters,
                tenant.clone(),
                org_id.clone(),
            );

            let types_future = fetch_types(&all_entries, tenant.clone(), org_id.clone());

            let (dimensions_result, functions_result, types_result) =
                join!(dimensions_future, functions_future, types_future);

            CombinedResource {
                dimensions: dimensions_result,
                functions: functions_result.map(|d| d.data).unwrap_or_default(),
                type_templates: types_result.map(|d| d.data).unwrap_or_default(),
            }
        },
    );

    let handle_validation_fn_select =
        Callback::new(move |selected_function: FunctionsName| {
            validation_fn_name_ws.update(|v| set_function(selected_function, v));
        });

    let handle_autocomplete_fn_select =
        Callback::new(move |selected_function: FunctionsName| {
            autocomplete_fn_name_ws.update(|v| set_function(selected_function, v));
        });

    let (error_message, set_error_message) = create_signal("".to_string());

    let on_submit = move |_| {
        req_inprogress_ws.set(true);
        let function_position = position_rs.get_untracked();
        let dimension_name = dimension_name_rs.get_untracked();
        let validation_fn_name = validation_fn_name_rs.get_untracked();
        let autocomplete_fn_name = autocomplete_fn_name_rs.get_untracked();
        let function_schema = dimension_schema_rs.get_untracked();
        let cohort_based_on = cohort_based_on_rs.get_untracked();
        let dimension_type = dimension_type_rs
            .get_untracked()
            .to_dimension_type(cohort_based_on);
        spawn_local({
            async move {
                let result = match (edit, update_request_rws.get_untracked()) {
                    (true, Some((_, update_payload))) => {
                        let future = dimensions::update(
                            workspace.get_untracked().0,
                            dimension_name,
                            update_payload,
                            org.get_untracked().0,
                        );
                        update_request_rws.set(None);
                        future.await.map(|_| ResponseType::Response)
                    }
                    (true, None) => {
                        let request_payload = try_update_payload(
                            function_position,
                            function_schema,
                            validation_fn_name,
                            autocomplete_fn_name,
                            description_rs.get_untracked(),
                            change_reason_rs.get_untracked(),
                        );
                        match request_payload {
                            Ok(payload) => {
                                update_request_rws.set(Some((dimension_name, payload)));
                                Ok(ResponseType::UpdatePrecheck)
                            }
                            Err(e) => Err(e),
                        }
                    }
                    _ => dimensions::create(
                        dimension_name,
                        function_position,
                        function_schema,
                        validation_fn_name,
                        autocomplete_fn_name,
                        description_rs.get_untracked(),
                        change_reason_rs.get_untracked(),
                        workspace.get_untracked().0,
                        org.get_untracked().0,
                        dimension_type,
                    )
                    .await
                    .map(|_| ResponseType::Response),
                };

                req_inprogress_ws.set(false);
                match result {
                    Ok(ResponseType::UpdatePrecheck) => (),
                    Ok(ResponseType::Response) => {
                        handle_submit.call(());
                        let success_message = if edit {
                            "Dimension updated successfully!"
                        } else {
                            "New Dimension created successfully!"
                        };
                        enqueue_alert(
                            String::from(success_message),
                            AlertType::Success,
                            5000,
                        );
                    }
                    Err(e) => {
                        set_error_message.set(e.clone());
                        enqueue_alert(e, AlertType::Error, 5000);
                    }
                }
            }
        });
    };
    view! {
        <Suspense fallback=move || view! { <Skeleton variant=SkeletonVariant::Block /> }>
            <form class="form-control w-full flex flex-col gap-5 bg-white text-gray-700">
                <div class="form-control">
                    <Label title="Dimension" />
                    <input
                        disabled=edit
                        type="text"
                        placeholder="Dimension"
                        class="input input-bordered w-full max-w-md"
                        value=move || dimension_name_rs.get()
                        on:change=move |ev| {
                            let value = event_target_value(&ev);
                            dimension_name_ws.set(value);
                        }
                    />
                </div>

                <ChangeForm
                    title="Description".to_string()
                    placeholder="Enter a description".to_string()
                    value=description_rs.get_untracked()
                    on_change=move |new_description| { description_ws.set(new_description) }
                />
                <ChangeForm
                    title="Reason for Change".to_string()
                    placeholder="Enter a reason for this change".to_string()
                    value=change_reason_rs.get_untracked()
                    on_change=move |new_change_reason| { change_reason_ws.set(new_change_reason) }
                />

                <div class="form-control">
                    <Label title="Dimension Type" />
                    <Dropdown
                        disabled=edit
                        dropdown_width="w-100"
                        dropdown_icon="".to_string()
                        dropdown_text=dimension_type_rs.get().to_string()
                        dropdown_direction=DropdownDirection::Down
                        dropdown_btn_type=DropdownBtnType::Select
                        dropdown_options=DimensionTypeOptions::iter().collect()
                        on_select=Callback::new(move |selected_item: DimensionTypeOptions| {
                            logging::log!("selected item {}", selected_item);
                            if !edit && selected_item == DimensionTypeOptions::LocalCohort {
                                dimension_schema_ws
                                    .set(
                                        json!(
                                            {
                                    "enum": [
                                        "otherwise"
                                    ],
                                    "type": "string",
                                    "definitions": {}
                                }
                                        ),
                                    );
                            }
                            dimension_type_ws.set(selected_item);
                        })
                    />
                </div>

                <Suspense>
                    {move || {
                        match dimension_type_rs.get() {
                            DimensionTypeOptions::Regular | DimensionTypeOptions::RemoteCohort => {
                                let options = combined_resources
                                    .with(|c| c.as_ref().map(|c| c.type_templates.clone()))
                                    .unwrap_or_default();
                                let dimension_options = combined_resources
                                    .with(|c| c.as_ref().map(|c| c.dimensions.clone()))
                                    .unwrap_or_default()
                                    .iter()
                                    .filter_map(|d| {
                                        (d.dimension != dimension_name_rs.get()
                                            && !matches!(
                                                d.dimension_type,
                                                DimensionType::LocalCohort(_)
                                            ))
                                            .then_some(d.dimension.clone())
                                    })
                                    .collect::<Vec<_>>();
                                let dimension_options = StoredValue::new(dimension_options);
                                let dimension_t = if dimension_type_template_rs.get().is_empty()
                                    && edit
                                {
                                    "change current type template".into()
                                } else if dimension_type_template_rs.get().is_empty() && !edit {
                                    "choose a type template".into()
                                } else {
                                    dimension_type_template_rs.get()
                                };
                                let dimension_type_schema = SchemaType::Single(
                                    JsonSchemaType::from(&dimension_schema_rs.get()),
                                );
                                view! {
                                    <div class="form-control">
                                        <Show when=move || {
                                            dimension_type_rs.get()
                                                == DimensionTypeOptions::RemoteCohort
                                        }>
                                            <Label title="Cohort Based on" />
                                            <Dropdown
                                                disabled=edit
                                                dropdown_text=cohort_based_on_rs.get()
                                                dropdown_direction=DropdownDirection::Down
                                                dropdown_btn_type=DropdownBtnType::Select
                                                dropdown_options=dimension_options.get_value()
                                                on_select=Callback::new(move |selected_item: String| {
                                                    logging::log!("selected item {:?}", selected_item);
                                                    cohort_based_on_ws.set(selected_item);
                                                })
                                            />
                                        </Show>

                                        <Label title="Set Schema" />
                                        <Dropdown
                                            dropdown_width="w-100"
                                            dropdown_icon="".to_string()
                                            dropdown_text=dimension_t
                                            dropdown_direction=DropdownDirection::Down
                                            dropdown_btn_type=DropdownBtnType::Select
                                            dropdown_options=options
                                            on_select=Callback::new(move |selected_item: TypeTemplate| {
                                                logging::log!("selected item {:?}", selected_item);
                                                dimension_type_template_ws.set(selected_item.type_name);
                                                dimension_schema_ws.set(selected_item.type_schema);
                                            })
                                        />
                                        <EditorProvider>
                                            <Input
                                                id="type-schema"
                                                class="mt-5 rounded-md resize-y w-full max-w-md pt-3"
                                                schema_type=dimension_type_schema
                                                value=dimension_schema_rs.get_untracked()
                                                on_change=move |new_type_schema| {
                                                    dimension_schema_ws.set(new_type_schema)
                                                }
                                                r#type=InputType::Monaco(vec![])
                                            />
                                        </EditorProvider>
                                    </div>
                                }
                            }
                            DimensionTypeOptions::LocalCohort => {
                                let dimension_name = dimension_name_rs.get();
                                let dropdown_options = combined_resources
                                    .with(|c| c.as_ref().map(|c| c.dimensions.clone()))
                                    .unwrap_or_default()
                                    .iter()
                                    .filter_map(|d| {
                                        (d.dimension != dimension_name
                                            && DimensionTypeOptions::from_dimension_type(
                                                &d.dimension_type,
                                            ) != DimensionTypeOptions::LocalCohort)
                                            .then_some(d.dimension.clone())
                                    })
                                    .collect::<Vec<_>>();
                                let dimension_type_schema = SchemaType::Single(
                                    JsonSchemaType::Object,
                                );
                                view! {
                                    <div class="form-control">
                                        <Label title="Cohort Based on" />
                                        <Dropdown
                                            disabled=edit
                                            dropdown_text=cohort_based_on_rs.get()
                                            dropdown_direction=DropdownDirection::Down
                                            dropdown_btn_type=DropdownBtnType::Select
                                            dropdown_options
                                            on_select=Callback::new(move |selected_item: String| {
                                                logging::log!("selected item {:?}", selected_item);
                                                cohort_based_on_ws.set(selected_item);
                                            })
                                        />

                                        <Label title="Set Cohort Definition" />
                                        <EditorProvider>
                                            <Input
                                                id="type-schema"
                                                class="mt-2 rounded-md resize-y w-full max-w-md pt-3"
                                                schema_type=dimension_type_schema
                                                value=dimension_schema_rs.get_untracked()
                                                on_change=move |new_type_schema| {
                                                    dimension_schema_ws.set(new_type_schema)
                                                }
                                                r#type=InputType::Monaco(vec![])
                                            />
                                        </EditorProvider>
                                    </div>
                                }
                            }
                        }
                    }}
                </Suspense>

                {move || {
                    view! {
                        <div class="form-control">
                            <Label title="Position" />
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
                                    logging::log!(
                                        "{:?}", event_target_value(& ev).parse::< u32 > ()
                                    );
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

                <Suspense fallback=move || {
                    view! { <Skeleton variant=SkeletonVariant::Block style_class="h-10" /> }
                }>
                    {move || {
                        let mut functions = combined_resources
                            .with(|c| c.as_ref().map(|c| c.functions.clone()))
                            .unwrap_or_default();
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
                                    description="Function to add validation logic to your dimension"
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
                                    on_select=handle_validation_fn_select
                                />
                            </div>

                            <div class="form-control">
                                <Label
                                    title="AutoComplete Function"
                                    description="Function to add auto complete suggestion to your dimension"
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
                                    on_select=handle_autocomplete_fn_select
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
                                on_submit(());
                            }
                            loading
                        />
                    }
                }}

                <p class="text-red-500">{move || error_message.get()}</p>
            </form>
        </Suspense>
        {move || match update_request_rws.get() {
            None => ().into_view(),
            Some((dimension_name, update_request)) => {
                view! {
                    <ChangeLogSummary
                        dimension_name
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
    Update(UpdateRequest),
}

#[component]
pub fn change_log_summary(
    dimension_name: String,
    change_type: ChangeType,
    #[prop(into)] on_confirm: Callback<()>,
    #[prop(into)] on_close: Callback<()>,
    #[prop(into, default = Signal::derive(|| false))] inprogress: Signal<bool>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let dimension = create_local_resource(
        move || (dimension_name.clone(), workspace.get().0, org.get().0),
        |(dimension_name, workspace, org)| async move {
            dimensions::get(&dimension_name, &workspace, &org).await
        },
    );

    let disabled_rws = RwSignal::new(true);
    let change_type = StoredValue::new(change_type);

    let (title, description, confirm_text) = match change_type.get_value() {
        ChangeType::Update(_) => (
            "Confirm Update",
            "Are you sure you want to update this dimension?",
            "Yes, Update",
        ),
        ChangeType::Delete => (
            "Confirm Delete",
            "Are you sure you want to delete this dimension? Action is irreversible.",
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
                        let dimension = dimension.get();
                        if let Some(Ok(_)) = dimension {
                            disabled_rws.set(false);
                        } else if let Some(Err(e)) = dimension {
                            logging::error!("Error fetching dimension: {}", e);
                        }
                    });
                }
                {move || match dimension.get() {
                    Some(Ok(dim)) => {
                        let (new_schema, new_values) = match change_type.get_value() {
                            ChangeType::Update(update_request) => {
                                let description = update_request
                                    .description
                                    .unwrap_or_else(|| dim.description.clone());
                                let position = update_request.position.unwrap_or(dim.position);
                                let valdiate_fn = update_request
                                    .function_name
                                    .clone()
                                    .unwrap_or_else(|| dim.function_name.clone());
                                let autocomplete_fn = update_request
                                    .autocomplete_function_name
                                    .clone()
                                    .unwrap_or_else(|| { dim.autocomplete_function_name.clone() });
                                (
                                    Some(
                                        update_request.schema.unwrap_or_else(|| dim.schema.clone()),
                                    ),
                                    Map::from_iter(
                                        vec![
                                            Some((
                                                "Description".to_string(),
                                                Value::String(description.deref().to_string()),
                                            )),
                                            Some((
                                                "Position".to_string(),
                                                Value::Number((*position).into()),
                                            )),
                                            valdiate_fn
                                                .map(|f| (
                                                    "Validation Function".to_string(),
                                                    Value::String(f.clone()),
                                                )),
                                            autocomplete_fn
                                                .map(|f| (
                                                    "Autocomplete Function".to_string(),
                                                    Value::String(f.clone()),
                                                )),
                                        ]
                                            .into_iter()
                                            .flatten(),
                                    ),
                                )
                            }
                            ChangeType::Delete => (None, Map::new()),
                        };
                        view! {
                            <JsonChangeSummary
                                title="Schema changes"
                                old_values=Some(dim.schema)
                                new_values=new_schema
                            />
                            <ChangeSummary
                                title="Other changes"
                                key_column="Property"
                                old_values=Map::from_iter(
                                    vec![
                                        Some((
                                            "Description".to_string(),
                                            Value::String(dim.description.deref().to_string()),
                                        )),
                                        Some((
                                            "Position".to_string(),
                                            Value::Number(Number::from(*dim.position)),
                                        )),
                                        dim
                                            .function_name
                                            .map(|f| (
                                                "Validation Function".to_string(),
                                                Value::String(f.clone()),
                                            )),
                                        dim
                                            .autocomplete_function_name
                                            .map(|f| (
                                                "Autocomplete Function".to_string(),
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
                        logging::error!("Error fetching dimension: {}", e);
                        view! { <div>Error fetching dimension</div> }.into_view()
                    }
                    None => view! { <div>Loading...</div> }.into_view(),
                }}
            </Suspense>
        </ChangeLogPopup>
    }
}
