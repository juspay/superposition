pub mod utils;

use std::ops::Deref;

use futures::join;
use leptos::*;
use leptos_router::use_navigate;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Number, Value};
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
    button::{Button, ButtonAnchor},
    change_form::ChangeForm,
    change_summary::{ChangeLogPopup, ChangeSummary, JsonChangeSummary},
    cohort_schema::CohortSchema,
    dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
    form::label::Label,
    input::{Input, InputType},
    skeleton::{Skeleton, SkeletonVariant},
};
use crate::providers::alert_provider::enqueue_alert;
use crate::providers::editor_provider::EditorProvider;
use crate::schema::{JsonSchemaType, SchemaType};
use crate::types::{DimensionTypeOptions, FunctionsName, OrganisationId, Tenant};
use crate::utils::set_function;
use crate::{
    api::{dimensions, fetch_functions, fetch_types},
    components::cohort_schema::CohortSchemaFormat,
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
    #[prop(default = None)] value_compute_function_name: Option<String>,
    #[prop(default = String::new())] description: String,
    #[prop(default = DimensionType::Regular{})] dimension_type: DimensionType,
    #[prop(optional)] dimensions: Option<Vec<DimensionResponse>>,
    #[prop(into)] redirect_url_cancel: String,
) -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let (position_rs, position_ws) = create_signal(position);
    let (dimension_name_rs, dimension_name_ws) = create_signal(dimension_name);
    let (dimension_type_template_rs, dimension_type_template_ws) =
        create_signal(dimension_type_template);
    let (dimension_type_rs, dimension_type_ws) =
        create_signal(DimensionTypeOptions::from(&dimension_type));
    let (dimension_schema_rs, dimension_schema_ws) = create_signal(dimension_schema);
    let (validation_fn_name_rs, validation_fn_name_ws) =
        create_signal(validation_function_name);
    let (value_compute_function_name_rs, value_compute_function_name_ws) =
        create_signal(value_compute_function_name);
    let (description_rs, description_ws) = create_signal(description);
    let (change_reason_rs, change_reason_ws) = create_signal(String::new());
    let (cohort_based_on_rs, cohort_based_on_ws) = create_signal(match dimension_type {
        DimensionType::Regular {} => String::new(),
        DimensionType::LocalCohort(cohort_based_on)
        | DimensionType::RemoteCohort(cohort_based_on) => cohort_based_on,
    });
    let (req_inprogress_rs, req_inprogress_ws) = create_signal(false);
    let update_request_rws = RwSignal::new(None);

    let combined_resources = create_blocking_resource(
        move || (dimensions.clone(), workspace.get().0, org.get().0),
        |(dimensions, workspace, org_id)| async move {
            let dimensions_future = async {
                match dimensions {
                    None => dimensions::fetch(
                        &PaginationParams::all_entries(),
                        &workspace,
                        &org_id,
                    )
                    .await
                    .map(|d| d.data)
                    .unwrap_or_default(),
                    Some(data) => data,
                }
            };

            let all_entries = PaginationParams::all_entries();
            let list_filters = ListFunctionFilters::default();
            let functions_future =
                fetch_functions(&all_entries, &list_filters, &workspace, &org_id);

            let types_future = fetch_types(&all_entries, &workspace, &org_id);

            let (dimensions_result, functions_result, types_result) =
                join!(dimensions_future, functions_future, types_future);

            CombinedResource {
                dimensions: dimensions_result,
                functions: functions_result.map(|d| d.data).unwrap_or_default(),
                type_templates: types_result.map(|d| d.data).unwrap_or_default(),
            }
        },
    );

    let handle_validation_fn_select = move |selected_function: FunctionsName| {
        validation_fn_name_ws.update(|v| set_function(selected_function, v));
    };

    let handle_value_compute_fn_select = move |selected_function: FunctionsName| {
        value_compute_function_name_ws.update(|v| set_function(selected_function, v));
    };

    let (error_message, set_error_message) = create_signal("".to_string());

    let on_submit = Callback::new(move |_| {
        req_inprogress_ws.set(true);
        let function_position = position_rs.get_untracked();
        let dimension_name = dimension_name_rs.get_untracked();
        let validation_fn_name = validation_fn_name_rs.get_untracked();
        let value_compute_function_name = value_compute_function_name_rs.get_untracked();
        let dimension_schema = dimension_schema_rs.get_untracked();
        let cohort_based_on = cohort_based_on_rs.get_untracked();
        let dimension_type = dimension_type_rs
            .get_untracked()
            .to_dimension_type(cohort_based_on);
        let redirect_url = format!(
            "/admin/{}/{}/dimensions/{}",
            org.get().0,
            workspace.get().0,
            dimension_name
        );

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
                            dimension_schema,
                            validation_fn_name,
                            value_compute_function_name,
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
                        dimension_schema,
                        validation_fn_name,
                        value_compute_function_name,
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
                        let navigate = use_navigate();
                        navigate(&redirect_url, Default::default());

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
    });

    view! {
        <Suspense fallback=move || view! { <Skeleton variant=SkeletonVariant::Block /> }>
            <form class="flex flex-col gap-10">
                <div class="flex justify-between items-center gap-2">
                    <h1 class="text-2xl font-extrabold">
                        {if edit {
                            dimension_name_rs.get()
                        } else {
                            "Create Dimension".to_string()
                        }}
                    </h1>
                    <div class="w-full max-w-fit flex flex-row join">
                        <Button
                            force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg rounded-lg"
                            on_click=move |ev: MouseEvent| {
                                ev.prevent_default();
                                on_submit.call(());
                            }
                            icon_class="ri-send-plane-line"
                            text="Submit"
                            loading=req_inprogress_rs.get()
                        />
                        <ButtonAnchor
                            force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg rounded-lg"
                            href=redirect_url_cancel.clone()
                            icon_class="ri-forbid-line"
                            text="Cancel"
                            loading=req_inprogress_rs.get()
                        />
                    </div>
                </div>
                <div class="form-control w-full flex flex-col gap-5 text-gray-700">
                    <Show when=move || !edit>
                        <div class="form-control">
                            <Label title="Dimension" />
                            <input
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
                    </Show>

                    <div class="flex flex-wrap gap-x-10 gap-y-5">
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
                            on_change=move |new_change_reason| {
                                change_reason_ws.set(new_change_reason)
                            }
                        />
                    </div>

                    <div class="flex flex-wrap gap-x-10 gap-y-5">
                        <div class="flex flex-col gap-5">
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
                                    on_select=Callback::new(move |
                                        selected_item: DimensionTypeOptions|
                                    {
                                        logging::log!("selected item {}", selected_item);
                                        if selected_item == DimensionTypeOptions::LocalCohort {
                                            dimension_schema_ws
                                                .set(
                                                    serde_json::to_value(CohortSchemaFormat::default())
                                                        .unwrap_or_default(),
                                                );
                                        } else {
                                            dimension_schema_ws.set(Value::Null);
                                            dimension_type_template_ws.set(String::new());
                                        }
                                        if selected_item == DimensionTypeOptions::Regular {
                                            cohort_based_on_ws.set(String::new());
                                        }
                                        dimension_type_ws.set(selected_item);
                                    })
                                />
                            </div>
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
                        </div>
                        <Suspense>
                            {move || {
                                view! {
                                    <div class="flex flex-col gap-5">
                                        <Show when=move || {
                                            !matches!(
                                                dimension_type_rs.get(),
                                                DimensionTypeOptions::Regular
                                            )
                                        }>
                                            {
                                                let dimension_name = dimension_name_rs.get();
                                                let dimension_options = combined_resources
                                                    .with(|c| c.as_ref().map(|c| c.dimensions.clone()))
                                                    .unwrap_or_default()
                                                    .iter()
                                                    .filter_map(|d| {
                                                        (d.dimension != dimension_name
                                                            && !matches!(
                                                                d.dimension_type,
                                                                DimensionType::LocalCohort(_)
                                                            ))
                                                            .then_some(d.dimension.clone())
                                                    })
                                                    .collect::<Vec<_>>();
                                                let dimension_options = StoredValue::new(dimension_options);
                                                let current_cohort = cohort_based_on_rs.get();
                                                let dropdown_text = if current_cohort.is_empty() {
                                                    "choose a dimension".into()
                                                } else {
                                                    current_cohort
                                                };

                                                view! {
                                                    <div class="form-control">
                                                        <Label title="Derived from" />
                                                        <Dropdown
                                                            disabled=edit
                                                            dropdown_text
                                                            dropdown_direction=DropdownDirection::Down
                                                            dropdown_btn_type=DropdownBtnType::Select
                                                            dropdown_options=dimension_options.get_value()
                                                            on_select=move |selected_item: String| {
                                                                logging::log!("selected item {:?}", selected_item);
                                                                if cohort_based_on_rs.get() != selected_item
                                                                    && dimension_type_rs.get()
                                                                        == DimensionTypeOptions::LocalCohort
                                                                {
                                                                    dimension_schema_ws
                                                                        .set(
                                                                            serde_json::to_value(CohortSchemaFormat::default())
                                                                                .unwrap_or_default(),
                                                                        );
                                                                }
                                                                cohort_based_on_ws.set(selected_item);
                                                            }
                                                        />
                                                    </div>
                                                }
                                            }
                                        </Show>
                                        {move || {
                                            match dimension_type_rs.get() {
                                                DimensionTypeOptions::LocalCohort => {
                                                    let cohort_based_on_name = cohort_based_on_rs.get();
                                                    let cohort_based_on = combined_resources
                                                        .with(|c| c.as_ref().map(|c| c.dimensions.clone()))
                                                        .unwrap_or_default()
                                                        .iter()
                                                        .find(|d| d.dimension == cohort_based_on_name)
                                                        .cloned();
                                                    let Some(cohort_based_on) = cohort_based_on else {
                                                        return ().into_view();
                                                    };

                                                    view! {
                                                        <CohortSchema
                                                            dimension_schema=dimension_schema_rs.get_untracked()
                                                            on_change=move |new_schema| {
                                                                dimension_schema_ws.set(new_schema);
                                                            }
                                                            cohort_based_on
                                                        />
                                                    }
                                                }
                                                DimensionTypeOptions::Regular
                                                | DimensionTypeOptions::RemoteCohort => {
                                                    let options = combined_resources
                                                        .with(|c| c.as_ref().map(|c| c.type_templates.clone()))
                                                        .unwrap_or_default();
                                                    let dimension_t = if dimension_type_template_rs
                                                        .get()
                                                        .is_empty() && edit
                                                    {
                                                        "change current type template".into()
                                                    } else if dimension_type_template_rs.get().is_empty()
                                                        && !edit
                                                    {
                                                        "choose a type template".into()
                                                    } else {
                                                        dimension_type_template_rs.get()
                                                    };
                                                    let dimension_type_schema = SchemaType::Single(
                                                        JsonSchemaType::from(&dimension_schema_rs.get()),
                                                    );
                                                    let monaco_margin = if matches!(
                                                        dimension_type_rs.get(),
                                                        DimensionTypeOptions::LocalCohort
                                                    ) {
                                                        ""
                                                    } else {
                                                        "mt-3"
                                                    };

                                                    view! {
                                                        <div class="form-control">
                                                            <Label title="Set Schema" />
                                                            <Dropdown
                                                                dropdown_width="w-100"
                                                                dropdown_icon="".to_string()
                                                                dropdown_text=dimension_t
                                                                dropdown_direction=DropdownDirection::Down
                                                                dropdown_btn_type=DropdownBtnType::Select
                                                                dropdown_options=options
                                                                on_select=move |selected_item: TypeTemplate| {
                                                                    logging::log!("selected item {:?}", selected_item);
                                                                    dimension_type_template_ws.set(selected_item.type_name);
                                                                    dimension_schema_ws
                                                                        .set(Value::from(selected_item.type_schema));
                                                                }
                                                            />
                                                            <EditorProvider>
                                                                <Input
                                                                    id="type-schema"
                                                                    class=format!(
                                                                        "{monaco_margin} rounded-md resize-y w-full max-w-md pt-3",
                                                                    )
                                                                    schema_type=dimension_type_schema
                                                                    value=dimension_schema_rs.get_untracked()
                                                                    on_change=move |new_type_schema| {
                                                                        dimension_schema_ws.set(new_type_schema)
                                                                    }
                                                                    r#type=InputType::Monaco(vec![])
                                                                    disabled=matches!(
                                                                        dimension_type_rs.get(),
                                                                        DimensionTypeOptions::LocalCohort
                                                                    )
                                                                />
                                                            </EditorProvider>
                                                        </div>
                                                    }
                                                        .into_view()
                                                }
                                            }
                                        }}
                                    </div>
                                }
                            }}
                        </Suspense>
                    </div>

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
                                        value_compute_function_names
                                            .push(ele.function_name.clone());
                                    }
                                });
                            view! {
                                <div class="flex flex-wrap gap-x-10 gap-y-5">
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
                                            title="Value Compute Function"
                                            description="Function to add value compute suggestion to your dimension"
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
                                            on_select=handle_value_compute_fn_select
                                        />
                                    </div>
                                </div>
                            }
                        }}
                    </Suspense>
                    <p class="text-red-500">{move || error_message.get()}</p>
                </div>
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
                                    .value_validation_function_name
                                    .clone()
                                    .unwrap_or_else(|| dim.value_validation_function_name.clone());
                                let value_compute_fn = update_request
                                    .value_compute_function_name
                                    .clone()
                                    .unwrap_or_else(|| { dim.value_compute_function_name.clone() });
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
                            ChangeType::Delete => (None, Map::new()),
                        };
                        view! {
                            <JsonChangeSummary
                                title="Schema changes"
                                old_values=Some(Value::from(dim.schema))
                                new_values=new_schema.map(Value::from)
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
                                            .value_validation_function_name
                                            .map(|f| (
                                                "Value Validation Function".to_string(),
                                                Value::String(f.clone()),
                                            )),
                                        dim
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
                        logging::error!("Error fetching dimension: {}", e);
                        view! { <div>Error fetching dimension</div> }.into_view()
                    }
                    None => view! { <div>Loading...</div> }.into_view(),
                }}
            </Suspense>
        </ChangeLogPopup>
    }
}
