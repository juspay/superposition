use std::collections::HashSet;
use std::ops::Deref;

use leptos::*;
use leptos_router::use_navigate;
use serde_json::{Map, Value};
use superposition_types::{
    api::{
        dimension::DimensionResponse,
        experiment_groups::ExpGroupFilters,
        experiments::{ExperimentResponse, OverrideKeysUpdateRequest},
        functions::FunctionEnvironment,
    },
    custom_query::{DimensionQuery, PaginationParams},
    database::models::{
        Metrics,
        cac::DefaultConfig,
        experimentation::{ExperimentGroup, ExperimentType, VariantType},
    },
};

use crate::api::experiment_groups;
use crate::api::fetch_experiment;
use crate::components::experiment_form::utils::{
    create_experiment, try_update_payload, update_experiment,
};
use crate::components::{
    alert::AlertType,
    button::{ButtonAnchor, ButtonStyle},
    change_form::ChangeForm,
    change_summary::{ChangeLogPopup, ChangeSummary, JsonChangeSummary},
    condition_pills::Condition,
    context_form::ContextForm,
    dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
    experiment_form::ExperimentFormType,
    form::label::Label,
    metrics_form::MetricsForm,
    skeleton::{Skeleton, SkeletonVariant},
    step_indicator::{Step, StepIndicator, StepNavigation, StepType},
    variant_form::{DeleteVariantForm, VariantForm},
};
use crate::logic::Conditions;
use crate::providers::{
    alert_provider::enqueue_alert,
    condition_collapse_provider::ConditionCollapseProvider,
    editor_provider::EditorProvider,
};
use crate::types::{OrganisationId, VariantFormT, VariantFormTs, Workspace};

#[allow(clippy::large_enum_variant)]
enum ResponseType {
    UpdatePrecheck,
    Response(ExperimentResponse),
}

#[component]
pub fn ExperimentFormPage(
    #[prop(default = false)] edit: bool,
    #[prop(optional)] edit_id: Option<String>,
    #[prop(default = String::new())] name: String,
    context: Conditions,
    #[prop(default = VariantFormTs::default())] variants: VariantFormTs,
    #[prop(default = ExperimentFormType::Default)]
    experiment_form_type: ExperimentFormType,
    default_config: Vec<DefaultConfig>,
    dimensions: Vec<DimensionResponse>,
    #[prop(default = String::new())] description: String,
    metrics: Metrics,
    #[prop(default = None)] experiment_group_id: Option<String>,
    #[prop(into)] redirect_url_cancel: String,
    #[prop(into)] redirect_url_success: String,
) -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let default_config = StoredValue::new(default_config);
    let dimensions = StoredValue::new(dimensions);
    let edit_id = StoredValue::new(edit_id);
    let experiment_form_type = StoredValue::new(experiment_form_type);

    let (experiment_name, set_experiment_name) = create_signal(name);
    let (context_rs, context_ws) = create_signal(context);
    let init_variants = variants
        .iter()
        .map(|variant| {
            let variant_value = if let ExperimentFormType::Delete(_) =
                experiment_form_type.get_value()
                && variant.variant_type == VariantType::EXPERIMENTAL
            {
                VariantFormT {
                    overrides: Vec::new(),
                    ..variant.clone()
                }
            } else {
                variant.clone()
            };
            (variant_value.id.clone(), variant_value)
        })
        .collect::<Vec<(String, VariantFormT)>>();
    let (variants_rs, variants_ws) = create_signal(init_variants);
    let (req_inprogress_rs, req_inprogress_ws) = create_signal(false);
    let (description_rs, description_ws) = create_signal(description);
    let (change_reason_rs, change_reason_ws) = create_signal(String::new());
    let metrics_rws = RwSignal::new(metrics);
    let update_request_rws = RwSignal::new(None);
    let (experiment_group_id_rs, experiment_group_id_ws) =
        create_signal(experiment_group_id);

    let current_step = RwSignal::new(StepType::ContextStep);

    let steps = vec![
        Step {
            title: "Context & Metadata".to_string(),
            description: Some("Define context conditions and metadata".to_string()),
            step_type: StepType::ContextStep,
        },
        Step {
            title: "Variants".to_string(),
            description: Some("Configure experiment variants".to_string()),
            step_type: StepType::OverrideStep,
        },
    ];

    let experiment_groups_resource: Resource<(String, String), Vec<ExperimentGroup>> =
        create_blocking_resource(
            move || (workspace.get().0, org.get().0),
            |(workspace, org)| async move {
                experiment_groups::list(
                    &ExpGroupFilters::default(),
                    &PaginationParams::all_entries(),
                    &DimensionQuery::default(),
                    &workspace,
                    &org,
                )
                .await
                .map(|data| data.data)
                .unwrap_or_default()
            },
        );

    let fn_environment = Memo::new(move |_| {
        let context = context_rs.get().into();
        let overrides = variants_rs
            .get()
            .iter()
            .find(|(_, v)| v.variant_type == VariantType::EXPERIMENTAL)
            .map(|(_, variant)| variant.overrides.iter().cloned().collect::<Map<_, _>>())
            .unwrap_or_default();
        FunctionEnvironment { context, overrides }
    });

    let handle_variant_form_change =
        move |updated_variants: Vec<(String, VariantFormT)>| {
            variants_ws.set_untracked(updated_variants);
        };

    let on_previous = Callback::new(move |_| {
        current_step.update(|step| {
            *step = match step {
                StepType::OverrideStep => StepType::ContextStep,
                StepType::ContextStep => StepType::ContextStep,
            };
        });
    });

    let on_next = Callback::new(move |_| {
        current_step.update(|step| {
            *step = match step {
                StepType::ContextStep => StepType::OverrideStep,
                StepType::OverrideStep => StepType::OverrideStep,
            };
        });
    });

    let redirect_url_success = StoredValue::new(redirect_url_success);
    let on_submit = Callback::new(move |_: ()| {
        req_inprogress_ws.set(true);
        spawn_local(async move {
            let f_variants = variants_rs
                .get_untracked()
                .into_iter()
                .map(|(_, variant)| variant)
                .collect::<Vec<VariantFormT>>();
            let workspace = workspace.get_untracked();
            let org = org.get_untracked();
            let redirect_url = redirect_url_success.get_value();
            let edit_id = edit_id.get_value();
            let f_experiment_name = experiment_name.get_untracked();

            let experiment_group_id = if let Some(experiment_group_id) =
                experiment_group_id_rs.get_untracked()
            {
                Value::String(experiment_group_id)
            } else {
                Value::Null
            };

            let result = match (edit_id.clone(), update_request_rws.get_untracked()) {
                (Some(ref experiment_id), Some((_, payload))) => {
                    let future =
                        update_experiment(experiment_id, payload, &workspace.0, &org.0);
                    update_request_rws.set(None);
                    future.await.map(ResponseType::Response)
                }
                (Some(experiment_id), None) => {
                    let request_payload = try_update_payload(
                        f_variants,
                        Some(metrics_rws.get_untracked()),
                        description_rs.get_untracked(),
                        change_reason_rs.get_untracked(),
                        experiment_group_id,
                    );
                    match request_payload {
                        Ok(payload) => {
                            update_request_rws.set(Some((experiment_id, payload)));
                            Ok(ResponseType::UpdatePrecheck)
                        }
                        Err(e) => Err(e),
                    }
                }
                _ => create_experiment(
                    context_rs.get_untracked(),
                    f_variants,
                    Some(metrics_rws.get_untracked()),
                    f_experiment_name,
                    ExperimentType::from(experiment_form_type.get_value()),
                    description_rs.get_untracked(),
                    change_reason_rs.get_untracked(),
                    experiment_group_id,
                    &workspace.0,
                    &org.0,
                )
                .await
                .map(ResponseType::Response),
            };

            let is_edit = edit_id.is_some();

            req_inprogress_ws.set(false);
            match result {
                Ok(ResponseType::UpdatePrecheck) => (),
                Ok(ResponseType::Response(res)) => {
                    let navigate = use_navigate();
                    let final_redirect_url = match is_edit {
                        true => redirect_url.clone(),
                        false => format!(
                            "/admin/{}/{}/experiments/{}",
                            org.0, workspace.0, res.id
                        ),
                    };
                    navigate(&final_redirect_url, Default::default());
                    let success_message = if is_edit {
                        "Experiment updated successfully!"
                    } else {
                        "New Experiment created successfully!"
                    };
                    enqueue_alert(
                        String::from(success_message),
                        AlertType::Success,
                        5000,
                    );
                }
                Err(e) => {
                    enqueue_alert(e, AlertType::Error, 5000);
                }
            }
        });
    });

    view! {
        <div class="flex flex-col gap-6">
            <div class="flex justify-between items-center">
                <h1 class="text-2xl font-extrabold">
                    {if edit { "Edit Experiment" } else { "Create Experiment" }}
                </h1>
                <ButtonAnchor
                    text="Cancel".to_string()
                    href=redirect_url_cancel.clone()
                    icon_class="ri-close-line".to_string()
                    style=ButtonStyle::Outline
                />
            </div>

            <StepIndicator steps=&steps current_step=current_step />

            <EditorProvider>
                <div class="card bg-base-100 shadow">
                    <div class="card-body">
                        <Show when=move || current_step.get() == StepType::ContextStep>
                            <div class="flex flex-col gap-5">
                                <div class="form-control w-full">
                                    <Label title="Experiment Name" />
                                    <input
                                        disabled=edit_id.get_value().is_some()
                                        value=move || experiment_name.get()
                                        on:input=move |ev| {
                                            set_experiment_name.set(event_target_value(&ev))
                                        }
                                        type="text"
                                        name="expName"
                                        id="expName"
                                        placeholder="ex: testing hyperpay release"
                                        class="input input-bordered w-full max-w-md"
                                    />
                                </div>

                                <ChangeForm
                                    title="Description".to_string()
                                    placeholder="Enter a description".to_string()
                                    value=description_rs.get_untracked()
                                    on_change=move |new_description| {
                                        description_ws.set(new_description)
                                    }
                                />
                                <MetricsForm
                                    metrics=metrics_rws.get_untracked()
                                    on_change=Callback::new(move |metrics| metrics_rws.set(metrics))
                                />

                                <Suspense fallback=move || {
                                    view! {
                                        <Skeleton
                                            variant=SkeletonVariant::Block
                                            style_class="h-10"
                                        />
                                    }
                                }>
                                    {move || {
                                        let experiment_groups = experiment_groups_resource
                                            .get()
                                            .unwrap_or_default();
                                        let mut experiment_options: Vec<Option<String>> = experiment_groups
                                            .iter()
                                            .map(|group| Some(group.id.to_string()))
                                            .collect();
                                        experiment_options.insert(0, None);
                                        view! {
                                            <div class="form-control">
                                                <Label title="Experiment Group" />
                                                <Dropdown
                                                    dropdown_width="w-100"
                                                    dropdown_icon="".to_string()
                                                    dropdown_direction=DropdownDirection::Down
                                                    dropdown_btn_type=DropdownBtnType::Select
                                                    dropdown_text=experiment_group_id_rs
                                                        .get()
                                                        .map(|id| id.to_string())
                                                        .unwrap_or("Select Experiment Group".to_string())
                                                    dropdown_options=experiment_options
                                                    on_select=Callback::new(move |selected: Option<String>| {
                                                        experiment_group_id_ws.set(selected);
                                                    })
                                                />
                                            </div>
                                        }
                                    }}
                                </Suspense>

                                <ChangeForm
                                    title="Reason for Change".to_string()
                                    placeholder="Enter a reason for this change".to_string()
                                    value=change_reason_rs.get_untracked()
                                    on_change=move |new_change_reason| {
                                        change_reason_ws.set(new_change_reason)
                                    }
                                />

                                <ContextForm
                                    dimensions=dimensions.get_value()
                                    context=context_rs.get_untracked()
                                    on_context_change=move |new_context| context_ws.set(new_context)
                                    disabled=edit_id.get_value().is_some()
                                        || (experiment_form_type.get_value()
                                            != ExperimentFormType::Default)
                                    heading_sub_text=String::from(
                                        "Define rules under which this experiment would run",
                                    )
                                    fn_environment
                                />
                            </div>
                        </Show>

                        <Show when=move || current_step.get() == StepType::OverrideStep>
                            <div class="flex flex-col gap-4">
                                <h2 class="card-title">"Context"</h2>
                                <div class="flex flex-row flex-wrap gap-2">
                                    <ConditionCollapseProvider>
                                        <Condition
                                            conditions=context_rs.get_untracked()
                                            id="experiment-form-context"
                                            class="h-fit w-[300px]"
                                        />
                                    </ConditionCollapseProvider>
                                </div>
                                <div class="overflow-visible">
                                    {move || {
                                        let variants = variants_rs.get();
                                        match experiment_form_type.get_value() {
                                            ExperimentFormType::Default => {
                                                view! {
                                                    <VariantForm
                                                        edit=edit_id.get_value().is_some()
                                                        context=context_rs
                                                        variants
                                                        default_config=default_config.get_value()
                                                        handle_change=handle_variant_form_change
                                                        fn_environment
                                                        vertical_split=true
                                                    />
                                                }
                                            }
                                            ExperimentFormType::Delete(data) => {
                                                view! {
                                                    <DeleteVariantForm
                                                        edit=edit_id.get_value().is_some()
                                                        context=context_rs.get()
                                                        context_data=data
                                                        variants
                                                        default_config=default_config.get_value()
                                                        handle_change=handle_variant_form_change
                                                        fn_environment
                                                    />
                                                }
                                            }
                                        }
                                    }}
                                </div>
                            </div>
                        </Show>

                        <StepNavigation
                            current_step=current_step
                            on_previous=on_previous
                            on_next=on_next
                            on_submit=on_submit
                            submit_loading=req_inprogress_rs
                        />
                    </div>
                </div>
            </EditorProvider>
        </div>

        {move || match update_request_rws.get() {
            None => ().into_view(),
            Some((experiment_id, update_request)) => {
                view! {
                    <ChangeLogSummary
                        experiment_id
                        update_request
                        on_confirm=on_submit
                        on_close=Callback::new(move |_| update_request_rws.set(None))
                    />
                }
            }
        }}
    }
}

#[component]
fn ChangeLogSummary(
    experiment_id: String,
    update_request: OverrideKeysUpdateRequest,
    #[prop(into)] on_confirm: Callback<()>,
    #[prop(into)] on_close: Callback<()>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let experiment = create_local_resource(
        move || (experiment_id.clone(), workspace.get().0, org.get().0),
        |(experiment_id, workspace, org)| async move {
            fetch_experiment(experiment_id, &workspace, &org).await
        },
    );

    let disabled_rws = RwSignal::new(true);
    let update_request = StoredValue::new(update_request);

    view! {
        <ChangeLogPopup
            title="Confirm Update"
            description="Are you sure you want to update this experiment?"
            confirm_text="Yes, Update"
            on_confirm
            on_close
            disabled=disabled_rws
        >
            <Suspense fallback=move || {
                view! { <Skeleton variant=SkeletonVariant::Block style_class="h-10".to_string() /> }
            }>
                {
                    Effect::new(move |_| {
                        let experiment = experiment.get();
                        if let Some(Ok(_)) = experiment {
                            disabled_rws.set(false);
                        } else if let Some(Err(e)) = experiment {
                            logging::error!("Error fetching experiment: {}", e);
                        }
                    });
                }
                {move || match experiment.get() {
                    Some(Ok(experiment)) => {
                        let update_request = update_request.get_value();
                        let description = update_request
                            .description
                            .unwrap_or_else(|| experiment.description.clone())
                            .to_string();
                        let experiment_group_id = update_request
                            .experiment_group_id
                            .map(|r| r.get_value().map(|v| v.to_string()))
                            .unwrap_or_else(|| experiment.experiment_group_id.clone());
                        let variant_ids = experiment
                            .variants
                            .iter()
                            .map(|v| v.id.clone())
                            .chain(update_request.variants.iter().map(|v| v.id.clone()))
                            .collect::<HashSet<_>>();
                        let variant_data = variant_ids
                            .iter()
                            .map(|id| {
                                (
                                    id.clone(),
                                    experiment
                                        .variants
                                        .iter()
                                        .find(|v| v.id == *id)
                                        .map(|v| {
                                            (*v.overrides.clone().into_inner().clone()).clone()
                                        })
                                        .unwrap_or_default(),
                                    update_request
                                        .variants
                                        .iter()
                                        .find(|v| v.id == *id)
                                        .map(|v| {
                                            (*v.overrides.clone().into_inner().clone()).clone()
                                        })
                                        .unwrap_or_default(),
                                )
                            })
                            .collect::<Vec<_>>();
                        view! {
                            {variant_data
                                .into_iter()
                                .map(|(variant_id, old_overrides, new_overrides)| {
                                    view! {
                                        <ChangeSummary
                                            title=format!("Override changes for {variant_id}")
                                            old_values=old_overrides
                                            new_values=new_overrides
                                        />
                                    }
                                })
                                .collect_view()}
                            <JsonChangeSummary
                                title="Metrics changes"
                                old_values=serde_json::to_value(experiment.metrics).ok()
                                new_values=serde_json::to_value(update_request.metrics).ok()
                            />
                            <ChangeSummary
                                title="Other changes"
                                key_column="Property"
                                old_values=Map::from_iter(
                                    vec![
                                        Some((
                                            "Description".to_string(),
                                            Value::String(experiment.description.deref().to_string()),
                                        )),
                                        experiment
                                            .experiment_group_id
                                            .as_ref()
                                            .map(|id| (
                                                "Experiment Group".to_string(),
                                                Value::String(id.clone()),
                                            )),
                                    ]
                                        .into_iter()
                                        .flatten(),
                                )
                                new_values=Map::from_iter(
                                    vec![
                                        Some((
                                            "Description".to_string(),
                                            Value::String(description),
                                        )),
                                        experiment_group_id
                                            .map(|id| (
                                                "Experiment Group".to_string(),
                                                Value::String(id.clone()),
                                            )),
                                    ]
                                        .into_iter()
                                        .flatten(),
                                )
                            />
                        }
                            .into_view()
                    }
                    Some(Err(e)) => {
                        logging::error!("Error fetching experiment: {}", e);
                        view! { <div>Error fetching experiment</div> }.into_view()
                    }
                    None => view! { <div>Loading...</div> }.into_view(),
                }}
            </Suspense>
        </ChangeLogPopup>
    }
}
