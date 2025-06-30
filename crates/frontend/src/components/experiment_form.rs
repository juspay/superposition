pub mod types;
pub mod utils;

use leptos::*;
use serde_json::{json, Map, Value};
use superposition_types::{
    api::{experiment_groups::ExpGroupFilters, workspace::WorkspaceResponse},
    custom_query::PaginationParams,
    database::{
        models::{
            cac::DefaultConfig,
            experimentation::{ExperimentGroup, ExperimentType},
            Metrics,
        },
        types::DimensionWithMandatory,
    },
};
use utils::{create_experiment, update_experiment};
use web_sys::MouseEvent;

use crate::{
    api::experiment_groups,
    components::{
        alert::AlertType,
        button::Button,
        change_form::ChangeForm,
        context_form::ContextForm,
        dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
        form::label::Label,
        metrics_form::MetricsForm,
        skeleton::{Skeleton, SkeletonVariant},
        variant_form::{DeleteVariantForm, VariantForm},
    },
    providers::alert_provider::enqueue_alert,
    types::{OrganisationId, Tenant, VariantFormT, VariantFormTs},
};

use crate::logic::Conditions;

fn get_init_state(variants: &[VariantFormT]) -> Vec<(String, VariantFormT)> {
    variants
        .iter()
        .map(|variant| (variant.id.to_string(), variant.clone()))
        .collect::<Vec<(String, VariantFormT)>>()
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExperimentFormType {
    Default,
    Delete(Option<(String, Map<String, Value>)>),
}

impl From<ExperimentFormType> for ExperimentType {
    fn from(experiment_form_type: ExperimentFormType) -> Self {
        match experiment_form_type {
            ExperimentFormType::Default => ExperimentType::Default,
            ExperimentFormType::Delete(_) => ExperimentType::DeleteOverrides,
        }
    }
}

impl From<ExperimentType> for ExperimentFormType {
    fn from(experiment_type: ExperimentType) -> Self {
        match experiment_type {
            ExperimentType::Default => ExperimentFormType::Default,
            ExperimentType::DeleteOverrides => ExperimentFormType::Delete(None),
        }
    }
}

#[component]
pub fn experiment_form(
    #[prop(optional)] edit_id: Option<String>,
    #[prop(default = String::new())] name: String,
    context: Conditions,
    #[prop(default = VariantFormTs::default())] variants: VariantFormTs,
    #[prop(default = ExperimentFormType::Default)]
    experiment_form_type: ExperimentFormType,
    #[prop(into)] handle_submit: Callback<String, ()>,
    default_config: Vec<DefaultConfig>,
    dimensions: Vec<DimensionWithMandatory>,
    #[prop(default = String::new())] description: String,
    metrics: Metrics,
    #[prop(default = None)] experiment_group_id: Option<String>,
) -> impl IntoView {
    let init_variants = get_init_state(&variants);
    let default_config = StoredValue::new(default_config);
    let edit_id = StoredValue::new(edit_id);
    let dimensions = StoredValue::new(dimensions);
    let experiment_form_type = StoredValue::new(experiment_form_type);
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let workspace_settings = use_context::<StoredValue<WorkspaceResponse>>().unwrap();

    let (experiment_name, set_experiment_name) = create_signal(name);
    let (context_rs, context_ws) = create_signal(context.clone());
    let (variants_rs, variants_ws) = create_signal(init_variants);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);

    let (description_rs, description_ws) = create_signal(description);
    let (change_reason_rs, change_reason_ws) = create_signal(String::new());
    let metrics_rws = RwSignal::new(metrics);
    let (experiment_group_id_rs, experiment_group_id_ws) =
        create_signal(experiment_group_id);

    let experiment_groups_resource: Resource<(String, String), Vec<ExperimentGroup>> =
        create_blocking_resource(
            move || (workspace.get().0, org.get().0),
            |(current_tenant, org)| async move {
                experiment_groups::fetch_all(
                    &ExpGroupFilters::default(),
                    &PaginationParams::all_entries(),
                    &current_tenant,
                    &org,
                )
                .await
                .map(|data| data.data)
                .unwrap_or_default()
            },
        );

    let handle_context_form_change = move |updated_ctx: Conditions| {
        context_ws.set_untracked(updated_ctx);
    };

    let handle_variant_form_change =
        move |updated_varaints: Vec<(String, VariantFormT)>| {
            variants_ws.set_untracked(updated_varaints);
        };

    let fn_environment = create_memo(move |_| {
        let context = context_rs.get();
        let overrides = variants_rs
            .get()
            .into_iter()
            .map(|(variant_id, o)| (variant_id, o.overrides.clone()))
            .collect::<Vec<_>>();
        json!({
            "context": context,
            "overrides": overrides,
        })
    });

    let on_submit = move |event: MouseEvent| {
        req_inprogress_ws.set(true);
        event.prevent_default();

        let f_experiment_name = experiment_name.get();
        let f_context = context_rs.get();
        let f_variants = variants_rs
            .get()
            .into_iter()
            .map(|(_, variant)| variant)
            .collect::<Vec<VariantFormT>>();
        let tenant = workspace.get().0;
        let org = org.get().0;

        spawn_local({
            async move {
                let experiment_group_id = if let Some(experiment_group_id) =
                    experiment_group_id_rs.get_untracked()
                {
                    Value::String(experiment_group_id)
                } else {
                    Value::Null
                };
                let result = if let Some(ref experiment_id) = edit_id.get_value() {
                    update_experiment(
                        experiment_id,
                        f_variants,
                        Some(metrics_rws.get_untracked()),
                        tenant,
                        org,
                        description_rs.get_untracked(),
                        change_reason_rs.get_untracked(),
                        experiment_group_id,
                    )
                    .await
                } else {
                    create_experiment(
                        f_context,
                        f_variants,
                        Some(metrics_rws.get_untracked()),
                        f_experiment_name,
                        ExperimentType::from(experiment_form_type.get_value()),
                        tenant,
                        description_rs.get_untracked(),
                        change_reason_rs.get_untracked(),
                        org,
                        experiment_group_id,
                    )
                    .await
                };

                req_inprogress_ws.set(false);
                match result {
                    Ok(res) => {
                        handle_submit.call(res.id);
                        let success_message = if edit_id.get_value().is_some() {
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
                        // Handle error
                        // We can consider logging or displaying the error
                    }
                }
            }
        });
    };

    view! {
        <div class="flex flex-col gap-5">
            <div class="form-control w-full">
                <Label title="Experiment Name" />
                <input
                    disabled=edit_id.get_value().is_some()
                    value=move || experiment_name.get()
                    on:input=move |ev| set_experiment_name.set(event_target_value(&ev))
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
                on_change=Callback::new(move |new_description| {
                    description_ws.set(new_description)
                })
            />
            <MetricsForm
                metrics=metrics_rws.get_untracked()
                on_change=Callback::new(move |metrics| metrics_rws.set(metrics))
            />

            <Suspense fallback=move || {
                view! { <Skeleton variant=SkeletonVariant::Block style_class="h-10" /> }
            }>
                {move || {
                    let experiment_groups = experiment_groups_resource.get().unwrap_or_default();
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
                on_change=Callback::new(move |new_change_reason| {
                    change_reason_ws.set(new_change_reason)
                })
            />

            {move || {
                view! {
                    <ContextForm
                        dimensions=dimensions.get_value()
                        context=context_rs.get_untracked()
                        on_context_change=move |new_context| context_ws.set(new_context)
                        handle_change=handle_context_form_change
                        resolve_mode=workspace_settings.get_value().strict_mode
                        disabled=edit_id.get_value().is_some()
                            || (experiment_form_type.get_value() != ExperimentFormType::Default)
                        heading_sub_text=String::from(
                            "Define rules under which this experiment would run",
                        )
                        fn_environment
                    />
                }
            }}

            {move || {
                let variants = variants_rs.get();
                match experiment_form_type.get_value() {
                    ExperimentFormType::Default => {
                        view! {
                            <VariantForm
                                edit=edit_id.get_value().is_some()
                                variants
                                default_config=default_config.get_value()
                                handle_change=handle_variant_form_change
                                fn_environment
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

            {move || {
                let loading = req_inprogess_rs.get();
                view! {
                    <Button
                        class="self-end h-12 w-48"
                        text="Submit"
                        icon_class="ri-send-plane-line"
                        on_click=on_submit
                        loading
                    />
                }
            }}
        </div>
    }
}
