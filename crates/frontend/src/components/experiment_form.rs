pub mod types;
pub mod utils;

use leptos::*;
use serde_json::{Map, Value};
use superposition_types::database::{
    models::{cac::DefaultConfig, experimentation::ExperimentType, Metrics, Workspace},
    types::DimensionWithMandatory,
};
use utils::{create_experiment, update_experiment};
use web_sys::MouseEvent;

use crate::components::change_form::ChangeForm;
use crate::components::context_form::ContextForm;
use crate::components::{
    metrics_form::MetricsForm,
    variant_form::{DeleteVariantForm, VariantForm},
};
use crate::providers::alert_provider::enqueue_alert;
use crate::types::{VariantFormT, VariantFormTs};
use crate::{
    components::{alert::AlertType, button::Button},
    types::{OrganisationId, Tenant},
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
) -> impl IntoView {
    let init_variants = get_init_state(&variants);
    let default_config = StoredValue::new(default_config);
    let edit_id = StoredValue::new(edit_id);
    let dimensions = StoredValue::new(dimensions);
    let experiment_form_type = StoredValue::new(experiment_form_type);
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();
    let workspace_settings = use_context::<StoredValue<Workspace>>().unwrap();

    let (experiment_name, set_experiment_name) = create_signal(name);
    let (f_context, set_context) = create_signal(context.clone());
    let (f_variants, set_variants) = create_signal(init_variants);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);

    let (description_rs, description_ws) = create_signal(description);
    let (change_reason_rs, change_reason_ws) = create_signal(String::new());
    let metrics_rws = RwSignal::new(metrics);

    let handle_context_form_change = move |updated_ctx: Conditions| {
        set_context.set_untracked(updated_ctx);
    };

    let handle_variant_form_change =
        move |updated_varaints: Vec<(String, VariantFormT)>| {
            set_variants.set_untracked(updated_varaints);
        };

    let on_submit = move |event: MouseEvent| {
        req_inprogress_ws.set(true);
        event.prevent_default();

        let f_experiment_name = experiment_name.get();
        let f_context = f_context.get();
        let f_variants = f_variants
            .get()
            .into_iter()
            .map(|(_, variant)| variant)
            .collect::<Vec<VariantFormT>>();
        let tenant = tenant_rws.get().0;
        let org = org_rws.get().0;

        spawn_local({
            async move {
                let result = if let Some(ref experiment_id) = edit_id.get_value() {
                    update_experiment(
                        experiment_id,
                        f_variants,
                        Some(metrics_rws.get_untracked()),
                        tenant,
                        org,
                        description_rs.get_untracked(),
                        change_reason_rs.get_untracked(),
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
        <div>
            <div class="form-control w-full">
                <label class="label">
                    <span class="label-text">Experiment Name</span>
                </label>
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
            <ChangeForm
                title="Reason for Change".to_string()
                placeholder="Enter a reason for this change".to_string()
                value=change_reason_rs.get_untracked()
                on_change=Callback::new(move |new_change_reason| {
                    change_reason_ws.set(new_change_reason)
                })
            />

            <div class="my-4">
                {move || {
                    let context = f_context.get();
                    view! {
                        <ContextForm
                            dimensions=dimensions.get_value()
                            context=context
                            handle_change=handle_context_form_change
                            resolve_mode=workspace_settings.get_value().strict_mode
                            disabled=edit_id.get_value().is_some()
                                || (experiment_form_type.get_value() != ExperimentFormType::Default)
                            heading_sub_text=String::from(
                                "Define rules under which this experiment would run",
                            )
                        />
                    }
                }}

            </div>

            {move || {
                let variants = f_variants.get();
                match experiment_form_type.get_value() {
                    ExperimentFormType::Default => {
                        view! {
                            <VariantForm
                                edit=edit_id.get_value().is_some()
                                variants
                                default_config=default_config.get_value()
                                handle_change=handle_variant_form_change
                            />
                        }
                    }
                    ExperimentFormType::Delete(data) => {
                        view! {
                            <DeleteVariantForm
                                edit=edit_id.get_value().is_some()
                                context=f_context.get()
                                context_data=data
                                variants
                                default_config=default_config.get_value()
                                handle_change=handle_variant_form_change
                            />
                        }
                    }
                }
            }}

            <div class="flex justify-start mt-8">
                {move || {
                    let loading = req_inprogess_rs.get();
                    view! {
                        <Button
                            class="pl-[70px] pr-[70px] w-48 h-12".to_string()
                            text="Submit".to_string()
                            on_click=on_submit
                            loading
                        />
                    }
                }}

            </div>
        </div>
    }
}
