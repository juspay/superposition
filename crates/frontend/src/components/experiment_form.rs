pub mod types;
pub mod utils;

use std::{collections::HashSet, ops::Deref};

use leptos::*;
use serde_json::{json, Map, Value};
use superposition_types::{
    api::{
        experiments::{ExperimentResponse, OverrideKeysUpdateRequest},
        workspace::WorkspaceResponse,
    },
    database::{
        models::{cac::DefaultConfig, experimentation::ExperimentType, Metrics},
        types::DimensionWithMandatory,
    },
};
use utils::{create_experiment, try_update_payload, update_experiment};

use crate::components::{
    change_summary::{ChangeLogPopup, ChangeSummary, JsonChangeSummary},
    context_form::ContextForm,
    skeleton::{Skeleton, SkeletonVariant},
};
use crate::components::{
    metrics_form::MetricsForm,
    variant_form::{DeleteVariantForm, VariantForm},
};
use crate::providers::alert_provider::enqueue_alert;
use crate::types::{VariantFormT, VariantFormTs};
use crate::{api::fetch_experiment, components::change_form::ChangeForm};
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

enum ResponseType {
    UpdatePrecheck,
    Response(ExperimentResponse),
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
    let workspace_settings = use_context::<StoredValue<WorkspaceResponse>>().unwrap();

    let (experiment_name, set_experiment_name) = create_signal(name);
    let (context_rs, context_ws) = create_signal(context.clone());
    let (variants_rs, variants_ws) = create_signal(init_variants);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);

    let (description_rs, description_ws) = create_signal(description);
    let (change_reason_rs, change_reason_ws) = create_signal(String::new());
    let metrics_rws = RwSignal::new(metrics);
    let update_request_rws = RwSignal::new(None);

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

    let on_submit = move || {
        req_inprogress_ws.set(true);

        let f_experiment_name = experiment_name.get_untracked();
        let f_context = context_rs.get_untracked();
        let f_variants = variants_rs
            .get_untracked()
            .into_iter()
            .map(|(_, variant)| variant)
            .collect::<Vec<VariantFormT>>();
        let tenant = tenant_rws.get_untracked().0;
        let org = org_rws.get_untracked().0;

        spawn_local({
            async move {
                let result =
                    match (edit_id.get_value(), update_request_rws.get_untracked()) {
                        (Some(ref experiment_id), Some((_, payload))) => {
                            update_experiment(experiment_id, payload, tenant, org)
                                .await
                                .map(ResponseType::Response)
                        }
                        (Some(experiment_id), None) => {
                            let request_payload = try_update_payload(
                                f_variants,
                                Some(metrics_rws.get_untracked()),
                                description_rs.get_untracked(),
                                change_reason_rs.get_untracked(),
                            );
                            match request_payload {
                                Ok(payload) => {
                                    update_request_rws
                                        .set(Some((experiment_id, payload)));
                                    Ok(ResponseType::UpdatePrecheck)
                                }
                                Err(e) => Err(e),
                            }
                        }
                        _ => create_experiment(
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
                        .map(ResponseType::Response),
                    };

                req_inprogress_ws.set(false);
                match result {
                    Ok(ResponseType::UpdatePrecheck) => (),
                    Ok(ResponseType::Response(res)) => {
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
                on_change=move |new_description| description_ws.set(new_description)
            />
            <MetricsForm
                metrics=metrics_rws.get_untracked()
                on_change=Callback::new(move |metrics| metrics_rws.set(metrics))
            />
            <ChangeForm
                title="Reason for Change".to_string()
                placeholder="Enter a reason for this change".to_string()
                value=change_reason_rs.get_untracked()
                on_change=move |new_change_reason| change_reason_ws.set(new_change_reason)
            />

            <div class="my-4">
                {move || {
                    view! {
                        <ContextForm
                            dimensions=dimensions.get_value()
                            context_rs
                            context_ws
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

            </div>

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
                        class="w-48 h-12 mt-8 px-[70px]".to_string()
                        text="Submit".to_string()
                        on_click=move |ev| {
                            ev.prevent_default();
                            on_submit();
                        }
                        loading
                    />
                }
            }}
        </div>
        {move || match update_request_rws.get() {
            None => ().into_view(),
            Some((experiment_id, update_request)) => {
                view! {
                    <ChangeLogSummary
                        experiment_id
                        update_request
                        on_confirm=move |_| on_submit()
                        on_close=move |_| update_request_rws.set(None)
                    />
                }
            }
        }}
    }
}

#[component]
fn change_log_summary(
    experiment_id: String,
    update_request: OverrideKeysUpdateRequest,
    #[prop(into)] on_confirm: Callback<()>,
    #[prop(into)] on_close: Callback<()>,
) -> impl IntoView {
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();

    let experiment = create_local_resource(
        move || (experiment_id.clone(), tenant_rws.get().0, org_rws.get().0),
        |(experiment_id, tenant, org)| async move {
            fetch_experiment(experiment_id, tenant, org).await
        },
    );

    let disabled_rws = RwSignal::new(true);
    let update_request = StoredValue::new(update_request);

    view! {
        <ChangeLogPopup
            title="Confirm Update"
            description="Are you sure you want to update this context?"
            confirm_text="Yes, Update"
            on_confirm
            on_close
            disabled=disabled_rws.read_only()
        >
            <Suspense fallback=move || {
                view! { <Skeleton variant=SkeletonVariant::Block style_class="h-10".to_string() /> }
            }>
                {
                    Effect::new(move |_| {
                        if let Some(Ok(_)) = experiment.get() {
                            disabled_rws.set(false);
                        } else if let Some(Err(e)) = experiment.get() {
                            logging::error!("Error fetching context: {}", e);
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
                            <ChangeSummary
                                title="Other changes"
                                key_column="Property"
                                old_values=Map::from_iter(
                                    vec![
                                        (
                                            "Description".to_string(),
                                            Value::String(experiment.description.deref().to_string()),
                                        ),
                                    ],
                                )
                                new_values=Map::from_iter(
                                    vec![("Description".to_string(), Value::String(description))],
                                )
                            />
                            <JsonChangeSummary
                                title="Metrics changes"
                                old_values=serde_json::to_value(experiment.metrics).ok()
                                new_values=serde_json::to_value(update_request.metrics).ok()
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
