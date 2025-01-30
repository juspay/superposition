pub mod types;
pub mod utils;

use leptos::*;
use superposition_types::{
    database::models::experimentation::VariantType,
    database::{models::cac::DefaultConfig, types::DimensionWithMandatory},
};
use utils::{create_experiment, update_experiment};
use web_sys::MouseEvent;

use crate::components::context_form::ContextForm;
use crate::components::dropdown::DropdownDirection;
use crate::components::variant_form::VariantForm;
use crate::providers::alert_provider::enqueue_alert;
use crate::types::{VariantFormT, VariantFormTs};
use crate::{
    components::{alert::AlertType, button::Button},
    types::{OrganisationId, Tenant},
};

use crate::logic::Conditions;

fn default_variants_for_form() -> Vec<(String, VariantFormT)> {
    vec![
        (
            "control-variant".to_string(),
            VariantFormT {
                id: "control".to_string(),
                variant_type: VariantType::CONTROL,
                overrides: vec![],
            },
        ),
        (
            "experimental-variant".to_string(),
            VariantFormT {
                id: "experimental".to_string(),
                variant_type: VariantType::EXPERIMENTAL,
                overrides: vec![],
            },
        ),
    ]
}

fn get_init_state(variants: &VariantFormTs) -> Vec<(String, VariantFormT)> {
    let init_variants = if variants.is_empty() {
        default_variants_for_form()
    } else {
        variants
            .iter()
            .map(|variant| (variant.id.to_string(), variant.clone()))
            .collect::<Vec<(String, VariantFormT)>>()
    };

    init_variants
}

#[component]
pub fn experiment_form(
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] id: String,
    name: String,
    context: Conditions,
    variants: VariantFormTs,
    #[prop(into)] handle_submit: Callback<String, ()>,
    #[prop(into, default = String::new())] class: String,
    default_config: Vec<DefaultConfig>,
    dimensions: Vec<DimensionWithMandatory>,
    #[prop(default = String::new())] description: String,
    #[prop(default = String::new())] change_reason: String,
) -> impl IntoView {
    let init_variants = get_init_state(&variants);
    let default_config = StoredValue::new(default_config);
    let tenant_s = use_context::<Signal<Tenant>>().unwrap();
    let org_s = use_context::<Signal<OrganisationId>>().unwrap();

    let (experiment_name, set_experiment_name) = create_signal(name);
    let (f_context, set_context) = create_signal(context.clone());
    let (f_variants, set_variants) = create_signal(init_variants);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);

    let (description_rs, description_ws) = create_signal(description);
    let (change_reason_rs, change_reason_ws) = create_signal(change_reason);

    let handle_context_form_change = move |updated_ctx: Conditions| {
        set_context.set_untracked(updated_ctx);
    };

    let handle_variant_form_change =
        move |updated_varaints: Vec<(String, VariantFormT)>| {
            set_variants.set_untracked(updated_varaints);
        };

    let dimensions = StoredValue::new(dimensions);
    let on_submit = move |event: MouseEvent| {
        req_inprogress_ws.set(true);
        event.prevent_default();
        logging::log!("Submitting experiment form");
        logging::log!("Variant Ids{:?}", f_variants.get());

        let f_experiment_name = experiment_name.get();
        let f_context = f_context.get();
        let f_variants = f_variants
            .get()
            .into_iter()
            .map(|(_, variant)| variant)
            .collect::<Vec<VariantFormT>>();
        let tenant = tenant_s.get().0;
        let org = org_s.get().0;
        let experiment_id = id.clone();

        logging::log!("Experiment name {:?}", f_experiment_name);
        logging::log!("Context Experiment form {:?}", f_context);

        spawn_local({
            async move {
                let result = if edit {
                    update_experiment(experiment_id, f_variants, tenant, org).await
                } else {
                    create_experiment(
                        f_context,
                        f_variants,
                        f_experiment_name,
                        tenant,
                        description_rs.get(),
                        change_reason_rs.get(),
                        org,
                    )
                    .await
                };

                match result {
                    Ok(experiment) => {
                        handle_submit.call(experiment.id);
                        let success_message = if edit {
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
                req_inprogress_ws.set(false);
            }
        });
    };

    view! {
        <div class="relative">
            <div class=format!("flex flex-wrap item-center {}", class)>
                <div class="flex flex-col gap-4 w-[45%]">
                    <div class="form-control w-full">
                        <label class="label">
                            <span class="label-text font-semibold text-base">Experiment Name</span>
                        </label>
                        <input
                            disabled=edit
                            value=move || experiment_name.get()
                            on:input=move |ev| set_experiment_name.set(event_target_value(&ev))
                            type="text"
                            name="expName"
                            id="expName"
                            placeholder="ex: testing hyperpay release"
                            class="input input-bordered w-full max-w-md"
                        />
                    </div>

                    <div class="form-control">
                        <label class="label">
                            <span class="label-text">Description</span>
                        </label>
                        <textarea
                            placeholder="Enter description"
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

                    <div class="divider"></div>

                    {move || {
                        let context = f_context.get();
                        view! {
                            <ContextForm
                                dimensions=dimensions.get_value()
                                context=context
                                handle_change=handle_context_form_change
                                disabled=edit
                                dropdown_direction=DropdownDirection::Down
                                heading_sub_text=String::from(
                                    "Define rules under which this experiment would run",
                                )
                            />
                        }
                    }}

                </div>

                <div class="divider divider-horizontal"></div>

                <div class="flex-1">
                    {move || {
                        let variants = f_variants.get();
                        view! {
                            <VariantForm
                                edit=edit
                                variants=variants
                                default_config=default_config.get_value()
                                handle_change=handle_variant_form_change
                            />
                        }
                    }}
                </div>

                <div class="w-full h-10" />
            </div>
            <div class="absolute bottom-0 right-0 p-4 flex justify-end items-end w-full bg-white border-r border-b border-l rounded-2xl rounded-t-none">
                {move || {
                    let loading = req_inprogess_rs.get();
                    view! {
                        <Button text="Submit".to_string() on_click=on_submit.clone() loading />
                    }
                }}

            </div>
        </div>
    }
}
