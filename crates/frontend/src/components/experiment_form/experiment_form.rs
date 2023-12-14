use super::utils::create_experiment;
use crate::components::Button::Button::Button;
use crate::components::{
    context_form::context_form::ContextForm, override_form::override_form::OverrideForm,
};
use crate::pages::ExperimentList::types::{
    DefaultConfig, Dimension, Variant, VariantType,
};
use leptos::*;
use serde_json::{Map, Value};
use std::sync::RwLock;
use wasm_bindgen::JsCast;
use web_sys::{HtmlInputElement, MouseEvent, SubmitEvent};

#[component]
pub fn ExperimentForm<NF>(
    name: String,
    context: Vec<(String, String, String)>,
    variants: Vec<Variant>,
    dimensions: Vec<Dimension>,
    default_config: Vec<DefaultConfig>,
    handle_submit: NF,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let (experiment_name, set_experiment_name) = create_signal(name);
    let (f_context, set_context) = create_signal(context.clone());
    let (f_variants, set_variants) = create_signal(variants);

    let handle_context_form_change = move |updated_ctx: Vec<(String, String, String)>| {
        set_context.set(updated_ctx);
    };

    let handle_override_form_change = move |variant_idx: usize| {
        let handle_change = move |updated_overrides: Map<String, Value>| {
            set_variants.update(|curr_variants| {
                curr_variants[variant_idx].overrides = updated_overrides.clone();
            });
        };
        handle_change
    };

    let on_submit = move |event: MouseEvent| {
        event.prevent_default();
        logging::log!("Submitting experiment form");

        let f_experiment_name = experiment_name.get();
        let f_context = f_context.get();
        let f_variants = f_variants.get();
        let tenant = tenant_rs.get();
        let handle_submit_clone = handle_submit.clone();

        logging::log!("{:?}", f_experiment_name);
        logging::log!("{:?}", f_variants);
        logging::log!("{:?}", f_context);

        spawn_local({
            async move {
                let result =
                    create_experiment(f_context, f_variants, f_experiment_name, tenant)
                        .await;

                match result {
                    Ok(value) => {
                        handle_submit_clone();
                    }
                    Err(_) => {
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
                    <span class="label-text">Name</span>
                </label>
                <input
                    value=move || experiment_name.get()
                    on:input=move |ev| set_experiment_name.set(event_target_value(&ev))
                    type="text"
                    name="expName"
                    id="expName"
                    placeholder="ex: testing hyperpay release"
                    class="input input-bordered w-full max-w-xs"
                />
            </div>

            <div class="my-4">
                <ContextForm
                    dimensions=dimensions
                    context=context
                    handle_change=handle_context_form_change
                    is_standalone=false
                />
            </div>

            <div class="form-control w-full">
                <div class="flex items-center justify-between gap-4">
                    <label class="label">
                        <span class="label-text font-semibold text-base">Variants</span>
                    </label>

                    // make some more random default id for this can lead to undefined behaviour if we use index based ids
                    <button
                        class="btn btn-outline btn-sm text-xs m-1"
                        on:click:undelegated=move |_| {
                            leptos::logging::log!("add new variant");
                            set_variants
                                .update(|curr_variants| {
                                    let total_variants = curr_variants.len();
                                    curr_variants
                                        .push(Variant {
                                            id: format!("variant-{}", total_variants),
                                            variant_type: VariantType::EXPERIMENTAL,
                                            context_id: None,
                                            override_id: None,
                                            overrides: Map::new(),
                                        })
                                });
                        }
                    >

                        <i class="ri-add-line"></i>
                        Add Variant
                    </button>
                </div>
                <For
                    each=move || {
                        f_variants.get().into_iter().enumerate().collect::<Vec<(usize, Variant)>>()
                    }

                    key=|(idx, variant)| format!("{}-{}", variant.id.to_string(), idx)
                    children=move |(idx, variant)| {
                        let config = default_config.clone();
                        let variant_clone = variant.clone();
                        let handle_change = handle_override_form_change(idx);
                        view! {
                            <div class="my-2 p-4 rounded bg-gray-50">
                                <div class="flex items-center gap-4">
                                    <div class="form-control w-1/3">
                                        <label class="label">
                                            <span class="label-text">ID</span>
                                        </label>
                                        <input
                                            name="variantId"
                                            value=move || variant.id.to_string()
                                            type="text"
                                            placeholder="Type a unique name here"
                                            class="input input-bordered w-full max-w-xs"
                                        />
                                    </div>
                                    <div class="form-control w-1/3">
                                        <label class="label font-medium text-sm">
                                            <span class="label-text">Type</span>
                                        </label>
                                        <select
                                            name="expType[]"
                                            value=move || variant.variant_type.to_string()
                                            on:change=move |ev| {
                                                let mut new_variant = variant_clone.clone();
                                                new_variant
                                                    .variant_type = match event_target_value(&ev).as_str() {
                                                    "CONTROL" => VariantType::CONTROL,
                                                    _ => VariantType::EXPERIMENTAL,
                                                };
                                                set_variants
                                                    .update(|value| {
                                                        value[idx] = new_variant;
                                                    })
                                            }

                                            class="select select-bordered"
                                        >
                                            <option disabled selected>
                                                Pick one
                                            </option>
                                            <option value=VariantType::CONTROL
                                                .to_string()>{VariantType::CONTROL.to_string()}</option>
                                            <option value=VariantType::EXPERIMENTAL
                                                .to_string()>
                                                {VariantType::EXPERIMENTAL.to_string()}
                                            </option>
                                        </select>
                                    </div>
                                </div>
                                <div class="mt-2">
                                    <OverrideForm
                                        overrides=variant.overrides
                                        default_config=config
                                        handle_change=handle_change
                                        is_standalone=false
                                    />
                                </div>
                            </div>
                        }
                    }
                />

            </div>
            <div class="flex justify-end">
                <Button text="Submit".to_string() on_click=on_submit/>
            </div>
        </div>
    }
}
