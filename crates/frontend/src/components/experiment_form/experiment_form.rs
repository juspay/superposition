use super::utils::create_experiment;
use crate::components::button::button::Button;
use crate::components::{
    context_form::context_form::ContextForm, override_form::override_form::OverrideForm,
};
use crate::pages::ExperimentList::types::{
    DefaultConfig, Dimension, Variant, VariantType,
};
use chrono::offset::Local;
use leptos::*;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use web_sys::MouseEvent;

fn default_variants_for_form() -> Vec<(String, Variant)> {
    vec![
        (
            "control-variant".to_string(),
            Variant {
                id: "control".to_string(),
                variant_type: VariantType::CONTROL,
                context_id: None,
                override_id: None,
                overrides: Map::new(),
            },
        ),
        (
            "experimental-variant".to_string(),
            Variant {
                id: "experimental".to_string(),
                variant_type: VariantType::EXPERIMENTAL,
                context_id: None,
                override_id: None,
                overrides: Map::new(),
            },
        ),
    ]
}

#[derive(Serialize, Deserialize, Clone, Debug)]
struct CombinedResource {
    dimensions: Vec<Dimension>,
    default_config: Vec<DefaultConfig>,
}

#[component]
pub fn ExperimentForm<NF>(
    name: String,
    context: Vec<(String, String, String)>,
    variants: Vec<Variant>,
    handle_submit: NF,
    default_config: Vec<DefaultConfig>,
    dimensions: Vec<Dimension>,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();

    let default_variants = if variants.len() == 0 {
        default_variants_for_form()
    } else {
        variants
            .into_iter()
            .map(|variant| (variant.id.to_string(), variant))
            .collect::<Vec<(String, Variant)>>()
    };

    let (experiment_name, set_experiment_name) = create_signal(name);
    let (f_context, set_context) = create_signal(context.clone());
    let (f_variants, set_variants) = create_signal(default_variants);

    let handle_context_form_change = move |updated_ctx: Vec<(String, String, String)>| {
        set_context.set(updated_ctx);
    };

    let handle_override_form_change = move |variant_idx: usize| {
        let handle_change = move |updated_overrides: Map<String, Value>| {
            set_variants.update(|curr_variants| {
                curr_variants[variant_idx].1.overrides = updated_overrides.clone();
            });
        };
        handle_change
    };

    let on_submit = move |event: MouseEvent| {
        event.prevent_default();
        logging::log!("Submitting experiment form");
        logging::log!("{:?}", f_variants.get());

        let f_experiment_name = experiment_name.get();
        let f_context = f_context.get();
        let f_variants = f_variants
            .get()
            .into_iter()
            .map(|(_, variant)| variant)
            .collect::<Vec<Variant>>();
        let tenant = tenant_rs.get();
        let handle_submit_clone = handle_submit.clone();

        logging::log!("{:?}", f_experiment_name);
        logging::log!("{:?}", f_context);

        spawn_local({
            async move {
                let result =
                    create_experiment(f_context, f_variants, f_experiment_name, tenant)
                        .await;

                match result {
                    Ok(_) => {
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

                    <button
                        class="btn btn-outline btn-sm text-xs m-1"
                        on:click:undelegated=move |_| {
                            leptos::logging::log!("add new variant");
                            set_variants
                                .update(|curr_variants| {
                                    let total_variants = curr_variants.len();
                                    let key = Local::now().timestamp().to_string();
                                    curr_variants
                                        .push((
                                            key,
                                            Variant {
                                                id: format!("variant-{}", total_variants),
                                                variant_type: VariantType::EXPERIMENTAL,
                                                context_id: None,
                                                override_id: None,
                                                overrides: Map::new(),
                                            },
                                        ))
                                });
                        }
                    >

                        <i class="ri-add-line"></i>
                        Add Variant
                    </button>
                </div>
                <For
                    each=move || {
                        f_variants
                            .get()
                            .into_iter()
                            .enumerate()
                            .collect::<Vec<(usize, (String, Variant))>>()
                    }

                    key=|(_, (key, _))| key.to_string()
                    children=move |(idx, (_, variant))| {
                        let variant_clone = variant.clone();
                        let default_config = default_config.clone();
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
                                            value=variant.variant_type.to_string()
                                            on:change=move |ev| {
                                                let mut new_variant = variant_clone.clone();
                                                new_variant
                                                    .variant_type = match event_target_value(&ev).as_str() {
                                                    "CONTROL" => VariantType::CONTROL,
                                                    _ => VariantType::EXPERIMENTAL,
                                                };
                                                set_variants
                                                    .update(|value| {
                                                        value[idx].1 = new_variant;
                                                    })
                                            }

                                            class="select select-bordered"
                                        >
                                            <option disabled>Pick one</option>
                                            <option
                                                value=VariantType::CONTROL.to_string()
                                                selected=VariantType::CONTROL == variant.variant_type
                                            >
                                                {VariantType::CONTROL.to_string()}
                                            </option>
                                            <option
                                                value=VariantType::EXPERIMENTAL.to_string()
                                                selected=VariantType::EXPERIMENTAL == variant.variant_type
                                            >
                                                {VariantType::EXPERIMENTAL.to_string()}
                                            </option>
                                        </select>
                                    </div>
                                </div>
                                <div class="mt-2">
                                    <OverrideForm
                                        overrides=variant.overrides
                                        default_config=default_config
                                        handle_change=handle_change
                                        is_standalone=false
                                    />
                                </div>
                            </div>
                        }
                    }
                />

            </div>
            <div class="flex justify-end mt-8">
                <Button text="Submit".to_string() on_click=on_submit/>
            </div>
        </div>
    }
}
