pub mod types;
pub mod utils;

use self::utils::{create_experiment, update_experiment};
use crate::components::button::Button;
use crate::components::context_form::ContextForm;
use crate::components::variant_form::VariantForm;
use crate::types::{DefaultConfig, Dimension, VariantFormT, VariantType};
use leptos::*;
use web_sys::MouseEvent;

use super::condition_pills::types::Condition;

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

fn get_init_state(variants: &[VariantFormT]) -> Vec<(String, VariantFormT)> {
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
pub fn experiment_form<NF>(
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] id: String,
    name: String,
    context: Vec<Condition>,
    variants: Vec<VariantFormT>,
    handle_submit: NF,
    default_config: Vec<DefaultConfig>,
    dimensions: Vec<Dimension>,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let init_variants = get_init_state(&variants);
    let default_config = StoredValue::new(default_config);
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();

    let (experiment_name, set_experiment_name) = create_signal(name);
    let (f_context, set_context) = create_signal(context.clone());
    let (f_variants, set_variants) = create_signal(init_variants);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);

    let handle_context_form_change = move |updated_ctx: Vec<Condition>| {
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
        let tenant = tenant_rs.get();
        let experiment_id = id.clone();
        let handle_submit_clone = handle_submit.clone();

        logging::log!("Experiment name {:?}", f_experiment_name);
        logging::log!("Context Experiment form {:?}", f_context);

        spawn_local({
            async move {
                let result = if edit {
                    update_experiment(experiment_id, f_variants, tenant).await
                } else {
                    create_experiment(
                        f_context,
                        f_variants,
                        f_experiment_name,
                        tenant,
                        dimensions.get_value().clone(),
                    )
                    .await
                };

                match result {
                    Ok(_) => {
                        handle_submit_clone();
                    }
                    Err(_) => {
                        // Handle error
                        // We can consider logging or displaying the error
                    }
                }
                req_inprogress_ws.set(false);
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

            <div class="divider"></div>

            <div class="my-4">
                {move || {
                    let context = f_context.get();
                    view! {
                        <ContextForm
                            // dimensions will now be a Vec<Dimension>
                            dimensions=dimensions.get_value()
                            context=context
                            handle_change=handle_context_form_change
                            is_standalone=false
                            disabled=edit
                            heading_sub_text=String::from(
                                "Define rules under which this experiment would run",
                            )
                        />
                    }
                }}

            </div>

            <div class="divider"></div>

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

            <div class="flex justify-start mt-8">
                {move || {
                    let loading = req_inprogess_rs.get();
                    view! {
                        <Button
                            class="pl-[70px] pr-[70px] w-48 h-12".to_string()
                            text="Submit".to_string()
                            on_click=on_submit.clone()
                            loading
                        />
                    }
                }}

            </div>
        </div>
    }
}
