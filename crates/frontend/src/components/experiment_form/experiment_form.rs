use super::utils::{create_experiment, update_experiment};
use crate::components::button::button::Button;
use crate::components::context_form::context_form::ContextForm;
use crate::components::variant_form::variant_form::VariantForm;
use crate::types::{DefaultConfig, Dimension, Variant, VariantType};
use leptos::*;
use serde_json::Map;
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

fn get_init_state(variants: &[Variant]) -> Vec<(String, Variant)> {
    let init_variants = if variants.is_empty() {
        default_variants_for_form()
    } else {
        variants
            .iter()
            .map(|variant| (variant.id.to_string(), variant.clone()))
            .collect::<Vec<(String, Variant)>>()
    };

    init_variants
}

#[component]
pub fn experiment_form<NF>(
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] id: String,
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
    let init_variants = get_init_state(&variants);
    let default_config = StoredValue::new(default_config);
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();

    let (experiment_name, set_experiment_name) = create_signal(name);
    let (f_context, set_context) = create_signal(context.clone());
    let (f_variants, set_variants) = create_signal(init_variants);

    let handle_context_form_change = move |updated_ctx: Vec<(String, String, String)>| {
        set_context.set_untracked(updated_ctx);
    };

    let handle_variant_form_change = move |updated_varaints: Vec<(String, Variant)>| {
        set_variants.set_untracked(updated_varaints);
    };

    let dimensions = StoredValue::new(dimensions);
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
        let experiment_id = id.clone();
        let handle_submit_clone = handle_submit.clone();

        logging::log!("{:?}", f_experiment_name);
        logging::log!("{:?}", f_context);

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
                        dimensions.get_value(),
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

            <div class="flex justify-end mt-8">
                <Button text="Submit".to_string() on_click=on_submit/>
            </div>
        </div>
    }
}
