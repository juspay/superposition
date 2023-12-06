use crate::components::{
    context_form::context_form::ContextForm, override_form::override_form::OverrideForm,
};
use crate::pages::ExperimentList::types::{
    DefaultConfig, Dimension, Variant, VariantType,
};
use leptos::*;
use serde_json::Map;
use wasm_bindgen::JsCast;
use web_sys::{SubmitEvent, HtmlInputElement};

#[component]
pub fn ExperimentForm(
    name: String,
    context: Vec<(String, String, String)>,
    variants: Vec<Variant>,
    dimensions: Vec<Dimension>,
    default_config: Vec<DefaultConfig>,
) -> impl IntoView {
    // let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let (experiment_name, set_experiment_name) = create_signal(name);
    let (variants, set_variants) = create_signal(variants);
    // let input_vector: InputVector = vec![(experiment_name, set_experiment_name)];
    let on_submit = move |ev: SubmitEvent| {
        ev.prevent_default();
        let document = document();
        let exp_name = document
            .get_element_by_id("expName")
            .expect("expName input not found");
        logging::log!("{:#?}", exp_name.get_attribute_names());
        logging::log!("{:#?}", exp_name.get_attribute("value").unwrap());
        let variant_ids = document.get_elements_by_name("variantId");
        logging::log!("{:#?}", variant_ids.length());
        logging::log!("{:#?}", variant_ids.item(0).expect("missing input").dyn_ref::<HtmlInputElement>().unwrap().value());
        let override_vec = document.get_elements_by_name("override");
        logging::log!("{:#?}", override_vec.length());
        logging::log!("{:#?}", override_vec.item(0).expect("missing override input").dyn_ref::<HtmlInputElement>().unwrap().value());
    };

    view! {
        <form on:submit=on_submit>
            <div class="form-control w-full">
                <label class="label">
                    <span class="label-text">Experiment Name</span>
                </label>
                <input
                    value=move || experiment_name.get()
                    on:input=move |ev| set_experiment_name.set(event_target_value(&ev))
                    type="text"
                    name="expName"
                    id="expName"
                    placeholder="ex. testing hyperpay release"
                    class="input input-bordered w-full max-w-xs"
                />
            </div>

            <ContextForm dimensions=dimensions context=context/>

            <div>
                <label>
                    <span class="label-text">Variants</span>
                </label>
            </div>
            <For
                each=move || variants.get()
                key=|variant: &Variant| variant.id.to_string()
                children=move |variant: Variant| {
                    let config = default_config.clone();
                    let (variant_rs, variant_ws) = create_signal(variant);
                    view! {
                        <div>
                            <div class="form-control w-full">
                                <label class="label">
                                    <span class="label-text">Id</span>
                                </label>
                                <input
                                    name="variantId"
                                    value=move || variant_rs.get().id
                                    type="text"
                                    placeholder="Type a unique name here"
                                    class="input w-full max-w-xs"
                                />
                            </div>
                            <div class="form-control w-full">
                                <label class="label font-medium text-sm">
                                    <span class="label-text">Type</span>
                                </label>
                                <select
                                    name="expType[]"
                                    value=move || variant_rs.get().variant_type.to_string()
                                    on:change=move |ev| {
                                        let mut new_variant = variant_rs.get().clone();
                                        new_variant
                                            .variant_type = match event_target_value(&ev).as_str() {
                                            "CONTROL" => VariantType::CONTROL,
                                            _ => VariantType::EXPERIMENTAL,
                                        };
                                        variant_ws.set(new_variant);
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
                            <div>
                                <OverrideForm
                                    overrides=variant_rs.get().overrides
                                    default_config=config
                                />
                            </div>
                        </div>
                    }
                }
            />

            // make some more random default id for this can lead to undefined behaviour if we use index based ids
            <button
                class="btn btn-circle btn-outline"
                on:click=move |_| {
                    leptos::logging::log!("add new variant");
                    set_variants
                        .update(|value| {
                            let total_variants = value.len();
                            value
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

                <i class="ri-add-circle-fill"></i>
            </button>
            <div class="modal-action">
                <form method="dialog">
                    <button class="btn">Save</button>
                </form>
            </div>

        </form>
    }
}
