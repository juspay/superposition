use leptos::*;
use serde_json::{Value, Map};
use std::collections::HashSet;
use crate::pages::ExperimentList::types::{Dimension, DefaultConfig, Variant, VariantType};
use crate::components::{
    context_form::context_form::ContextForm,
    override_form::override_form::OverrideForm,
};

#[component]
pub fn ExperimentForm(
    name: String,
    context: Vec<(String, String, String)>,
    variants: Vec<Variant>,

    dimensions: Vec<Dimension>,
    default_config: Vec<DefaultConfig>
) -> impl IntoView {
    let (experiment_name, set_experiment_name) = create_signal(name);
    let (variants, set_variants) = create_signal(variants);

    view! {
        <div>
            <h2>Create Experiment</h2>

            <div class="form-control w-full">
                <label class="label">
                    <span class="label-text">Name</span>
                </label>
                <input type="text" placeholder="Type here" class="input input-bordered w-full max-w-xs" />
            </div>

            /*** Context Form **/
            <ContextForm
                dimensions={dimensions}
                context={context}
            />
            /*** Context Form **/

            /*** OVerride Form **/
            <div>
                <label>
                    <span class="label-text">
                        Variants
                    </span>
                </label>
            </div>
            <For
                each=move || variants.get()
                key=|variant: &Variant| variant.id.to_string()
                children=move |variant: Variant| {
                    let config = default_config.clone();
                    view! {
                        <div>
                            <div class="form-control w-full">
                                <label class="label">
                                    <span class="label-text">Id</span>
                                </label>
                                <input type="text" placeholder="Type here" class="input input-bordered w-full max-w-xs" />
                            </div>
                            <div class="form-control w-full">
                                <label class="label font-medium text-sm">
                                    <span class="label-text">Type</span>
                                </label>
                                <select class="select select-bordered">
                                    <option disabled selected>Pick one</option>
                                    <option value={VariantType::CONTROL.to_string()}>{VariantType::CONTROL.to_string()}</option>
                                    <option value={VariantType::EXPERIMENTAL.to_string()}>{VariantType::EXPERIMENTAL.to_string()}</option>
                                </select>
                            </div>
                            <div>
                                <OverrideForm overrides={variant.overrides} default_config={config} />
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
                    set_variants.update(|value| {
                        let total_variants = value.len();
                        value.push(Variant {
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

            /*** OVerride Form **/
        </div>
    }
}