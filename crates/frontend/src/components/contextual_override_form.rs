use leptos::*;
use serde_json::{Map, Value};

use crate::{
    components::{
        button::Button,
        context_form::{
            utils::{create_context, update_context},
            ContextForm,
        },
        override_form::OverrideForm,
    },
    logic::Conditions,
    types::{DefaultConfig, Dimension},
};

#[component]
pub fn contextual_override_form(
    edit: bool,
    #[prop(into, default=String::new())] class: String,
    #[prop(into, default=String::new())] width: String,
    context: Conditions,
    overrides: Vec<(String, Value)>,
    dimensions: Vec<Dimension>,
    default_config: Vec<DefaultConfig>,
    on_submit: Callback<(), ()>,
) -> impl IntoView {
    let tenant_rs = use_context::<Signal<String>>().unwrap();
    let (context, set_context) = create_signal(context);
    let (overrides, set_overrides) = create_signal(overrides);
    let dimensions = StoredValue::new(dimensions);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);

    let on_submit = move |_| {
        req_inprogress_ws.set(true);
        spawn_local(async move {
            let f_context = context.get();
            let f_overrides = overrides.get();
            let result = if edit {
                update_context(
                    tenant_rs.get().clone(),
                    Map::from_iter(f_overrides),
                    f_context,
                )
                .await
            } else {
                create_context(
                    tenant_rs.get().clone(),
                    Map::from_iter(f_overrides),
                    f_context,
                )
                .await
            };

            match result {
                Ok(_) => {
                    logging::log!("Context and overrides submitted successfully");
                    on_submit.call(());
                }
                Err(e) => {
                    logging::log!("Error submitting context and overrides: {:?}", e);
                }
            }
            req_inprogress_ws.set(false);
        });
    };
    view! {
        <div class=format!("relative {}", width)>
            <div class=format!("flex flex-col gap-8 pb-20 {}", class)>
                <ContextForm
                    dimensions=dimensions.get_value()
                    context=context.get_untracked()
                    handle_change=move |new_context| {
                        set_context
                            .update(|value| {
                                *value = new_context;
                            });
                    }

                    disabled=edit
                />
                <OverrideForm
                    overrides=overrides.get_untracked()
                    default_config=default_config
                    handle_change=move |new_overrides| {
                        set_overrides
                            .update(|value| {
                                *value = new_overrides;
                            });
                    }
                />
            </div>
            <div class="w-full absolute bottom-0 right-0 p-4 flex justify-end items-end bg-white border-r border-b border-l rounded-2xl rounded-t-none">
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
