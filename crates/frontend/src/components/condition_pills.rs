use crate::{
    logic::{Condition, Conditions, Operator},
    schema::HtmlDisplay,
};

use leptos::{leptos_dom::helpers::WindowListenerHandle, *};
use wasm_bindgen::JsCast;
use web_sys::Element;

use derive_more::{Deref, DerefMut};

#[derive(Debug, Clone, Deref, DerefMut, Default)]
pub struct ConditionId(pub Option<String>);

pub fn use_condition_collapser() -> WindowListenerHandle {
    let condition_id_ws = use_context::<WriteSignal<ConditionId>>().expect(
        "use_condition_collapser must be used inside condition_collapse_provider",
    );

    window_event_listener(ev::click, move |ev| {
        if let Some(t) = ev.target() {
            let target_element = t.dyn_into::<Element>();
            if let Ok(te) = target_element {
                let parent_id = te.parent_element().map(|e| e.id());
                condition_id_ws.set(ConditionId(parent_id));
            }
        }
    })
}

#[component]
pub fn condition_expression(
    #[prop(into)] id: String,
    #[prop(into)] list_id: String,
    condition: Condition,
    strict_mode: bool,
) -> impl IntoView {
    let id = store_value(id);
    let condition = store_value(condition);

    let (expand_rs, expand_ws) = create_signal(false);
    let condition_id_rs = use_context::<ReadSignal<ConditionId>>().expect(
        "condition_expression component must be used inside condition_collapse_provider",
    );

    create_effect(move |_| {
        if let ConditionId(Some(c_id)) = condition_id_rs.get() {
            if !c_id.contains(&list_id) {
                expand_ws.set(false);
            }
        }
    });

    view! {
        {move || {
            let (list_item_class, value_class) = if expand_rs.get() {
                ("condition-item", "condition-value")
            } else {
                ("condition-item-collapsed", "condition-value-collapsed")
            };
            let (dimension, operator, operands) = condition
                .with_value(|v| {
                    (
                        v.variable.clone(),
                        Operator::from(v),
                        v
                            .expression
                            .to_constants_vec()
                            .iter()
                            .map(|c| c.html_display())
                            .collect::<Vec<_>>(),
                    )
                });
            view! {
                <li
                    id=id.get_value()
                    class=list_item_class
                    on:click=move |_| {
                        if !expand_rs.get() {
                            expand_ws.set(true);
                        }
                    }
                >

                    <span class="font-medium context_condition text-gray-500">{dimension}</span>
                    <span class="font-medium text-gray-650 context_condition">
                        {if strict_mode {
                            operator.strict_mode_display()
                        } else {
                            operator.to_string()
                        }}
                    </span>

                    {match condition.get_value().expression {
                        #[cfg(feature = "jsonlogic")]
                        crate::logic::Expression::Between(c1, c2) => {
                            view! {
                                <>
                                    <span class="font-semibold context_condition">
                                        {c1.html_display()}
                                    </span>
                                    <span class="font-medium text-gray-650 context_condition">
                                        {"and"}
                                    </span>
                                    <span class="font-semibold context_condition">
                                        {c2.html_display()}
                                    </span>
                                </>
                            }
                                .into_view()
                        }
                        _ => {
                            let rendered_value = operands.join(", ");
                            view! { <span class=value_class>{rendered_value}</span> }.into_view()
                        }
                    }}

                </li>
            }
        }}
    }
}

#[component]
pub fn condition(
    #[prop(into)] id: String,
    #[prop(into)] conditions: Conditions,
    #[prop(into, default=String::new())] class: String,
    #[prop(default = true)] grouped_view: bool,
    strict_mode: bool,
) -> impl IntoView {
    let conditions = store_value(conditions);

    let outer_div_class = if grouped_view {
        format!("{} condition grouped", class)
    } else {
        format!("{} condition", class)
    };

    view! {
        <div class=outer_div_class>
            <ol id=id
                .clone()>
                {conditions
                    .get_value()
                    .iter()
                    .enumerate()
                    .map(|(idx, condition)| {
                        let item_id = format!("{}-{}", id, idx);
                        view! {
                            <ConditionExpression
                                condition=condition.clone()
                                id=item_id
                                list_id=id.clone()
                                strict_mode
                            />
                        }
                    })
                    .collect::<Vec<_>>()}
            </ol>
            <Show when=move || grouped_view>
                <span class="and">"and"</span>
            </Show>
        </div>
    }
}
