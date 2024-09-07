pub mod types;
pub mod utils;

use crate::components::condition_pills::types::ConditionOperator;

use self::types::Condition;
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
            let Condition { left_operand: dimension, operator, right_operand: value } = condition
                .get_value();
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

                    <span class="font-mono font-medium context_condition text-gray-500">
                        {dimension}
                    </span>
                    <span class="font-mono font-medium text-gray-650 context_condition">
                        {operator.to_string()}
                    </span>

                    {match operator {
                        ConditionOperator::Between => {
                            let split_val: Vec<String> = value
                                .clone()
                                .split(',')
                                .map(String::from)
                                .collect();
                            view! {
                                <>
                                    <span class="font-mono font-semibold context_condition">
                                        {&split_val[0]}
                                    </span>
                                    <span class="font-mono font-medium text-gray-650 context_condition ">
                                        {"and"}
                                    </span>
                                    <span class="font-mono font-semibold context_condition">
                                        {&split_val[1]}
                                    </span>
                                </>
                            }
                        }
                        _ => {
                            view! {
                                <>
                                    <span class=value_class>{value}</span>
                                </>
                            }
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
    #[prop(into)] conditions: Vec<Condition>,
    #[prop(into, default=String::new())] class: String,
    #[prop(default = true)] grouped_view: bool,
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
                    .into_iter()
                    .enumerate()
                    .map(|(idx, condition)| {
                        let item_id = format!("{}-{}", id, idx);
                        view! { <ConditionExpression condition id=item_id list_id=id.clone()/> }
                    })
                    .collect::<Vec<_>>()}
            </ol>
            <Show when=move || grouped_view>
                <span class="and">"and"</span>
            </Show>
        </div>
    }
}

#[component]
pub fn condition_pills(#[prop(into)] conditions: Vec<Condition>) -> impl IntoView {
    view! {
        {conditions
            .into_iter()
            .map(|condition| {
                let dimension = condition.left_operand;
                let op = condition.operator;
                let val = condition.right_operand;
                view! {
                    <span class="inline-flex items-center rounded-md bg-gray-50 px-2 py-1 text-xs ring-1 ring-inset ring-purple-700/10 shadow-md gap-x-2">
                        <span class="font-mono font-medium context_condition text-gray-500">
                            {dimension}
                        </span>
                        <span class="font-mono font-medium text-gray-650 context_condition ">
                            {op.to_string()}
                        </span>

                        {match op {
                            ConditionOperator::Between => {
                                let split_val: Vec<String> = val
                                    .clone()
                                    .split(',')
                                    .map(String::from)
                                    .collect();
                                view! {
                                    <>
                                        <span class="font-mono font-semibold context_condition">
                                            {&split_val[0]}
                                        </span>
                                        <span class="font-mono font-medium text-gray-650 context_condition ">
                                            {"and"}
                                        </span>
                                        <span class="font-mono font-semibold context_condition">
                                            {&split_val[1]}
                                        </span>
                                    </>
                                }
                            }
                            _ => {
                                view! {
                                    <>
                                        <span class="font-mono font-semibold context_condition">
                                            {val}
                                        </span>
                                    </>
                                }
                            }
                        }}

                    </span>
                }
            })
            .collect::<Vec<_>>()}
    }
}
