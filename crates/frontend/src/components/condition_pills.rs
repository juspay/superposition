pub mod types;
pub mod utils;

use crate::components::condition_pills::types::ConditionOperator;

use self::types::Condition;
use leptos::*;
use wasm_bindgen::JsCast;
use web_sys::Element;

#[component]
pub fn condition_expression(
    #[prop(into)] id: String,
    #[prop(into)] list_id: String,
    condition: Condition,
) -> impl IntoView {
    let (expand_rs, expand_ws) = create_signal(false);

    let classes = Signal::derive(move || {
        if expand_rs.get() {
            (
                "pointer flex items-center w-max max-w-full rounded-md bg-gray-50 px-2 py-1 text-xs ring-1 ring-inset ring-purple-700/10 shadow-md gap-x-2 overflow-hidden",
                "font-mono font-semibold context_condition w-full text-wrap word-break-break"
            )
        } else {
            (
                "pointer flex items-center w-max max-w-[300px] rounded-md bg-gray-50 px-2 py-1 text-xs ring-1 ring-inset ring-purple-700/10 shadow-md gap-x-2 overflow-hidden whitespace-nowrap",

                "font-mono font-semibold context_condition w-full text-ellipsis overflow-hidden whitespace-nowrap"
            )
        }
    });

    let condition = store_value(condition);
    let id = store_value(id);

    let click_event_handler = window_event_listener(ev::click, move |ev| {
        if let Some(t) = ev.target() {
            let target_element = t.dyn_into::<Element>();
            if let Ok(te) = target_element {
                let parent_id = te.parent_element().map(|e| e.id());

                if let Some(p_id) = parent_id {
                    if !p_id.contains(&list_id) {
                        expand_ws.set(false);
                    }
                }
            }
        }
    });
    on_cleanup(|| {
        click_event_handler.remove();
    });

    view! {
        {move || {
            let (list_item_class, value_class) = classes.get();
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
) -> impl IntoView {
    let outer_div_class = format!("{} pt-3 relative flex flex-col w-full py-4 pl-6 border-l-2 border-gray-300 rounded-lg", class);
    view! {
        <div class=outer_div_class>
            <ol id=id.clone() class="flex flex-col gap-4 w-full pl-3 list-none">
                {conditions
                    .into_iter()
                    .enumerate()
                    .map(|(idx, condition)| {
                        let item_id = format!("{}-{}", id, idx);
                        view! { <ConditionExpression condition id=item_id list_id=id.clone()/> }
                    })
                    .collect::<Vec<_>>()}
            </ol>
            <span class="absolute badge badge-ghost capitalize top-1/2 left-0 -translate-x-1/2 -translate-y-1/2 m-0 and-badge">
                "and"
            </span>
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
