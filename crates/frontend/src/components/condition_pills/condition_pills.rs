use crate::components::condition_pills::types::ConditionOperator;

use super::utils::extract_conditions;
use leptos::*;
use serde_json::Value;

#[component]
pub fn context_pills(context: Value) -> impl IntoView {
    let conditions = extract_conditions(&context);

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
                                    .split(",")
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
