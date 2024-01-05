use super::utils::{extract_and_format, parse_conditions};
use leptos::*;
use serde_json::Value;

#[component]
pub fn context_pills(context: Value) -> impl IntoView {
    let condition = extract_and_format(&context);
    let ctx_values = parse_conditions(condition.clone());

    view! {
        {ctx_values
            .into_iter()
            .map(|(dim, op, val)| {
                let operator = op.clone();
                view! {
                    <span class="inline-flex items-center rounded-md bg-gray-50 px-2 py-1 text-xs ring-1 ring-inset ring-purple-700/10 shadow-md gap-x-2">
                        <span class="font-mono font-medium context_condition text-gray-500">
                            {dim}
                        </span>
                        <span class="font-mono font-medium text-gray-650 context_condition ">
                            {op}
                        </span>

                        {match operator.trim() {
                            "BETWEEN" => {
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
