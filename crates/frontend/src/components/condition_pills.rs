pub mod types;
pub mod utils;

use crate::components::condition_pills::types::ConditionOperator;

use self::types::Condition;
use leptos::*;



#[component]
pub fn condition(#[prop(into)] conditions: Vec<Condition>) -> impl IntoView {
    view! {
        <ol class="mt-3 ml-3 relative flex flex-col w-full py-4 pl-6 border-l-2 border-gray-300 rounded-lg list-none">
            <div class="space-y-4 ml-3">
            {conditions
                .into_iter()
                .map(|condition| {
                    let dimension = condition.left_operand;
                    let op = condition.operator;
                    let val = condition.right_operand;
                    view! {
                        <li class="flex items-center w-max max-w-[300px] rounded-md bg-gray-50 px-2 py-1 text-xs ring-1 ring-inset ring-purple-700/10 shadow-md gap-x-2 overflow-hidden whitespace-nowrap">
                            <span class="font-mono font-medium context_condition text-gray-500">
                                {dimension}
                            </span>
                            <span class="font-mono font-medium text-gray-650 context_condition">
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
                                            <span class="font-mono font-semibold context_condition w-full text-ellipsis overflow-hidden whitespace-nowrap">
                                                {val}
                                            </span>
                                        </>
                                    }
                                }
                            }}

                        </li>
                    }
                })
                .collect::<Vec<_>>()}
            </div>
            <span class="absolute badge badge-ghost capitalize top-1/2 left-0 -translate-x-1/2 -translate-y-1/2 m-0 and-badge">
                "and"
            </span>
        </ol>
    }
}

// #[component]
// pub fn condition(#[prop(into)] conditions: Vec<Condition>) -> impl IntoView {
//     view! {
//         <div class="flex flex-wrap w-full space-x-3">
//             {conditions
//                 .into_iter()
//                 .map(|condition| {
//                     let dimension = condition.left_operand;
//                     let op = condition.operator;
//                     let val = condition.right_operand;
//                     view! {
//                         <div class="card min-w-48 max-w-sm border">
//                             <div class="card-body grid grid-cols-3 p-0">
//                                 // add spacing between these elements
//                                 <div class="col-span-2 border-r pl-4 py-1">
//                                     <span class="col-span-2 text-ellipsis overflow-hidden font-mono font-medium context_condition">
//                                         {dimension}
//                                     </span>
//                                 </div>
//                                 <div class="pr-4 py-1 text-center">
//                                     <span class="badge badge-ghost">{op.to_string()}</span>
//                                 </div>
//                             </div>
//                             <div class="card-action max-w-full px-4 py-1 border-t text-ellipsis overflow-hidden">
//                                 <span class="">{val}</span>
//                             </div>
//                         </div>
//                     }
//                 })
//                 .collect::<Vec<_>>()}
//         </div>
//     }
// }

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
