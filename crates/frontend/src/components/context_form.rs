pub mod utils;
use std::collections::{HashMap, HashSet};

use crate::components::{
    condition_pills::types::ConditionOperator,
    dropdown::{Dropdown, DropdownDirection},
    input_components::{BooleanToggle, EnumDropdown},
};
use crate::types::Dimension;
use crate::utils::get_key_type;
use leptos::*;
use serde_json::{Map, Value};
use web_sys::MouseEvent;

use super::condition_pills::types::Condition;

#[component]
pub fn context_form<NF>(
    handle_change: NF,
    dimensions: Vec<Dimension>,
    #[prop(default = false)] is_standalone: bool,
    context: Vec<Condition>,
    #[prop(default = String::new())] heading_sub_text: String,
    #[prop(default = false)] disabled: bool,
    #[prop(default = DropdownDirection::Right)] dropdown_direction: DropdownDirection,
    #[prop(default = false)] resolve_mode: bool,
) -> impl IntoView
where
    NF: Fn(Vec<Condition>) + 'static,
{
    // let _has_dimensions = !dimensions.is_empty();

    let (used_dimensions, set_used_dimensions) = create_signal(
        context
            .iter()
            .map(|condition| condition.left_operand.clone())
            .collect::<HashSet<String>>(),
    );
    let (context, set_context) = create_signal(context.clone());

    let dimensions = StoredValue::new(dimensions);
    let mandatory_dimensions = StoredValue::new(
        dimensions
            .get_value()
            .into_iter()
            .filter_map(|dim| {
                if dim.mandatory {
                    Some(dim.dimension)
                } else {
                    None
                }
            })
            .collect::<HashSet<String>>(),
    );

    let last_idx = create_memo(move |_| context.get().len().max(1) - 1);

    let on_click = move |event: MouseEvent| {
        event.prevent_default();
        logging::log!("Context form submit");
        //TODO: submit logic for this
    };

    create_effect(move |_| {
        let f_context = context.get(); // context will now be a Value
        logging::log!("Context form effect {:?}", f_context);
        handle_change(f_context.clone()); // handle_change now expects Value
    });

    let handle_select_dropdown_option =
        Callback::new(move |selected_dimension: Dimension| {
            let dimension_name = selected_dimension.dimension;
            set_used_dimensions.update(|value: &mut HashSet<String>| {
                value.insert(dimension_name.clone());
            });
            set_context.update(|value| {
                value.push(Condition {
                    left_operand: dimension_name.clone(),
                    operator: ConditionOperator::Is,
                    right_operand: vec![Value::String("".to_string())],
                })
            });
        });

    view! {
        <div>
            <div class="form-control w-full ">
                <div class="gap-1">
                    <label class="label flex-col justify-center items-start">
                        <span class="label-text font-semibold text-base">Context</span>
                        <span class="label-text text-slate-400">{heading_sub_text}</span>
                    </label>
                </div>
                <div class="card w-full bg-slate-50">
                    <div class="card-body">
                        <Show when=move || context.get().is_empty()>
                            <div class="flex justify-center">
                                <Dropdown
                                    dropdown_width="w-80"
                                    dropdown_icon="ri-add-line".to_string()
                                    dropdown_text="Add Context".to_string()
                                    dropdown_direction
                                    dropdown_options=dimensions.get_value()
                                    disabled=disabled
                                    on_select=handle_select_dropdown_option
                                />
                            </div>
                        </Show>
                        {move || {
                            let dimensions_map = dimensions
                                .get_value()
                                .into_iter()
                                .map(|ele| (ele.dimension.clone(), ele))
                                .collect::<HashMap<String, Dimension>>();
                            view! {
                                <For
                                    each=move || {
                                        context
                                            .get()
                                            .into_iter()
                                            .enumerate()
                                            .collect::<Vec<(usize, Condition)>>()
                                    }

                                    key=|(idx, condition)| {
                                        format!(
                                            "{}-{}-{}",
                                            condition.left_operand,
                                            idx,
                                            condition.operator,
                                        )
                                    }

                                    children=move |(idx, condition)| {
                                        let dimension_label = condition.left_operand.to_string();
                                        let dimension_name = StoredValue::new(
                                            condition.left_operand.to_string(),
                                        );
                                        let schema: Map<String, Value> = serde_json::from_value(
                                                dimensions_map.get(&dimension_label).unwrap().schema.clone(),
                                            )
                                            .unwrap();
                                        let dimension_type = get_key_type(&schema);
                                        if let ConditionOperator::Other(ref op_str) = condition
                                            .operator
                                        {
                                            if op_str.is_empty() {
                                                set_context
                                                    .update_untracked(|curr_context| {
                                                        curr_context[idx].operator = ConditionOperator::Is;
                                                    });
                                                let mut_operator = String::from("==");
                                                set_context
                                                    .update_untracked(|curr_context| {
                                                        curr_context[idx]
                                                            .operator = ConditionOperator::Other(mut_operator.clone());
                                                    });
                                            }
                                        }
                                        view! {
                                            // let dimension_label = dimension.to_string();

                                            //

                                            <div class="flex gap-x-6">
                                                <div class="form-control">
                                                    <label class="label font-mono text-sm">
                                                        <span class="label-text">Dimension</span>
                                                    </label>
                                                    <input
                                                        value=dimension_label
                                                        class="input w-full max-w-xs"
                                                        name="context-dimension-name"
                                                        disabled=true
                                                    />
                                                </div>
                                                <div class="form-control w-20">
                                                    <label class="label font-medium font-mono text-sm">
                                                        <span class="label-text">Operator</span>
                                                    </label>

                                                    <select
                                                        disabled=disabled || resolve_mode
                                                        value=condition.operator.to_string()
                                                        on:input=move |event| {
                                                            let input_value = event_target_value(&event);
                                                            let new_operator = ConditionOperator::from(
                                                                input_value.clone(),
                                                            );
                                                            set_context
                                                                .update(|curr_context| {
                                                                    curr_context[idx].operator = new_operator.clone();
                                                                    match new_operator {
                                                                        ConditionOperator::Between => {
                                                                            curr_context[idx]
                                                                                .right_operand = vec![Value::Null, Value::Null];
                                                                        }
                                                                        ConditionOperator::Is
                                                                        | ConditionOperator::In
                                                                        | ConditionOperator::Has => {
                                                                            curr_context[idx]
                                                                                .right_operand = vec![Value::String("".to_string())];
                                                                        }
                                                                        _ => {
                                                                            curr_context[idx].right_operand = Vec::new();
                                                                        }
                                                                    }
                                                                });
                                                        }

                                                        name="context-dimension-operator"
                                                        class="select select-bordered w-full max-w-xs text-sm rounded-lg h-10 px-4 appearance-none leading-tight focus:outline-none focus:shadow-outline"
                                                    >
                                                        <option
                                                            value="=="
                                                            selected={
                                                                matches!(condition.operator, ConditionOperator::Is)
                                                            } || resolve_mode
                                                        >
                                                            {"IS"}
                                                        </option>
                                                        <option
                                                            value="in"
                                                            selected=matches!(condition.operator, ConditionOperator::In)
                                                        >
                                                            {"IN"}
                                                        </option>
                                                        <option
                                                            value="has"
                                                            selected=matches!(
                                                                condition.operator,
                                                                ConditionOperator::Has
                                                            )
                                                        >
                                                            {"HAS"}
                                                        </option>
                                                        <option
                                                            value="<="
                                                            selected=matches!(
                                                                condition.operator,
                                                                ConditionOperator::Between
                                                            )
                                                        >
                                                            {"BETWEEN (inclusive)"}
                                                        </option>
                                                    </select>

                                                </div>
                                                <div class="form-control">
                                                    <label class="label font-mono text-sm">
                                                        <span class="label-text">Value</span>
                                                    </label>
                                                    <div class="flex gap-x-6 items-center">

                                                        {
                                                            let input_fields = match &condition.right_operand {
                                                                values => {
                                                                    let filtered_elements: Vec<_> = values
                                                                        .iter()
                                                                        .filter(|v| !v.is_object() || !v.get("var").is_some())
                                                                        .collect();
                                                                    filtered_elements
                                                                        .into_iter()
                                                                        .enumerate()
                                                                        .map(|(i, element)| match element {
                                                                            Value::String(s) => {
                                                                                view! {
                                                                                    // Generate input fields based on the condition's right_operand (Vec<Value>)
                                                                                    // Filter out any elements that are objects containing a "var" key
                                                                                    // Exclude elements with "var"

                                                                                    // Directly return the input fields
                                                                                    // Use `into_iter` to consume the filtered_elements
                                                                                    <input
                                                                                        disabled=disabled
                                                                                        // Use the string directly
                                                                                        value=s.clone()
                                                                                        on:change=move |event| {
                                                                                            let input_value = event_target_value(&event);
                                                                                            set_context
                                                                                                .update(|curr_context| {
                                                                                                    if let Some(elem) = curr_context[idx]
                                                                                                        .right_operand
                                                                                                        .get_mut(i)
                                                                                                    {
                                                                                                        if !elem.is_object() || !elem.get("var").is_some() {
                                                                                                            *elem = Value::String(input_value);
                                                                                                        }
                                                                                                    }
                                                                                                });
                                                                                        }

                                                                                        name="context-dimension-value"
                                                                                        type="text"
                                                                                        placeholder="Type here"
                                                                                        class="input input-bordered w-full bg-white text-gray-700 shadow-md"
                                                                                    />
                                                                                }
                                                                                    .into_view()
                                                                            }
                                                                            Value::Number(n) => {
                                                                                view! {
                                                                                    // Generate input fields based on the condition's right_operand (Vec<Value>)
                                                                                    // Filter out any elements that are objects containing a "var" key
                                                                                    // Exclude elements with "var"

                                                                                    // Directly return the input fields
                                                                                    // Use `into_iter` to consume the filtered_elements
                                                                                    // Use the string directly
                                                                                    // Exclude elements with "var"
                                                                                    // Assign the new value to the element

                                                                                    <input
                                                                                        disabled=disabled
                                                                                        // Convert number to string for display purposes
                                                                                        value=n.to_string()
                                                                                        on:change=move |event| {
                                                                                            let input_value = event_target_value(&event);
                                                                                            if let Ok(parsed) = input_value.parse::<f64>() {
                                                                                                set_context
                                                                                                    .update(|curr_context| {
                                                                                                        if let Some(elem) = curr_context[idx]
                                                                                                            .right_operand
                                                                                                            .get_mut(i)
                                                                                                        {
                                                                                                            if !elem.is_object() || !elem.get("var").is_some() {
                                                                                                                *elem = Value::Number(
                                                                                                                    serde_json::Number::from_f64(parsed).unwrap(),
                                                                                                                );
                                                                                                            }
                                                                                                        }
                                                                                                    });
                                                                                            }
                                                                                        }

                                                                                        name="context-dimension-value"
                                                                                        type="number"
                                                                                        placeholder="Type here"
                                                                                        class="input input-bordered w-full bg-white text-gray-700 shadow-md"
                                                                                    />
                                                                                }
                                                                                    .into_view()
                                                                            }
                                                                            Value::Bool(b) => {
                                                                                view! {
                                                                                    // Generate input fields based on the condition's right_operand (Vec<Value>)
                                                                                    // Filter out any elements that are objects containing a "var" key
                                                                                    // Exclude elements with "var"

                                                                                    // Directly return the input fields
                                                                                    // Use `into_iter` to consume the filtered_elements
                                                                                    // Use the string directly
                                                                                    // Exclude elements with "var"
                                                                                    // Assign the new value to the element

                                                                                    // Convert number to string for display purposes
                                                                                    // Try to parse input as f64
                                                                                    // Exclude elements with "var"

                                                                                    <BooleanToggle
                                                                                        name="context-dimension-value"
                                                                                        value=*b
                                                                                        on_change=Callback::new(move |flag: bool| {
                                                                                            set_context
                                                                                                .update(|curr_context| {
                                                                                                    if let Some(elem) = curr_context[idx]
                                                                                                        .right_operand
                                                                                                        .get_mut(i)
                                                                                                    {
                                                                                                        if !elem.is_object() || !elem.get("var").is_some() {
                                                                                                            *elem = Value::Bool(flag);
                                                                                                        }
                                                                                                    }
                                                                                                });
                                                                                        })

                                                                                        class=String::from("mt-2")
                                                                                        disabled=disabled
                                                                                    />
                                                                                }
                                                                                    .into_view()
                                                                            }
                                                                            _ => {
                                                                                view! {
                                                                                    // Generate input fields based on the condition's right_operand (Vec<Value>)
                                                                                    // Filter out any elements that are objects containing a "var" key
                                                                                    // Exclude elements with "var"

                                                                                    // Directly return the input fields
                                                                                    // Use `into_iter` to consume the filtered_elements
                                                                                    // Use the string directly
                                                                                    // Exclude elements with "var"
                                                                                    // Assign the new value to the element

                                                                                    // Convert number to string for display purposes
                                                                                    // Try to parse input as f64
                                                                                    // Exclude elements with "var"

                                                                                    // Exclude elements with "var"
                                                                                    // Assign the new value to the element

                                                                                    <input
                                                                                        disabled=disabled
                                                                                        // Display as a string for unsupported types
                                                                                        value=element.to_string()
                                                                                        on:change=move |event| {
                                                                                            let input_value = event_target_value(&event);
                                                                                            set_context
                                                                                                .update(|curr_context| {
                                                                                                    if let Some(elem) = curr_context[idx]
                                                                                                        .right_operand
                                                                                                        .get_mut(i)
                                                                                                    {
                                                                                                        if !elem.is_object() || !elem.get("var").is_some() {
                                                                                                            *elem = Value::String(input_value);
                                                                                                        }
                                                                                                    }
                                                                                                });
                                                                                        }

                                                                                        name="context-dimension-value"
                                                                                        type="text"
                                                                                        placeholder="Type here"
                                                                                        class="input input-bordered w-full bg-white text-gray-700 shadow-md"
                                                                                    />
                                                                                }
                                                                                    .into_view()
                                                                            }
                                                                        })
                                                                        .collect::<Vec<_>>()
                                                                }
                                                            };
                                                            match condition.operator {
                                                                ConditionOperator::Is => {
                                                                    match dimension_type.as_str() {
                                                                        "ENUM" => {
                                                                            let filtered_value = condition
                                                                                .right_operand
                                                                                .iter()
                                                                                .find(|v| match v {
                                                                                    Value::Object(obj) => !obj.contains_key("var"),
                                                                                    _ => true,
                                                                                })
                                                                                .map(|v| v.to_string().replace('"', ""))
                                                                                .unwrap_or_else(|| String::new());
                                                                            view! {
                                                                                // Generate input fields based on the condition's right_operand (Vec<Value>)
                                                                                // Filter out any elements that are objects containing a "var" key
                                                                                // Exclude elements with "var"

                                                                                // Directly return the input fields
                                                                                // Use `into_iter` to consume the filtered_elements
                                                                                // Use the string directly
                                                                                // Exclude elements with "var"
                                                                                // Assign the new value to the element

                                                                                // Convert number to string for display purposes
                                                                                // Try to parse input as f64
                                                                                // Exclude elements with "var"

                                                                                // Exclude elements with "var"
                                                                                // Assign the new value to the element

                                                                                // Display as a string for unsupported types
                                                                                // Exclude elements with "var"
                                                                                // Assign the new value to the element

                                                                                // Collect the result into a Vec

                                                                                <EnumDropdown
                                                                                    schema
                                                                                    name="context-dimension-value"
                                                                                    config_value=filtered_value
                                                                                    handle_change=Callback::new(move |selected_enum: String| {
                                                                                        set_context
                                                                                            .update(|curr_context| {
                                                                                                curr_context[idx]
                                                                                                    .right_operand = vec![Value::String(selected_enum)];
                                                                                            });
                                                                                    })

                                                                                    disabled=disabled
                                                                                />
                                                                            }
                                                                                .into_view()
                                                                        }
                                                                        "BOOLEAN" => {
                                                                            let is_checked = match &condition.right_operand[0] {
                                                                                Value::Bool(b) => *b,
                                                                                _ => false,
                                                                            };
                                                                            view! {
                                                                                // Generate input fields based on the condition's right_operand (Vec<Value>)
                                                                                // Filter out any elements that are objects containing a "var" key
                                                                                // Exclude elements with "var"

                                                                                // Directly return the input fields
                                                                                // Use `into_iter` to consume the filtered_elements
                                                                                // Use the string directly
                                                                                // Exclude elements with "var"
                                                                                // Assign the new value to the element

                                                                                // Convert number to string for display purposes
                                                                                // Try to parse input as f64
                                                                                // Exclude elements with "var"

                                                                                // Exclude elements with "var"
                                                                                // Assign the new value to the element

                                                                                // Display as a string for unsupported types
                                                                                // Exclude elements with "var"
                                                                                // Assign the new value to the element

                                                                                // Collect the result into a Vec

                                                                                // Update with selected enum

                                                                                // Ensure we handle Value properly and default to false if not a valid boolean
                                                                                // Extract the boolean value directly
                                                                                // Default to false if not a boolean
                                                                                <BooleanToggle
                                                                                    name="context-dimension-value"
                                                                                    // Use the extracted boolean value
                                                                                    value=is_checked
                                                                                    on_change=Callback::new(move |flag: bool| {
                                                                                        set_context
                                                                                            .update(|curr_context| {
                                                                                                curr_context[idx].right_operand = vec![Value::Bool(flag)];
                                                                                            });
                                                                                    })

                                                                                    class=String::from("mt-2")
                                                                                    disabled=disabled
                                                                                />
                                                                            }
                                                                                .into_view()
                                                                        }
                                                                        _ => {
                                                                            view! {
                                                                                // Generate input fields based on the condition's right_operand (Vec<Value>)
                                                                                // Filter out any elements that are objects containing a "var" key
                                                                                // Exclude elements with "var"

                                                                                // Directly return the input fields
                                                                                // Use `into_iter` to consume the filtered_elements
                                                                                // Use the string directly
                                                                                // Exclude elements with "var"
                                                                                // Assign the new value to the element

                                                                                // Convert number to string for display purposes
                                                                                // Try to parse input as f64
                                                                                // Exclude elements with "var"

                                                                                // Exclude elements with "var"
                                                                                // Assign the new value to the element

                                                                                // Display as a string for unsupported types
                                                                                // Exclude elements with "var"
                                                                                // Assign the new value to the element

                                                                                // Collect the result into a Vec

                                                                                // Update with selected enum

                                                                                // Ensure we handle Value properly and default to false if not a valid boolean
                                                                                // Extract the boolean value directly
                                                                                // Default to false if not a boolean
                                                                                // Use the extracted boolean value
                                                                                // Update as Value::Bool directly

                                                                                {
                                                                                    logging::log!(
                                                                                        "Condition operator and saurav {:?}", condition.operator
                                                                                    );
                                                                                    input_fields.into_view()
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                                _ => {
                                                                    view! {
                                                                        // Generate input fields based on the condition's right_operand (Vec<Value>)
                                                                        // Filter out any elements that are objects containing a "var" key
                                                                        // Exclude elements with "var"

                                                                        // Directly return the input fields
                                                                        // Use `into_iter` to consume the filtered_elements
                                                                        // Use the string directly
                                                                        // Exclude elements with "var"
                                                                        // Assign the new value to the element

                                                                        // Convert number to string for display purposes
                                                                        // Try to parse input as f64
                                                                        // Exclude elements with "var"

                                                                        // Exclude elements with "var"
                                                                        // Assign the new value to the element

                                                                        // Display as a string for unsupported types
                                                                        // Exclude elements with "var"
                                                                        // Assign the new value to the element

                                                                        // Collect the result into a Vec

                                                                        // Update with selected enum

                                                                        // Ensure we handle Value properly and default to false if not a valid boolean
                                                                        // Extract the boolean value directly
                                                                        // Default to false if not a boolean
                                                                        // Use the extracted boolean value
                                                                        // Update as Value::Bool directly

                                                                        // Fallback to input field if not ENUM or BOOLEAN

                                                                        {input_fields.into_view()}
                                                                    }
                                                                }
                                                            }
                                                        }
                                                        <Show when=move || {
                                                            !disabled
                                                                && !mandatory_dimensions
                                                                    .get_value()
                                                                    .contains(&dimension_name.get_value())
                                                        }>
                                                            <button
                                                                class="btn btn-ghost btn-circle btn-sm mt-1"
                                                                disabled=disabled
                                                                on:click=move |_| {
                                                                    let mut current_context = context.get();
                                                                    current_context.remove(idx);
                                                                    set_used_dimensions
                                                                        .update(|value| {
                                                                            value.remove(&dimension_name.get_value());
                                                                        });
                                                                    set_context.set(current_context);
                                                                }
                                                            >

                                                                <i class="ri-delete-bin-2-line text-xl text-2xl font-bold"></i>
                                                            </button>
                                                        </Show>
                                                    </div>
                                                </div>
                                            </div>

                                            {move || {
                                                if last_idx.get() != idx {
                                                    view! {
                                                        <div class="my-3 ml-5 ml-6 ml-7">
                                                            <span class="font-mono text-xs">"&&"</span>
                                                        </div>
                                                    }
                                                        .into_view()
                                                } else {
                                                    view! {}.into_view()
                                                }
                                            }}
                                        }
                                    }
                                />
                            }
                        }}

                        <Show when=move || { !context.get().is_empty() && !disabled }>
                            <div class="mt-4">

                                {move || {
                                    let dimensions = dimensions
                                        .get_value()
                                        .into_iter()
                                        .filter(|dimension| {
                                            !used_dimensions.get().contains(&dimension.dimension)
                                        })
                                        .collect::<Vec<Dimension>>();
                                    view! {
                                        <Dropdown
                                            dropdown_icon="ri-add-line".to_string()
                                            dropdown_text="Add Context".to_string()
                                            dropdown_options=dimensions
                                            disabled=disabled
                                            dropdown_direction
                                            on_select=handle_select_dropdown_option
                                        />
                                    }
                                }}

                            </div>
                        </Show>

                    </div>
                </div>
            </div>
            <Show when=move || is_standalone>
                <div class="flex justify-end">
                    <button class="btn" on:click:undelegated=on_click disabled=disabled>
                        Save
                    </button>
                </div>
            </Show>
        </div>
    }
}
