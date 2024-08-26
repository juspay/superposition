pub mod utils;
use std::collections::{HashMap, HashSet};

use crate::components::{
    dropdown::{Dropdown, DropdownDirection},
    input_components::{BooleanToggle, EnumDropdown, Toggle},
};
use crate::types::Dimension;
use crate::utils::get_key_type;
use leptos::*;
use serde_json::{Map, Value};
use web_sys::MouseEvent;

#[component]
pub fn context_form<NF>(
    handle_change: NF,
    dimensions: Vec<Dimension>,
    #[prop(default = false)] is_standalone: bool,
    context: Vec<(String, String, String)>,
    #[prop(default = String::new())] heading_sub_text: String,
    #[prop(default = false)] disabled: bool,
    #[prop(default = DropdownDirection::Right)] dropdown_direction: DropdownDirection,
    #[prop(default = false)] resolve_mode: bool,
) -> impl IntoView
where
    NF: Fn(Vec<(String, String, String)>) + 'static,
{
    let _has_dimensions = !dimensions.is_empty();

    let (used_dimensions, set_used_dimensions) = create_signal(
        context
            .iter()
            .map(|(d, _, _)| d.clone())
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
        let f_context = context.get();
        logging::log!("effect {:?}", f_context);
        handle_change(f_context.clone());
    });

    let handle_select_dropdown_option =
        Callback::new(move |selected_dimension: Dimension| {
            let dimension_name = selected_dimension.dimension;
            set_used_dimensions.update(|value: &mut HashSet<String>| {
                value.insert(dimension_name.clone());
            });
            set_context.update(|value| {
                value.push((dimension_name.clone(), "".to_string(), "".to_string()))
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
                                            .collect::<Vec<(usize, (String, String, String))>>()
                                    }

                                    key=|(idx, (dimension, op, _))| {
                                        format!("{}-{}-{}", dimension, idx, op)
                                    }

                                    children=move |(idx, (dimension, mut operator, value))| {
                                        let dimension_label = dimension.to_string();
                                        let dimension_name = StoredValue::new(
                                            dimension.to_string(),
                                        );
                                        let schema: Map<String, Value> = serde_json::from_value(
                                                dimensions_map.get(&dimension_label).unwrap().schema.clone(),
                                            )
                                            .unwrap();
                                        let dimension_type = get_key_type(&schema);
                                        if operator.is_empty() {
                                            set_context
                                                .update_untracked(|curr_context| {
                                                    curr_context[idx].1 = String::from("==");
                                                });
                                            operator = String::from("==");
                                        }
                                        view! {
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
                                                        value=operator.clone()
                                                        on:input=move |event| {
                                                            let input_value = event_target_value(&event);
                                                            set_context
                                                                .update(|curr_context| {
                                                                    curr_context[idx].1 = input_value;
                                                                    curr_context[idx].2 = String::from("");
                                                                });
                                                        }

                                                        name="context-dimension-operator"
                                                        class="select select-bordered w-full max-w-xs text-sm rounded-lg h-10 px-4 appearance-none leading-tight focus:outline-none focus:shadow-outline"
                                                    >
                                                        <option
                                                            value="=="
                                                            selected=operator == "==" || resolve_mode
                                                        >
                                                            "IS"
                                                        </option>
                                                        <option value="in" selected=operator == "in">
                                                            "HAS"
                                                        </option>
                                                        <option value="<=" selected=operator == "<=">
                                                            "BETWEEN (inclusive)"
                                                        </option>
                                                    </select>

                                                </div>
                                                <div class="form-control">
                                                    <label class="label font-mono text-sm">
                                                        <span class="label-text">Value</span>
                                                    </label>
                                                    <div class="flex gap-x-6 items-center">

                                                        {
                                                            let string_input = view! {
                                                                <input
                                                                    disabled=disabled
                                                                    value=value.clone()
                                                                    on:change=move |event| {
                                                                        let input_value = event_target_value(&event);
                                                                        set_context
                                                                            .update(|curr_context| {
                                                                                curr_context[idx].2 = input_value;
                                                                            });
                                                                    }

                                                                    name="context-dimension-value"
                                                                    type="text"
                                                                    placeholder="Type here"
                                                                    class="input input-bordered w-full bg-white text-gray-700 shadow-md"
                                                                />
                                                            }
                                                                .into_view();
                                                            match operator.as_str() {
                                                                "==" => {
                                                                    match dimension_type.as_str() {
                                                                        "ENUM" => {
                                                                            view! {
                                                                                <EnumDropdown
                                                                                    schema
                                                                                    name="context-dimension-value"
                                                                                    config_value=value
                                                                                    handle_change=Callback::new(move |selected_enum: String| {
                                                                                        set_context
                                                                                            .update(|curr_context| {
                                                                                                curr_context[idx].2 = selected_enum;
                                                                                            });
                                                                                    })

                                                                                    disabled=disabled
                                                                                />
                                                                            }
                                                                                .into_view()
                                                                        }
                                                                        "BOOLEAN" => {
                                                                            if value.is_empty() {
                                                                                set_context
                                                                                    .update_untracked(|curr_context| {
                                                                                        curr_context[idx]
                                                                                            .2 = value.parse::<bool>().unwrap_or(false).to_string();
                                                                                    });
                                                                            }
                                                                            view! {
                                                                                <Toggle
                                                                                    name="context-dimension-value"
                                                                                    value={value.parse::<bool>().unwrap_or(false)}
                                                                                    on_change=Callback::new(move |flag: bool| {
                                                                                        set_context
                                                                                            .update(|curr_context| {
                                                                                                curr_context[idx].2 = flag.to_string();
                                                                                            });
                                                                                    })

                                                                                    class=String::from("mt-2")
                                                                                    disabled=disabled
                                                                                />
                                                                            }
                                                                                .into_view()
                                                                        }
                                                                        _ => string_input,
                                                                    }
                                                                }
                                                                _ => string_input,
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
