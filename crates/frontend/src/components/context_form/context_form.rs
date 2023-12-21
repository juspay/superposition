use crate::pages::ExperimentList::types::Dimension;
use leptos::*;
use std::cmp;
use std::collections::HashSet;
use web_sys::MouseEvent;

#[component]
pub fn ContextForm<NF>(
    handle_change: NF,
    dimensions: Vec<Dimension>,
    is_standalone: bool,
    context: Vec<(String, String, String)>,
) -> impl IntoView
where
    NF: Fn(Vec<(String, String, String)>) + 'static,
{
    let _has_dimensions = dimensions.len() > 0;

    let (context, set_context) = create_signal(context.clone());
    let (used_dimensions, set_used_dimensions) = create_signal(HashSet::new());

    let last_idx = create_memo(move |_| {
        let len = context.get().len();
        cmp::max(0, len - 1)
    });

    let on_click = move |event: MouseEvent| {
        event.prevent_default();
        logging::log!("Context form submit");
        //TODO: submit logic for this
    };

    create_effect(move |_| {
        let f_context = context.get();
        handle_change(f_context.clone());
    });

    view! {
        <div>
            <div class="form-control w-full ">
                <div class="flex gap-4 justify-between">
                    <label class="label">
                        <span class="label-text font-semibold text-base">Context</span>
                    </label>
                    <div>
                        <div class="dropdown dropdown-left">
                            <label tabindex="0" class="btn btn-outline btn-sm text-xs m-1">
                                <i class="ri-add-line"></i>
                                Add Context
                            </label>
                            <ul
                                tabindex="0"
                                class="dropdown-content z-[1] menu p-2 shadow bg-base-100 rounded-box w-52"
                            >
                                <For
                                    each=move || {
                                        dimensions
                                            .clone()
                                            .into_iter()
                                            .filter(|dim| {
                                                !used_dimensions.get().contains(&dim.dimension)
                                            })
                                            .collect::<Vec<Dimension>>()
                                    }

                                    key=|dimension: &Dimension| dimension.dimension.to_string()
                                    children=move |dimension: Dimension| {
                                        let dimension_name = dimension.dimension.to_string();
                                        let label = dimension_name.to_string();
                                        view! {
                                            <li on:click=move |_| {
                                                set_context
                                                    .update(|value| {
                                                        leptos::logging::log!("{:?}", value);
                                                        value
                                                            .push((
                                                                dimension_name.to_string(),
                                                                "".to_string(),
                                                                "".to_string(),
                                                            ))
                                                    });
                                                set_used_dimensions
                                                    .update(|value: &mut HashSet<String>| {
                                                        value.insert(dimension_name.to_string());
                                                    });
                                            }>

                                                <a>{label.to_string()}</a>
                                            </li>
                                        }
                                    }
                                />

                            </ul>
                        </div>
                    </div>
                </div>
                <Show when=move || context.get().len() == 0>
                    <div class="p-4 text-gray-400 flex flex-col justify-center items-center">
                        <div>
                            <i class="ri-add-circle-line text-xl"></i>
                        </div>
                        <div>
                            <span class="text-semibold text-sm">Add Context</span>
                        </div>
                    </div>
                </Show>
                <div class="p-4">
                    <For
                        each=move || {
                            context
                                .get()
                                .into_iter()
                                .enumerate()
                                .collect::<Vec<(usize, (String, String, String))>>()
                        }

                        key=|(idx, (dimension, _, _))| format!("{}-{}", dimension, idx)
                        children=move |(idx, (dimension, operator, value))| {
                            let dimension_label = dimension.to_string();
                            let dimension_name = dimension.to_string();
                            view! {
                                <div class="flex gap-x-6">
                                    <div class="form-control w-20">
                                        <label class="label font-medium font-mono text-sm">
                                            <span class="label-text">Operator</span>
                                        </label>
                                        <select
                                            bind:value=operator
                                            on:input=move |event| {
                                                let input_value = event_target_value(&event);
                                                set_context
                                                    .update(|curr_context| {
                                                        curr_context[idx].1 = input_value;
                                                    });
                                            }

                                            name="context-dimension-operator"
                                            class="select select-bordered w-full text-sm rounded-lg h-10 px-4 appearance-none leading-tight focus:outline-none focus:shadow-outline"
                                        >
                                            <option disabled selected>
                                                Pick one
                                            </option>
                                            <option value="==">"IS"</option>
                                            <option value="IN">"HAS"</option>
                                        </select>

                                    </div>
                                    <div class="form-control">
                                        <label class="label font-mono text-sm">
                                            <span class="label-text" name="context-dimension-name">
                                                {dimension_label}
                                            </span>
                                        </label>
                                        <div class="flex gap-x-6 items-center">
                                            <input
                                                bind:value=value
                                                on:input=move |event| {
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
                                            <button
                                                class="btn btn-ghost btn-circle btn-sm"
                                                on:click=move |_| {
                                                    set_context
                                                        .update(|value| {
                                                            value.remove(idx);
                                                        });
                                                    set_used_dimensions
                                                        .update(|value| {
                                                            value.remove(&dimension_name);
                                                        });
                                                }
                                            >

                                                <i class="ri-delete-bin-2-line text-xl text-2xl font-bold"></i>
                                            </button>
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

                </div>
            </div>
            <Show when=move || is_standalone>
                <div class="flex justify-end">
                    <button class="btn" on:click:undelegated=on_click>
                        Save
                    </button>
                </div>
            </Show>
        </div>
    }
}
