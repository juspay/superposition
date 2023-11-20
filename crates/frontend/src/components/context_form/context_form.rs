use crate::pages::ExperimentList::types::Dimension;
use leptos::*;
use std::collections::HashSet;

#[component]
pub fn ContextForm(
    dimensions: Vec<Dimension>,
    context: Vec<(String, String, String)>,
) -> impl IntoView {
    let (context, set_context) = create_signal(context);
    let (used_dimensions, set_used_dimensions) = create_signal(HashSet::new());
    let total_dimensions = dimensions.len();

    // please suggest a better way to write this
    let last_idx = create_memo(move |_| {
        let len = context.get().len();
        if len == 0 {
            0
        } else {
            len - 1
        }
    });

    view! {
        <div class="form-control w-full">
            <label class="label">
                <span class="label-text">Context</span>
            </label>
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
                                    <select class="select select-bordered">
                                        <option disabled selected>
                                            Pick one
                                        </option>
                                        <option value="==">"=="</option>
                                        <option value="!=">"!="</option>
                                    </select>

                                </div>
                                <div class="form-control">
                                    <label class="label capitalize font-mono text-sm">
                                        <span class="label-text">{dimension_label}</span>
                                    </label>
                                    <div class="flex gap-x-6 items-center">
                                        <input
                                            type="text"
                                            placeholder="Type here"
                                            class="input input-bordered w-full max-w-xs"
                                        />
                                        <button
                                            class="text-error text-xl font-light font-thin"
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

                                            <i class="ri-delete-bin-2-line"></i>
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

                <div class="mt-2">
                    <div class="dropdown">
                        <label tabindex="0" class="btn btn-circle btn-info text-white btn-sm m-1">
                            <i class="ri-add-line font-bold text-2xl"></i>
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
                                                    value
                                                        .push((
                                                            dimension_name.to_string(),
                                                            "".to_string(),
                                                            "".to_string(),
                                                        ))
                                                });
                                            set_used_dimensions
                                                .update(|value| {
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
        </div>
    }
}
