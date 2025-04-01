pub mod utils;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

use crate::components::input::{Input, InputType};
use crate::logic::{Condition, Conditions, Expression, Operator};
use crate::schema::EnumVariants;
use crate::{
    components::dropdown::{Dropdown, DropdownDirection},
    schema::SchemaType,
};
use leptos::*;
use serde_json::{Map, Value};
use superposition_types::database::types::DimensionWithMandatory;

#[component]
pub fn condition_input(
    disabled: bool,
    resolve_mode: bool,
    allow_remove: bool,
    condition: StoredValue<Condition>,
    input_type: StoredValue<InputType>,
    schema_type: StoredValue<SchemaType>,
    #[prop(into)] on_remove: Callback<String, ()>,
    #[prop(into)] on_value_change: Callback<Expression, ()>,
    #[prop(into)] on_operator_change: Callback<Operator, ()>,
    tooltip_text: String,
) -> impl IntoView {
    let (dimension, operator): (String, Operator) =
        condition.with_value(|v| (v.variable.clone(), v.into()));

    let inputs: Vec<(Value, Callback<Value, ()>)> = match condition.get_value().expression
    {
        Expression::Is(c) => {
            vec![(
                c,
                Callback::new(move |value: Value| {
                    on_value_change.call(Expression::Is(value));
                }),
            )]
        }
        Expression::In(c) => {
            vec![(
                c,
                Callback::new(move |value: Value| {
                    on_value_change.call(Expression::In(value));
                }),
            )]
        }
        Expression::Has(c) => {
            vec![(
                c,
                Callback::new(move |value: Value| {
                    on_value_change.call(Expression::Has(value));
                }),
            )]
        }
        Expression::Between(c1, c2) => {
            let c2_clone = c2.clone();
            vec![
                (
                    c1.clone(),
                    Callback::new(move |value: Value| {
                        on_value_change
                            .call(Expression::Between(value, c2_clone.clone()));
                    }),
                ),
                (
                    c2.clone(),
                    Callback::new(move |value: Value| {
                        on_value_change.call(Expression::Between(c1.clone(), value));
                    }),
                ),
            ]
        }
        _ => vec![],
    };
    view! {
        <div class="flex gap-x-6">
            <div class="form-control">
                <label class="label font-mono text-sm">
                    <span class="label-text">Dimension</span>
                </label>
                <input
                    value=dimension.clone()
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
                    value=operator.to_string()
                    on:input=move |event| {
                        on_operator_change
                            .call(Operator::from_operator_input(event_target_value(&event)));
                    }

                    name="context-dimension-operator"
                    class="select select-bordered w-full max-w-xs text-sm rounded-lg h-10 px-4 appearance-none leading-tight focus:outline-none focus:shadow-outline"
                >
                    <option
                        value="=="
                        selected={ matches!(operator, Operator::Is) } || resolve_mode
                    >
                        "IS"
                    </option>
                    <option value="in" selected=matches!(operator, Operator::In)>
                        "IN"
                    </option>
                    <option value="has" selected=matches!(operator, Operator::Has)>

                        "HAS"
                    </option>
                    <option value="<=" selected=matches!(operator, Operator::Between)>

                        "BETWEEN (inclusive)"
                    </option>
                </select>

            </div>
        </div>
        <div class="form-control">
            <label class="label font-mono text-sm">
                <span class="label-text">Value</span>
            </label>

            <div class="flex gap-x-6 items-center">

                {inputs
                    .into_iter()
                    .enumerate()
                    .map(|(idx, (value, on_change)): (usize, (Value, Callback<Value, ()>))| {
                        view! {
                            <Input
                                value=value
                                schema_type=schema_type.get_value()
                                on_change=on_change
                                r#type=input_type.get_value()
                                disabled=disabled
                                id=format!(
                                    "{}-{}",
                                    condition
                                        .with_value(|v| {
                                            format!(
                                                "{}-{}",
                                                v.variable,
                                                (<&Condition as Into<Operator>>::into(v)),
                                            )
                                        }),
                                    idx,
                                )
                                class="w-[450px]"
                                name=""
                                operator=Some(operator.clone())
                            />
                        }
                    })
                    .collect_view()} <Show when=move || allow_remove>
                    <button
                        class="btn btn-ghost btn-circle btn-sm"
                        disabled=disabled
                        on:click=move |_| {
                            on_remove.call(condition.with_value(|v| v.variable.clone()));
                        }
                    >
                        <i class="ri-delete-bin-2-line text-2xl font-bold"></i>
                    </button>
                </Show> <Show when=move || !allow_remove>
                    <div class="tooltip" data-tip=tooltip_text.clone()>
                        <i class="ri-information-line text-2xl"></i>
                    </div>
                </Show>
            </div>
        </div>
    }
}

#[component]
pub fn context_form<NF>(
    handle_change: NF,
    context: Conditions,
    dimensions: Vec<DimensionWithMandatory>,
    #[prop(default = false)] disabled: bool,
    #[prop(default = false)] resolve_mode: bool,
    #[prop(default = String::new())] heading_sub_text: String,
    #[prop(default = DropdownDirection::Right)] dropdown_direction: DropdownDirection,
) -> impl IntoView
where
    NF: Fn(Conditions) + 'static,
{
    let dimension_map = store_value(
        dimensions
            .iter()
            .map(|v| (v.dimension.clone(), v.clone()))
            .collect::<HashMap<String, DimensionWithMandatory>>(),
    );
    let (used_dimensions_rs, used_dimensions_ws) = create_signal(
        context
            .iter()
            .map(|condition| condition.variable.clone())
            .collect::<HashSet<String>>(),
    );

    let dimensions = StoredValue::new(dimensions);
    let mandatory_dimensions = StoredValue::new(
        dimensions
            .get_value()
            .into_iter()
            .filter(|dim| dim.mandatory)
            .map(|dim| dim.dimension)
            .collect::<HashSet<String>>(),
    );

    let (dependent_dimension_locks_rs, dependent_dimension_locks_ws) =
        create_signal(used_dimensions_rs.get());

    let update_context_with_dependent_dimensions =
        move |dimension_info: &DimensionWithMandatory,
              update_context_callback: &dyn Fn(String, SchemaType)| {
            if let Some(dependency_graph) = dimension_info
                .dependency_graph
                .clone()
                .and_then(|val| serde_json::from_value::<Map<String, Value>>(val).ok())
            {
                let dependent_dimensions = dependency_graph
                    .keys()
                    .filter(|key| **key != dimension_info.dimension.clone())
                    .cloned()
                    .collect::<Vec<String>>();
                dependent_dimensions.iter().for_each(|dependent| {
                    if !used_dimensions_rs.get().contains(dependent) {
                        used_dimensions_ws.update(|value: &mut HashSet<String>| {
                            value.insert(dependent.clone());
                        });
                        if let Some(Ok(r#type)) = dimension_map
                            .get_value()
                            .get(dependent)
                            .map(|d| SchemaType::try_from(d.schema.clone()))
                        {
                            update_context_callback(dependent.to_string(), r#type);
                        }
                    }
                    dependent_dimension_locks_ws.update(|value| {
                        value.insert(dependent.clone());
                    });
                });
            }
        };

    let context_data = {
        let context = RefCell::new(context.clone());
        if !disabled {
            dimensions
                .get_value()
                .into_iter()
                .filter(|dim| dim.mandatory)
                .for_each(|dimension_info| {
                    if !used_dimensions_rs.get().contains(&dimension_info.dimension) {
                        used_dimensions_ws.update(|value: &mut HashSet<String>| {
                            value.insert(dimension_info.dimension.clone());
                        });
                        context.borrow_mut().push(
                            Condition::new_with_default_expression(
                                dimension_info.dimension.clone(),
                                SchemaType::try_from(dimension_info.schema.clone())
                                    .unwrap(),
                            ),
                        );
                    }
                    // Not checking for used_dimensions_rs here as the dependent dimensions may not be a part of the context
                    update_context_with_dependent_dimensions(
                        &dimension_info,
                        &|d_name, r#type| {
                            context.borrow_mut().push(
                                Condition::new_with_default_expression(d_name, r#type),
                            );
                        },
                    );
                });
        }
        context.into_inner()
    };

    let (context_rs, context_ws) = create_signal(context_data);

    let last_idx = create_memo(move |_| context_rs.get().len().max(1) - 1);

    create_effect(move |_| {
        let f_context = context_rs.get();
        logging::log!("Context form effect {:?}", f_context);
        handle_change(f_context.clone());
    });

    let on_select_dimension =
        Callback::new(move |selected_dimension: DimensionWithMandatory| {
            let dimension_name = selected_dimension.dimension.clone();

            if let Ok(r#type) = SchemaType::try_from(selected_dimension.schema.clone()) {
                used_dimensions_ws.update(|value: &mut HashSet<String>| {
                    value.insert(dimension_name.clone());
                });
                context_ws.update(|value| {
                    value.push(Condition::new_with_default_expression(
                        dimension_name,
                        r#type,
                    ))
                });
                update_context_with_dependent_dimensions(
                    &selected_dimension,
                    &|d_name, r#type| {
                        context_ws.update(|value| {
                            value.push(Condition::new_with_default_expression(
                                d_name, r#type,
                            ))
                        });
                    },
                );
            }
        });

    let on_operator_change =
        Callback::new(move |(idx, expression): (usize, Expression)| {
            context_ws.update(|v| {
                if idx < v.len() {
                    v[idx].expression = expression;
                }
            });
        });

    let on_value_change = Callback::new(move |(idx, expression): (usize, Expression)| {
        context_ws.update(|v| {
            if idx < v.len() {
                v[idx].expression = expression;
            }
        })
    });

    let on_remove = Callback::new(move |(idx, d_name): (usize, String)| {
        used_dimensions_ws.update(|value| {
            value.remove(&d_name);
        });
        context_ws.update(|v| {
            v.remove(idx);
        });
        if let Some(removed_dimension) = dimensions
            .get_value()
            .iter()
            .find(|d| d.dimension == d_name)
        {
            if let Some(immediate_childrens) =
                removed_dimension.immediate_childrens.as_ref()
            {
                immediate_childrens.iter().for_each(|dependent| {
                    dependent_dimension_locks_ws.update(|value| {
                        value.remove(dependent);
                    });
                })
            }
        }
    });

    let get_tool_tip_text = move |condition: StoredValue<Condition>| {
        let variable = condition.get_value().variable;
        if mandatory_dimensions.get_value().contains(&variable) {
            return "Mandatory Dimension".to_string();
        }
        if dependent_dimension_locks_rs.get().contains(&variable) {
            if let Some(parents) = dimension_map
                .get_value()
                .get(&variable)
                .and_then(|d| d.immediate_parents.as_ref())
            {
                if parents
                    .iter()
                    .all(|parent| used_dimensions_rs.get().contains(parent))
                {
                    return format!(
                        "Dependent on: {}",
                        parents
                            .iter()
                            .map(ToString::to_string)
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                }
            }
        }
        "Blocked Removal".to_string()
    };

    view! {
        <div class="form-control w-full">
            <div class="gap-1">
                <label class="label flex-col justify-center items-start">
                    <span class="label-text font-semibold text-base">Context</span>
                    <span class="label-text text-slate-400">{heading_sub_text}</span>
                </label>
            </div>
            <div class="card w-full bg-slate-50">
                <div class="card-body">
                    <Show when=move || context_rs.get().is_empty()>
                        <div class="flex justify-center">
                            <Dropdown
                                dropdown_width="w-80"
                                dropdown_icon="ri-add-line".to_string()
                                dropdown_text="Add Context".to_string()
                                dropdown_direction
                                dropdown_options=dimensions.get_value()
                                disabled=disabled
                                on_select=on_select_dimension
                            />
                        </div>
                    </Show>
                    <For
                        each=move || {
                            context_rs
                                .get()
                                .0
                                .into_iter()
                                .enumerate()
                                .collect::<Vec<(usize, Condition)>>()
                        }

                        key=move |(idx, condition)| {
                            format!(
                                "{}-{}-{}-{}",
                                condition.variable,
                                idx,
                                condition.expression.to_operator(),
                                dependent_dimension_locks_rs.get().contains(&condition.variable),
                            )
                        }

                        children=move |(idx, condition)| {
                            let (schema_type, enum_variants) = dimension_map
                                .with_value(|v| {
                                    let d = v.get(&condition.variable).unwrap();
                                    (
                                        SchemaType::try_from(d.schema.clone()),
                                        EnumVariants::try_from(d.schema.clone()),
                                    )
                                });
                            let operator = Operator::from(&condition);
                            if schema_type.is_err() || enum_variants.is_err() {
                                return view! { <span class="text-sm red">An error occured</span> }
                                    .into_view();
                            }
                            let schema_type = store_value(schema_type.unwrap());
                            let allow_remove = !disabled
                                && !mandatory_dimensions.get_value().contains(&condition.variable)
                                && !dependent_dimension_locks_rs
                                    .get()
                                    .contains(&condition.variable);
                            let input_type = store_value(
                                InputType::from((
                                    schema_type.get_value(),
                                    enum_variants.unwrap(),
                                    operator,
                                )),
                            );
                            let condition = store_value(condition);
                            let on_remove = move |d_name| on_remove.call((idx, d_name));
                            let on_value_change = move |expression| {
                                on_value_change.call((idx, expression))
                            };
                            let on_operator_change = move |operator| {
                                on_operator_change
                                    .call((
                                        idx,
                                        Expression::from((schema_type.get_value(), operator)),
                                    ))
                            };
                            let tooltip_text = get_tool_tip_text(condition);
                            view! {
                                <ConditionInput
                                    disabled
                                    resolve_mode
                                    allow_remove
                                    condition
                                    input_type
                                    schema_type
                                    on_remove
                                    on_value_change
                                    on_operator_change
                                    tooltip_text
                                />
                                {move || {
                                    if last_idx.get() != idx {
                                        view! {
                                            <div class="my-3 ml-7">
                                                <span class="font-mono text-xs font-bold">"&&"</span>
                                            </div>
                                        }
                                            .into_view()
                                    } else {
                                        view! {}.into_view()
                                    }
                                }}
                            }
                                .into_view()
                        }
                    />

                    <Show when=move || { !context_rs.get().is_empty() && !disabled }>
                        <div class="mt-4">

                            {move || {
                                let dimensions = dimensions
                                    .get_value()
                                    .into_iter()
                                    .filter(|dimension| {
                                        !used_dimensions_rs.get().contains(&dimension.dimension)
                                    })
                                    .collect::<Vec<DimensionWithMandatory>>();
                                view! {
                                    <Dropdown
                                        dropdown_icon="ri-add-line".to_string()
                                        dropdown_text="Add Context".to_string()
                                        dropdown_options=dimensions
                                        disabled=disabled
                                        dropdown_direction
                                        on_select=on_select_dimension
                                    />
                                }
                            }}

                        </div>
                    </Show>

                </div>
            </div>
        </div>
    }
}
