pub mod utils;

use std::collections::{HashMap, HashSet};

use crate::components::dropdown::DropdownBtnType;
use crate::components::input::{Input, InputType};
use crate::logic::{Condition, Conditions, Expression, Operator};
use crate::schema::EnumVariants;
use crate::{
    components::dropdown::{Dropdown, DropdownDirection},
    schema::SchemaType,
};

use leptos::*;
use serde_json::Value;
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
        <div class="flex flex-wrap gap-y-2 gap-x-6">
            <div class="form-control w-full">
                <label class="label font-mono font-bold">
                    <span class="label-text underline">{dimension}</span>
                </label>
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
            <div class="form-control flex-1">
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
                            class="btn btn-ghost btn-circle btn-sm mt-1"
                            disabled=disabled
                            on:click=move |_| {
                                on_remove.call(condition.with_value(|v| v.variable.clone()));
                            }
                        >

                            <i class="ri-delete-bin-2-line text-2xl font-bold"></i>
                        </button>
                    </Show>
                </div>
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
    #[prop(default = DropdownDirection::Left)] dropdown_direction: DropdownDirection,
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
    let (context_rs, context_ws) = create_signal(context.clone());

    let dimensions = StoredValue::new(dimensions);
    let mandatory_dimensions = StoredValue::new(
        dimensions
            .get_value()
            .into_iter()
            .filter(|dim| dim.mandatory)
            .map(|dim| dim.dimension)
            .collect::<HashSet<String>>(),
    );

    let last_idx = create_memo(move |_| context_rs.get().len().max(1) - 1);

    create_effect(move |_| {
        let f_context = context_rs.get(); // context will now be a Value
        logging::log!("Context form effect {:?}", f_context);
        handle_change(f_context.clone()); // handle_change now expects Value
    });

    let on_select_dimension =
        Callback::new(move |selected_dimension: DimensionWithMandatory| {
            let dimension_name = selected_dimension.dimension;

            if let Ok(r#type) = SchemaType::try_from(selected_dimension.schema) {
                used_dimensions_ws.update(|value: &mut HashSet<String>| {
                    value.insert(dimension_name.clone());
                });
                context_ws.update(|value| {
                    value.push(Condition::new_with_default_expression(
                        dimension_name,
                        r#type,
                    ))
                });
            }
            // TODO show alert in case of invalid dimension
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
    });

    view! {
        <div class="form-control w-full">
            <div class="flex justify-between">
                <label class="label flex-col justify-center items-start">
                    <span class="label-text font-semibold text-base">Context</span>
                    <span class="label-text text-slate-400">{heading_sub_text}</span>
                </label>
                <Show when=move || {
                    !context_rs.get().is_empty() && !disabled
                }>
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
                                dropdown_width="w-fit"
                                dropdown_btn_type=DropdownBtnType::Link
                                dropdown_options=dimensions
                                disabled=disabled
                                dropdown_direction
                                on_select=on_select_dimension
                            />
                        }
                    }}
                </Show>
            </div>
            <div class="card w-full bg-slate-50">
                <div class="card-body">
                    <Show when=move || context_rs.get().is_empty()>
                        <div class="flex justify-center">
                            <Dropdown
                                dropdown_width="w-fit"
                                dropdown_icon="ri-add-line".to_string()
                                dropdown_text="Add Context".to_string()
                                dropdown_btn_type=DropdownBtnType::Link
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

                        key=|(idx, condition)| {
                            format!(
                                "{}-{}-{}",
                                condition.variable,
                                idx,
                                condition.expression.to_operator(),
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
                                && !mandatory_dimensions.get_value().contains(&condition.variable);
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
                </div>
            </div>
        </div>
    }
}
