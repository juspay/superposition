use std::collections::{HashMap, HashSet};

use leptos::*;
use serde_json::Value;
use superposition_types::api::{
    dimension::DimensionResponse, workspace::WorkspaceResponse,
};

use crate::components::form::label::Label;
use crate::components::input::{Input, InputType};
use crate::logic::{Condition, Conditions, Expression, Operator};
use crate::schema::EnumVariants;
use crate::types::{AutoCompleteCallbacks, OrganisationId, Tenant};
use crate::utils::autocomplete_fn_generator;
use crate::{
    components::dropdown::{Dropdown, DropdownDirection},
    schema::SchemaType,
};

#[component]
pub fn condition_input(
    disabled: bool,
    resolve_mode: bool,
    allow_remove: bool,
    condition: StoredValue<Condition>,
    input_type: StoredValue<InputType>,
    schema_type: StoredValue<SchemaType>,
    autocomplete_callbacks: AutoCompleteCallbacks,
    #[prop(into)] on_remove: Callback<String, ()>,
    #[prop(into)] on_value_change: Callback<Expression, ()>,
    #[prop(into)] on_operator_change: Callback<Operator, ()>,
    #[prop(default = String::new())] tooltip_text: String,
) -> impl IntoView {
    let workspace_settings = use_context::<StoredValue<WorkspaceResponse>>().unwrap();
    let (dimension, operator): (String, Operator) =
        condition.with_value(|v| (v.variable.clone(), v.into()));
    let tooltip_text = StoredValue::new(tooltip_text);

    let autocomplete_callback = autocomplete_callbacks.get(&dimension).cloned();

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

    let strict_mode = workspace_settings.with_value(|v| v.strict_mode);

    view! {
        <div class="flex gap-x-6">
            <div class="form-control">
                <label class="label text-sm">
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
                <label class="label font-medium text-sm">
                    <span class="label-text">Operator</span>
                </label>

                <select
                    disabled=disabled || resolve_mode || strict_mode
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
                        selected={ matches!(operator, Operator::Is) } || resolve_mode || strict_mode
                    >
                        {if strict_mode {
                            { Operator::Is.strict_mode_display().to_uppercase() }
                        } else {
                            { Operator::Is.to_string().to_uppercase() }
                        }}
                    </option>
                    <option value="in" selected=matches!(operator, Operator::In)>
                        {if strict_mode {
                            { Operator::In.strict_mode_display().to_uppercase() }
                        } else {
                            { Operator::In.to_string().to_uppercase() }
                        }}
                    </option>
                    <option value="has" selected=matches!(operator, Operator::Has)>
                        {if strict_mode {
                            { Operator::Has.strict_mode_display().to_uppercase() }
                        } else {
                            { Operator::Has.to_string().to_uppercase() }
                        }}
                    </option>
                    <option value="<=" selected=matches!(operator, Operator::Between)>
                        "BETWEEN (inclusive)"
                    </option>
                </select>

            </div>
        </div>
        <div class="form-control">
            <label class="label text-sm">
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
                                autocomplete_function=autocomplete_callback
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
                </Show> <Show when=move || !tooltip_text.get_value().is_empty()>
                    <div class="tooltip" data-tip=tooltip_text.get_value()>
                        <i class="ri-information-line text-2xl"></i>
                    </div>
                </Show>
            </div>
        </div>
    }
}

#[component]
pub fn context_form(
    context: Conditions,
    dimensions: Vec<DimensionResponse>,
    fn_environment: Memo<Value>,
    #[prop(default = false)] disabled: bool,
    #[prop(default = false)] resolve_mode: bool,
    #[prop(into, default = String::new())] heading_sub_text: String,
    #[prop(default = DropdownDirection::Down)] dropdown_direction: DropdownDirection,
    #[prop(into)] on_context_change: Callback<Conditions, ()>,
) -> impl IntoView {
    let dimension_map = store_value(
        dimensions
            .iter()
            .map(|v| (v.dimension.clone(), v.clone()))
            .collect::<HashMap<String, DimensionResponse>>(),
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

    let insert_dimension =
        move |context: &mut Conditions, dimension: &DimensionResponse| {
            if !context.includes(&dimension.dimension) {
                context.push(Condition::new_with_default_expression(
                    dimension.dimension.clone(),
                    SchemaType::try_from(dimension.schema.clone()).unwrap(),
                ));
            }
            dimension
                .dependency_graph
                .keys()
                .filter(|key| **key != dimension.dimension)
                .for_each(|dependency| {
                    if let Some(r#type) = dimension_map
                        .get_value()
                        .get(dependency)
                        .and_then(|d| SchemaType::try_from(d.schema.clone()).ok())
                    {
                        if !context.includes(dependency) {
                            context.push(Condition::new_with_default_expression(
                                dependency.clone(),
                                r#type.clone(),
                            ));
                        }
                    }
                });
        };

    let context_data = {
        let mut context = context;
        if !disabled && !resolve_mode {
            dimensions
                .get_value()
                .into_iter()
                .filter(|dim| dim.mandatory)
                .for_each(|dimension| {
                    insert_dimension(&mut context, &dimension);
                });
        }
        context
    };

    let (context_rs, context_ws) = create_signal(context_data);

    Effect::new(move |_| {
        let context = context_rs.get();
        logging::log!("Context form effect {:?}", context);
        on_context_change.call(context.clone());
    });

    let used_dimensions = Signal::derive(move || {
        context_rs
            .get()
            .iter()
            .map(|condition| condition.variable.clone())
            .collect::<HashSet<_>>()
    });

    let context_dependencies = Signal::derive(move || {
        used_dimensions
            .get()
            .iter()
            .flat_map(|dimension| {
                dimension_map
                    .get_value()
                    .get(dimension)
                    .map(|d| d.dependencies.clone())
                    .unwrap_or_default()
            })
            .collect::<HashSet<_>>()
    });

    let last_idx = create_memo(move |_| context_rs.get().len().max(1) - 1);

    let on_select_dimension =
        Callback::new(move |selected_dimension: DimensionResponse| {
            let mut context = context_rs.get();
            if !resolve_mode {
                insert_dimension(&mut context, &selected_dimension);
            } else {
                context.push(Condition::new_with_default_expression(
                    selected_dimension.dimension.clone(),
                    SchemaType::try_from(selected_dimension.schema.clone()).unwrap(),
                ));
            }
            context_ws.update(|v| {
                *v = context;
            });
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

    let on_remove = Callback::new(move |(idx, _): (usize, String)| {
        context_ws.update(|v| {
            v.remove(idx);
        });
    });

    let get_tool_tip_text = move |condition: StoredValue<Condition>| -> String {
        let variable = condition.get_value().variable;
        if mandatory_dimensions.get_value().contains(&variable) {
            return "Mandatory Dimension".to_string();
        }
        if context_dependencies.get().contains(&variable) {
            if let Some(parents) = dimension_map
                .get_value()
                .get(&variable)
                .map(|d| d.dependents.clone())
            {
                let parents_in_context = parents
                    .into_iter()
                    .filter(|parent| used_dimensions.get().contains(parent))
                    .collect::<Vec<String>>()
                    .join(", ");
                return format!("Required by: {parents_in_context}",);
            }
        }
        String::new()
    };

    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org_id = use_context::<Signal<OrganisationId>>().unwrap();
    let autocomplete_callbacks = dimensions
        .get_value()
        .iter()
        .filter_map(|d| {
            autocomplete_fn_generator(
                d.dimension.clone(),
                d.autocomplete_function_name.clone(),
                fn_environment,
                workspace.get_untracked().0,
                org_id.get_untracked().0,
            )
        })
        .collect::<AutoCompleteCallbacks>();

    view! {
        <div class="form-control w-full">
            <Label title="Context" description=heading_sub_text />
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
                                context_dependencies.with(|v| v.contains(&condition.variable)),
                            )
                        }

                        children=move |(idx, condition)| {
                            let (schema_type, enum_variants) = dimension_map
                                .with_value(|v| {
                                    v.get(&condition.variable)
                                        .map(|d| {
                                            (
                                                SchemaType::try_from(d.schema.clone()),
                                                EnumVariants::try_from(d.schema.clone()),
                                            )
                                        })
                                        .unwrap_or((Err("".to_string()), Err("".to_string())))
                                });
                            let operator = Operator::from(&condition);
                            if schema_type.is_err() || enum_variants.is_err() {
                                return view! { <span class="text-sm red">An error occured</span> }
                                    .into_view();
                            }
                            let schema_type = store_value(schema_type.unwrap());
                            let is_mandatory = mandatory_dimensions
                                .with_value(|v| v.contains(&condition.variable));
                            let has_context_dependency = context_dependencies
                                .with(|v| v.contains(&condition.variable));
                            let allow_remove = if resolve_mode {
                                !disabled
                            } else {
                                !disabled && !is_mandatory && !has_context_dependency
                            };
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
                                    autocomplete_callbacks=autocomplete_callbacks.clone()
                                />
                                {move || {
                                    if last_idx.get() != idx {
                                        view! {
                                            <div class="my-3 ml-7">
                                                <span class="text-xs font-bold">"&&"</span>
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
                                        !used_dimensions.get().contains(&dimension.dimension)
                                    })
                                    .collect::<Vec<DimensionResponse>>();
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
