use std::collections::{HashMap, HashSet};

use leptos::*;
use serde_json::{Map, Value};
use strum_macros::Display;
use superposition_types::api::functions::KeyType;
use superposition_types::api::{
    dimension::DimensionResponse, functions::FunctionEnvironment,
};

use crate::components::form::label::Label;
use crate::components::input::{Input, InputType, StringArrayInput};
use crate::components::tooltip::{Tooltip, TooltipPosition};
use crate::logic::{Condition, Conditions};
use crate::schema::EnumVariants;
use crate::types::{OrganisationId, ValueComputeCallbacks, Workspace};
use crate::utils::value_compute_fn_generator;
use crate::{
    components::dropdown::{Dropdown, DropdownDirection},
    schema::SchemaType,
};

#[allow(clippy::large_enum_variant)]
pub enum TooltipType {
    Info(String),
    Error(View),
    None,
}

#[derive(Clone, Copy, Display)]
enum ResolveOperator {
    #[strum(serialize = "==")]
    Is,
    #[strum(serialize = "in")]
    In,
}

impl From<&str> for ResolveOperator {
    fn from(value: &str) -> Self {
        match value {
            "variantIds" => Self::In,
            _ => Self::Is,
        }
    }
}

#[component]
pub fn ConditionInput(
    disabled: bool,
    allow_remove: bool,
    condition: Condition,
    input_type: InputType,
    schema_type: SchemaType,
    value_compute_callbacks: ValueComputeCallbacks,
    #[prop(into)] on_remove: Callback<()>,
    #[prop(into)] on_value_change: Callback<Value>,
    #[prop(default = TooltipType::None)] tooltip: TooltipType,
) -> impl IntoView {
    let operator = ResolveOperator::from(condition.variable.as_str());
    let dimension = condition.variable.clone();

    let value_compute_callback = value_compute_callbacks.get(&dimension).cloned();

    view! {
        <div class="flex gap-x-6">
            <div class="form-control">
                <label class="label font-medium text-sm">
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
                    disabled=true
                    value=operator.to_string()
                    name="context-dimension-operator"
                    class="select select-bordered w-full max-w-xs text-sm rounded-lg h-10 px-4 appearance-none leading-tight focus:outline-none focus:shadow-outline"
                >
                    <option value=operator.to_string() selected=true>
                        {operator.to_string().to_uppercase()}
                    </option>
                </select>

            </div>
        </div>
        <div class="form-control">
            <label class="w-fit label flex gap-1 text-sm">
                <span class="label-text font-medium">Value</span>
                <Show when=move || matches!(operator, ResolveOperator::In)>
                    <span class="label-text font-light text-slate-400">
                        "(Press enter to add option)"
                    </span>
                </Show>
            </label>
            <div class="flex gap-x-6 items-center">
                {match operator {
                    ResolveOperator::Is => {
                        view! {
                            <Input
                                value=condition.value.clone()
                                schema_type=schema_type.clone()
                                on_change=move |value: Value| on_value_change.call(value)
                                r#type=input_type.clone()
                                disabled
                                id=format!("{}-{}", condition.variable.clone(), operator)
                                class="w-[450px]"
                                name=""
                                value_compute_function=value_compute_callback
                            />
                        }
                    }
                    ResolveOperator::In => {
                        view! {
                            <StringArrayInput
                                options=condition
                                    .value
                                    .as_array()
                                    .unwrap_or(&vec![])
                                    .iter()
                                    .filter_map(|v| v.as_str().map(|s| s.to_string()))
                                    .collect()
                                unique=true
                                show_label=false
                                on_change=Callback::new(move |value: Vec<String>| {
                                    on_value_change
                                        .call(
                                            Value::Array(value.into_iter().map(Value::String).collect()),
                                        )
                                })
                            />
                        }
                    }
                }} <Show when=move || allow_remove>
                    <button
                        class="btn btn-ghost btn-circle btn-sm"
                        disabled=disabled
                        on:click=move |_| on_remove.call(())
                    >
                        <i class="ri-delete-bin-2-line text-2xl font-bold" />
                    </button>
                </Show>
                {match tooltip {
                    TooltipType::Info(msg) => {
                        view! {
                            <div class="tooltip tooltip-left" data-tip=msg>
                                <i class="ri-information-line text-2xl" />
                            </div>
                        }
                            .into_view()
                    }
                    TooltipType::Error(children) => {
                        view! {
                            <Tooltip
                                position=TooltipPosition::Left
                                icon_class="ri-error-warning-line text-2xl text-red-600"
                            >
                                {children}
                            </Tooltip>
                        }
                            .into_view()
                    }
                    TooltipType::None => ().into_view(),
                }}
            </div>
        </div>
    }
}

#[component]
pub fn ContextForm(
    context: Conditions,
    dimensions: Vec<DimensionResponse>,
    fn_environment: Memo<FunctionEnvironment>,
    #[prop(default = false)] disabled: bool,
    #[prop(default = false)] resolve_mode: bool,
    #[prop(into, default = String::new())] heading_sub_text: String,
    #[prop(default = DropdownDirection::Down)] dropdown_direction: DropdownDirection,
    #[prop(into)] on_context_change: Callback<Conditions, ()>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org_id = use_context::<Signal<OrganisationId>>().unwrap();

    let dimensions = StoredValue::new(if resolve_mode {
        dimensions
    } else {
        dimensions
            .into_iter()
            .filter(|dimension| dimension.dimension != "variantIds")
            .collect::<Vec<_>>()
    });
    let dimension_map = StoredValue::new(
        dimensions
            .get_value()
            .into_iter()
            .map(|v| (v.dimension.clone(), v.clone()))
            .collect::<HashMap<String, DimensionResponse>>(),
    );
    let mandatory_dimensions = StoredValue::new(
        dimensions
            .get_value()
            .into_iter()
            .filter(|dim| dim.mandatory)
            .collect::<Vec<_>>(),
    );
    let mandatory_dimensions_set = StoredValue::new(
        mandatory_dimensions
            .get_value()
            .into_iter()
            .map(|dim| dim.dimension)
            .collect::<HashSet<_>>(),
    );
    let value_compute_callbacks = Signal::derive(move || {
        dimensions
            .get_value()
            .into_iter()
            .filter_map(|d| {
                value_compute_fn_generator(
                    d.dimension.clone(),
                    d.value_compute_function_name.clone(),
                    fn_environment,
                    &KeyType::Dimension,
                    workspace.get_untracked().0,
                    org_id.get_untracked().0,
                )
            })
            .collect::<ValueComputeCallbacks>()
    });

    let insert_dimension =
        move |context: &mut Conditions, dimension: &DimensionResponse| {
            if !context.includes(&dimension.dimension) {
                logging::log!(
                    "Inserting dimension {} with schema {:?}",
                    dimension.dimension,
                    dimension.schema
                );
                let default_value = if dimension.dimension == "variantIds" {
                    Value::Array(vec![])
                } else {
                    SchemaType::try_from(&dimension.schema as &Map<String, Value>)
                        .unwrap_or_default()
                        .default_value()
                };

                context.push(Condition {
                    variable: dimension.dimension.clone(),
                    value: default_value,
                });
            }
        };

    let (context_rs, context_ws) = create_signal({
        let mut context = context;
        if !disabled && !resolve_mode {
            mandatory_dimensions
                .get_value()
                .into_iter()
                .for_each(|dimension| {
                    insert_dimension(&mut context, &dimension);
                });
        }
        context
    });

    Effect::new(move |_| {
        let context = context_rs.get();
        logging::log!("Context form effect {:?}", context);
        on_context_change.call(context.clone());
    });

    let used_dimensions = Signal::derive(move || {
        context_rs
            .get()
            .0
            .into_iter()
            .map(|condition| condition.variable)
            .collect::<HashSet<_>>()
    });

    // a dimension is invalid if, it is used and is present in the dependency graph of another used dimension
    let invalid_dimensions = Signal::derive(move || {
        let used_dimensions = used_dimensions.get();
        let dimension_map = dimension_map.get_value();
        let mut invalid_dims = HashMap::new();

        for dim in used_dimensions.iter() {
            if let Some(dimension_info) = dimension_map.get(dim) {
                for (dep_dim, _) in dimension_info.dependency_graph.iter() {
                    if dep_dim != dim && used_dimensions.contains(dep_dim) {
                        match invalid_dims.get(dep_dim) {
                            Some(existing) => {
                                if dimension_info.dependency_graph.get(existing).is_some()
                                {
                                    invalid_dims.insert(dep_dim.clone(), dim.clone());
                                }
                            }
                            None => {
                                invalid_dims.insert(dep_dim.clone(), dim.clone());
                            }
                        };
                    }
                }
            }
        }
        invalid_dims
    });

    let last_idx = create_memo(move |_| context_rs.get().len().max(1) - 1);

    let on_select_dimension =
        Callback::new(move |selected_dimension: DimensionResponse| {
            let mut context = context_rs.get();
            insert_dimension(&mut context, &selected_dimension);
            context_ws.update(|v| {
                *v = context;
            });
        });

    let on_value_change = Callback::new(move |(idx, condition): (usize, Condition)| {
        context_ws.update(|v| {
            if idx < v.len() {
                v[idx] = condition;
            }
        })
    });

    let on_remove = Callback::new(move |idx| {
        context_ws.update(|v| {
            v.remove(idx);
        });
    });

    let get_tool_tip_text = move |variable: &String| -> TooltipType {
        if mandatory_dimensions_set.with_value(|s| s.contains(variable)) {
            return TooltipType::Info("Mandatory Dimension".to_string());
        }
        if let Some(parent) = invalid_dimensions.with(|m| m.get(variable).cloned()) {
            return TooltipType::Error(view! {
                <div class="text-base w-[250px]">
                    {"Dimension "} <span class="w-fit italic font-semibold">{variable}</span>
                    {" is a cohort dimension which can be derived from "}
                    <span class="w-fit italic font-semibold">{parent}</span>
                    {" dimension using the cohort definitions. Hence, usage of this dimension is not allowed."}
                </div>
            }.into_view());
        }
        TooltipType::None
    };

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
                                ResolveOperator::from(condition.variable.as_str()),
                                invalid_dimensions
                                    .with(|m| {
                                        m.get(condition.variable.as_str())
                                            .cloned()
                                            .unwrap_or_default()
                                    }),
                            )
                        }

                        children=move |(idx, condition)| {
                            let (schema_type, enum_variants) = dimension_map
                                .with_value(|v| {
                                    v.get(&condition.variable)
                                        .map(|d| {
                                            let dimension_schema: &Map<String, Value> = &d.schema;
                                            (
                                                SchemaType::try_from(dimension_schema),
                                                EnumVariants::try_from(dimension_schema),
                                            )
                                        })
                                        .unwrap_or((Err("".to_string()), Err("".to_string())))
                                });
                            let Ok(schema_type) = schema_type else {
                                return view! { <span class="text-sm red">An error occured</span> }
                                    .into_view();
                            };
                            let Ok(enum_variants) = enum_variants else {
                                return view! { <span class="text-sm red">An error occured</span> }
                                    .into_view();
                            };
                            let is_mandatory = mandatory_dimensions_set
                                .with_value(|v| v.contains(&condition.variable));
                            let allow_remove = if resolve_mode {
                                !disabled
                            } else {
                                !disabled && !is_mandatory
                            };
                            let input_type = InputType::from((schema_type.clone(), enum_variants));
                            let d_name = condition.variable.clone();
                            let on_value_change = move |value| {
                                on_value_change
                                    .call((
                                        idx,
                                        Condition {
                                            variable: d_name.clone(),
                                            value,
                                        },
                                    ))
                            };
                            let tooltip = get_tool_tip_text(&condition.variable.clone());

                            view! {
                                <ConditionInput
                                    disabled
                                    allow_remove
                                    condition
                                    input_type
                                    schema_type
                                    on_remove=move |_| on_remove.call(idx)
                                    on_value_change
                                    tooltip
                                    value_compute_callbacks=value_compute_callbacks.get()
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
                                        ().into_view()
                                    }
                                }}
                            }
                                .into_view()
                        }
                    />

                    <Show when=move || { !context_rs.get().is_empty() && !disabled }>
                        <div class="mt-4">

                            {move || {
                                let unused_dimensions = Signal::derive(move || {
                                    let used_dimensions = used_dimensions.get();
                                    dimensions
                                        .get_value()
                                        .into_iter()
                                        .filter(|dimension| {
                                            !used_dimensions.contains(&dimension.dimension)
                                        })
                                        .collect::<Vec<DimensionResponse>>()
                                });

                                view! {
                                    <Dropdown
                                        dropdown_icon="ri-add-line".to_string()
                                        dropdown_text="Add Context".to_string()
                                        dropdown_options=unused_dimensions.get()
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
