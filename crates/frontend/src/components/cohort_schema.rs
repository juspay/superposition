use std::collections::HashMap;

use leptos::*;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value, json};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use superposition_types::api::functions::FunctionEnvironment;
use superposition_types::api::{dimension::DimensionResponse, functions::KeyType};
use web_sys::MouseEvent;

use crate::components::alert::AlertType;
use crate::components::context_form::ConditionInput;
use crate::logic::{Condition, Conditions, jsonlogic};
use crate::providers::editor_provider::EditorProvider;
use crate::schema::{EnumVariants, JsonSchemaType, SchemaType};
use crate::types::{OrganisationId, Workspace};
use crate::utils::{to_title_case, value_compute_fn_generator};
use crate::{
    components::{
        button::Button,
        drawer::PortalDrawer,
        form::label::Label,
        input::{Input, InputType, NumberArrayInput, StringArrayInput},
        sortable::Sortable,
    },
    providers::alert_provider::enqueue_alert,
};

#[derive(Serialize, Deserialize, Clone)]
pub struct CohortSchemaFormat {
    r#enum: Vec<String>,
    definitions: Map<String, Value>,
    r#type: String,
}

impl Default for CohortSchemaFormat {
    fn default() -> Self {
        Self {
            r#enum: vec!["otherwise".to_string()],
            definitions: Map::new(),
            r#type: "string".to_string(),
        }
    }
}

#[derive(Clone)]
enum Action {
    None,
    View(&'static str, Option<String>, Value),
    Add(DimensionResponse),
    Update(String, DimensionResponse),
    Remove(String),
}

fn validate_cohort_info(enum_name: &str, enum_value: &Value) -> Result<String, String> {
    let enum_name = enum_name.trim().to_string();
    if enum_name.is_empty() {
        return Err("Cohort name cannot be empty".to_string());
    }
    if enum_name == "otherwise" {
        return Err("Cohort name cannot be 'otherwise'".to_string());
    }
    if enum_value.is_null() {
        return Err("Cohort definition cannot be empty".to_string());
    }
    Ok(enum_name)
}

#[component]
pub fn CohortSchema(
    dimension_schema: Value,
    #[prop(into)] on_change: Callback<Value>,
    #[prop(optional)] cohort_based_on: Option<DimensionResponse>,
    #[prop(default = false)] hide_label: bool,
) -> impl IntoView {
    let cohort_schema_format_rws = RwSignal::new(
        serde_json::from_value::<CohortSchemaFormat>(dimension_schema.clone())
            .unwrap_or_default(),
    );
    let action_rws = RwSignal::new(Action::None);

    let view_only = cohort_based_on.is_none();
    let cohort_based_on = StoredValue::new(cohort_based_on);

    Effect::new(move |_| {
        on_change.call(
            serde_json::to_value(cohort_schema_format_rws.get()).unwrap_or_default(),
        );
    });

    let item_renderer = move |index: usize, item: String, inactive: bool| {
        let item_st = StoredValue::new(item);
        let disabled = inactive || view_only;
        view! {
            <div
                class=format!(
                    "sortable-item max-w-[inherit] w-full p-3 flex justify-between items-center gap-3 bg-white border rounded-lg shadow-sm transition-shadow {}",
                    if disabled { "" } else { "hover:shadow-md" },
                )
                data-id=index.to_string()
            >
                <div class=format!(
                    "{} w-8 h-8 text-purple-500 select-none flex items-center justify-center",
                    if disabled {
                        "cursor-not-allowed opacity-50"
                    } else {
                        "drag-handle bg-purple-50 rounded border border-purple-200 cursor-grab hover:text-purple-700"
                    },
                )>"⋮⋮"</div>
                <div class="w-full font-medium text-purple-700 text-ellipsis overflow-hidden whitespace-nowrap">
                    {item_st.get_value()}
                </div>
                <div class="flex items-center gap-4">
                    {match (inactive, cohort_based_on.get_value()) {
                        (false, Some(cohort_based_on)) => {
                            view! {
                                <i
                                    class="ri-edit-2-line text-xl text-blue-500 hover:text-blue-700 cursor-pointer"
                                    on:click=move |_| {
                                        action_rws
                                            .set(
                                                Action::Update(item_st.get_value(), cohort_based_on.clone()),
                                            );
                                    }
                                />
                                <i
                                    class="ri-delete-bin-6-line text-xl text-red-500 hover:text-red-700 cursor-pointer"
                                    on:click=move |_| {
                                        action_rws.set(Action::Remove(item_st.get_value()));
                                    }
                                />
                            }
                                .into_view()
                        }
                        (false, None) => {
                            view! {
                                <i
                                    class="ri-eye-line text-xl text-blue-500 hover:text-blue-700 cursor-pointer"
                                    on:click=move |_| {
                                        action_rws
                                            .set(
                                                Action::View(
                                                    "Cohort Enum Definition",
                                                    Some(item_st.get_value()),
                                                    cohort_schema_format_rws
                                                        .with(|s| {
                                                            s.definitions
                                                                .get(&item_st.get_value())
                                                                .cloned()
                                                                .unwrap_or_default()
                                                        }),
                                                ),
                                            );
                                    }
                                />
                            }
                                .into_view()
                        }
                        (true, _) => ().into_view(),
                    }}
                </div>
            </div>
        }.into_view()
    };

    view! {
        <div class="form-control">
            <Show when=move || !hide_label>
                <Label
                    title="Cohort Definition"
                    description="Define the cohort options in terms of JSONLogic. The schema must have an enum with 'otherwise' as one of the values. Order of the enum defines the priority of the cohort options."
                    class="max-w-md"
                />
            </Show>
            <div class="max-w-md flex flex-col gap-2">
                <div
                    class="px-1 text-sm text-blue-500 underline underline-offset-2 cursor-pointer"
                    on:click=move |_| {
                        action_rws
                            .set(
                                Action::View(
                                    "Cohort Schema Definition",
                                    None,
                                    serde_json::to_value(cohort_schema_format_rws.get())
                                        .unwrap_or_default(),
                                ),
                            );
                    }
                >
                    <i class="ri-eye-line" />
                    "View Raw Schema definition"
                </div>
                <Sortable
                    items=Signal::derive(move || {
                        cohort_schema_format_rws
                            .get()
                            .r#enum
                            .iter()
                            .filter(|&e| e != "otherwise")
                            .cloned()
                            .collect::<Vec<String>>()
                    })
                    on_change=move |new_items| {
                        let new_items: Vec<String> = new_items;
                        let mut new_schema = cohort_schema_format_rws.get();
                        new_schema.r#enum = new_items
                            .into_iter()
                            .filter(|e| e != "otherwise")
                            .collect::<Vec<_>>();
                        new_schema.r#enum.push("otherwise".to_string());
                        cohort_schema_format_rws.set(new_schema);
                    }
                    class="space-y-2"
                    animation=200
                    render_item=move |(index, item)| item_renderer(index, item, false)
                    disabled=view_only
                />
                <div class="flex flex-col justify-center items-center gap-3">
                    <Show when=move || !view_only>
                        <div
                            class="w-full px-2 py-1 flex justify-center items-center text-xs text-gray-600 font-mono bg-gray-100 rounded cursor-pointer hover:bg-gray-200"
                            on:click=move |_| {
                                if let Some(cohort_based_on) = cohort_based_on.get_value() {
                                    action_rws.set(Action::Add(cohort_based_on));
                                }
                            }
                        >
                            "Add +"
                        </div>
                    </Show>
                    {item_renderer(
                        cohort_schema_format_rws.with(|s| s.r#enum.len()),
                        "otherwise".to_string(),
                        true,
                    )}
                    <div class="w-full text-xs text-purple-600 font-mono bg-purple-50 px-2 py-1 rounded">
                        "Note: 'otherwise' will always be the last option"
                    </div>
                </div>
            </div>
        </div>
        {move || {
            match action_rws.get() {
                Action::None => ().into_view(),
                Action::View(label, description, value) => {
                    let value = StoredValue::new(value);
                    view! {
                        <PortalDrawer
                            title=label
                            handle_close=move |_| action_rws.set(Action::None)
                        >
                            {match description.clone() {
                                None => ().into_view(),
                                Some(desc) => view! { <Label title=desc /> }.into_view(),
                            }}
                            <EditorProvider>
                                <Input
                                    id="cohort-schema-definition-preview"
                                    class="rounded-md resize-y w-full max-w-md"
                                    schema_type=SchemaType::Single(JsonSchemaType::Object)
                                    value=value.get_value()
                                    on_change=move |_| ()
                                    r#type=InputType::Monaco(vec![])
                                    disabled=true
                                />
                            </EditorProvider>
                        </PortalDrawer>
                    }
                        .into_view()
                }
                Action::Add(cohort_based_on) => {
                    view! {
                        <PortalDrawer
                            title="Add Cohort Option"
                            handle_close=move |_| action_rws.set(Action::None)
                        >
                            <CohortForm
                                value=Value::Null
                                on_change=move |(mut enum_name, enum_definition): (String, Value)| {
                                    enum_name = validate_cohort_info(&enum_name, &enum_definition)?;
                                    let mut new_schema = cohort_schema_format_rws.get();
                                    if new_schema.r#enum.contains(&enum_name) {
                                        return Err("Cohort name must be unique".to_string());
                                    }
                                    new_schema
                                        .definitions
                                        .insert(enum_name.clone(), enum_definition);
                                    new_schema
                                        .r#enum
                                        .insert(new_schema.r#enum.len() - 1, enum_name);
                                    cohort_schema_format_rws.set(new_schema);
                                    action_rws.set(Action::None);
                                    Ok(())
                                }
                                cohort_based_on=cohort_based_on.clone()
                            />
                        </PortalDrawer>
                    }
                        .into_view()
                }
                Action::Update(cohort_name, cohort_based_on) => {
                    view! {
                        <PortalDrawer
                            title="Update Cohort Option"
                            handle_close=move |_| action_rws.set(Action::None)
                        >
                            <CohortForm
                                cohort_name=cohort_name.clone()
                                value=cohort_schema_format_rws
                                    .with(|s| {
                                        s.definitions.get(&cohort_name).cloned().unwrap_or_default()
                                    })
                                on_change=move |(mut enum_name, enum_definition): (String, Value)| {
                                    enum_name = validate_cohort_info(&enum_name, &enum_definition)?;
                                    cohort_schema_format_rws
                                        .update(|s| {
                                            s.definitions.insert(enum_name.clone(), enum_definition);
                                        });
                                    action_rws.set(Action::None);
                                    Ok(())
                                }
                                cohort_based_on=cohort_based_on.clone()
                                edit=true
                            />
                        </PortalDrawer>
                    }
                        .into_view()
                }
                Action::Remove(cohort_name) => {
                    let cohort_name = StoredValue::new(cohort_name);
                    view! {
                        <PortalDrawer
                            title="Remove Cohort Option"
                            handle_close=move |_| action_rws.set(Action::None)
                        >
                            <div class="flex flex-col gap-5">
                                <span>
                                    {format!(
                                        "Are you sure you want to remove the cohort option '{}'? This action cannot be undone.",
                                        cohort_name.get_value(),
                                    )}
                                </span>
                                <div class="form-control">
                                    <Label
                                        title="Preview"
                                        description="Preview of the cohort definition"
                                    />
                                    <EditorProvider>
                                        <Input
                                            id="cohort-definition-preview"
                                            class="rounded-md resize-y w-full max-w-md"
                                            schema_type=SchemaType::Single(JsonSchemaType::Object)
                                            value=cohort_schema_format_rws
                                                .with(|s| {
                                                    s.definitions
                                                        .get(&cohort_name.get_value())
                                                        .cloned()
                                                        .unwrap_or_default()
                                                })
                                            on_change=move |_| ()
                                            r#type=InputType::Monaco(vec![])
                                            disabled=true
                                        />
                                    </EditorProvider>
                                </div>
                                <div class="flex justify-end gap-2">
                                    <Button
                                        class="h-12 w-48 bg-red-500 hover:bg-red-600"
                                        text="Remove"
                                        icon_class="ri-delete-bin-6-line"
                                        on_click=move |_| {
                                            let cohort_name = cohort_name.get_value();
                                            cohort_schema_format_rws
                                                .update(|s| {
                                                    s.r#enum.retain(|e| e != &cohort_name);
                                                    s.definitions.remove(&cohort_name);
                                                });
                                            action_rws.set(Action::None);
                                        }
                                    />
                                    <Button
                                        class="h-12 w-48"
                                        text="Cancel"
                                        icon_class="ri-forbid-line"
                                        on_click=move |_| action_rws.set(Action::None)
                                    />
                                </div>
                            </div>
                        </PortalDrawer>
                    }
                        .into_view()
                }
            }
        }}
    }
}

#[derive(EnumIter, strum_macros::Display, PartialEq, Clone, Copy)]
enum CohortInputType {
    StringArray,
    NumberArray,
    Simple,
    Custom,
}

#[component]
fn CohortForm(
    #[prop(into, default = String::new())] cohort_name: String,
    value: Value,
    #[prop(into)] on_change: Callback<(String, Value), Result<(), String>>,
    cohort_based_on: DimensionResponse,
    #[prop(default = false)] edit: bool,
) -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org_id = use_context::<Signal<OrganisationId>>().unwrap();

    let cohort_name_rws = RwSignal::new(cohort_name.clone());
    let value_rws = RwSignal::new(value.clone());
    let schema_type = StoredValue::new(
        SchemaType::try_from(&cohort_based_on.schema as &Map<String, Value>)
            .unwrap_or_default(),
    );

    let filter_option = move |option: &CohortInputType| {
        let schema_type = schema_type.get_value();
        match option {
            CohortInputType::StringArray => match schema_type {
                SchemaType::Single(JsonSchemaType::String) => true,
                SchemaType::Single(_) => false,
                SchemaType::Multiple(types) => types.contains(&JsonSchemaType::String),
            },
            CohortInputType::NumberArray => match schema_type {
                SchemaType::Single(JsonSchemaType::Number)
                | SchemaType::Single(JsonSchemaType::Integer) => true,
                SchemaType::Single(_) => false,
                SchemaType::Multiple(types) => {
                    types.contains(&JsonSchemaType::Number)
                        || types.contains(&JsonSchemaType::Integer)
                }
            },
            _ => true,
        }
    };

    let input_type_options = CohortInputType::iter()
        .filter(filter_option)
        .collect::<Vec<_>>();
    let input_type_rws = RwSignal::new(
        input_type_options
            .first()
            .cloned()
            .unwrap_or(CohortInputType::Simple),
    );

    view! {
        <form class="w-full flex flex-col gap-5 text-gray-700 bg-white">
            <div class="form-control">
                <Label title="Cohort Name" />
                <Input
                    id="cohort-name"
                    class="rounded-md w-full max-w-md"
                    schema_type=SchemaType::Single(JsonSchemaType::String)
                    value=Value::String(cohort_name_rws.get())
                    on_change=move |new_value: Value| {
                        cohort_name_rws.set(new_value.as_str().unwrap_or_default().to_string());
                    }
                    r#type=InputType::Text
                    disabled=edit
                />
            </div>
            <div class="form-control">
                <Label
                    title="Cohort Definition"
                    description="Definition of the cohort in terms of jsonlogic"
                />
                <div role="tablist" class="tabs tabs-lifted mb-2">
                    {move || {
                        input_type_options
                            .iter()
                            .cloned()
                            .map(|tab| {
                                view! {
                                    <div
                                        attr:role="tab"
                                        class=if input_type_rws.with(|t| *t == tab) {
                                            "tab tab-active [--tab-border-color:#a651f5] text-center font-bold"
                                        } else {
                                            "tab text-center font-bold"
                                        }
                                        on:click=move |_| {
                                            input_type_rws.set(tab);
                                        }
                                    >
                                        {to_title_case(&tab.to_string())}
                                    </div>
                                }
                            })
                            .collect_view()
                    }}
                </div>
                <div>
                    {move || match input_type_rws.get() {
                        CohortInputType::Simple => {
                            let dimension_schema: &Map<String, Value> = &cohort_based_on.schema;
                            let Ok(schema_type) = SchemaType::try_from(dimension_schema) else {
                                return view! { <span class="text-sm red">An error occured</span> }
                                    .into_view();
                            };
                            let Ok(enum_variants) = EnumVariants::try_from(dimension_schema) else {
                                return view! { <span class="text-sm red">An error occured</span> }
                                    .into_view();
                            };
                            let condition = Signal::derive({
                                let dimension = cohort_based_on.dimension.clone();
                                let schema_type = schema_type.clone();
                                move || {
                                    jsonlogic::condition::try_from_condition_json(&value_rws.get())
                                        .unwrap_or_else(|_| Condition {
                                            variable: dimension.clone(),
                                            value: schema_type.default_value(),
                                        })
                                }
                            });
                            let input_type = InputType::from((schema_type.clone(), enum_variants));
                            let fn_environment = Memo::new(move |_| {
                                FunctionEnvironment {
                                    context: Conditions(vec![condition.get()]).into(),
                                    overrides: Map::new(),
                                }
                            });
                            let mut value_compute_callbacks = HashMap::new();
                            value_compute_fn_generator(
                                    cohort_based_on.dimension.clone(),
                                    cohort_based_on.value_compute_function_name.clone(),
                                    fn_environment,
                                    &KeyType::Dimension,
                                    workspace.get().0,
                                    org_id.get().0,
                                )
                                .map(|(dimension, cb)| {
                                    value_compute_callbacks.insert(dimension, cb)
                                });

                            view! {
                                <div class="form-control gap-3">
                                    <span class="label-text text-slate-400">
                                        "Use simple condition to form a cohort"
                                    </span>
                                    <ConditionInput
                                        disabled=false
                                        allow_remove=false
                                        condition=condition.get()
                                        input_type
                                        schema_type
                                        value_compute_callbacks
                                        on_remove=|_| ()
                                        on_value_change=move |new_expr| {
                                            value_rws
                                                .set(
                                                    jsonlogic::expression::to_condition_json(
                                                        &new_expr,
                                                        &condition.get().variable,
                                                    ),
                                                );
                                        }
                                    />
                                </div>
                            }
                                .into_view()
                        }
                        CohortInputType::NumberArray if filter_option(
                            &CohortInputType::NumberArray,
                        ) => {
                            let cohort_based_on = cohort_based_on.dimension.clone();

                            view! {
                                <div class="form-control gap-3">
                                    <span class="label-text text-slate-400">
                                        "Enter number values which together form a cohort"
                                    </span>
                                    <NumberArrayInput
                                        unique=true
                                        options=value_rws
                                            .with_untracked(|v| {
                                                v.as_object()
                                                    .and_then(|obj| obj.get("in"))
                                                    .and_then(Value::as_array)
                                                    .and_then(|arr| arr.get(1))
                                                    .and_then(Value::as_array)
                                                    .map(|arr| arr.iter().filter_map(Value::as_f64).collect())
                                                    .unwrap_or_default()
                                            })
                                        on_change=move |new_val| {
                                            value_rws
                                                .set(
                                                    json!(
                                                        {
                                                        "in": [
                                                            {
                                                                "var": cohort_based_on
                                                            },
                                                            new_val
                                                        ]
                                                    }
                                                    ),
                                                )
                                        }
                                    />
                                </div>
                            }
                                .into_view()
                        }
                        CohortInputType::StringArray if filter_option(
                            &CohortInputType::StringArray,
                        ) => {
                            let cohort_based_on = cohort_based_on.dimension.clone();
                            view! {
                                <div class="form-control gap-3">
                                    <span class="label-text text-slate-400">
                                        "Enter string values which together form a cohort"
                                    </span>
                                    <StringArrayInput
                                        unique=true
                                        options=value_rws
                                            .with_untracked(|v| {
                                                v.as_object()
                                                    .and_then(|obj| obj.get("in"))
                                                    .and_then(Value::as_array)
                                                    .and_then(|arr| arr.get(1))
                                                    .and_then(Value::as_array)
                                                    .map(|arr| {
                                                        arr.iter()
                                                            .filter_map(Value::as_str)
                                                            .map(String::from)
                                                            .collect()
                                                    })
                                                    .unwrap_or_default()
                                            })
                                        on_change=move |new_val| {
                                            value_rws
                                                .set(
                                                    json!(
                                                        {
                                                        "in": [
                                                            {
                                                                "var": cohort_based_on
                                                            },
                                                            new_val
                                                        ]
                                                    }
                                                    ),
                                                )
                                        }
                                    />
                                </div>
                            }
                                .into_view()
                        }
                        CohortInputType::Custom => {
                            view! {
                                <div class="form-control gap-3">
                                    <span class="label-text text-slate-400">
                                        "Provide a valid jsonlogic to define the cohort"
                                    </span>
                                    <EditorProvider>
                                        <Input
                                            id="cohort-definition"
                                            class="rounded-md resize-y w-full max-w-md"
                                            schema_type=SchemaType::Single(JsonSchemaType::Object)
                                            value=value_rws.get()
                                            on_change=move |new_value| value_rws.set(new_value)
                                            r#type=InputType::Monaco(vec![])
                                        />
                                    </EditorProvider>
                                    <div class="form-control gap-1">
                                        <span class="label-text text-slate-400">
                                            "Hint: Use the following format to access the cohort variable"
                                        </span>
                                        <pre class="w-fit p-2 text-sm font-mono bg-gray-100 rounded">
                                            {format!(
                                                "{{\n\t\"var\": \"{}\"\n}}",
                                                cohort_based_on.dimension,
                                            )}
                                        </pre>
                                    </div>
                                </div>
                            }
                                .into_view()
                        }
                        _ => ().into_view(),
                    }}
                </div>
            </div>
            <Show when=move || input_type_rws.get() != CohortInputType::Custom>
                <div class="form-control">
                    <Label title="Preview" description="Preview of the cohort definition" />
                    <EditorProvider>
                        {move || {
                            view! {
                                <Input
                                    id="cohort-definition-preview"
                                    class="rounded-md resize-y w-full max-w-md"
                                    schema_type=SchemaType::Single(JsonSchemaType::Object)
                                    value=value_rws.get()
                                    on_change=move |_| ()
                                    r#type=InputType::Monaco(vec![])
                                    disabled=true
                                />
                            }
                        }}
                    </EditorProvider>
                </div>
            </Show>
            <div class="flex justify-end gap-2">
                <Button
                    class="h-12 w-48"
                    text="Save"
                    icon_class="ri-save-2-line"
                    on_click=move |event: MouseEvent| {
                        event.prevent_default();
                        let result = on_change.call((cohort_name_rws.get(), value_rws.get()));
                        match result {
                            Ok(_) => {}
                            Err(err) => enqueue_alert(err, AlertType::Error, 5000),
                        }
                    }
                />
                <Button
                    class="h-12 w-48"
                    text="Reset"
                    icon_class="ri-restart-line"
                    on_click=move |event: MouseEvent| {
                        event.prevent_default();
                        cohort_name_rws.set(cohort_name.clone());
                        value_rws.set(value.clone());
                    }
                />
            </div>
        </form>
    }.into_view()
}
