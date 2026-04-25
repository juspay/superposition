use std::ops::Deref;

use leptos::*;
use leptos_router::use_navigate;
use serde_json::{Map, Value};
use superposition_types::{
    api::{
        context::UpdateRequest, dimension::DimensionResponse,
        functions::FunctionEnvironment,
    },
    database::models::cac::DefaultConfig,
};

use superposition_types::database::models::{ChangeReason, Description};

use crate::{
    components::{
        alert::AlertType,
        button::{ButtonAnchor, ButtonStyle},
        change_form::ChangeForm,
        change_summary::ChangeLogPopup,
        condition_pills::Condition,
        context_form::ContextForm,
        override_form::OverrideForm,
        skeleton::{Skeleton, SkeletonVariant},
        step_indicator::{Step, StepIndicator, StepNavigation, StepType},
        table::{Table, types::Column},
    },
    logic::Conditions,
    pages::context_override::utils::{
        create_context, try_update_context_payload, update_context,
    },
    providers::{
        alert_provider::enqueue_alert,
        condition_collapse_provider::ConditionCollapseProvider,
        editor_provider::EditorProvider,
    },
    schema::HtmlDisplay,
    types::{OrganisationId, Workspace},
};

enum ResponseType {
    Response(Option<String>), // Option<String> for new context_id on create
    UpdatePrecheck,
}

#[derive(Clone)]
pub enum ChangeType {
    Delete,
    Update(UpdateRequest),
}

#[component]
pub fn ContextOverrideForm(
    #[prop(default = false)] edit: bool,
    #[prop(optional)] context_id: Option<String>,
    context: Conditions,
    overrides: Vec<(String, Value)>,
    dimensions: Vec<DimensionResponse>,
    default_config: Vec<DefaultConfig>,
    #[prop(default = String::new())] description: String,
    #[prop(default = String::new())] change_reason: String,
    #[prop(into)] redirect_url_cancel: String,
    #[prop(into)] redirect_url_success: String,
) -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let dimensions = StoredValue::new(dimensions);
    let default_config = StoredValue::new(default_config);
    let context_id = StoredValue::new(context_id);

    let (context_rs, context_ws) = create_signal(context);
    let (overrides_rs, overrides_ws) = create_signal(overrides);
    let (description_rs, description_ws) = create_signal(description);
    let (change_reason_rs, change_reason_ws) = create_signal(change_reason);
    let (req_inprogress_rs, req_inprogress_ws) = create_signal(false);
    let update_request_rws = RwSignal::new(None);

    // Step state using StepType enum
    let current_step = RwSignal::new(StepType::ContextStep);

    let steps = vec![
        Step {
            title: "Context & Metadata".to_string(),
            description: Some("Define context conditions and description".to_string()),
            step_type: StepType::ContextStep,
        },
        Step {
            title: "Overrides".to_string(),
            description: Some("Configure override values".to_string()),
            step_type: StepType::OverrideStep,
        },
    ];

    let fn_environment = Memo::new(move |_| FunctionEnvironment {
        context: context_rs.get().into(),
        overrides: Map::from_iter(overrides_rs.get()),
    });

    let on_previous = Callback::new(move |_| {
        current_step.update(|step| {
            *step = match step {
                StepType::OverrideStep => StepType::ContextStep,
                StepType::ContextStep => StepType::ContextStep,
            };
        });
    });

    let on_next = Callback::new(move |_| {
        current_step.update(|step| {
            *step = match step {
                StepType::ContextStep => StepType::OverrideStep,
                StepType::OverrideStep => StepType::OverrideStep,
            };
        });
    });

    let redirect_url_success = StoredValue::new(redirect_url_success);
    let on_submit = Callback::new(move |_: ()| {
        req_inprogress_ws.set(true);
        spawn_local(async move {
            let f_overrides = overrides_rs.get_untracked();
            let workspace = workspace.get_untracked();
            let org = org.get_untracked();
            let redirect_url = redirect_url_success.get_value();
            let result =
                match (context_id.get_value(), update_request_rws.get_untracked()) {
                    (Some(_), Some((_, payload))) => {
                        let future = update_context(payload, &workspace, &org);
                        update_request_rws.set(None);
                        future.await.map(|_| ResponseType::Response(None))
                    }
                    (Some(ctx_id), None) => {
                        let request_payload = try_update_context_payload(
                            ctx_id.clone(),
                            Map::from_iter(f_overrides),
                            description_rs.get_untracked(),
                            change_reason_rs.get_untracked(),
                        );
                        match request_payload {
                            Ok(payload) => {
                                update_request_rws.set(Some((ctx_id, payload)));
                                Ok(ResponseType::UpdatePrecheck)
                            }
                            Err(e) => Err(e),
                        }
                    }
                    _ => create_context(
                        Map::from_iter(f_overrides),
                        context_rs.get_untracked(),
                        description_rs.get_untracked(),
                        change_reason_rs.get_untracked(),
                        &workspace,
                        &org,
                    )
                    .await
                    .map(|ctx| ResponseType::Response(Some(ctx.id))),
                };

            let is_edit = context_id.get_value().is_some();

            req_inprogress_ws.set(false);
            match result {
                Ok(ResponseType::UpdatePrecheck) => (),
                Ok(ResponseType::Response(new_context_id)) => {
                    logging::log!("Context and overrides submitted successfully");
                    let navigate = use_navigate();
                    let final_redirect_url = match new_context_id {
                        Some(id) => {
                            // For create, navigate to the detail page of the newly created override
                            format!("/admin/{}/{}/overrides/{}", org.0, workspace.0, id)
                        }
                        None => redirect_url.clone(),
                    };
                    navigate(&final_redirect_url, Default::default());
                    let success_message = if is_edit {
                        "Context and overrides updated successfully!"
                    } else {
                        "Context and overrides created successfully!"
                    };
                    enqueue_alert(
                        String::from(success_message),
                        AlertType::Success,
                        5000,
                    );
                }
                Err(e) => {
                    logging::log!("Error submitting context and overrides: {:?}", e);
                    enqueue_alert(e, AlertType::Error, 5000);
                }
            }
        });
    });

    view! {
        <div class="flex flex-col gap-6">
            <div class="flex justify-between items-center">
                <h1 class="text-2xl font-extrabold">
                    {if edit { "Edit Override" } else { "Create Override" }}
                </h1>
                <ButtonAnchor
                    text="Cancel".to_string()
                    href=redirect_url_cancel.clone()
                    icon_class="ri-close-line".to_string()
                    style=ButtonStyle::Outline
                />
            </div>

            <StepIndicator steps=&steps current_step=current_step />

            <EditorProvider>
                <div class="card bg-base-100 shadow">
                    <div class="card-body">
                        <Show when=move || current_step.get() == StepType::ContextStep>
                            <div class="flex flex-col gap-5">
                                <ContextForm
                                    dimensions=dimensions.get_value()
                                    context=context_rs.get_untracked()
                                    on_context_change=move |new_context| context_ws.set(new_context)
                                    fn_environment=fn_environment
                                    disabled=edit
                                />

                                <ChangeForm
                                    title="Description".to_string()
                                    placeholder="Enter a description".to_string()
                                    value=description_rs.get_untracked()
                                    on_change=move |new_description| {
                                        description_ws.set(new_description)
                                    }
                                />

                                <ChangeForm
                                    title="Reason for Change".to_string()
                                    placeholder="Enter a reason for this change".to_string()
                                    value=change_reason_rs.get_untracked()
                                    on_change=move |new_change_reason| {
                                        change_reason_ws.set(new_change_reason)
                                    }
                                />
                            </div>
                        </Show>

                        <Show when=move || current_step.get() == StepType::OverrideStep>
                            <div class="flex flex-col gap-4">
                                <h2 class="card-title">"Context"</h2>
                                <div class="flex flex-row flex-wrap gap-2">
                                    {context_rs
                                        .get()
                                        .0
                                        .iter()
                                        .map(|condition| {
                                            let dimension = condition.variable.clone();
                                            view! {
                                                <div class="stat w-3/12">
                                                    <div class="stat-title">{dimension}</div>
                                                    <div class="stat-value text-base">
                                                        {condition.value.html_display()}
                                                    </div>
                                                </div>
                                            }
                                        })
                                        .collect_view()}
                                </div>
                                <div class="min-h-[400px] max-h-[60vh] overflow-y-auto">
                                    <OverrideForm
                                        overrides=overrides_rs.get_untracked()
                                        default_config=default_config.get_value()
                                        handle_change=move |new_overrides| {
                                            overrides_ws.set(new_overrides)
                                        }
                                        fn_environment=fn_environment
                                    />
                                </div>
                            </div>
                        </Show>

                        <StepNavigation
                            current_step=current_step
                            on_previous=on_previous
                            on_next=on_next
                            on_submit=on_submit
                            submit_loading=req_inprogress_rs
                        />
                    </div>
                </div>
            </EditorProvider>
        </div>

        {move || match update_request_rws.get() {
            None => ().into_view(),
            Some((ctx_id, update_request)) => {
                view! {
                    <ChangeLogSummary
                        context_id=ctx_id
                        change_type=ChangeType::Update(update_request)
                        on_confirm=on_submit
                        on_close=Callback::new(move |_| update_request_rws.set(None))
                    />
                }
            }
        }}
    }
}

#[component]
fn ChangeLogSummary(
    context_id: String,
    change_type: ChangeType,
    #[prop(into)] on_confirm: Callback<()>,
    #[prop(into)] on_close: Callback<()>,
    #[prop(default = Signal::derive(|| false))] inprogress: Signal<bool>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let context_resource = create_local_resource(
        move || (workspace.get().0, org.get().0, context_id.clone()),
        |(workspace, org, context_id)| async move {
            crate::api::get_context(&context_id, &workspace, &org)
                .await
                .map(|context| {
                    let conditions =
                        Conditions::from_iter(context.value.clone().into_inner());
                    (context, conditions)
                })
        },
    );

    let disabled_rws = RwSignal::new(true);
    let change_type = StoredValue::new(change_type);

    let (title, description, confirm_text) = match change_type.get_value() {
        ChangeType::Update(_) => (
            "Confirm Update",
            "Are you sure you want to update this context?",
            "Yes, Update",
        ),
        ChangeType::Delete => (
            "Confirm Delete",
            "Are you sure you want to delete this context? Action is irreversible.",
            "Yes, Delete",
        ),
    };

    view! {
        <ChangeLogPopup
            title
            description
            confirm_text
            on_confirm
            on_close
            disabled=disabled_rws
            inprogress
        >
            <Suspense fallback=move || {
                view! { <Skeleton variant=SkeletonVariant::Block style_class="h-10".to_string() /> }
            }>
                {
                    Effect::new(move |_| {
                        let context = context_resource.get();
                        if let Some(Ok(_)) = context {
                            disabled_rws.set(false);
                        } else if let Some(Err(e)) = context {
                            logging::error!("Error fetching context: {}", e);
                        }
                    });
                }
                {move || match context_resource.get() {
                    Some(Ok((context, _))) => {
                        let (new_overrides, title, desc) = match change_type.get_value() {
                            ChangeType::Update(update_request) => {
                                (
                                    update_request.override_.clone().into_inner().into(),
                                    "Override changes",
                                    update_request
                                        .description
                                        .unwrap_or_else(|| context.description.clone()),
                                )
                            }
                            ChangeType::Delete => {
                                (Map::new(), "Overrides to be deleted", context.description.clone())
                            }
                        };
                        view! {
                            <crate::components::change_summary::ChangeSummary
                                title
                                old_values=context.override_.clone().into()
                                new_values=new_overrides
                            />
                            <crate::components::change_summary::ChangeSummary
                                title="Other changes"
                                key_column="Property"
                                old_values=Map::from_iter(
                                    vec![
                                        (
                                            "Description".to_string(),
                                            Value::String(context.description.deref().to_string()),
                                        ),
                                    ],
                                )
                                new_values=Map::from_iter(
                                    vec![
                                        (
                                            "Description".to_string(),
                                            Value::String(desc.deref().to_string()),
                                        ),
                                    ],
                                )
                            />
                        }
                            .into_view()
                    }
                    Some(Err(e)) => {
                        logging::error!("Error fetching context: {}", e);
                        view! { <div>Error fetching context</div> }.into_view()
                    }
                    None => view! { <div>Loading...</div> }.into_view(),
                }}
            </Suspense>
        </ChangeLogPopup>
    }
}

/// Component to display override details in a read-only view
#[component]
pub fn OverrideDetailView(
    context: Conditions,
    overrides: Vec<(String, Value)>,
    description: Description,
    override_id: String,
    change_reason: String,
    created_by: String,
    created_at: chrono::DateTime<chrono::Utc>,
    last_modified_by: String,
    last_modified_at: chrono::DateTime<chrono::Utc>,
) -> impl IntoView {
    let override_table_rows = overrides
        .into_iter()
        .map(|(k, v)| {
            Map::from_iter(vec![
                (String::from("KEY"), Value::String(k)),
                (String::from("VALUE"), v),
            ])
        })
        .collect::<Vec<Map<String, Value>>>();

    let table_columns = vec![
        Column::default_no_collapse("KEY".to_string()),
        Column::default("VALUE".to_string()),
    ];

    view! {
        <div class="flex flex-col gap-4">
            <div class="block rounded-lg shadow bg-base-100 p-6 flex flex-col gap-4">
                <div class="flex justify-between items-center">
                    <div class="flex gap-4 items-center">
                        <div class="stat-title">"Override ID"</div>
                        <div class="text-sm font-mono bg-base-200 px-2 py-1 rounded">
                            {override_id}
                        </div>
                    </div>
                </div>
            </div>

            <crate::components::description::ContentDescription
                description=description
                change_reason=ChangeReason::try_from(change_reason).unwrap_or_default()
                created_by=created_by
                created_at=created_at
                last_modified_by=last_modified_by
                last_modified_at=last_modified_at
            />

            <div class="card bg-base-100 shadow">
                <div class="card-body flex flex-row gap-2 flex-wrap">
                    <h3 class="card-title text-base timeline-box text-gray-800 bg-base-100 shadow-md m-0 w-max">
                        "Condition"
                    </h3>
                    <ConditionCollapseProvider>
                        <Condition
                            conditions=context
                            id="override-detail-context"
                            class="h-fit w-[300px]"
                        />
                    </ConditionCollapseProvider>
                </div>
            </div>

            <div class="card bg-base-100 shadow">
                <div class="card-body">
                    <Table rows=override_table_rows key_column="KEY" columns=table_columns />
                </div>
            </div>
        </div>
    }
}
