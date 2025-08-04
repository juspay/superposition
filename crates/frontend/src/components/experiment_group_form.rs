use std::ops::Deref;

use leptos::*;
use serde_json::{json, Map, Number, Value};
use superposition_types::{
    api::{
        dimension::DimensionResponse,
        experiment_groups::{ExpGroupMemberRequest, ExpGroupUpdateRequest},
    },
    database::models::ChangeReason,
};
use web_sys::MouseEvent;

use crate::{
    api::experiment_groups::{add_members, create, fetch, try_update_payload, update},
    components::{
        alert::AlertType,
        button::Button,
        change_form::ChangeForm,
        change_summary::{ChangeLogPopup, ChangeSummary, JsonChangeSummary},
        context_form::ContextForm,
        form::label::Label,
        input::{Input, InputType},
        skeleton::{Skeleton, SkeletonVariant},
    },
    logic::Conditions,
    providers::{alert_provider::enqueue_alert, editor_provider::EditorProvider},
    schema::{JsonSchemaType, SchemaType},
    types::{OrganisationId, Tenant},
};

#[component]
pub fn add_experiment_to_group_form(
    experiment_group_id: i64,
    #[prop(into)] handle_submit: Callback<()>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let members_selected_rws = create_rw_signal(Vec::new());
    let change_reason_rws = create_rw_signal(String::new());
    let loading_rws = create_rw_signal(false);

    let on_submit = move |event: MouseEvent| {
        loading_rws.set(true);
        event.prevent_default();

        let tenant = workspace.get().0;
        let org_id = org.get().0;

        let change_reason =
            match ChangeReason::try_from(change_reason_rws.get_untracked()) {
                Ok(reason) => reason,
                Err(err) => {
                    logging::error!("{}", err);
                    enqueue_alert(err, AlertType::Error, 5000);
                    loading_rws.set(false);
                    return;
                }
            };

        spawn_local({
            async move {
                let payload = ExpGroupMemberRequest {
                    change_reason,
                    member_experiment_ids: members_selected_rws.get_untracked(),
                };

                let result = add_members(
                    &experiment_group_id.to_string(),
                    payload,
                    &tenant,
                    &org_id,
                )
                .await;

                loading_rws.set(false);
                match result {
                    Ok(_) => {
                        handle_submit.call(());
                        let success_message =
                            "Members added to Experiment Group successfully!";
                        enqueue_alert(
                            String::from(success_message),
                            AlertType::Success,
                            5000,
                        );
                    }
                    Err(e) => {
                        logging::error!("Error submitting member management form: {}", e);
                        enqueue_alert(e, AlertType::Error, 5000);
                    }
                }
            }
        });
    };
    view! {
        <EditorProvider>
            <ChangeForm
                title="Reason for Change".to_string()
                placeholder="Enter a reason for this change".to_string()
                value=change_reason_rws.get_untracked()
                on_change=move |new_change_reason| {
                    change_reason_rws.set_untracked(new_change_reason)
                }
            />
            <div class="form-control w-full">
                <Label title="Experiment Group Members" />
                <Input
                    id="experiment_group_members"
                    class="mt-5 rounded-md resize-y w-full max-w-md pt-3"
                    schema_type=SchemaType::Single(JsonSchemaType::Array)
                    value=Value::Array(
                        members_selected_rws
                            .get_untracked()
                            .iter()
                            .map(|id| Value::String(id.to_string()))
                            .collect(),
                    )
                    on_change=Callback::new(move |new_members| {
                        let new_members: Vec<i64> = match new_members {
                            Value::Array(arr) => {
                                arr.iter()
                                    .filter_map(|v| match v {
                                        Value::String(s) => s.parse().ok(),
                                        Value::Number(n) => n.as_i64(),
                                        _ => None,
                                    })
                                    .collect()
                            }
                            _ => {
                                logging::error!("Invalid format for group members");
                                enqueue_alert(
                                    "Invalid format for group members".to_string(),
                                    AlertType::Error,
                                    5000,
                                );
                                return;
                            }
                        };
                        members_selected_rws.set_untracked(new_members)
                    })
                    r#type=InputType::Monaco(vec![])
                />
            </div>

            {move || {
                let loading = loading_rws.get();
                view! {
                    <Button
                        class="self-end h-12 w-48"
                        text="Submit"
                        icon_class="ri-send-plane-line"
                        on_click=on_submit
                        loading
                    />
                }
            }}

        </EditorProvider>
    }
}

enum ResponseType {
    UpdatePrecheck,
    Response,
}

#[component]
pub fn experiment_group_form(
    group_id: String,
    context: Conditions,
    group_name: String,
    group_description: String,
    traffic_percentage: i32,
    dimensions: Vec<DimensionResponse>,
    is_edit: bool,
    #[prop(into)] handle_submit: Callback<()>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let experiment_group_id = StoredValue::new(group_id);
    let (context_rs, context_ws) = create_signal(context);
    let group_name_rws = create_rw_signal(group_name);
    let group_description_rws = create_rw_signal(group_description);
    let change_reason_rws = create_rw_signal(String::new());
    let traffic_percentage_rws = create_rw_signal(traffic_percentage);
    let group_members_rws = create_rw_signal(Vec::new() as Vec<i64>);
    let update_request_rws = RwSignal::new(None);
    let loading_rws = create_rw_signal(false);

    let fn_environment = create_memo(move |_| {
        let context = context_rs.get();
        json!({
            "context": context,
            "overrides": [],
        })
    });

    let on_submit = move |_| {
        loading_rws.set(true);

        let name = group_name_rws.get_untracked();
        let traffic_percentage = traffic_percentage_rws.get_untracked();
        let change_reason = change_reason_rws.get_untracked();
        let description = group_description_rws.get_untracked();
        let workspace = workspace.get_untracked().0;
        let org_id = org.get_untracked().0;
        let members = match group_members_rws.get_untracked() {
            members if members.is_empty() => None,
            members => Some(members),
        };
        let conditions = context_rs.get_untracked();
        let experiment_group_id = experiment_group_id.get_value();

        spawn_local({
            async move {
                let result = match (is_edit, update_request_rws.get_untracked()) {
                    (true, Some((_, update_payload))) => {
                        let future = update(
                            &experiment_group_id,
                            update_payload,
                            &workspace,
                            &org_id,
                        );
                        update_request_rws.set(None);
                        future.await.map(|_| ResponseType::Response)
                    }
                    (true, None) => {
                        let request_payload = try_update_payload(
                            traffic_percentage,
                            description,
                            change_reason,
                        );
                        match request_payload {
                            Ok(payload) => {
                                update_request_rws
                                    .set(Some((experiment_group_id, payload)));
                                Ok(ResponseType::UpdatePrecheck)
                            }
                            Err(e) => Err(e),
                        }
                    }
                    _ => create(
                        name,
                        description,
                        change_reason,
                        traffic_percentage,
                        members,
                        conditions,
                        &workspace,
                        &org_id,
                    )
                    .await
                    .map(|_| ResponseType::Response),
                };

                loading_rws.set(false);
                match result {
                    Ok(ResponseType::UpdatePrecheck) => (),
                    Ok(ResponseType::Response) => {
                        handle_submit.call(());
                        let success_message = if is_edit {
                            "Experiment Group updated successfully!"
                        } else {
                            "New Experiment Group created successfully!"
                        };
                        enqueue_alert(
                            String::from(success_message),
                            AlertType::Success,
                            5000,
                        );
                    }
                    Err(e) => {
                        logging::error!("Error submitting experiment group form: {}", e);
                        enqueue_alert(e, AlertType::Error, 5000);
                    }
                }
            }
        });
    };
    view! {
        <EditorProvider>
            <div class="flex flex-col gap-5">
                <div class="form-control w-full">
                    <Label title="Experiment Group Name" />
                    <input
                        disabled=is_edit
                        value=group_name_rws.get_untracked()
                        on:input=move |ev| group_name_rws.set_untracked(event_target_value(&ev))
                        type="text"
                        name="exp_group_name"
                        id="exp_group_name"
                        placeholder="Enter an experiment group name, eg: Bangalore Region"
                        class="input input-bordered w-full max-w-md"
                    />
                </div>

                <ContextForm
                    dimensions=dimensions
                    context=context_rs.get_untracked()
                    on_context_change=move |new_context| context_ws.set(new_context)
                    disabled=is_edit
                    heading_sub_text="Define rules under which this experiment group would function"
                    fn_environment
                />

                <ChangeForm
                    title="Description".to_string()
                    placeholder="Enter a description".to_string()
                    value=group_description_rws.get_untracked()
                    on_change=move |new_description: String| {
                        group_description_rws.set_untracked(new_description)
                    }
                />
                <ChangeForm
                    title="Reason for Change".to_string()
                    placeholder="Enter a reason for this change".to_string()
                    value=change_reason_rws.get_untracked()
                    on_change=move |new_change_reason| {
                        change_reason_rws.set_untracked(new_change_reason)
                    }
                />

                <div class="form-control w-full">
                    <Label title="Traffic Percentage" />
                    <input
                        value=traffic_percentage_rws.get_untracked().to_string()
                        on:input=move |ev| {
                            let value = event_target_value(&ev);
                            let value: i32 = match value.parse() {
                                Ok(v) => v,
                                Err(_) => {
                                    enqueue_alert(
                                        "Invalid traffic percentage".to_string(),
                                        AlertType::Error,
                                        5000,
                                    );
                                    return;
                                }
                            };
                            traffic_percentage_rws.set_untracked(value);
                        }
                        type="number"
                        name="exp_group_percentage"
                        id="exp_group_percentage"
                        class="input input-bordered w-full max-w-md"
                    />
                </div>

                <Show when=move || !is_edit>
                    <div class="form-control w-full">
                        <Label title="Experiment Group Members" info="(optional)" />
                        <Input
                            id="experiment_group_members"
                            class="w-full max-w-md pt-3 rounded-md resize-y"
                            schema_type=SchemaType::Single(JsonSchemaType::Array)
                            value=Value::Array(
                                group_members_rws
                                    .get_untracked()
                                    .iter()
                                    .map(|id| Value::String(id.to_string()))
                                    .collect(),
                            )
                            on_change=Callback::new(move |new_members| {
                                let new_members: Vec<i64> = match new_members {
                                    Value::Array(arr) => {
                                        arr.iter()
                                            .filter_map(|v| match v {
                                                Value::String(s) => s.parse().ok(),
                                                Value::Number(n) => n.as_i64(),
                                                _ => None,
                                            })
                                            .collect()
                                    }
                                    _ => {
                                        logging::error!("Invalid format for group members");
                                        enqueue_alert(
                                            "Invalid format for group members".to_string(),
                                            AlertType::Error,
                                            5000,
                                        );
                                        return;
                                    }
                                };
                                group_members_rws.set_untracked(new_members)
                            })
                            r#type=InputType::Monaco(vec![])
                        />
                    </div>
                </Show>

                {move || {
                    let loading = loading_rws.get();
                    view! {
                        <Button
                            class="h-12 w-48 self-end"
                            text="Submit"
                            icon_class="ri-send-plane-line"
                            on_click=move |ev: MouseEvent| {
                                ev.prevent_default();
                                on_submit(());
                            }
                            loading
                        />
                    }
                }}

            </div>
        </EditorProvider>
        {move || match update_request_rws.get() {
            None => ().into_view(),
            Some((group_id, update_request)) => {
                view! {
                    <ChangeLogSummary
                        group_id
                        change_type=ChangeType::Update(update_request)
                        on_confirm=on_submit
                        on_close=move |_| update_request_rws.set(None)
                    />
                }
                    .into_view()
            }
        }}
    }
}

#[derive(Clone)]
pub enum ChangeType {
    Delete,
    Update(ExpGroupUpdateRequest),
}

#[component]
pub fn change_log_summary(
    group_id: String,
    change_type: ChangeType,
    #[prop(into)] on_confirm: Callback<()>,
    #[prop(into)] on_close: Callback<()>,
    #[prop(into, default = Signal::derive(|| false))] inprogress: Signal<bool>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let exp_group = create_local_resource(
        move || (group_id.clone(), workspace.get().0, org.get().0),
        |(group_id, workspace, org)| async move { fetch(&group_id, &workspace, &org).await },
    );

    let disabled_rws = RwSignal::new(true);
    let change_type = StoredValue::new(change_type);

    let (title, description, confirm_text) = match change_type.get_value() {
        ChangeType::Update(_) => (
            "Confirm Update",
            "Are you sure you want to update this experiment group?",
            "Yes, Update",
        ),
        ChangeType::Delete => (
            "Confirm Delete",
            "Are you sure you want to delete this experiment group? Action is irreversible.",
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
                        let exp_group = exp_group.get();
                        if let Some(Ok(_)) = exp_group {
                            disabled_rws.set(false);
                        } else if let Some(Err(e)) = exp_group {
                            logging::error!("Error fetching experiment group: {}", e);
                        }
                    });
                }
                {move || match exp_group.get() {
                    Some(Ok(exp_group)) => {
                        let (new_context, new_values) = match change_type.get_value() {
                            ChangeType::Update(update_request) => {
                                let description = update_request
                                    .description
                                    .unwrap_or_else(|| exp_group.description.clone())
                                    .deref()
                                    .to_string();
                                let traffic_percentage = update_request
                                    .traffic_percentage
                                    .unwrap_or(exp_group.traffic_percentage);
                                let val = Map::from_iter(
                                    vec![
                                        ("Description".to_string(), Value::String(description)),
                                        (
                                            "Traffic Percentage".to_string(),
                                            Value::Number(Number::from(*traffic_percentage)),
                                        ),
                                        (
                                            "Members".to_string(),
                                            Value::Array(
                                                exp_group
                                                    .member_experiment_ids
                                                    .iter()
                                                    .map(|id| Value::String(id.to_string()))
                                                    .collect(),
                                            ),
                                        ),
                                    ],
                                );
                                (Some(Value::Object(exp_group.context.clone().into())), val)
                            }
                            ChangeType::Delete => (None, Map::new()),
                        };

                        view! {
                            <ChangeSummary
                                title="Changes"
                                key_column="Property"
                                old_values=Map::from_iter(
                                    vec![
                                        (
                                            "Description".to_string(),
                                            Value::String(exp_group.description.deref().to_string()),
                                        ),
                                        (
                                            "Traffic Percentage".to_string(),
                                            Value::Number(Number::from(*exp_group.traffic_percentage)),
                                        ),
                                        (
                                            "Members".to_string(),
                                            Value::Array(
                                                exp_group
                                                    .member_experiment_ids
                                                    .iter()
                                                    .map(|id| Value::String(id.to_string()))
                                                    .collect(),
                                            ),
                                        ),
                                    ],
                                )
                                new_values
                            />
                            <JsonChangeSummary
                                title="Context changes"
                                old_values=Some(Value::Object(exp_group.context.into()))
                                new_values=new_context
                            />
                        }
                            .into_view()
                    }
                    Some(Err(e)) => {
                        logging::error!("Error fetching experiment group: {}", e);
                        view! { <div>Error fetching experiment group</div> }.into_view()
                    }
                    None => view! { <div>Loading...</div> }.into_view(),
                }}
            </Suspense>
        </ChangeLogPopup>
    }
}
