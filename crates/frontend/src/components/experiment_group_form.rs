use leptos::*;
use serde_json::{json, Value};
use superposition_types::{
    api::{
        experiment_groups::{
            ExpGroupCreateRequest, ExpGroupMemberRequest, ExpGroupUpdateRequest,
        },
        workspace::WorkspaceResponse,
    },
    database::{
        models::{
            experimentation::{ExperimentGroup, TrafficPercentage},
            ChangeReason, Description,
        },
        types::DimensionWithMandatory,
    },
    Condition, Exp,
};
use web_sys::MouseEvent;

use crate::{
    api::experiment_groups::{add_members, create, update},
    components::{
        alert::AlertType,
        button::Button,
        change_form::ChangeForm,
        context_form::ContextForm,
        input::{Input, InputType},
    },
    logic::Conditions,
    providers::{alert_provider::enqueue_alert, editor_provider::EditorProvider},
    schema::{JsonSchemaType, SchemaType},
    types::{OrganisationId, Tenant},
};

#[component]
pub fn add_experiment_to_group_form(
    experiment_group: StoredValue<ExperimentGroup>,
    handle_submit: Callback<()>,
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
                    &experiment_group.get_value().id.to_string(),
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
                <label class="label">
                    <span class="label-text">Experiment Group Members</span>
                </label>
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
                        class="pl-[70px] pr-[70px] w-48 h-12".to_string()
                        text="Submit".to_string()
                        on_click=on_submit
                        loading
                    />
                }
            }}

        </EditorProvider>
    }
}

#[component]
pub fn experiment_group_form(
    group_id: String,
    context: Conditions,
    group_name: String,
    group_description: String,
    traffic_percentage: i32,
    dimensions: Vec<DimensionWithMandatory>,
    is_edit: bool,
    handle_submit: Callback<()>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let experiment_group_id_rws = create_rw_signal(group_id);
    let workspace_settings = use_context::<StoredValue<WorkspaceResponse>>().unwrap();
    let (context_rs, context_ws) = create_signal(context);
    let group_name_rws = create_rw_signal(group_name);
    let group_description_rws = create_rw_signal(group_description);
    let change_reason_rws = create_rw_signal(String::new());
    let traffic_percentage_rws = create_rw_signal(traffic_percentage);
    let group_members_rws = create_rw_signal(Vec::new());
    let loading_rws = create_rw_signal(false);

    let fn_environment = create_memo(move |_| {
        let context = context_rs.get();
        json!({
            "context": context,
            "overrides": [],
        })
    });

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

        let description =
            match Description::try_from(group_description_rws.get_untracked()) {
                Ok(des) => des,
                Err(err) => {
                    logging::error!("{}", err);
                    enqueue_alert(err, AlertType::Error, 5000);
                    loading_rws.set(false);
                    return;
                }
            };

        let traffic_percentage =
            match TrafficPercentage::try_from(traffic_percentage_rws.get_untracked()) {
                Ok(traffic) => traffic,
                Err(err) => {
                    logging::error!("{}", err);
                    enqueue_alert(err, AlertType::Error, 5000);
                    loading_rws.set(false);
                    return;
                }
            };

        spawn_local({
            async move {
                let result = if is_edit {
                    let group_id = experiment_group_id_rws.get_untracked();
                    let update_request = ExpGroupUpdateRequest {
                        change_reason,
                        description: Some(description),
                        traffic_percentage: Some(traffic_percentage),
                    };
                    update(&group_id, update_request, &tenant, &org_id).await
                } else {
                    let members = match group_members_rws.get() {
                        members if members.is_empty() => None,
                        members => Some(members),
                    };
                    let context = match Exp::<Condition>::try_from(
                        context_rs.get().as_context_json(),
                    ) {
                        Ok(context) => context,
                        Err(err) => {
                            logging::error!("Failed to parse context: {}", err);
                            enqueue_alert(
                                format!("Failed to parse context: {}", err),
                                AlertType::Error,
                                5000,
                            );
                            return;
                        }
                    };
                    let create_request = ExpGroupCreateRequest {
                        name: group_name_rws.get(),
                        description,
                        change_reason,
                        context,
                        traffic_percentage,
                        member_experiment_ids: members,
                    };
                    create(create_request, &tenant, &org_id).await
                };

                loading_rws.set(false);
                match result {
                    Ok(_) => {
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
            <div>
                <div class="form-control w-full">
                    <label class="label">
                        <span class="label-text">Experiment Group Name</span>
                    </label>
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

                <div class="my-4">
                    <ContextForm
                        dimensions=dimensions
                        context_rs
                        context_ws
                        handle_change=move |new_context: Conditions| {
                            context_ws.set_untracked(new_context);
                        }
                        resolve_mode=workspace_settings.get_value().strict_mode
                        disabled=is_edit
                        heading_sub_text=String::from(
                            "Define rules under which this experiment group would function",
                        )
                        fn_environment
                    />
                </div>

                <ChangeForm
                    title="Description".to_string()
                    placeholder="Enter a description".to_string()
                    value=String::from(&group_description_rws.get_untracked())
                    on_change=Callback::new(move |new_description: String| {
                        group_description_rws.set_untracked(new_description)
                    })
                />
                <ChangeForm
                    title="Reason for Change".to_string()
                    placeholder="Enter a reason for this change".to_string()
                    value=String::from(&change_reason_rws.get_untracked())
                    on_change=Callback::new(move |new_change_reason| {
                        change_reason_rws.set_untracked(new_change_reason)
                    })
                />

                <div class="form-control w-full">
                    <label class="label">
                        <span class="label-text">Traffic Percentage</span>
                    </label>
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
                        <label class="label">
                            <span class="label-text">Experiment Group Members (Optional)</span>
                        </label>
                        <Input
                            id="experiment_group_members"
                            class="mt-5 rounded-md resize-y w-full max-w-md pt-3"
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
                            class="pl-[70px] pr-[70px] w-48 h-12".to_string()
                            text="Submit".to_string()
                            on_click=on_submit
                            loading
                        />
                    }
                }}

            </div>
        </EditorProvider>
    }
}
