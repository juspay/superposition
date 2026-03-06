use leptos::{Resource as LeptosResource, *};
use serde_json::{Map, Value};
use strum::IntoEnumIterator;
use superposition_types::{
    Resource,
    api::authz::casbin::{
        ActionGroupPolicyRequest, GroupingPolicyRequest, PolicyRequest,
    },
};

use crate::{
    api::casbin::{self, AuthzScope},
    components::{
        alert::AlertType,
        button::Button,
        form::label::Label,
        table::{
            Table,
            types::{
                Column, ColumnSortable, Expandable, default_column_formatter,
                default_formatter,
            },
        },
    },
    providers::alert_provider::enqueue_alert,
};

#[component]
fn TabularViewer(
    resource: LeptosResource<(), Result<Vec<Map<String, Value>>, String>>,
    columns: StoredValue<Vec<Column>>,
) -> impl IntoView {
    view! {
        <Suspense fallback=move || {
            view! { <div>"Loading..."</div> }
        }>
            {move || {
                resource
                    .get()
                    .map(|res| match res {
                        Ok(rows) => {
                            view! { <Table rows key_column="" columns=columns.get_value() /> }
                        }
                        Err(e) => view! { <div class="text-sm text-red-600">{e}</div> }.into_view(),
                    })
                    .unwrap_or_else(|| ().into_view())
            }}
        </Suspense>
    }
}

fn policy_columns() -> Vec<Column> {
    vec![
        Column::new(
            "subject".to_string(),
            false,
            default_formatter,
            ColumnSortable::No,
            Expandable::Disabled,
            |_| default_column_formatter("User / Role"),
        ),
        Column::new(
            "domain".to_string(),
            false,
            default_formatter,
            ColumnSortable::No,
            Expandable::Disabled,
            |_| default_column_formatter("Domain"),
        ),
        Column::new(
            "object".to_string(),
            false,
            default_formatter,
            ColumnSortable::No,
            Expandable::Disabled,
            |_| default_column_formatter("Resource"),
        ),
        Column::new(
            "action".to_string(),
            false,
            default_formatter,
            ColumnSortable::No,
            Expandable::Disabled,
            |_| default_column_formatter("Action"),
        ),
        Column::new(
            "attribute".to_string(),
            false,
            default_formatter,
            ColumnSortable::No,
            Expandable::Disabled,
            |_| default_column_formatter("Attribute"),
        ),
    ]
}

#[component]
fn PolicyViewer(authz_scope: StoredValue<AuthzScope>) -> impl IntoView {
    let p_sub = RwSignal::new(String::new());
    let p_obj = RwSignal::new(Resource::Config);
    let p_act = RwSignal::new(String::new());
    let p_attr = RwSignal::new(String::from("*"));

    let columns = StoredValue::new(policy_columns());

    let policies_resource = create_blocking_resource(
        move || (),
        move |_| async move {
            casbin::list_policies(authz_scope.get_value())
                .await
                .map(|res| {
                    res.into_iter()
                        .map(|r| {
                            let sub = r.first().cloned().unwrap_or_default();
                            let dom = r.get(1).cloned().unwrap_or_default();
                            let obj = r.get(2).cloned().unwrap_or_default();
                            let act = r.get(3).cloned().unwrap_or_default();
                            let attr = r.get(4).cloned().unwrap_or_default();
                            Map::from_iter([
                                ("subject".to_string(), Value::String(sub)),
                                ("domain".to_string(), Value::String(dom)),
                                ("object".to_string(), Value::String(obj)),
                                ("action".to_string(), Value::String(act)),
                                ("attribute".to_string(), Value::String(attr)),
                            ])
                        })
                        .collect()
                })
        },
    );

    let add_policy_action = create_action(move |_| async move {
        let sub = p_sub.get_untracked().trim().to_string();
        let act = p_act.get_untracked().trim().to_string();

        if sub.is_empty() {
            return Err("Subject is required".to_string());
        }
        if act.is_empty() {
            return Err("Action is required".to_string());
        }

        let attr = p_attr.get_untracked().trim().to_string();
        casbin::add_policy(
            PolicyRequest {
                sub,
                obj: p_obj.get_untracked(),
                act,
                attr: Some(if attr.is_empty() {
                    "*".to_string()
                } else {
                    attr
                }),
            },
            authz_scope.get_value(),
        )
        .await?;

        policies_resource.refetch();
        enqueue_alert("Policy added".to_string(), AlertType::Success, 4000);
        Ok(())
    });

    Effect::new(move |_| {
        if let Some(Err(e)) = add_policy_action.value().get() {
            enqueue_alert(e, AlertType::Error, 5000);
        }
    });

    view! {
        <div class="card bg-base-100 shadow">
            <div class="card-body gap-4">
                <h3 class="card-title">"Grant permission"</h3>
                <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
                    <div class="form-control">
                        <Label title="Subject" description="User or role name" />
                        <input
                            class="input input-bordered"
                            value=p_sub.get_untracked()
                            on:input=move |ev| p_sub.set(event_target_value(&ev))
                            placeholder="email_id / admin / qa"
                        />
                    </div>

                    <div class="form-control">
                        <Label title="Resource" />
                        <select
                            class="select select-bordered"
                            on:change=move |ev| {
                                let v = event_target_value(&ev);
                                if let Ok(obj) = Resource::try_from(v.as_str()) {
                                    p_obj.set(obj);
                                }
                            }
                        >
                            {Resource::iter()
                                .map(|r| {
                                    let value = r.to_string();
                                    view! {
                                        <option
                                            value=value.clone()
                                            selected=move || p_obj.get() == r
                                        >
                                            {value}
                                        </option>
                                    }
                                })
                                .collect_view()}
                        </select>
                    </div>

                    <div class="form-control">
                        <Label title="Action" description="Action, e.g. read/write/create" />
                        <input
                            class="input input-bordered"
                            value=p_act.get_untracked()
                            on:input=move |ev| p_act.set(event_target_value(&ev))
                            placeholder="read"
                        />
                    </div>

                    <div class="form-control">
                        <Label
                            title="Attribute"
                            description="Attribute match, e.g. * or key_group.*"
                        />
                        <input
                            class="input input-bordered"
                            value=p_attr.get_untracked()
                            on:input=move |ev| p_attr.set(event_target_value(&ev))
                            placeholder="*"
                        />
                    </div>
                </div>

                <div class="flex gap-2">
                    <Button
                        text="Add"
                        icon_class="ri-add-line"
                        on_click=move |_| add_policy_action.dispatch(())
                    />
                    <Button
                        text="Refresh"
                        icon_class="ri-refresh-line"
                        on_click=move |_| policies_resource.refetch()
                    />
                </div>
                <TabularViewer columns resource=policies_resource />
            </div>
        </div>
    }
}

fn role_policy_columns() -> Vec<Column> {
    vec![
        Column::new(
            "subject".to_string(),
            false,
            default_formatter,
            ColumnSortable::No,
            Expandable::Disabled,
            |_| default_column_formatter("User / Role"),
        ),
        Column::new(
            "role".to_string(),
            false,
            default_formatter,
            ColumnSortable::No,
            Expandable::Disabled,
            |_| default_column_formatter("Role"),
        ),
        Column::new(
            "domain".to_string(),
            false,
            default_formatter,
            ColumnSortable::No,
            Expandable::Disabled,
            |_| default_column_formatter("Domain"),
        ),
    ]
}

#[component]
fn RolePolicyViewer(authz_scope: StoredValue<AuthzScope>) -> impl IntoView {
    let g_user = RwSignal::new(String::new());
    let g_role = RwSignal::new(String::new());

    let columns = StoredValue::new(role_policy_columns());

    let roles_resource = create_blocking_resource(
        move || (),
        move |_| async move {
            casbin::list_roles(authz_scope.get_value())
                .await
                .map(|res| {
                    res.into_iter()
                        .map(|r| {
                            let sub = r.first().cloned().unwrap_or_default();
                            let role = r.get(1).cloned().unwrap_or_default();
                            let dom = r.get(2).cloned().unwrap_or_default();
                            Map::from_iter([
                                ("subject".to_string(), Value::String(sub)),
                                ("role".to_string(), Value::String(role)),
                                ("domain".to_string(), Value::String(dom)),
                            ])
                        })
                        .collect()
                })
        },
    );

    let add_role_action = create_action({
        move |_| async move {
            let user = g_user.get_untracked().trim().to_string();
            let role = g_role.get_untracked().trim().to_string();
            if user.is_empty() {
                return Err("User is required".to_string());
            }
            if role.is_empty() {
                return Err("Role is required".to_string());
            }

            casbin::add_role(
                GroupingPolicyRequest { user, role },
                authz_scope.get_value(),
            )
            .await?;

            roles_resource.refetch();
            enqueue_alert("Role assigned".to_string(), AlertType::Success, 4000);
            Ok(())
        }
    });

    Effect::new(move |_| {
        if let Some(Err(e)) = add_role_action.value().get() {
            enqueue_alert(e, AlertType::Error, 5000);
        }
    });

    view! {
        <div class="card bg-base-100 shadow">
            <div class="card-body gap-4">
                <h3 class="card-title">"Assign role"</h3>
                <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
                    <div class="form-control">
                        <Label title="User or role" />
                        <input
                            class="input input-bordered"
                            on:input=move |ev| g_user.set(event_target_value(&ev))
                            placeholder="username"
                        />
                    </div>
                    <div class="form-control">
                        <Label title="Role" />
                        <input
                            class="input input-bordered"
                            on:input=move |ev| g_role.set(event_target_value(&ev))
                            placeholder="qa / admin / reader"
                        />
                    </div>
                </div>

                <div class="flex gap-2">
                    <Button
                        text="Add"
                        icon_class="ri-add-line"
                        on_click=move |_| add_role_action.dispatch(())
                    />
                    <Button
                        text="Refresh"
                        icon_class="ri-refresh-line"
                        on_click=move |_| roles_resource.refetch()
                    />
                </div>

                <TabularViewer columns resource=roles_resource />
            </div>
        </div>
    }
}

fn domain_resource_action_group_columns() -> Vec<Column> {
    vec![
        Column::new(
            "subject".to_string(),
            false,
            default_formatter,
            ColumnSortable::No,
            Expandable::Disabled,
            |_| default_column_formatter("Resource:Action"),
        ),
        Column::new(
            "domain".to_string(),
            false,
            default_formatter,
            ColumnSortable::No,
            Expandable::Disabled,
            |_| default_column_formatter("Domain"),
        ),
        Column::new(
            "group".to_string(),
            false,
            default_formatter,
            ColumnSortable::No,
            Expandable::Disabled,
            |_| default_column_formatter("Action Group"),
        ),
    ]
}

#[component]
fn DomainResourceActionGroupViewer(
    authz_scope: StoredValue<AuthzScope>,
) -> impl IntoView {
    let g2_resource = RwSignal::new(Resource::DefaultConfig);
    let g2_action = RwSignal::new(String::new());
    let g2_action_group = RwSignal::new(String::new());

    let columns = StoredValue::new(domain_resource_action_group_columns());

    let action_groups_resource = create_blocking_resource(
        move || (),
        move |_| async move {
            casbin::list_domain_action_groups(authz_scope.get_value())
                .await
                .map(|res| {
                    res.into_iter()
                        .map(|r| {
                            let res = r.first().cloned().unwrap_or_default();
                            let dom = r.get(1).cloned().unwrap_or_default();
                            let group = r.get(2).cloned().unwrap_or_default();
                            Map::from_iter([
                                ("subject".to_string(), Value::String(res)),
                                ("domain".to_string(), Value::String(dom)),
                                ("group".to_string(), Value::String(group)),
                            ])
                        })
                        .collect()
                })
        },
    );

    let add_action_group_action = create_action({
        move |_| async move {
            let action = g2_action.get_untracked().trim().to_string();
            let action_group = g2_action_group.get_untracked().trim().to_string();

            if action.is_empty() {
                return Err("Action is required (e.g. create)".to_string());
            }
            if action_group.is_empty() {
                return Err("Action group is required (e.g. write)".to_string());
            }

            casbin::add_domain_action_group(
                ActionGroupPolicyRequest {
                    resource: g2_resource.get_untracked(),
                    action,
                    action_group,
                },
                authz_scope.get_value(),
            )
            .await?;

            action_groups_resource.refetch();
            enqueue_alert(
                "Action group mapping added".to_string(),
                AlertType::Success,
                4000,
            );
            Ok(())
        }
    });

    Effect::new(move |_| {
        if let Some(Err(e)) = add_action_group_action.value().get() {
            enqueue_alert(e, AlertType::Error, 5000);
        }
    });

    view! {
        <div class="card bg-base-100 shadow">
            <div class="card-body gap-4">
                <h3 class="card-title">"Action group mapping"</h3>
                <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
                    <div class="form-control">
                        <Label title="Resource" description="Resource part of resource:action" />
                        <select
                            class="select select-bordered"
                            on:change=move |ev| {
                                let v = event_target_value(&ev);
                                if let Ok(obj) = Resource::try_from(v.as_str()) {
                                    g2_resource.set(obj);
                                }
                            }
                        >
                            {Resource::iter()
                                .map(|r| {
                                    let value = r.to_string();
                                    view! {
                                        <option
                                            value=value.clone()
                                            selected=move || g2_resource.get() == r
                                        >
                                            {value}
                                        </option>
                                    }
                                })
                                .collect_view()}
                        </select>
                    </div>
                    <div class="form-control">
                        <Label
                            title="Action"
                            description="Action part of resource:action (e.g. get)"
                        />
                        <input
                            class="input input-bordered"
                            on:input=move |ev| g2_action.set(event_target_value(&ev))
                            placeholder="get"
                        />
                    </div>
                    <div class="form-control">
                        <Label
                            title="Action group"
                            description="Group for the resource:action (e.g. write)"
                        />
                        <input
                            class="input input-bordered"
                            on:input=move |ev| g2_action_group.set(event_target_value(&ev))
                            placeholder="write"
                        />
                    </div>
                </div>

                <div class="flex gap-2">
                    <Button
                        text="Add"
                        icon_class="ri-add-line"
                        on_click=move |_| add_action_group_action.dispatch(())
                    />
                    <Button
                        text="Refresh"
                        icon_class="ri-refresh-line"
                        on_click=move |_| action_groups_resource.refetch()
                    />
                </div>

                <TabularViewer columns resource=action_groups_resource />
            </div>
        </div>
    }
}

fn resource_action_group_columns() -> Vec<Column> {
    vec![
        Column::new(
            "subject".to_string(),
            false,
            default_formatter,
            ColumnSortable::No,
            Expandable::Disabled,
            |_| default_column_formatter("Resource:Action"),
        ),
        Column::new(
            "group".to_string(),
            false,
            default_formatter,
            ColumnSortable::No,
            Expandable::Disabled,
            |_| default_column_formatter("Action Group"),
        ),
    ]
}

#[component]
fn ResourceActionGroupViewer() -> impl IntoView {
    let g2_resource = RwSignal::new(Resource::DefaultConfig);
    let g2_action = RwSignal::new(String::new());
    let g2_action_group = RwSignal::new(String::new());

    let columns = StoredValue::new(resource_action_group_columns());

    let action_groups_resource = create_blocking_resource(
        move || (),
        move |_| async move {
            casbin::list_action_groups().await.map(|res| {
                res.into_iter()
                    .map(|r| {
                        let res = r.first().cloned().unwrap_or_default();
                        let group = r.get(1).cloned().unwrap_or_default();
                        Map::from_iter([
                            ("subject".to_string(), Value::String(res)),
                            ("group".to_string(), Value::String(group)),
                        ])
                    })
                    .collect()
            })
        },
    );

    let add_action_group_action = create_action({
        move |_| async move {
            let action = g2_action.get_untracked().trim().to_string();
            let action_group = g2_action_group.get_untracked().trim().to_string();

            if action.is_empty() {
                return Err("Action is required (e.g. create)".to_string());
            }
            if action_group.is_empty() {
                return Err("Action group is required (e.g. write)".to_string());
            }

            casbin::add_action_group(ActionGroupPolicyRequest {
                resource: g2_resource.get_untracked(),
                action,
                action_group,
            })
            .await?;

            action_groups_resource.refetch();
            enqueue_alert(
                "Action group mapping added".to_string(),
                AlertType::Success,
                4000,
            );
            Ok(())
        }
    });

    Effect::new(move |_| {
        if let Some(Err(e)) = add_action_group_action.value().get() {
            enqueue_alert(e, AlertType::Error, 5000);
        }
    });

    view! {
        <div class="card bg-base-100 shadow">
            <div class="card-body gap-4">
                <h3 class="card-title">"Action group mapping"</h3>
                <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
                    <div class="form-control">
                        <Label title="Resource" description="Resource part of resource:action" />
                        <select
                            class="select select-bordered"
                            on:change=move |ev| {
                                let v = event_target_value(&ev);
                                if let Ok(obj) = Resource::try_from(v.as_str()) {
                                    g2_resource.set(obj);
                                }
                            }
                        >
                            {Resource::iter()
                                .map(|r| {
                                    let value = r.to_string();
                                    view! {
                                        <option
                                            value=value.clone()
                                            selected=move || g2_resource.get() == r
                                        >
                                            {value}
                                        </option>
                                    }
                                })
                                .collect_view()}
                        </select>
                    </div>
                    <div class="form-control">
                        <Label
                            title="Action"
                            description="Action part of resource:action (e.g. create)"
                        />
                        <input
                            class="input input-bordered"
                            on:input=move |ev| g2_action.set(event_target_value(&ev))
                            placeholder="create"
                        />
                    </div>
                    <div class="form-control">
                        <Label
                            title="Action group"
                            description="Group for the resource:action (e.g. write)"
                        />
                        <input
                            class="input input-bordered"
                            on:input=move |ev| g2_action_group.set(event_target_value(&ev))
                            placeholder="write"
                        />
                    </div>
                </div>

                <div class="flex gap-2">
                    <Button
                        text="Add"
                        icon_class="ri-add-line"
                        on_click=move |_| add_action_group_action.dispatch(())
                    />
                    <Button
                        text="Refresh"
                        icon_class="ri-refresh-line"
                        on_click=move |_| action_groups_resource.refetch()
                    />
                </div>

                <TabularViewer columns resource=action_groups_resource />
            </div>
        </div>
    }
}

#[component]
pub fn AuthzEditor(
    #[prop(into)] title: String,
    #[prop(into)] description: String,
    authz_scope: AuthzScope,
) -> impl IntoView {
    let authz_scope = StoredValue::new(authz_scope);

    view! {
        <div class="flex flex-col gap-6">
            <div class="card bg-base-100 shadow">
                <div class="card-body gap-3">
                    <h2 class="card-title">{title}</h2>
                    <p class="text-sm text-gray-600">{description}</p>
                </div>
            </div>

            <div class="grid grid-cols-1 gap-6">
                <PolicyViewer authz_scope />
                <RolePolicyViewer authz_scope />
                {match authz_scope.get_value() {
                    AuthzScope::Admin => view! { <ResourceActionGroupViewer /> },
                    AuthzScope::Org(_) | AuthzScope::Workspace(_, _) => {
                        view! { <DomainResourceActionGroupViewer authz_scope /> }
                    }
                }}
            </div>
        </div>
    }
}
