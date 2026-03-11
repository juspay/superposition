use std::collections::HashMap;

use leptos::{Resource as LeptosResource, *};
use serde_json::{Map, Value};
use strum::IntoEnumIterator;
use superposition_types::{
    Resource,
    api::authz::{
        ResourceActionType,
        casbin::{ActionGroupPolicyRequest, GroupingPolicyRequest, PolicyRequest},
    },
    database::models::NonEmptyString,
};

use crate::{
    api::casbin::{self, AuthzScope},
    components::{
        alert::AlertType,
        button::Button,
        dropdown::{Dropdown, DropdownBtnType, DropdownDirection, utils::DropdownOption},
        form::label::Label,
        skeleton::{Skeleton, SkeletonVariant},
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
                            view! { <Table rows key_column="idx" columns=columns.get_value() /> }
                        }
                        Err(e) => view! { <div class="text-sm text-red-600">{e}</div> }.into_view(),
                    })
                    .unwrap_or_else(|| ().into_view())
            }}
        </Suspense>
    }
}

impl DropdownOption for ResourceActionType {
    fn key(&self) -> String {
        self.get_name().to_string()
    }
    fn label(&self) -> String {
        match self {
            Self::Action(action) => action.to_string(),
            Self::Group(group) => format!("{} (group)", group),
        }
    }
}

#[component]
fn ActionDropdown(
    resource_action_map_resource: LeptosResource<
        (),
        HashMap<Resource, Vec<ResourceActionType>>,
    >,
    action_rws: RwSignal<Option<ResourceActionType>>,
    #[prop(into)] resource_rs: Signal<Resource>,
    #[prop(default = false)] allow_group: bool,
) -> impl IntoView {
    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::Block style_class="h-10 w-[28rem]" /> }
        }>
            {move || {
                let dropdown_options = resource_action_map_resource
                    .get()
                    .unwrap_or_default()
                    .get(&resource_rs.get())
                    .unwrap_or(&vec![])
                    .to_owned()
                    .into_iter()
                    .filter(|action| {
                        allow_group || !matches!(action, ResourceActionType::Group(_))
                    })
                    .collect::<Vec<ResourceActionType>>();
                view! {
                    <Dropdown
                        dropdown_width="w-80"
                        dropdown_text=action_rws
                            .get()
                            .map(|act| act.get_name().to_string())
                            .unwrap_or_default()
                        dropdown_direction=DropdownDirection::Down
                        dropdown_btn_type=DropdownBtnType::Select
                        dropdown_options
                        on_select=move |selected: ResourceActionType| {
                            action_rws.set(Some(selected));
                        }
                    />
                }
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

impl DropdownOption for Resource {
    fn key(&self) -> String {
        self.to_string()
    }
    fn label(&self) -> String {
        self.to_string()
    }
}

#[component]
fn PolicyViewer(
    authz_scope: StoredValue<AuthzScope>,
    resource_action_map_resource: LeptosResource<
        (),
        HashMap<Resource, Vec<ResourceActionType>>,
    >,
) -> impl IntoView {
    let p_sub = RwSignal::new(String::new());
    let p_obj = RwSignal::new(Resource::Config);
    let p_act = RwSignal::new(None as Option<ResourceActionType>);
    let p_attr = RwSignal::new(String::from("*"));

    let columns = StoredValue::new(policy_columns());

    let policies_resource = create_blocking_resource(
        move || (),
        move |_| async move {
            casbin::policy::list(authz_scope.get_value())
                .await
                .map(|res| {
                    res.into_iter()
                        .enumerate()
                        .map(|(idx, r)| {
                            let sub = r.first().cloned().unwrap_or_default();
                            let dom = r.get(1).cloned().unwrap_or_default();
                            let obj = r.get(2).cloned().unwrap_or_default();
                            let act = r.get(3).cloned().unwrap_or_default();
                            let attr = r.get(4).cloned().unwrap_or_default();
                            Map::from_iter([
                                ("idx".to_string(), Value::String(idx.to_string())),
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
        let act = p_act.get_untracked();

        let sub = NonEmptyString::try_from(sub)
            .map_err(|_| "Subject is required".to_string())?;
        let Some(act) = act else {
            return Err("Action is required".to_string());
        };

        let attr = p_attr.get_untracked().trim().to_string();
        casbin::policy::add(
            PolicyRequest {
                sub,
                obj: p_obj.get_untracked(),
                act,
                attr: Some(
                    NonEmptyString::try_from(attr)
                        .or_else(|_| NonEmptyString::try_from("*".to_string()))?,
                ),
            },
            authz_scope.get_value(),
        )
        .await?;

        policies_resource.refetch();
        resource_action_map_resource.refetch();
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
                <div class="flex flex-wrap gap-4">
                    <div class="form-control">
                        <Label title="Subject" description="User or role name" />
                        <input
                            class="input input-bordered w-[28rem]"
                            value=p_sub.get_untracked()
                            on:input=move |ev| p_sub.set(event_target_value(&ev))
                            placeholder="email_id / admin / qa"
                        />
                    </div>

                    <div class="form-control">
                        <Label title="Resource" description="Resource to manage" />
                        <Dropdown
                            dropdown_width="w-80"
                            dropdown_text=p_obj.get()
                            dropdown_direction=DropdownDirection::Down
                            dropdown_btn_type=DropdownBtnType::Select
                            dropdown_options=Resource::iter().collect()
                            on_select=move |selected: Resource| p_obj.set(selected)
                        />
                    </div>

                    <div class="form-control">
                        <Label
                            title="Action or Action Group"
                            description="Custom action groups can also be used"
                        />
                        <ActionDropdown
                            resource_action_map_resource
                            action_rws=p_act
                            resource_rs=p_obj
                            allow_group=true
                        />
                    </div>

                    <div class="form-control">
                        <Label
                            title="Attribute"
                            description="Attribute match, e.g. * or key_group.*"
                        />
                        <input
                            class="input input-bordered w-[28rem]"
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
                        on_click=move |_| {
                            resource_action_map_resource.refetch();
                            policies_resource.refetch();
                        }
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
            casbin::role::list(authz_scope.get_value())
                .await
                .map(|res| {
                    res.into_iter()
                        .enumerate()
                        .map(|(idx, r)| {
                            let sub = r.first().cloned().unwrap_or_default();
                            let role = r.get(1).cloned().unwrap_or_default();
                            let dom = r.get(2).cloned().unwrap_or_default();
                            Map::from_iter([
                                ("idx".to_string(), Value::String(idx.to_string())),
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
            let user = NonEmptyString::try_from(user)
                .map_err(|_| "User is required".to_string())?;
            let role = NonEmptyString::try_from(role)
                .map_err(|_| "Role is required".to_string())?;

            casbin::role::add(
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
                <div class="flex flex-wrap gap-4">
                    <div class="form-control">
                        <Label title="Subject" description="User or role" />
                        <input
                            class="input input-bordered w-[28rem]"
                            on:input=move |ev| g_user.set(event_target_value(&ev))
                            placeholder="email_id / existing_role"
                        />
                    </div>
                    <div class="form-control">
                        <Label
                            title="Role"
                            description="Using a new value will create a new role"
                        />
                        <input
                            class="input input-bordered w-[28rem]"
                            on:input=move |ev| g_role.set(event_target_value(&ev))
                            placeholder="context_reader / config_reader / new_role"
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
    resource_action_map_resource: LeptosResource<
        (),
        HashMap<Resource, Vec<ResourceActionType>>,
    >,
) -> impl IntoView {
    let g2_resource = RwSignal::new(Resource::DefaultConfig);
    let g2_action = RwSignal::new(None as Option<ResourceActionType>);
    let g2_action_group = RwSignal::new(String::new());

    let columns = StoredValue::new(domain_resource_action_group_columns());

    let action_groups_resource = create_blocking_resource(
        move || (),
        move |_| async move {
            casbin::action_group::list_domain(authz_scope.get_value())
                .await
                .map(|res| {
                    res.into_iter()
                        .enumerate()
                        .map(|(idx, r)| {
                            let res = r.first().cloned().unwrap_or_default();
                            let dom = r.get(1).cloned().unwrap_or_default();
                            let group = r.get(2).cloned().unwrap_or_default();
                            Map::from_iter([
                                ("idx".to_string(), Value::String(idx.to_string())),
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
            let action = g2_action.get_untracked();
            let action_group = g2_action_group.get_untracked().trim().to_string();

            let Some(action) = action else {
                return Err("Action is required (e.g. create)".to_string());
            };
            let action_group = NonEmptyString::try_from(action_group)
                .map_err(|_| "Action group is required (e.g. write)".to_string())?;

            casbin::action_group::add_domain(
                ActionGroupPolicyRequest {
                    resource: g2_resource.get_untracked(),
                    action: NonEmptyString::try_from(action.get_name().to_string())
                        .map_err(|_| "Invalid action".to_string())?,
                    action_group,
                },
                authz_scope.get_value(),
            )
            .await?;

            action_groups_resource.refetch();
            resource_action_map_resource.refetch();
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
                <div class="flex flex-wrap gap-4">
                    <div class="form-control">
                        <Label title="Resource" description="Resource part of resource:action" />
                        <Dropdown
                            dropdown_width="w-80"
                            dropdown_text=g2_resource.get()
                            dropdown_direction=DropdownDirection::Down
                            dropdown_btn_type=DropdownBtnType::Select
                            dropdown_options=Resource::iter().collect()
                            on_select=move |selected: Resource| g2_resource.set(selected)
                        />
                    </div>
                    <div class="form-control">
                        <Label
                            title="Action"
                            description="Action part of resource:action (e.g. get)"
                        />
                        <ActionDropdown
                            resource_action_map_resource
                            action_rws=g2_action
                            resource_rs=g2_resource
                        />
                    </div>
                    <div class="form-control">
                        <Label
                            title="Action group"
                            description="Group for the resource:action (e.g. write)"
                        />
                        <input
                            class="input input-bordered w-[28rem]"
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
                        on_click=move |_| {
                            action_groups_resource.refetch();
                            resource_action_map_resource.refetch();
                        }
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
fn ResourceActionGroupViewer(
    resource_action_map_resource: LeptosResource<
        (),
        HashMap<Resource, Vec<ResourceActionType>>,
    >,
) -> impl IntoView {
    let g3_resource = RwSignal::new(Resource::DefaultConfig);
    let g3_action = RwSignal::new(None as Option<ResourceActionType>);
    let g3_action_group = RwSignal::new(String::new());

    let columns = StoredValue::new(resource_action_group_columns());

    let action_groups_resource = create_blocking_resource(
        move || (),
        move |_| async move {
            casbin::action_group::list().await.map(|res| {
                res.into_iter()
                    .enumerate()
                    .map(|(idx, r)| {
                        let res = r.first().cloned().unwrap_or_default();
                        let group = r.get(1).cloned().unwrap_or_default();
                        Map::from_iter([
                            ("idx".to_string(), Value::String(idx.to_string())),
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
            let action = g3_action.get_untracked();
            let action_group = g3_action_group.get_untracked().trim().to_string();

            let Some(action) = action else {
                return Err("Action is required (e.g. create)".to_string());
            };
            let action_group = NonEmptyString::try_from(action_group)
                .map_err(|_| "Action group is required (e.g. write)".to_string())?;

            casbin::action_group::add(ActionGroupPolicyRequest {
                resource: g3_resource.get_untracked(),
                action: NonEmptyString::try_from(action.get_name().to_string())
                    .map_err(|_| "Invalid action".to_string())?,
                action_group,
            })
            .await?;

            action_groups_resource.refetch();
            resource_action_map_resource.refetch();
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
                <div class="flex flex-wrap gap-4">
                    <div class="form-control">
                        <Label title="Resource" description="Resource part of resource:action" />
                        <Dropdown
                            dropdown_width="w-80"
                            dropdown_text=g3_resource.get()
                            dropdown_direction=DropdownDirection::Down
                            dropdown_btn_type=DropdownBtnType::Select
                            dropdown_options=Resource::iter().collect()
                            on_select=move |selected: Resource| g3_resource.set(selected)
                        />
                    </div>
                    <div class="form-control">
                        <Label title="Action" description="Custom action groups can also be used" />
                        <ActionDropdown
                            resource_action_map_resource
                            action_rws=g3_action
                            resource_rs=g3_resource
                        />
                    </div>
                    <div class="form-control">
                        <Label
                            title="Action group"
                            description="Group for the resource:action (e.g. write)"
                        />
                        <input
                            class="input input-bordered w-[28rem]"
                            on:input=move |ev| g3_action_group.set(event_target_value(&ev))
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
                        on_click=move |_| {
                            action_groups_resource.refetch();
                            resource_action_map_resource.refetch();
                        }
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

    let resource_action_map_resource = create_blocking_resource(
        move || (),
        move |_| async move {
            casbin::get_resource_action_map(authz_scope.get_value())
                .await
                .unwrap_or_default()
        },
    );

    view! {
        <div class="flex flex-col gap-6">
            <div class="card bg-base-100 shadow">
                <div class="card-body gap-3">
                    <h2 class="card-title">{title}</h2>
                    <p class="text-sm text-gray-600">{description}</p>
                </div>
            </div>

            <div class="flex flex-col gap-6">
                <PolicyViewer authz_scope resource_action_map_resource />
                <RolePolicyViewer authz_scope />
                {match authz_scope.get_value() {
                    AuthzScope::Admin => {
                        view! { <ResourceActionGroupViewer resource_action_map_resource /> }
                    }
                    AuthzScope::Org(_) | AuthzScope::Workspace(_, _) => {
                        view! {
                            <Show when=move || false>
                                <DomainResourceActionGroupViewer
                                    resource_action_map_resource
                                    authz_scope
                                />
                            </Show>
                        }
                    }
                }}
            </div>
        </div>
    }
}
