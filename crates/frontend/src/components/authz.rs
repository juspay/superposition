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
        modal::PortalModal,
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

fn policy_columns(
    delete_click_handler: Callback<Map<String, Value>>,
    authz_scope: AuthzScope,
) -> Vec<Column> {
    let actions_col_formatter = move |_: &str, row: &Map<String, Value>| {
        let domain = row
            .get("domain")
            .and_then(|v| v.as_str())
            .unwrap_or_default()
            .to_string();

        if domain != authz_scope.to_string() {
            ().into_view()
        } else {
            let row = row.clone();
            view! {
                <span
                    class="cursor-pointer"
                    on:click=move |_| delete_click_handler.call(row.clone())
                    title="Delete policy"
                >
                    <i class="ri-delete-bin-6-line ri-xl text-red-600" />
                </span>
            }
            .into_view()
        }
    };

    vec![
        Column::new(
            "sub".to_string(),
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
            "obj".to_string(),
            false,
            default_formatter,
            ColumnSortable::No,
            Expandable::Disabled,
            |_| default_column_formatter("Resource"),
        ),
        Column::new(
            "act".to_string(),
            false,
            default_formatter,
            ColumnSortable::No,
            Expandable::Disabled,
            |_| default_column_formatter("Action"),
        ),
        Column::new(
            "attr".to_string(),
            false,
            default_formatter,
            ColumnSortable::No,
            Expandable::Disabled,
            |_| default_column_formatter("Attribute"),
        ),
        Column::default_with_cell_formatter("actions".to_string(), actions_col_formatter),
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

    let delete_item_rws = RwSignal::new(None as Option<Map<String, Value>>);
    let columns = StoredValue::new(policy_columns(
        Callback::new(move |row| delete_item_rws.set(Some(row))),
        authz_scope.get_value(),
    ));

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
                                ("sub".to_string(), Value::String(sub)),
                                ("domain".to_string(), Value::String(dom)),
                                ("obj".to_string(), Value::String(obj)),
                                ("act".to_string(), Value::String(act)),
                                ("attr".to_string(), Value::String(attr)),
                            ])
                        })
                        .collect()
                })
        },
    );

    let on_delete = Callback::new(move |row: Map<String, Value>| {
        spawn_local(async move {
            let policy_request = match serde_json::from_value(Value::Object(row)) {
                Ok(pr) => pr,
                Err(e) => {
                    logging::error!("Failed to parse policy for deletion: {}", e);
                    enqueue_alert(
                        "Error converting to policy request".to_string(),
                        AlertType::Error,
                        4000,
                    );
                    return;
                }
            };

            let resp =
                casbin::policy::delete(policy_request, authz_scope.get_value()).await;

            match resp {
                Ok(resp) => {
                    policies_resource.refetch();
                    delete_item_rws.set(None);
                    enqueue_alert(resp.message, AlertType::Success, 4000);
                }
                Err(e) => {
                    enqueue_alert(
                        format!("Failed to delete policy: {}", e),
                        AlertType::Error,
                        5000,
                    );
                }
            }
        })
    });

    let add_policy_action = Action::new(move |_| async move {
        let sub = p_sub.get_untracked().trim().to_string();
        let act = p_act.get_untracked();

        let sub = NonEmptyString::try_from(sub)
            .map_err(|_| "Subject is required".to_string())?;
        let Some(act) =
            act.and_then(|s| NonEmptyString::try_from(s.get_name().to_string()).ok())
        else {
            return Err("Action is required".to_string());
        };

        let attr = p_attr.get_untracked().trim().to_string();
        let resp = casbin::policy::add(
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
        enqueue_alert(resp.message, AlertType::Success, 4000);
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
        {move || match delete_item_rws.get() {
            Some(row) => {
                view! {
                    <PortalModal
                        heading="Confirm policy deletion"
                        handle_close=move |_| delete_item_rws.set(None)
                        class="w-full max-w-5xl"
                    >
                        <p>"Are you sure you want to delete this item?"</p>
                        <Table
                            rows=vec![row.clone()]
                            key_column="idx"
                            columns={
                                let mut cols = columns.get_value();
                                cols.pop();
                                cols
                            }
                        />
                        <div class="modal-action">
                            <Button
                                text="Cancel"
                                icon_class="ri-close-line"
                                on_click=move |_| delete_item_rws.set(None)
                            />
                            <Button
                                text="Delete"
                                icon_class="ri-delete-bin-6-line"
                                on_click={
                                    let row = row.clone();
                                    move |_| on_delete.call(row.clone())
                                }
                            />
                        </div>
                    </PortalModal>
                }
            }
            None => ().into_view(),
        }}
    }
}

fn role_policy_columns(
    delete_click_handler: Callback<Map<String, Value>>,
    authz_scope: AuthzScope,
) -> Vec<Column> {
    let actions_col_formatter = move |_: &str, row: &Map<String, Value>| {
        let domain = row
            .get("domain")
            .and_then(|v| v.as_str())
            .unwrap_or_default()
            .to_string();

        if domain != authz_scope.to_string() {
            ().into_view()
        } else {
            let row = row.clone();
            view! {
                <span
                    class="cursor-pointer"
                    on:click=move |_| delete_click_handler.call(row.clone())
                    title="Delete role assignment"
                >
                    <i class="ri-delete-bin-6-line ri-xl text-red-600" />
                </span>
            }
            .into_view()
        }
    };

    vec![
        Column::new(
            "user".to_string(),
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
        Column::default_with_cell_formatter("actions".to_string(), actions_col_formatter),
    ]
}

#[component]
fn RolePolicyViewer(authz_scope: StoredValue<AuthzScope>) -> impl IntoView {
    let g_user = RwSignal::new(String::new());
    let g_role = RwSignal::new(String::new());

    let delete_item_rws = RwSignal::new(None as Option<Map<String, Value>>);
    let columns = StoredValue::new(role_policy_columns(
        Callback::new(move |row| delete_item_rws.set(Some(row))),
        authz_scope.get_value(),
    ));

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
                                ("user".to_string(), Value::String(sub)),
                                ("role".to_string(), Value::String(role)),
                                ("domain".to_string(), Value::String(dom)),
                            ])
                        })
                        .collect()
                })
        },
    );

    let on_delete = Callback::new(move |row: Map<String, Value>| {
        spawn_local(async move {
            let role_request = match serde_json::from_value(Value::Object(row)) {
                Ok(pr) => pr,
                Err(e) => {
                    logging::error!("Failed to parse role for deletion: {}", e);
                    enqueue_alert(
                        "Error converting to role request".to_string(),
                        AlertType::Error,
                        4000,
                    );
                    return;
                }
            };

            let resp = casbin::role::delete(role_request, authz_scope.get_value()).await;

            match resp {
                Ok(resp) => {
                    roles_resource.refetch();
                    delete_item_rws.set(None);
                    enqueue_alert(resp.message, AlertType::Success, 4000);
                }
                Err(e) => {
                    enqueue_alert(
                        format!("Failed to delete role: {}", e),
                        AlertType::Error,
                        5000,
                    );
                }
            }
        })
    });

    let add_role_action = Action::new({
        move |_| async move {
            let user = g_user.get_untracked().trim().to_string();
            let role = g_role.get_untracked().trim().to_string();
            let user = NonEmptyString::try_from(user)
                .map_err(|_| "User is required".to_string())?;
            let role = NonEmptyString::try_from(role)
                .map_err(|_| "Role is required".to_string())?;

            let resp = casbin::role::add(
                GroupingPolicyRequest { user, role },
                authz_scope.get_value(),
            )
            .await?;

            roles_resource.refetch();
            enqueue_alert(resp.message, AlertType::Success, 4000);
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
        {move || match delete_item_rws.get() {
            Some(row) => {
                view! {
                    <PortalModal
                        heading="Confirm role assignment deletion"
                        handle_close=move |_| delete_item_rws.set(None)
                        class="w-full max-w-5xl"
                    >
                        <p>"Are you sure you want to delete this item?"</p>
                        <Table
                            rows=vec![row.clone()]
                            key_column="idx"
                            columns={
                                let mut cols = columns.get_value();
                                cols.pop();
                                cols
                            }
                        />
                        <div class="modal-action">
                            <Button
                                text="Cancel"
                                icon_class="ri-close-line"
                                on_click=move |_| delete_item_rws.set(None)
                            />
                            <Button
                                text="Delete"
                                icon_class="ri-delete-bin-6-line"
                                on_click={
                                    let row = row.clone();
                                    move |_| on_delete.call(row.clone())
                                }
                            />
                        </div>
                    </PortalModal>
                }
            }
            None => ().into_view(),
        }}
    }
}

fn domain_resource_action_group_columns(
    delete_click_handler: Callback<Map<String, Value>>,
    authz_scope: AuthzScope,
) -> Vec<Column> {
    let actions_col_formatter = move |_: &str, row: &Map<String, Value>| {
        let domain = row
            .get("domain")
            .and_then(|v| v.as_str())
            .unwrap_or_default()
            .to_string();

        if domain != authz_scope.to_string() {
            ().into_view()
        } else {
            let row = row.clone();
            view! {
                <span
                    class="cursor-pointer"
                    on:click=move |_| delete_click_handler.call(row.clone())
                    title="Delete action group mapping"
                >
                    <i class="ri-delete-bin-6-line ri-xl text-red-600" />
                </span>
            }
            .into_view()
        }
    };

    vec![
        Column::new(
            "resource".to_string(),
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
            "domain".to_string(),
            false,
            default_formatter,
            ColumnSortable::No,
            Expandable::Disabled,
            |_| default_column_formatter("Domain"),
        ),
        Column::new(
            "action_group".to_string(),
            false,
            default_formatter,
            ColumnSortable::No,
            Expandable::Disabled,
            |_| default_column_formatter("Action Group"),
        ),
        Column::default_with_cell_formatter("actions".to_string(), actions_col_formatter),
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

    let delete_item_rws = RwSignal::new(None as Option<Map<String, Value>>);
    let columns = StoredValue::new(domain_resource_action_group_columns(
        Callback::new(move |row| delete_item_rws.set(Some(row))),
        authz_scope.get_value(),
    ));

    let action_groups_resource = create_blocking_resource(
        move || (),
        move |_| async move {
            casbin::action_group::list_domain(authz_scope.get_value())
                .await
                .map(|res| {
                    res.into_iter()
                        .enumerate()
                        .map(|(idx, r)| {
                            let (res, action) = r
                                .first()
                                .and_then(|s| s.split_once(":"))
                                .map(|(res, action)| {
                                    (res.to_string(), action.to_string())
                                })
                                .unwrap_or_default();
                            let dom = r.get(1).cloned().unwrap_or_default();
                            let group = r.get(2).cloned().unwrap_or_default();
                            Map::from_iter([
                                ("idx".to_string(), Value::String(idx.to_string())),
                                ("resource".to_string(), Value::String(res)),
                                ("action".to_string(), Value::String(action)),
                                ("domain".to_string(), Value::String(dom)),
                                ("action_group".to_string(), Value::String(group)),
                            ])
                        })
                        .collect()
                })
        },
    );

    let on_delete = Callback::new(move |row: Map<String, Value>| {
        spawn_local(async move {
            let action_group_request = match serde_json::from_value(Value::Object(row)) {
                Ok(pr) => pr,
                Err(e) => {
                    logging::error!("Failed to parse action group for deletion: {}", e);
                    enqueue_alert(
                        "Error converting to action group request".to_string(),
                        AlertType::Error,
                        4000,
                    );
                    return;
                }
            };

            let resp = casbin::action_group::delete_domain(
                action_group_request,
                authz_scope.get_value(),
            )
            .await;

            match resp {
                Ok(resp) => {
                    action_groups_resource.refetch();
                    delete_item_rws.set(None);
                    enqueue_alert(resp.message, AlertType::Success, 4000);
                }
                Err(e) => {
                    enqueue_alert(
                        format!("Failed to delete role: {}", e),
                        AlertType::Error,
                        5000,
                    );
                }
            }
        })
    });

    let add_action_group_action = Action::new({
        move |_| async move {
            let action = g2_action.get_untracked();
            let action_group = g2_action_group.get_untracked().trim().to_string();

            let Some(action) = action else {
                return Err("Action is required (e.g. create)".to_string());
            };
            let action_group = NonEmptyString::try_from(action_group)
                .map_err(|_| "Action group is required (e.g. write)".to_string())?;

            let resp = casbin::action_group::add_domain(
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
            enqueue_alert(resp.message, AlertType::Success, 4000);
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
        {move || match delete_item_rws.get() {
            Some(row) => {
                view! {
                    <PortalModal
                        heading="Confirm resource action mapping deletion"
                        handle_close=move |_| delete_item_rws.set(None)
                        class="w-full max-w-5xl"
                    >
                        <p>"Are you sure you want to delete this item?"</p>
                        <Table
                            rows=vec![row.clone()]
                            key_column="idx"
                            columns={
                                let mut cols = columns.get_value();
                                cols.pop();
                                cols
                            }
                        />
                        <div class="modal-action">
                            <Button
                                text="Cancel"
                                icon_class="ri-close-line"
                                on_click=move |_| delete_item_rws.set(None)
                            />
                            <Button
                                text="Delete"
                                icon_class="ri-delete-bin-6-line"
                                on_click={
                                    let row = row.clone();
                                    move |_| on_delete.call(row.clone())
                                }
                            />
                        </div>
                    </PortalModal>
                }
            }
            None => ().into_view(),
        }}
    }
}

fn resource_action_group_columns(
    delete_click_handler: Callback<Map<String, Value>>,
) -> Vec<Column> {
    let actions_col_formatter = move |_: &str, row: &Map<String, Value>| {
        let row = row.clone();
        view! {
            <span
                class="cursor-pointer"
                on:click=move |_| delete_click_handler.call(row.clone())
                title="Delete action group mapping"
            >
                <i class="ri-delete-bin-6-line ri-xl text-red-600" />
            </span>
        }
        .into_view()
    };

    vec![
        Column::new(
            "resource".to_string(),
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
            "action_group".to_string(),
            false,
            default_formatter,
            ColumnSortable::No,
            Expandable::Disabled,
            |_| default_column_formatter("Action Group"),
        ),
        Column::default_with_cell_formatter("actions".to_string(), actions_col_formatter),
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

    let delete_item_rws = RwSignal::new(None as Option<Map<String, Value>>);
    let columns =
        StoredValue::new(resource_action_group_columns(Callback::new(move |row| {
            delete_item_rws.set(Some(row))
        })));

    let action_groups_resource = create_blocking_resource(
        move || (),
        move |_| async move {
            casbin::action_group::list().await.map(|res| {
                res.into_iter()
                    .enumerate()
                    .map(|(idx, r)| {
                        let (res, action) = r
                            .first()
                            .and_then(|s| s.split_once(":"))
                            .map(|(res, action)| (res.to_string(), action.to_string()))
                            .unwrap_or_default();
                        let group = r.get(1).cloned().unwrap_or_default();
                        Map::from_iter([
                            ("idx".to_string(), Value::String(idx.to_string())),
                            ("resource".to_string(), Value::String(res)),
                            ("action".to_string(), Value::String(action)),
                            ("action_group".to_string(), Value::String(group)),
                        ])
                    })
                    .collect()
            })
        },
    );

    let on_delete = Callback::new(move |row: Map<String, Value>| {
        spawn_local(async move {
            let action_group_request = match serde_json::from_value(Value::Object(row)) {
                Ok(pr) => pr,
                Err(e) => {
                    logging::error!("Failed to parse action group for deletion: {}", e);
                    enqueue_alert(
                        "Error converting to action group request".to_string(),
                        AlertType::Error,
                        4000,
                    );
                    return;
                }
            };

            let resp = casbin::action_group::delete(action_group_request).await;

            match resp {
                Ok(resp) => {
                    action_groups_resource.refetch();
                    delete_item_rws.set(None);
                    enqueue_alert(resp.message, AlertType::Success, 4000);
                }
                Err(e) => {
                    enqueue_alert(
                        format!("Failed to delete action group: {}", e),
                        AlertType::Error,
                        5000,
                    );
                }
            }
        })
    });

    let add_action_group_action = Action::new({
        move |_| async move {
            let action = g3_action.get_untracked();
            let action_group = g3_action_group.get_untracked().trim().to_string();

            let Some(action) = action else {
                return Err("Action is required (e.g. create)".to_string());
            };
            let action_group = NonEmptyString::try_from(action_group)
                .map_err(|_| "Action group is required (e.g. write)".to_string())?;

            let resp = casbin::action_group::add(ActionGroupPolicyRequest {
                resource: g3_resource.get_untracked(),
                action: NonEmptyString::try_from(action.get_name().to_string())
                    .map_err(|_| "Invalid action".to_string())?,
                action_group,
            })
            .await?;

            action_groups_resource.refetch();
            resource_action_map_resource.refetch();
            enqueue_alert(resp.message, AlertType::Success, 4000);
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
        {move || match delete_item_rws.get() {
            Some(row) => {
                view! {
                    <PortalModal
                        heading="Confirm resource action mapping deletion"
                        handle_close=move |_| delete_item_rws.set(None)
                        class="w-full max-w-5xl"
                    >
                        <p>"Are you sure you want to delete this item?"</p>
                        <Table
                            rows=vec![row.clone()]
                            key_column="idx"
                            columns={
                                let mut cols = columns.get_value();
                                cols.pop();
                                cols
                            }
                        />
                        <div class="modal-action">
                            <Button
                                text="Cancel"
                                icon_class="ri-close-line"
                                on_click=move |_| delete_item_rws.set(None)
                            />
                            <Button
                                text="Delete"
                                icon_class="ri-delete-bin-6-line"
                                on_click={
                                    let row = row.clone();
                                    move |_| on_delete.call(row.clone())
                                }
                            />
                        </div>
                    </PortalModal>
                }
            }
            None => ().into_view(),
        }}
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
