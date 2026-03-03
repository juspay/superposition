use leptos::*;
use leptos_router::A;
use strum::IntoEnumIterator;
use superposition_types::{
    Resource,
    api::authz::casbin::{
        ActionGroupPolicyRequest, GroupingPolicyRequest, PolicyRequest,
    },
    custom_query::PaginationParams,
};

use crate::{
    api::casbin,
    api::workspaces,
    components::{alert::AlertType, button::Button},
    providers::alert_provider::enqueue_alert,
    types::{OrganisationId, Workspace},
    utils::use_url_base,
};

fn field_label(title: &'static str, description: &'static str) -> impl IntoView {
    view! {
        <label class="label flex flex-col items-start justify-center">
            <div class="flex items-center gap-1 label-text font-semibold text-base">{title}</div>
            <span class="label-text text-slate-400">{description}</span>
        </label>
    }
}

fn authz_body(
    workspace: Signal<Workspace>,
    org: Signal<OrganisationId>,
) -> impl IntoView {
    let base = use_url_base();
    let raw_link = Signal::derive(move || {
        let workspace = workspace.get().0;
        if workspace.is_empty() {
            return "".to_string();
        }

        format!("{base}/admin/{}/{}/authz/raw", org.get().0, workspace)
    });

    // ---- Form state: Policy (p)
    let p_sub = RwSignal::new(String::new());
    let p_obj = RwSignal::new(Resource::Config);
    let p_act = RwSignal::new(String::new());
    let p_attr = RwSignal::new(String::from("*"));

    // ---- Form state: Role assignment (g)
    let g_user = RwSignal::new(String::new());
    let g_role = RwSignal::new(String::new());

    // ---- Form state: Action group (g2)
    let g2_resource = RwSignal::new(Resource::DefaultConfig);
    let g2_action = RwSignal::new(String::new());
    let g2_action_group = RwSignal::new(String::new());

    let refresh_policies = RwSignal::new(0u64);
    let refresh_roles = RwSignal::new(0u64);
    let refresh_action_groups = RwSignal::new(0u64);

    let policies_resource = create_blocking_resource(
        move || (workspace.get().0, org.get().0, refresh_policies.get()),
        |(workspace, org, _)| async move {
            if workspace.is_empty() {
                return Ok(Vec::new());
            }
            casbin::list_policies(&workspace, &org).await
        },
    );

    let roles_resource = create_blocking_resource(
        move || (workspace.get().0, org.get().0, refresh_roles.get()),
        |(workspace, org, _)| async move {
            if workspace.is_empty() {
                return Ok(Vec::new());
            }
            casbin::list_roles(&workspace, &org).await
        },
    );

    let action_groups_resource = create_blocking_resource(
        move || (workspace.get().0, org.get().0, refresh_action_groups.get()),
        |(workspace, org, _)| async move {
            if workspace.is_empty() {
                return Ok(Vec::new());
            }
            casbin::list_action_groups(&workspace, &org).await
        },
    );

    let add_policy_action = create_action(move |_| async move {
        let workspace = workspace.get_untracked().0;
        if workspace.is_empty() {
            return Err("Select a workspace".to_string());
        }

        let sub = p_sub.get_untracked().trim().to_string();
        let act = p_act.get_untracked().trim().to_string();

        if sub.is_empty() {
            return Err("Subject (sub) is required".to_string());
        }
        if act.is_empty() {
            return Err("Action (act) is required".to_string());
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
            &workspace,
            &org.get_untracked().0,
        )
        .await?;

        refresh_policies.update(|v| *v += 1);
        enqueue_alert("Policy added".to_string(), AlertType::Success, 4000);
        Ok(())
    });

    let add_role_action = create_action(move |_| async move {
        let workspace = workspace.get_untracked().0;
        if workspace.is_empty() {
            return Err("Select a workspace".to_string());
        }

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
            &workspace,
            &org.get_untracked().0,
        )
        .await?;

        refresh_roles.update(|v| *v += 1);
        enqueue_alert("Role assigned".to_string(), AlertType::Success, 4000);
        Ok(())
    });

    let add_action_group_action = create_action(move |_| async move {
        let workspace = workspace.get_untracked().0;
        if workspace.is_empty() {
            return Err("Select a workspace".to_string());
        }

        let action = g2_action.get_untracked().trim().to_string();
        let action_group = g2_action_group.get_untracked().trim().to_string();

        if action.is_empty() {
            return Err("Action is required (e.g. create)".to_string());
        }
        if action_group.is_empty() {
            return Err("Action group is required (e.g. write)".to_string());
        }

        casbin::add_action_group(
            ActionGroupPolicyRequest {
                resource: g2_resource.get_untracked(),
                action,
                action_group,
            },
            &workspace,
            &org.get_untracked().0,
        )
        .await?;

        refresh_action_groups.update(|v| *v += 1);
        enqueue_alert(
            "Action group mapping added".to_string(),
            AlertType::Success,
            4000,
        );
        Ok(())
    });

    create_effect(move |_| {
        if let Some(Err(e)) = add_policy_action.value().get() {
            enqueue_alert(e, AlertType::Error, 5000);
        }
    });

    create_effect(move |_| {
        if let Some(Err(e)) = add_role_action.value().get() {
            enqueue_alert(e, AlertType::Error, 5000);
        }
    });

    create_effect(move |_| {
        if let Some(Err(e)) = add_action_group_action.value().get() {
            enqueue_alert(e, AlertType::Error, 5000);
        }
    });

    view! {
        <div class="flex flex-col gap-6">
            <div class="card bg-base-100 shadow">
                <div class="card-body gap-3">
                    <div class="flex items-center justify-between gap-4">
                        <h2 class="card-title">"Authorization"</h2>
                        <Show when=move || !workspace.get().0.is_empty() fallback=|| view! {}>
                            <A class="link link-primary text-sm" href=move || raw_link.get()>
                                "Open raw editor"
                            </A>
                        </Show>
                    </div>
                    <p class="text-sm text-gray-600">
                        "Manage permissions, role assignments, and action-groups for this workspace."
                    </p>
                </div>
            </div>

            <Show
                when=move || !workspace.get().0.is_empty()
                fallback=move || {
                    view! {
                        <div class="card bg-base-100 shadow">
                            <div class="card-body">
                                <div class="text-sm text-gray-600">"Select a workspace to manage AuthZ."</div>
                            </div>
                        </div>
                    }
                }
            >

            <div class="grid grid-cols-1 gap-6">
                <div class="card bg-base-100 shadow">
                    <div class="card-body gap-4">
                        <h3 class="card-title">"Grant permission"</h3>
                        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
                            <div class="form-control">
                                {field_label("Subject", "User or role name (sub)")}
                                <input
                                    class="input input-bordered"
                                    value=p_sub.get_untracked()
                                    on:input=move |ev| p_sub.set(event_target_value(&ev))
                                    placeholder="username / admin / qa"
                                />
                            </div>

                            <div class="form-control">
                                {field_label("Object", "Resource (obj)")}
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
                                            view! { <option value=value.clone() selected=move || p_obj.get() == r>{value}</option> }
                                        })
                                        .collect_view()}
                                </select>
                            </div>

                            <div class="form-control">
                                {field_label("Action", "Action (act), e.g. read/write/create")}
                                <input
                                    class="input input-bordered"
                                    value=p_act.get_untracked()
                                    on:input=move |ev| p_act.set(event_target_value(&ev))
                                    placeholder="read"
                                />
                            </div>

                            <div class="form-control">
                                {field_label("Attribute", "Attribute match (attr), e.g. * or key_group.*")}
                                <input
                                    class="input input-bordered"
                                    value=p_attr.get_untracked()
                                    on:input=move |ev| p_attr.set(event_target_value(&ev))
                                    placeholder="*"
                                />
                            </div>
                        </div>

                        <div class="flex gap-2">
                            <Button text="Add".to_string() on_click=Callback::new(move |_| add_policy_action.dispatch(())) />
                            <Button
                                text="Refresh".to_string()
                                on_click=Callback::new(move |_| refresh_policies.update(|v| *v += 1))
                            />
                        </div>

                        <Suspense fallback=move || view! { <div>"Loading..."</div> }>
                            {move || {
                                policies_resource.get().map(|res| match res {
                                    Ok(rows) => view! {
                                        <div class="overflow-auto">
                                            <table class="table table-zebra">
                                                <thead>
                                                    <tr>
                                                        <th>"Subject"</th>
                                                        <th>"Domain"</th>
                                                        <th>"Object"</th>
                                                        <th>"Action"</th>
                                                        <th>"Attribute"</th>
                                                    </tr>
                                                </thead>
                                                <tbody>
                                                    {rows.into_iter().map(|r| {
                                                        let sub = r.first().cloned().unwrap_or_default();
                                                        let dom = r.get(1).cloned().unwrap_or_default();
                                                        let obj = r.get(2).cloned().unwrap_or_default();
                                                        let act = r.get(3).cloned().unwrap_or_default();
                                                        let attr = r.get(4).cloned().unwrap_or_default();
                                                        view! { <tr><td>{sub}</td><td>{dom}</td><td>{obj}</td><td>{act}</td><td>{attr}</td></tr> }
                                                    }).collect_view()}
                                                </tbody>
                                            </table>
                                        </div>
                                    }.into_view(),
                                    Err(e) => {
                                        view! { <div class="text-sm text-red-600">{e}</div> }.into_view()
                                    }
                                }).unwrap_or_else(|| view! {}.into_view())
                            }}
                        </Suspense>
                    </div>
                </div>

                <div class="card bg-base-100 shadow">
                    <div class="card-body gap-4">
                        <h3 class="card-title">"Assign role"</h3>
                        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
                            <div class="form-control">
                                {field_label("User", "User (g.user)")}
                                <input class="input input-bordered" on:input=move |ev| g_user.set(event_target_value(&ev)) placeholder="username" />
                            </div>
                            <div class="form-control">
                                {field_label("Role", "Role (g.role)")}
                                <input class="input input-bordered" on:input=move |ev| g_role.set(event_target_value(&ev)) placeholder="qa / admin / reader" />
                            </div>
                        </div>

                        <div class="flex gap-2">
                            <Button text="Add".to_string() on_click=Callback::new(move |_| add_role_action.dispatch(())) />
                            <Button text="Refresh".to_string() on_click=Callback::new(move |_| refresh_roles.update(|v| *v += 1)) />
                        </div>

                        <Suspense fallback=move || view! { <div>"Loading..."</div> }>
                            {move || {
                                roles_resource.get().map(|res| match res {
                                    Ok(rows) => view! {
                                        <div class="overflow-auto">
                                            <table class="table table-zebra">
                                                <thead>
                                                    <tr>
                                                        <th>"User"</th>
                                                        <th>"Role"</th>
                                                        <th>"Domain"</th>
                                                    </tr>
                                                </thead>
                                                <tbody>
                                                    {rows.into_iter().map(|r| {
                                                        let user = r.first().cloned().unwrap_or_default();
                                                        let role = r.get(1).cloned().unwrap_or_default();
                                                        let dom = r.get(2).cloned().unwrap_or_default();
                                                        view! { <tr><td>{user}</td><td>{role}</td><td>{dom}</td></tr> }
                                                    }).collect_view()}
                                                </tbody>
                                            </table>
                                        </div>
                                    }.into_view(),
                                    Err(e) => {
                                        view! { <div class="text-sm text-red-600">{e}</div> }.into_view()
                                    }
                                }).unwrap_or_else(|| view! {}.into_view())
                            }}
                        </Suspense>
                    </div>
                </div>

                <div class="card bg-base-100 shadow">
                    <div class="card-body gap-4">
                        <h3 class="card-title">"Action group mapping"</h3>
                        <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
                            <div class="form-control">
                                {field_label("Resource", "Resource part of resource:action")}
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
                                            view! { <option value=value.clone() selected=move || g2_resource.get() == r>{value}</option> }
                                        })
                                        .collect_view()}
                                </select>
                            </div>
                            <div class="form-control">
                                {field_label("Action", "Action part of resource:action (e.g. create)")}
                                <input class="input input-bordered" on:input=move |ev| g2_action.set(event_target_value(&ev)) placeholder="create" />
                            </div>
                            <div class="form-control">
                                {field_label("Action group", "Group (e.g. write)")}
                                <input class="input input-bordered" on:input=move |ev| g2_action_group.set(event_target_value(&ev)) placeholder="write" />
                            </div>
                        </div>

                        <div class="flex gap-2">
                            <Button text="Add".to_string() on_click=Callback::new(move |_| add_action_group_action.dispatch(())) />
                            <Button text="Refresh".to_string() on_click=Callback::new(move |_| refresh_action_groups.update(|v| *v += 1)) />
                        </div>

                        <Suspense fallback=move || view! { <div>"Loading..."</div> }>
                            {move || {
                                action_groups_resource.get().map(|res| match res {
                                    Ok(rows) => view! {
                                        <div class="overflow-auto">
                                            <table class="table table-zebra">
                                                <thead>
                                                    <tr>
                                                        <th>"Subject"</th>
                                                        <th>"Domain"</th>
                                                        <th>"Action group"</th>
                                                    </tr>
                                                </thead>
                                                <tbody>
                                                    {rows.into_iter().map(|r| {
                                                        let subject = r.first().cloned().unwrap_or_default();
                                                        let dom = r.get(1).cloned().unwrap_or_default();
                                                        let group = r.get(2).cloned().unwrap_or_default();
                                                        view! { <tr><td>{subject}</td><td>{dom}</td><td>{group}</td></tr> }
                                                    }).collect_view()}
                                                </tbody>
                                            </table>
                                        </div>
                                    }.into_view(),
                                    Err(e) => {
                                        view! { <div class="text-sm text-red-600">{e}</div> }.into_view()
                                    }
                                }).unwrap_or_else(|| view! {}.into_view())
                            }}
                        </Suspense>
                    </div>
                </div>
            </div>
            </Show>
        </div>
    }
}

#[component]
pub fn Authz() -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    authz_body(workspace, org)
}

#[component]
pub fn AuthzWorkspacePicker() -> impl IntoView {
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let selected_workspace = RwSignal::new(String::new());

    let workspaces_resource = create_blocking_resource(
        move || org.get().0,
        |org_id| async move {
            workspaces::list(&PaginationParams::all_entries(), &org_id)
                .await
                .unwrap_or_default()
        },
    );

    let workspace = Signal::derive(move || Workspace(selected_workspace.get()));

    view! {
        <div class="flex flex-col gap-6">
            <div class="card bg-base-100 shadow">
                <div class="card-body gap-4">
                    <h2 class="card-title">"Authorization"</h2>
                    <div class="form-control max-w-md">
                        {field_label("Workspace", "Choose a workspace")}
                        <Suspense fallback=move || view! { <div class="text-sm text-gray-600">"Loading workspaces..."</div> }>
                            {move || {
                                workspaces_resource
                                    .get()
                                    .map(|resp| {
                                        let options = resp.data;
                                        view! {
                                        <select
                                            class="select select-bordered"
                                            on:change=move |ev| selected_workspace.set(event_target_value(&ev))
                                        >
                                            <option value="">"Select workspace"</option>
                                            {options
                                                .into_iter()
                                                .map(|w| {
                                                    let name = w.workspace_name;
                                                    view! { <option value=name.clone()>{name}</option> }
                                                })
                                                .collect_view()}
                                        </select>
                                    }
                                            .into_view()
                                    })
                                    .unwrap_or_else(|| view! {}.into_view())
                            }}
                        </Suspense>
                    </div>
                </div>
            </div>

            {authz_body(workspace, org)}
        </div>
    }
}
