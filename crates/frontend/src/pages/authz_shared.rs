use leptos::*;
use std::rc::Rc;
use strum::IntoEnumIterator;
use superposition_types::{
    Resource,
    api::authz::casbin::{
        ActionGroupPolicyRequest, GroupingPolicyRequest, PolicyRequest,
    },
};

use crate::{
    api::casbin::{self, AuthzScope},
    components::{alert::AlertType, button::Button},
    providers::alert_provider::enqueue_alert,
};

#[derive(Clone, Debug, PartialEq)]
pub struct AuthzContext {
    pub scope: AuthzScope,
    pub workspace: Option<String>,
    pub org_id: Option<String>,
}

fn field_label(title: &'static str, description: &'static str) -> impl IntoView {
    view! {
        <label class="label flex flex-col items-start justify-center">
            <div class="flex items-center gap-1 label-text font-semibold text-base">{title}</div>
            <span class="label-text text-slate-400">{description}</span>
        </label>
    }
}

pub fn authz_editor(
    title: &'static str,
    description: &'static str,
    context_key: impl Fn() -> AuthzContext + Clone + 'static,
    context_value: impl Fn() -> AuthzContext + Clone + 'static,
) -> impl IntoView {
    let context_key: Rc<dyn Fn() -> AuthzContext> = Rc::new(context_key);
    let context_value: Rc<dyn Fn() -> AuthzContext> = Rc::new(context_value);
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
        {
            let context_key = context_key.clone();
            move || {
                let ctx = context_key();
                (ctx.workspace.clone(), ctx.org_id.clone(), refresh_policies.get())
            }
        },
        {
            let context_key = context_key.clone();
            move |_| {
                let ctx = context_key();
                async move {
                    casbin::list_policies(
                        ctx.scope,
                        ctx.workspace.as_deref(),
                        ctx.org_id.as_deref(),
                    )
                    .await
                }
            }
        },
    );

    let roles_resource = create_blocking_resource(
        {
            let context_key = context_key.clone();
            move || {
                let ctx = context_key();
                (ctx.workspace.clone(), ctx.org_id.clone(), refresh_roles.get())
            }
        },
        {
            let context_key = context_key.clone();
            move |_| {
                let ctx = context_key();
                async move {
                    casbin::list_roles(
                        ctx.scope,
                        ctx.workspace.as_deref(),
                        ctx.org_id.as_deref(),
                    )
                    .await
                }
            }
        },
    );

    let action_groups_resource = create_blocking_resource(
        {
            let context_key = context_key.clone();
            move || {
                let ctx = context_key();
                (
                    ctx.workspace.clone(),
                    ctx.org_id.clone(),
                    refresh_action_groups.get(),
                )
            }
        },
        {
            let context_key = context_key.clone();
            move |_| {
                let ctx = context_key();
                async move {
                    casbin::list_action_groups(
                        ctx.scope,
                        ctx.workspace.as_deref(),
                        ctx.org_id.as_deref(),
                    )
                    .await
                }
            }
        },
    );

    let add_policy_action = create_action({
        let context_value = context_value.clone();
        move |_| {
            let ctx = context_value();
            async move {
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
                ctx.scope,
                ctx.workspace.as_deref(),
                ctx.org_id.as_deref(),
            )
            .await?;

            refresh_policies.update(|v| *v += 1);
            enqueue_alert("Policy added".to_string(), AlertType::Success, 4000);
            Ok(())
            }
        }
    });

    let add_role_action = create_action({
        let context_value = context_value.clone();
        move |_| {
            let ctx = context_value();
            async move {
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
                ctx.scope,
                ctx.workspace.as_deref(),
                ctx.org_id.as_deref(),
            )
            .await?;

            refresh_roles.update(|v| *v += 1);
            enqueue_alert("Role assigned".to_string(), AlertType::Success, 4000);
            Ok(())
            }
        }
    });

    let add_action_group_action = create_action({
        let context_value = context_value.clone();
        move |_| {
            let ctx = context_value();
            async move {
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
                ctx.scope,
                ctx.workspace.as_deref(),
                ctx.org_id.as_deref(),
            )
            .await?;

            refresh_action_groups.update(|v| *v += 1);
            enqueue_alert(
                "Action group mapping added".to_string(),
                AlertType::Success,
                4000,
            );
            Ok(())
            }
        }
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
                    <h2 class="card-title">{title}</h2>
                    <p class="text-sm text-gray-600">{description}</p>
                </div>
            </div>

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
        </div>
    }
}
