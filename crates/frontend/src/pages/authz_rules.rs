use leptos::*;
use superposition_types::{
    Resource,
    api::authz::casbin::{
        ActionGroupPolicyRequest, GroupingPolicyRequest, PolicyRequest,
    },
};

use crate::{
    api::casbin,
    components::{alert::AlertType, button::Button, change_form::ChangeForm},
    providers::alert_provider::enqueue_alert,
    types::{OrganisationId, Workspace},
};

#[derive(Clone, Debug, PartialEq)]
enum ParsedEntry {
    Policy {
        sub: String,
        dom: String,
        obj: Resource,
        act: String,
        attr: String,
    },
    Grouping {
        user: String,
        role: String,
        dom: String,
    },
    ActionGrouping {
        resource: Resource,
        action: String,
        dom: String,
        action_group: String,
    },
}

fn parse_entries(raw: &str) -> Result<Vec<ParsedEntry>, String> {
    let mut out = Vec::new();

    for (idx, line) in raw.lines().enumerate() {
        let line_no = idx + 1;
        let trimmed = line.trim();

        if trimmed.is_empty() {
            continue;
        }
        if trimmed.starts_with('#') {
            continue;
        }

        let parts = trimmed
            .split(',')
            .map(|s| s.trim())
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>();

        let kind = parts
            .first()
            .ok_or_else(|| format!("Invalid entry at line {line_no}"))?;

        match *kind {
            "p" => {
                if parts.len() != 6 {
                    return Err(format!(
                        "Invalid p entry at line {line_no}: expected 6 fields (p, sub, dom, obj, act, attr)"
                    ));
                }

                let sub = parts[1].to_string();
                let dom = parts[2].to_string();
                let obj = Resource::try_from(parts[3]).map_err(|e| e.to_string())?;
                let act = parts[4].to_string();
                let attr = parts[5].to_string();

                out.push(ParsedEntry::Policy {
                    sub,
                    dom,
                    obj,
                    act,
                    attr,
                });
            }
            "g" => {
                if parts.len() != 4 {
                    return Err(format!(
                        "Invalid g entry at line {line_no}: expected 4 fields (g, user, role, dom)"
                    ));
                }

                out.push(ParsedEntry::Grouping {
                    user: parts[1].to_string(),
                    role: parts[2].to_string(),
                    dom: parts[3].to_string(),
                });
            }
            "g2" => {
                if parts.len() != 4 {
                    return Err(format!(
                        "Invalid g2 entry at line {line_no}: expected 4 fields (g2, subject, dom, action_group)"
                    ));
                }

                let Some((res, act)) = parts[1].split_once(":") else {
                    return Err(format!(
                        "Invalid g2 entry at line {line_no}: subject should be in format resource:action"
                    ));
                };

                out.push(ParsedEntry::ActionGrouping {
                    resource: Resource::try_from(res).map_err(|e| e.to_string())?,
                    action: act.to_string(),
                    dom: parts[2].to_string(),
                    action_group: parts[3].to_string(),
                });
            }
            _ => {
                return Err(format!(
                    "Unknown entry kind '{kind}' at line {line_no} (expected p/g/g2)"
                ));
            }
        }
    }

    Ok(out)
}

#[component]
pub fn AuthzRules() -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let entries_rws = RwSignal::new(String::new());

    let refresh_policies = RwSignal::new(0u64);
    let refresh_roles = RwSignal::new(0u64);
    let refresh_action_groups = RwSignal::new(0u64);

    let policies_resource = create_blocking_resource(
        move || (workspace.get().0, org.get().0, refresh_policies.get()),
        |(workspace, org, _)| async move {
            casbin::list_policies(Some(&workspace), &org).await
        },
    );

    let roles_resource = create_blocking_resource(
        move || (workspace.get().0, org.get().0, refresh_roles.get()),
        |(workspace, org, _)| async move { casbin::list_roles(Some(&workspace), &org).await },
    );

    let action_groups_resource = create_blocking_resource(
        move || (workspace.get().0, org.get().0, refresh_action_groups.get()),
        |(workspace, org, _)| async move {
            casbin::list_action_groups(Some(&workspace), &org).await
        },
    );

    let apply_action = create_action(move |raw: &String| {
        let raw = raw.clone();
        async move {
            let entries = parse_entries(&raw)?;

            let mut ok = 0usize;
            let mut err = 0usize;

            for entry in entries {
                let res = match entry {
                    ParsedEntry::Policy {
                        sub,
                        obj,
                        act,
                        attr,
                        ..
                    } => {
                        casbin::add_policy(
                            PolicyRequest {
                                sub,
                                obj,
                                act,
                                attr: Some(attr),
                            },
                            Some(&workspace.get_untracked().0),
                            &org.get_untracked().0,
                        )
                        .await
                    }
                    ParsedEntry::Grouping { user, role, .. } => {
                        casbin::add_role(
                            GroupingPolicyRequest { user, role },
                            Some(&workspace.get_untracked().0),
                            &org.get_untracked().0,
                        )
                        .await
                    }
                    ParsedEntry::ActionGrouping {
                        resource,
                        action,
                        action_group,
                        ..
                    } => {
                        casbin::add_action_group(
                            ActionGroupPolicyRequest {
                                resource,
                                action,
                                action_group,
                            },
                            Some(&workspace.get_untracked().0),
                            &org.get_untracked().0,
                        )
                        .await
                    }
                };

                match res {
                    Ok(_) => ok += 1,
                    Err(_) => err += 1,
                }
            }

            refresh_policies.update(|v| *v += 1);
            refresh_roles.update(|v| *v += 1);
            refresh_action_groups.update(|v| *v += 1);

            if err == 0 {
                enqueue_alert(format!("Applied {ok} entries"), AlertType::Success, 4000);
                Ok(())
            } else {
                Err(format!("Applied {ok} entries; {err} failed"))
            }
        }
    });

    Effect::new(move |_| {
        if let Some(Err(e)) = apply_action.value().get() {
            enqueue_alert(e, AlertType::Error, 5000);
        }
    });

    view! {
        <div class="flex flex-col gap-6">
            <div class="card bg-base-100 shadow">
                <div class="card-body gap-4">
                    <h2 class="card-title">"Authorization (Casbin)"</h2>
                    <p class="text-sm text-gray-600">
                        "Paste entries like: p, sub, dom, obj, act, attr | g, user, role, dom | g2, subject, dom, action_group"
                    </p>

                    <div class="flex flex-col gap-3">
                        <ChangeForm
                            title="Entries".to_string()
                            placeholder=(r#"p, username, o1_w1, config, read, *
g, username, config_reader, o1_w1
g2, default_config:create, o1_w1, write"#).to_string()
                            class="max-w-none".to_string()
                            textarea_class="textarea textarea-bordered w-full min-h-[220px] font-mono".to_string()
                            value=entries_rws.get()
                            on_change=Callback::new(move |v| entries_rws.set(v))
                        />

                        <div class="flex gap-2">
                            <Button
                                text="Apply".to_string()
                                on_click=Callback::new(move |_| {
                                    let raw = entries_rws.get_untracked();
                                    apply_action.dispatch(raw);
                                })
                            />
                            <Button
                                text="Refresh".to_string()
                                on_click=Callback::new(move |_| {
                                    refresh_policies.update(|v| *v += 1);
                                    refresh_roles.update(|v| *v += 1);
                                    refresh_action_groups.update(|v| *v += 1);
                                })
                            />
                        </div>

                    </div>
                </div>
            </div>

            <div class="grid grid-cols-1 gap-6">
                <div class="card bg-base-100 shadow">
                    <div class="card-body gap-4">
                        <div class="flex items-center justify-between">
                            <h3 class="card-title">"Policies (p)"</h3>
                            <Button
                                text="Refresh".to_string()
                                on_click=Callback::new(move |_| refresh_policies.update(|v| *v += 1))
                            />
                        </div>
                        <Suspense fallback=move || view! { <div>"Loading..."</div> }>
                            {move || {
                                policies_resource
                                    .get()
                                    .map(|res| match res {
                                        Ok(rows) => view! {
                                            <div class="overflow-auto">
                                                <table class="table table-zebra">
                                                    <thead>
                                                        <tr>
                                                            <th>"sub"</th>
                                                            <th>"dom"</th>
                                                            <th>"obj"</th>
                                                            <th>"act"</th>
                                                            <th>"attr"</th>
                                                        </tr>
                                                    </thead>
                                                    <tbody>
                                                        {rows
                                                            .into_iter()
                                                            .map(|r| {
                                                                let sub = r.first().cloned().unwrap_or_default();
                                                                let dom = r.get(1).cloned().unwrap_or_default();
                                                                let obj = r.get(2).cloned().unwrap_or_default();
                                                                let act = r.get(3).cloned().unwrap_or_default();
                                                                let attr = r.get(4).cloned().unwrap_or_default();
                                                                view! {
                                                                    <tr>
                                                                        <td>{sub}</td>
                                                                        <td>{dom}</td>
                                                                        <td>{obj}</td>
                                                                        <td>{act}</td>
                                                                        <td>{attr}</td>
                                                                    </tr>
                                                                }
                                                            })
                                                            .collect_view()}
                                                    </tbody>
                                                </table>
                                            </div>
                                        }
                                        .into_view(),
                                        Err(e) => {
                                            view! { <div class="text-sm text-red-600">{e}</div> }.into_view()
                                        }
                                    })
                                    .unwrap_or_else(|| view! {}.into_view())
                            }}
                        </Suspense>
                    </div>
                </div>

                <div class="card bg-base-100 shadow">
                    <div class="card-body gap-4">
                        <div class="flex items-center justify-between">
                            <h3 class="card-title">"Groupings (g)"</h3>
                            <Button
                                text="Refresh".to_string()
                                on_click=Callback::new(move |_| refresh_roles.update(|v| *v += 1))
                            />
                        </div>
                        <Suspense fallback=move || view! { <div>"Loading..."</div> }>
                            {move || {
                                roles_resource
                                    .get()
                                    .map(|res| match res {
                                        Ok(rows) => view! {
                                            <div class="overflow-auto">
                                                <table class="table table-zebra">
                                                    <thead>
                                                        <tr>
                                                            <th>"user"</th>
                                                            <th>"role"</th>
                                                            <th>"dom"</th>
                                                        </tr>
                                                    </thead>
                                                    <tbody>
                                                        {rows
                                                            .into_iter()
                                                            .map(|r| {
                                                                let user = r.first().cloned().unwrap_or_default();
                                                                let role = r.get(1).cloned().unwrap_or_default();
                                                                let dom = r.get(2).cloned().unwrap_or_default();
                                                                view! {
                                                                    <tr>
                                                                        <td>{user}</td>
                                                                        <td>{role}</td>
                                                                        <td>{dom}</td>
                                                                    </tr>
                                                                }
                                                            })
                                                            .collect_view()}
                                                    </tbody>
                                                </table>
                                            </div>
                                        }
                                        .into_view(),
                                        Err(e) => {
                                            view! { <div class="text-sm text-red-600">{e}</div> }.into_view()
                                        }
                                    })
                                    .unwrap_or_else(|| view! {}.into_view())
                            }}
                        </Suspense>
                    </div>
                </div>

                <div class="card bg-base-100 shadow">
                    <div class="card-body gap-4">
                        <div class="flex items-center justify-between">
                            <h3 class="card-title">"Action Groups (g2)"</h3>
                            <Button
                                text="Refresh".to_string()
                                on_click=Callback::new(move |_| {
                                    refresh_action_groups.update(|v| *v += 1)
                                })
                            />
                        </div>
                        <Suspense fallback=move || view! { <div>"Loading..."</div> }>
                            {move || {
                                action_groups_resource
                                    .get()
                                    .map(|res| match res {
                                        Ok(rows) => view! {
                                            <div class="overflow-auto">
                                                <table class="table table-zebra">
                                                    <thead>
                                                        <tr>
                                                            <th>"subject"</th>
                                                            <th>"dom"</th>
                                                            <th>"action_group"</th>
                                                        </tr>
                                                    </thead>
                                                    <tbody>
                                                        {rows
                                                            .into_iter()
                                                            .map(|r| {
                                                                let subject = r.first().cloned().unwrap_or_default();
                                                                let dom = r.get(1).cloned().unwrap_or_default();
                                                                let action_group = r.get(2).cloned().unwrap_or_default();
                                                                view! {
                                                                    <tr>
                                                                        <td>{subject}</td>
                                                                        <td>{dom}</td>
                                                                        <td>{action_group}</td>
                                                                    </tr>
                                                                }
                                                            })
                                                            .collect_view()}
                                                    </tbody>
                                                </table>
                                            </div>
                                        }
                                        .into_view(),
                                        Err(e) => {
                                            view! { <div class="text-sm text-red-600">{e}</div> }.into_view()
                                        }
                                    })
                                    .unwrap_or_else(|| view! {}.into_view())
                            }}
                        </Suspense>
                    </div>
                </div>
            </div>
        </div>
    }
}
