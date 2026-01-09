use std::ops::Deref;

use leptos::*;
use leptos_router::{use_navigate, use_params_map};
use superposition_types::database::models::others::Variable;

use crate::api::variables;
use crate::components::{
    alert::AlertType,
    button::Button,
    description::ContentDescription,
    drawer::PortalDrawer,
    skeleton::{Skeleton, SkeletonVariant},
    variable_form::{ChangeLogSummary, ChangeType, VariableForm},
};
use crate::providers::alert_provider::enqueue_alert;
use crate::types::{OrganisationId, Workspace};

#[component]
fn VariableInfo(variable: Variable) -> impl IntoView {
    view! {
        <div class="card bg-base-100 max-w-screen shadow">
            <div class="card-body">
                <h2 class="card-title">"Info"</h2>
                <div class="flex flex-col gap-4">
                    <div class="flex flex-row gap-6 flex-wrap">
                        <div class="h-fit flex items-center gap-4">
                            <div class="stat-title">"Value"</div>
                            <div class="stat-value text-base">{variable.value}</div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    }
}

#[derive(Clone)]
enum Action {
    None,
    Edit,
    Delete,
}

#[component]
pub fn Variable() -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let params = use_params_map();
    let variable_name = Signal::derive(move || {
        params.with(|p| p.get("variable_name").cloned().unwrap_or_default())
    });

    let action_rws = RwSignal::new(Action::None);
    let delete_inprogress_rws = RwSignal::new(false);

    let variable_resource = create_blocking_resource(
        move || (variable_name.get(), workspace.get().0, org.get().0),
        |(var_name, workspace, org_id)| async move {
            variables::get(&var_name, &workspace, &org_id).await.ok()
        },
    );

    let on_delete_confirm = move |_| {
        delete_inprogress_rws.set(true);
        let var_name = variable_name.get_untracked();
        spawn_local(async move {
            let result = variables::delete(
                var_name,
                &workspace.get_untracked().0,
                &org.get_untracked().0,
            )
            .await;
            delete_inprogress_rws.set(false);
            match result {
                Ok(_) => {
                    logging::log!("Variable deleted successfully");
                    let navigate = use_navigate();
                    let redirect_url = format!(
                        "/admin/{}/{}/variables",
                        org.get().0,
                        workspace.get().0,
                    );
                    navigate(&redirect_url, Default::default());
                    enqueue_alert(
                        String::from("Variable deleted successfully"),
                        AlertType::Success,
                        5000,
                    );
                }
                Err(e) => {
                    logging::error!("Error deleting variable: {:?}", e);
                    enqueue_alert(e, AlertType::Error, 5000);
                }
            }
        });
    };

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::DetailPage /> }
        }>
            {move || {
                let variable = match variable_resource.get() {
                    Some(Some(var)) => var,
                    _ => return view! { <h1>"Error fetching variable"</h1> }.into_view(),
                };
                let variable_st = StoredValue::new(variable.clone());
                view! {
                    <div class="flex flex-col gap-4">
                        <div class="flex justify-between items-center">
                            <h1 class="text-2xl font-extrabold">{variable.name.deref().clone()}</h1>
                            <div class="flex flex-row join">
                                <Button
                                    force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg rounded-lg"
                                    on_click=move |_| action_rws.set(Action::Edit)
                                    icon_class="ri-edit-line"
                                    text="Edit"
                                />
                                <Button
                                    force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg rounded-lg"
                                    on_click=move |_| action_rws.set(Action::Delete)
                                    icon_class="ri-delete-bin-line"
                                    text="Delete"
                                />
                            </div>
                        </div>
                        <ContentDescription
                            description=variable.description.clone()
                            change_reason=variable.change_reason.clone()
                            created_by=variable.created_by.clone()
                            created_at=variable.created_at
                            last_modified_by=variable.last_modified_by.clone()
                            last_modified_at=variable.last_modified_at
                        />
                        <VariableInfo variable />
                    </div>
                    {match action_rws.get() {
                        Action::None => ().into_view(),
                        Action::Edit => {
                            view! {
                                <PortalDrawer
                                    title="Edit Variable"
                                    handle_close=move |_| action_rws.set(Action::None)
                                >
                                    <VariableForm
                                        edit=true
                                        variable_name=variable_st
                                            .with_value(|v| v.name.deref().clone())
                                        variable_value=variable_st.with_value(|v| v.value.clone())
                                        description=variable_st
                                            .with_value(|v| v.description.deref().to_string())
                                        handle_submit=move |_| {
                                            variable_resource.refetch();
                                            action_rws.set(Action::None);
                                        }
                                    />
                                </PortalDrawer>
                            }
                                .into_view()
                        }
                        Action::Delete => {
                            view! {
                                <ChangeLogSummary
                                    variable_name=variable_name.get()
                                    change_type=ChangeType::Delete
                                    on_close=move |_| action_rws.set(Action::None)
                                    on_confirm=on_delete_confirm
                                    inprogress=delete_inprogress_rws
                                />
                            }
                                .into_view()
                        }
                    }}
                }
                    .into_view()
            }}
        </Suspense>
    }
}
