use std::ops::Deref;

use leptos::*;
use leptos_router::{use_navigate, use_params_map};

use crate::api::secrets;
use crate::components::{
    alert::AlertType,
    button::Button,
    description::ContentDescription,
    drawer::PortalDrawer,
    secret_form::{ChangeLogSummary, ChangeType, SecretForm},
    skeleton::{Skeleton, SkeletonVariant},
};
use crate::providers::alert_provider::enqueue_alert;
use crate::types::{OrganisationId, Workspace};

#[derive(Clone)]
enum Action {
    None,
    Edit,
    Delete,
}

#[component]
pub fn secret() -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let params = use_params_map();
    let secret_name = Signal::derive(move || {
        params.with(|p| p.get("secret_name").cloned().unwrap_or_default())
    });

    let action_rws = RwSignal::new(Action::None);
    let delete_inprogress_rws = RwSignal::new(false);

    let secret_resource = create_blocking_resource(
        move || (secret_name.get(), workspace.get().0, org.get().0),
        |(sec_name, workspace, org_id)| async move {
            secrets::get(&sec_name, &workspace, &org_id).await.ok()
        },
    );

    let on_delete_confirm = move |_| {
        delete_inprogress_rws.set(true);
        let sec_name = secret_name.get_untracked();
        spawn_local(async move {
            let result = secrets::delete(
                sec_name,
                &workspace.get_untracked().0,
                &org.get_untracked().0,
            )
            .await;
            delete_inprogress_rws.set(false);
            match result {
                Ok(_) => {
                    logging::log!("Secret deleted successfully");
                    let navigate = use_navigate();
                    let redirect_url =
                        format!("/admin/{}/{}/secrets", org.get().0, workspace.get().0,);
                    navigate(&redirect_url, Default::default());
                    enqueue_alert(
                        String::from("Secret deleted successfully"),
                        AlertType::Success,
                        5000,
                    );
                }
                Err(e) => {
                    logging::error!("Error deleting secret: {:?}", e);
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
                let secret = match secret_resource.get() {
                    Some(Some(sec)) => sec,
                    _ => return view! { <h1>"Error fetching secret"</h1> }.into_view(),
                };
                let secret_st = StoredValue::new(secret.clone());
                view! {
                    <div class="flex flex-col gap-4">
                        <div class="flex justify-between items-center">
                            <h1 class="text-2xl font-extrabold">{secret.name.to_string()}</h1>
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
                            description=secret.description.clone()
                            change_reason=secret.change_reason.clone()
                            created_by=secret.created_by.clone()
                            created_at=secret.created_at
                            last_modified_by=secret.last_modified_by.clone()
                            last_modified_at=secret.last_modified_at
                        />
                    </div>
                    {match action_rws.get() {
                        Action::None => ().into_view(),
                        Action::Edit => {
                            view! {
                                <PortalDrawer
                                    title="Edit Secret"
                                    handle_close=move |_| action_rws.set(Action::None)
                                >
                                    <SecretForm
                                        edit=true
                                        secret_name=secret_st.with_value(|s| s.name.to_string())
                                        secret_value=String::new()
                                        description=secret_st
                                            .with_value(|s| s.description.deref().to_string())
                                        handle_submit=move |_| {
                                            secret_resource.refetch();
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
                                    secret_name=secret_name.get()
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
