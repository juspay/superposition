use leptos::*;
use leptos_router::use_params_map;

use crate::api::snapshots::fetch;
use crate::components::alert::{Alert, AlertType};
use crate::components::badge::Badge;
use crate::components::skeleton::{Skeleton, SkeletonVariant};
use crate::components::toast::Toast;
use crate::types::{OrganisationId, Tenant};

#[component]
pub fn config_version() -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org_id = use_context::<Signal<OrganisationId>>().unwrap();
    let params = use_params_map();
    let version = params.with(|p| p.get("version").cloned().unwrap_or_default());

    let snapshot_resource = create_blocking_resource(
        move || (workspace.get().0, version.clone(), org_id.get().0),
        |(workspace, version, org_id)| async move { fetch(&version, workspace, org_id).await },
    );

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::DetailPage /> }
        }>
            {move || {
                match snapshot_resource.get() {
                    Some(Ok(snapshot)) => {
                        let config_json = serde_json::to_string_pretty(&snapshot.config)
                            .unwrap_or_default();
                        view! {
                            <div class="h-full flex flex-col gap-10 overflow-x-auto bg-transparent">
                                <h1 class="text-2xl font-extrabold">{snapshot.id}</h1>

                                <div class="card bg-base-100 max-w-screen shadow">
                                    <div class="card-body flex flex-col gap-2">
                                        <div class="flex gap-2 flex-wrap">
                                            <div class="h-fit w-[250px]">
                                                <div class="stat-title">"Description"</div>
                                                <div
                                                    class="tooltip tooltip-bottom w-[inherit] text-left"
                                                    data-tip=String::from(&snapshot.description)
                                                >
                                                    <div class="stat-value text-sm text-ellipsis overflow-hidden">
                                                        {String::from(&snapshot.description)}
                                                    </div>
                                                </div>
                                            </div>
                                            <div class="h-fit w-[250px]">
                                                <div class="stat-title">"Created at"</div>
                                                <div class="stat-value text-sm">
                                                    {snapshot.created_at.format("%v %T").to_string()}
                                                </div>
                                            </div>
                                        </div>
                                        {snapshot
                                            .tags
                                            .map(|tags| {
                                                view! {
                                                    <div class="flex flex-row gap-6 flex-wrap">
                                                        <div class="h-fit flex items-center gap-4">
                                                            <div class="stat-title">"Tags"</div>
                                                            <Badge options=Signal::derive(move || tags.clone()) />
                                                        </div>
                                                    </div>
                                                }
                                            })}
                                    </div>
                                </div>

                                <div class="w-full p-1 bg-base-100 rounded-2xl shadow">
                                    <andypf-json-viewer
                                        indent="4"
                                        expanded="true"
                                        theme="default-light"
                                        show-data-types="false"
                                        show-toolbar="true"
                                        expand-icon-type="arrow"
                                        expanded="1"
                                        show-copy="true"
                                        show-size="false"
                                        data=config_json
                                    />
                                </div>
                            </div>
                        }
                            .into_view()
                    }
                    Some(Err(_)) => {
                        view! {
                            <Toast alerts=vec![
                                Alert::new(
                                    0,
                                    "Error loading config.".to_string(),
                                    AlertType::Error,
                                    5000,
                                ),
                            ] />
                        }
                            .into_view()
                    }
                    None => {
                        view! { <Skeleton variant=SkeletonVariant::DetailPage /> }
                    }
                }
            }}
        </Suspense>
    }
}
