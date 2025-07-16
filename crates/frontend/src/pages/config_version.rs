use crate::api::fetch_config;
use crate::components::alert::{Alert, AlertType};
use crate::components::skeleton::{Skeleton, SkeletonVariant};
use crate::components::toast::Toast;
use crate::types::{OrganisationId, Tenant};
use leptos::*;
use leptos_router::use_params_map;

#[component]
pub fn config_version() -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org_id = use_context::<Signal<OrganisationId>>().unwrap();
    let params = use_params_map();
    let version = params.with(|p| p.get("version").cloned());

    let config_resource = create_blocking_resource(
        move || (workspace.get().0, version.clone(), org_id.get().0),
        |(workspace, version, org_id)| async move {
            fetch_config(workspace, version, org_id).await
        },
    );

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::DetailPage /> }
        }>
            {move || {
                match config_resource.get() {
                    Some(Ok(config)) => {
                        let config_json = serde_json::to_string_pretty(&config).unwrap_or_default();
                        view! {
                            <div>
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
                                ></andypf-json-viewer>
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
