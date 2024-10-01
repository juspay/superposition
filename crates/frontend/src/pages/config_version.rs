use crate::api::fetch_config;
use crate::components::alert::{Alert, AlertType};
use crate::components::skeleton::Skeleton;
use crate::components::toast::Toast;
use leptos::*;
use leptos_router::use_params_map;

#[component]
pub fn config_version() -> impl IntoView {
    let params = use_params_map();
    let tenant = params.with(|p| p.get("tenant").cloned().unwrap_or_default());
    let version = params.with(|p| p.get("version").cloned());

    let config_resource = create_blocking_resource(
        move || (tenant.clone(), version.clone()),
        |(tenant, version)| async move { fetch_config(tenant, version).await },
    );

    view! {
        <div class="p-8">
            <Suspense fallback=move || view! { <Skeleton /> }>
                {move || {
                    match config_resource.get() {
                        Some(Ok(config)) => {
                            let config_json = serde_json::to_string_pretty(&config)
                                .unwrap_or_default();
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
                                <Toast
                                    alerts=vec![Alert::new(
                                        0,
                                        "Error loading config.".to_string(),
                                        AlertType::Error,
                                        5000,
                                    )]
                                />
                            }.into_view()
                        }
                        None => {
                            view! { <Skeleton /> }
                        }
                    }
                }}
            </Suspense>
        </div>
    }
}
