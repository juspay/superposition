use leptos::*;
use serde_json::{Map, Value};
use superposition_types::custom_query::{CustomQuery, Query};
use web_sys::{Crypto, MouseEvent};

use crate::api::fetch_organisations;
use crate::components::alert::AlertType;
use crate::components::button::Button;
use crate::components::form::label::Label;
use crate::components::key_rotation_modal::MasterKeyRotationModal;
use crate::components::modal::PortalModal;
use crate::components::table::types::{Expandable, default_column_formatter};
use crate::components::{
    skeleton::Skeleton,
    stat::Stat,
    table::{
        Table,
        types::{Column, ColumnSortable},
    },
};
use crate::providers::alert_provider::enqueue_alert;
use crate::query_updater::use_signal_from_query;
use crate::types::AdminPageParams;
use crate::utils::use_url_base;

fn key_generation_instructions() -> [View; 5] {
    [
        view! {
            "Copy this key immediately - it will "
            <strong>"NOT"</strong>
            " be shown again."
        }
        .into_view(),
        view! { "Store it securely in your secrets manager." }.into_view(),
        view! {
            <p>"Set it as environment variable:"</p>
            <code class="block mt-1 p-2 bg-base-300 rounded text-xs font-mono">
                "MASTER_ENCRYPTION_KEY=<key>"
            </code>
        }
        .into_view(),
        view! { "Restart the service for the key to take effect." }.into_view(),
        view! {
            "Run the "
            <code class="px-1 py-0.5 bg-base-300 rounded text-xs font-mono">"db/migrate"</code>
            " endpoint for each workspace to initialize workspace-level encryption keys."
        }
        .into_view(),
    ]
}

fn try_generate_master_key() -> Option<String> {
    let mut key_bytes = [0u8; 32];
    web_sys::window()
        .and_then(|win| win.crypto().ok())
        .and_then(|c| {
            Crypto::get_random_values_with_u8_array(&c, &mut key_bytes)
                .map_err(|e| logging::log!("Error from crypto {e:?}"))
                .ok()
        })
        .and_then(|_| {
            web_sys::window().and_then(|win| {
                let u16_array: Vec<u16> =
                    key_bytes.into_iter().map(|b| b as u16).collect::<Vec<_>>();
                win.btoa(
                    &js_sys::JsString::from_char_code(&u16_array)
                        .as_string()
                        .unwrap_or_default(),
                )
                .map_err(|e| logging::log!("Error from btoa {e:?}"))
                .ok()
            })
        })
}

#[component]
pub fn Organisations() -> impl IntoView {
    let base = use_url_base();
    // this has to remain as `create_local_resource` as this calls needs to be made from the client side, as long as cookie forwarding is not supported
    let organisation_resource = create_local_resource(
        || (),
        |_| async { fetch_organisations().await.unwrap_or_default() },
    );

    let (admin_params_rws,) = use_signal_from_query(move |query_string| {
        (Query::<AdminPageParams>::extract_non_empty(query_string).into_inner(),)
    });

    let key_generation_rws = RwSignal::new(None);
    let show_master_rotation_modal = RwSignal::new(false);

    let generate_master_key = move || match try_generate_master_key() {
        Some(resp) => key_generation_rws.set(Some(Some(resp))),
        None => {
            enqueue_alert(
                "Failed to generate encryption key".to_string(),
                AlertType::Warning,
                5000,
            );
            logging::log!("Failed to generate master encryption key");
        }
    };

    let table_columns = StoredValue::new({
        let base = base.clone();
        let navigate = move |value: &str, _row: &Map<String, Value>| {
            let organisation_id = value.to_string();
            view! {
                <button
                    formaction=format!("{base}/organisations/switch/{organisation_id}")
                    class="cursor-pointer text-blue-500"
                >
                    {organisation_id}
                </button>
            }
            .into_view()
        };

        vec![Column::new(
            "organisation_id".to_string(),
            false,
            navigate,
            ColumnSortable::No,
            Expandable::Disabled,
            default_column_formatter,
        )]
    });

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton /> }
        }>
            {move || {
                let organisations = organisation_resource.get().unwrap_or_default();
                let table_rows = organisations
                    .clone()
                    .into_iter()
                    .map(|organisation| {
                        let mut map = Map::new();
                        map.insert(String::from("organisation_id"), Value::String(organisation));
                        map
                    })
                    .collect::<Vec<Map<String, Value>>>();
                view! {
                    <form class="h-full flex flex-col gap-4">
                        <div class="flex justify-between">
                            <Stat
                                heading="Organisations"
                                icon="ri-building-fill"
                                number=organisations.len().to_string()
                            />
                            <Show when=move || {
                                admin_params_rws.with(|p| p.admin.unwrap_or_default())
                            }>
                                <div class="flex items-end gap-4">
                                    <Button
                                        text="Generate MasterKey"
                                        icon_class="ri-key-2-line"
                                        on_click=move |ev: MouseEvent| {
                                            ev.prevent_default();
                                            key_generation_rws.set(Some(None));
                                        }
                                    />
                                    <Button
                                        text="Rotate Master Key"
                                        icon_class="ri-refresh-line"
                                        on_click=move |ev: MouseEvent| {
                                            ev.prevent_default();
                                            show_master_rotation_modal.set(true);
                                        }
                                    />
                                </div>
                            </Show>
                        </div>

                        <div class="card w-full bg-base-100 rounded-xl overflow-hidden shadow">
                            <div class="card-body overflow-y-auto overflow-x-visible">
                                <Table
                                    class="!overflow-y-auto"
                                    rows=table_rows
                                    key_column="organisation_id"
                                    columns=table_columns.get_value()
                                />
                            </div>
                        </div>
                    </form>
                }
            }}
        </Suspense>

        {move || match key_generation_rws.get() {
            Some(popup_state) => {
                view! {
                    <PortalModal
                        heading="Generate Master Encryption Key"
                        handle_close=move |_| key_generation_rws.set(None)
                    >
                        <div class="alert alert-warning">
                            <i class="ri-alert-line" />
                            <span>
                                "This will generate a new master encryption key. Save it securely - it will not be shown again."
                            </span>
                        </div>
                        {match popup_state.clone() {
                            Some(generated_key) => {
                                let key_to_copy = generated_key.clone();
                                let copy_to_clipboard = move |_| {
                                    let key = key_to_copy.clone();
                                    if let Some(window) = web_sys::window() {
                                        let promise = window
                                            .navigator()
                                            .clipboard()
                                            .write_text(&key);
                                        wasm_bindgen_futures::spawn_local(async move {
                                            match wasm_bindgen_futures::JsFuture::from(promise).await {
                                                Ok(_) => {
                                                    enqueue_alert(
                                                        "Key copied to clipboard".to_string(),
                                                        AlertType::Success,
                                                        3000,
                                                    )
                                                }
                                                Err(_) => {
                                                    enqueue_alert(
                                                        "Failed to copy key to clipboard".to_string(),
                                                        AlertType::Error,
                                                        3000,
                                                    )
                                                }
                                            }
                                        });
                                    }
                                };

                                view! {
                                    <div class="flex flex-col gap-2">
                                        <div class="control-form">
                                            <Label title="Generated Key" />
                                            <div class="p-4 flex justify-between items-center bg-gray-900 text-green-400 rounded-lg">
                                                <div class="font-mono text-[14px] break-all select-all">
                                                    {generated_key}
                                                </div>
                                                <button
                                                    type="button"
                                                    class="px-1 bg-gray-800 hover:bg-gray-700 text-white rounded transition-colors"
                                                    on:click=copy_to_clipboard
                                                    title="Copy to clipboard"
                                                >
                                                    <i class="ri-file-copy-line" />
                                                </button>
                                            </div>
                                        </div>
                                        <div class="control-form">
                                            <Label title="Instructions" />
                                            <ol class="p-4 flex flex-col gap-3 bg-base-200 rounded-lg list-none">
                                                {key_generation_instructions()
                                                    .into_iter()
                                                    .enumerate()
                                                    .map(|(index, instruction)| {
                                                        view! {
                                                            <li class="flex gap-3 items-center">
                                                                <span class="flex-shrink-0 w-6 h-6 flex items-center justify-center bg-primary text-primary-content rounded-full text-xs font-semibold">
                                                                    {(index + 1).to_string()}
                                                                </span>
                                                                <div class="text-[14px] flex-1">{instruction}</div>
                                                            </li>
                                                        }
                                                    })
                                                    .collect_view()}
                                            </ol>
                                        </div>
                                    </div>
                                }
                                    .into_view()
                            }
                            None => ().into_view(),
                        }}
                        {match popup_state.clone() {
                            Some(_) => {
                                view! {
                                    <Button
                                        class="h-12 w-48 self-end"
                                        text="Close"
                                        on_click=move |_| key_generation_rws.set(None)
                                        icon_class="ri-close-line"
                                    />
                                }
                            }
                            None => {
                                view! {
                                    <Button
                                        class="h-12 w-48 self-end"
                                        text="Generate Key"
                                        on_click=move |_| generate_master_key()
                                        icon_class="ri-key-2-line"
                                    />
                                }
                            }
                        }}
                    </PortalModal>
                }
            }
            None => ().into_view(),
        }}

        {move || {
            view! {
                <Show when=move || show_master_rotation_modal.get()>
                    <MasterKeyRotationModal
                        on_close=move |_| show_master_rotation_modal.set(false)
                        on_success=move |_| show_master_rotation_modal.set(false)
                    />
                </Show>
            }
        }}
    }
}
