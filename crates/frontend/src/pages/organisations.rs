use leptos::*;
use serde_json::{Map, Value};
use web_sys::{Crypto, MouseEvent};

use crate::api::{fetch_organisations, master_encryption_key};
use crate::components::alert::AlertType;
use crate::components::button::Button;
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
use crate::utils::use_url_base;

#[component]
pub fn Organisations() -> impl IntoView {
    let base = use_url_base();
    // this has to remain as `create_local_resource` as this calls needs to be made from the client side, as long as cookie forwarding is not supported
    let organisation_resource = create_local_resource(
        || (),
        |_| async { fetch_organisations().await.unwrap_or_default() },
    );

    let (generated_key, set_generated_key) = create_signal::<Option<String>>(None);
    let (is_generating, set_is_generating) = create_signal(false);
    let (show_master_key_generate_modal, set_show_master_key_generate_modal) =
        create_signal(false);

    let generate_master_key_action = create_action(move |_: &()| async move {
        set_is_generating.set(true);
        set_generated_key.set(None);

        let mut keyBytes = [0u8; 32];
        let result = web_sys::window()
            .and_then(|win| win.crypto().ok())
            .and_then(|c| {
                Crypto::get_random_values_with_u8_array(&c, &mut keyBytes)
                    .map_err(|e| logging::log!("Error from crypto {e:?}"))
                    .ok()
            })
            .and_then(|_| {
                web_sys::window().and_then(|win| {
                    let u16_array: Vec<u16> =
                        keyBytes.into_iter().map(|b| b as u16).collect::<Vec<_>>();
                    win.btoa(
                        &js_sys::JsString::from_char_code(&u16_array)
                            .as_string()
                            .unwrap_or_default(),
                    )
                    .map_err(|e| logging::log!("Error from btoa {e:?}"))
                    .ok()
                })
            });

        set_is_generating.set(false);

        match result {
            Some(resp) => set_generated_key.set(Some(resp)),
            None => {
                enqueue_alert(
                    "Failed to generate encryption key".to_string(),
                    AlertType::Warning,
                    5000,
                );
                logging::log!("Failed to generate master encryption key");
            }
        }
    });

    let rotate_master_key_action = create_action(move |_: &()| async move {
        let result = master_encryption_key::rotate().await;

        match result {
            Ok(_) => (),
            Err(e) => logging::log!("Failed to rotate master encryption key: {}", e),
        }
    });

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
                        <Stat
                            heading="Oraganisations"
                            icon="ri-building-fill"
                            number=organisations.len().to_string()
                        />

                        // Master Key Management Buttons
                        <div class="flex gap-2 mb-2 ml-auto">
                            <Button
                                text="Generate MasterKey"
                                icon_class="ri-key-2-line"
                                on_click=move |ev: MouseEvent| {
                                    ev.prevent_default();
                                    set_show_master_key_generate_modal.set(true)
                                }
                            />

                            <Button
                                text="Rotate Master Key"
                                icon_class="ri-refresh-line"
                                on_click=move |ev: MouseEvent| {
                                    ev.prevent_default();
                                    rotate_master_key_action.dispatch(())
                                }
                            />

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

        // Master Key Generate Modal
        {move || {
            show_master_key_generate_modal
                .get()
                .then(|| {
                    view! {
                        <PortalModal
                            heading="Generate Master Encryption Key"
                            handle_close=Callback::new(move |_| {
                                set_show_master_key_generate_modal.set(false);
                                set_generated_key.set(None);
                            })
                        >
                            <div class="alert alert-warning mb-4">
                                <i class="ri-alert-line"></i>
                                <span>
                                    "This will generate a new master encryption key. Save it securely - it will not be shown again."
                                </span>
                            </div>

                            {move || {
                                if let Some(generated_key) = generated_key.get() {
                                    view! {
                                        <>
                                            <div class="bg-gray-900 text-green-400 p-4 rounded-lg font-mono text-sm mt-2 break-all select-all">
                                                {generated_key}
                                            </div>
                                            <pre class="text-sm mt-2 whitespace-pre-wrap bg-base-200 p-3 rounded">
                                                {"1. Copy this key immediately - it will NOT be shown again.\n2. Store it securely in your secrets manager.\n3. Set it as environment variable: MASTER_ENCRYPTION_KEY=<key>\n4. Restart the service for the key to take effect."}
                                            </pre>
                                            <Button
                                                text="Close"
                                                on_click=move |_| {
                                                    set_show_master_key_generate_modal.set(false);
                                                    set_generated_key.set(None);
                                                }
                                                icon_class="ri-close-line"
                                                class="mt-4 w-full"
                                            />
                                        </>
                                    }
                                } else {
                                    view! {
                                        <>
                                            <button
                                                class="btn btn-primary w-full"
                                                disabled=is_generating.get()
                                                on:click=move |_| generate_master_key_action.dispatch(())
                                            >
                                                {if is_generating.get() {
                                                    "Generating..."
                                                } else {
                                                    "Generate Key"
                                                }}
                                                <i class="ri-key-2-line"></i>
                                            </button>
                                        </>
                                    }
                                }
                            }}
                        </PortalModal>
                    }
                })
        }}
    }
}
