use leptos::*;

use crate::api::fetch_organisations;
use crate::components::skeleton::Skeleton;
use crate::utils::use_host_server;

#[component]
pub fn organisations() -> impl IntoView {
    let host = StoredValue::new(use_host_server());
    let (organisation_rs, organisation_ws) = create_signal::<Option<String>>(None);

    let organisation_resource = create_local_resource(
        || (),
        |_| async { fetch_organisations().await.unwrap_or_default() },
    );

    view! {
        <div class="h-screen w-full flex flex-col items-center justify-center">
            <Suspense fallback=move || {
                view! { <Skeleton /> }
            }>
                {move || {
                    view! {
                        <form action=format!(
                            "{}/organisations/switch/{}",
                            host.get_value(),
                            organisation_rs.get().unwrap_or_default(),
                        )>
                            <div>Select Organisation</div>
                            <select
                                class="w-[300px] border border-black"
                                value=organisation_rs.get().unwrap_or_default()
                                on:change=move |event| {
                                    let organisation = event_target_value(&event);
                                    organisation_ws
                                        .set(
                                            if organisation.as_str() != "" { Some(organisation) } else { None },
                                        );
                                }
                            >
                                <option value=String::from("")>Select Organisation</option>
                                <For
                                    each=move || organisation_resource.get().clone().unwrap_or_default()
                                    key=|organisation| organisation.clone()
                                    children=move |organisation| {
                                        view! { <option value=organisation.clone() selected={organisation == organisation_rs.get().unwrap_or_default()}>{organisation}</option> }
                                    }
                                />
                            </select>
                            <button disabled=organisation_rs.get().is_none()>Submit</button>
                        </form>
                    }
                }}

            </Suspense>
        </div>
    }
}
