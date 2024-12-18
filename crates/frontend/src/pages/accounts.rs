use leptos::*;

use crate::api::fetch_accounts;
use crate::components::skeleton::Skeleton;
use crate::utils::use_host_server;

#[component]
pub fn accounts() -> impl IntoView {
    let host = StoredValue::new(use_host_server());
    let (account_rs, account_ws) = create_signal::<Option<String>>(None);

    let account_resource = create_local_resource(
        || (),
        |_| async { fetch_accounts().await.unwrap_or_default() },
    );

    view! {
        <div class="h-screen w-full flex flex-col items-center justify-center">
            <Suspense fallback=move || {
                view! { <Skeleton /> }
            }>
                {move || {
                    view! {
                        <form action=format!(
                            "{}/oidc/accounts/switch/{}",
                            host.get_value(),
                            account_rs.get().unwrap_or_default(),
                        )>
                            <div>Select Account</div>
                            <select
                                class="w-[300px] border border-black"
                                value=account_rs.get().unwrap_or_default()
                                on:change=move |event| {
                                    let account = event_target_value(&event);
                                    account_ws
                                        .set(
                                            if account.as_str() != "" { Some(account) } else { None },
                                        );
                                }
                            >
                                <option value=String::from("")>Select Account</option>
                                <For
                                    each=move || account_resource.get().clone().unwrap_or_default()
                                    key=|account| account.clone()
                                    children=move |account| {
                                        view! { <option value=account.clone()>{account}</option> }
                                    }
                                />
                            </select>
                            <button disabled=account_rs.get().is_none()>Submit</button>
                        </form>
                    }
                }}

            </Suspense>
        </div>
    }
}
