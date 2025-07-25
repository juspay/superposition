use leptos::*;

use super::alert::Alert;

#[component]
pub fn toast(alerts: Vec<Alert>) -> impl IntoView {
    view! {
        <div class="toast toast-end z-[999999999]">

            {alerts
                .into_iter()
                .map(|alert| {
                    view! { <Alert alert /> }
                })
                .collect_view()}

        </div>
    }
}
