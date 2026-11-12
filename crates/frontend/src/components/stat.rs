use leptos::*;

use crate::utils::escape_html;

#[component]
pub fn Stat(heading: &'static str, icon: &'static str, number: String) -> impl IntoView {
    let icon_class = format!("{} text-5xl", icon);
    view! {
        <div class="stats min-h-fit w-fit shadow">
            <div class="stat">
                <div class="stat-figure text-purple-700">
                    <i class=icon_class></i>
                </div>
                <div class="stat-title" inner_html=escape_html(heading)></div>
                <div class="stat-value" inner_html=escape_html(&number)></div>
            </div>
        </div>
    }
}
