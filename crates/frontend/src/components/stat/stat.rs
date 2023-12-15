use leptos::*;

#[component]
pub fn Stat(heading: &'static str, icon: &'static str, number: String) -> impl IntoView {
    let icon_class = format!("{} text-5xl", icon);
    view! {
        <div class="stats shadow">
            <div class="stat">
                <div class="stat-figure text-purple-700">
                    <i class=icon_class></i>
                </div>
                <div class="stat-title">{heading}</div>
                <div class="stat-value">{number}</div>
            </div>
        </div>
    }
}
