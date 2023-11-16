use crate::components::side_nav::side_nav::SideNav;
use leptos::*;

#[component]
pub fn Layout(cx: Scope, children: Children) -> impl IntoView {
    view! {
        cx,
        <div>
            <SideNav />
            <main class="ease-soft-in-out xl:ml-96 relative h-full max-h-screen rounded-xl transition-all duration-200">
                {children(cx)}
            </main>
        </div>
    }
}
