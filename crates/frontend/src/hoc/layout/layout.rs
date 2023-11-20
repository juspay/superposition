use crate::components::side_nav::side_nav::SideNav;
use leptos::*;

#[component]
pub fn Layout(children: Children) -> impl IntoView {
    view! {
        <div>
            <SideNav/>
            <main class="ease-soft-in-out xl:ml-96 relative h-full max-h-screen rounded-xl transition-all duration-200">
                {children()}
            </main>
        </div>
    }
}
