use crate::components::side_nav::side_nav::SideNav;
use leptos::*;
use leptos_router::*;

#[component]
pub fn Layout(children: Children) -> impl IntoView {
    let _params = use_params_map();
    let location = use_location();

    let tenant = match location
        .pathname
        .get()
        .split("/")
        .collect::<Vec<&str>>()
        .get(2)
    {
        Some(s) => s.to_string(),
        None => "mjos".to_string(),
    };

    let (tenant_rs, tenant_ws) = create_signal(tenant);
    provide_context(tenant_rs);
    provide_context(tenant_ws);

    view! {
        <div>
            <SideNav/>
            <main class="ease-soft-in-out xl:ml-96 relative h-full max-h-screen rounded-xl transition-all duration-200 overflow-y-auto">
                {children()}
            </main>
        </div>
    }
}
