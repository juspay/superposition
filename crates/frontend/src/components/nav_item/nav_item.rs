use leptos::*;
use leptos_router::A;

#[component]
pub fn NavItem(
    is_active: bool,
    href: String,
    text: String,
    icon: String,
) -> impl IntoView {
    let (anchor_class, icon_wrapper_class, icon_class) = if is_active {
        (
            "py-2.5 px-4 flex items-center whitespace-nowrap active".to_string(),
            "rounded-lg bg-primary w-8 h-8 flex content-center justify-center pt-0.5 px-1".to_string(),
            format!("{} text-lg text-white font-normal", icon)
        )
    } else {
        (
            "py-2.5 px-4 flex items-center whitespace-nowrap".to_string(),
            "rounded-lg shadow-xl shadow-slate-300 bg-white w-8 h-8 flex content-center justify-center pt-0.5 px-1".to_string(),
            format!("{} text-lg text-primary font-normal", icon)
        )
    };

    view! {
        <A href={href} class={anchor_class}>
            <div class={icon_wrapper_class}>
                <i class={icon_class} />
            </div>
            <span class="ml-1 duration-300 opacity-100 pointer-events-none ease-soft">{text}</span>
        </A>
    }
}
