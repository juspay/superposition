use leptos::*;

#[component]
pub fn pagination(
    #[prop(default= String::new())] class: String,
    current_page: i64,
    total_pages: i64,
    next: Callback<i64>,
    previous: Callback<()>,
) -> impl IntoView {
    view! {
        <div class=format!("join {class}")>
            <button class="join-item btn" on:click=move |_| previous.call(())>
                "«"
            </button>
            <button class="join-item btn">
                {format!("Page {} / {}", current_page, total_pages)}
            </button>
            <button class="join-item btn" on:click=move |_| next.call(total_pages)>
                "»"
            </button>
        </div>
    }
}
