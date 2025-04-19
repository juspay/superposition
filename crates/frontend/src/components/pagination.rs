use std::cmp::max;

use leptos::*;

#[component]
pub fn pagination(
    #[prop(default = String::new())] class: String,
    current_page: i64,
    total_pages: i64,
    next: Callback<i64>,
    previous: Callback<i64>,
) -> impl IntoView {
    let current_page = max(1, current_page);
    let total_pages = max(1, total_pages);

    let next_page = if current_page < total_pages {
        current_page + 1
    } else {
        current_page
    };

    let previous_page = if current_page > 1 {
        current_page - 1
    } else {
        current_page
    };

    view! {
        <div class=format!("join {class}")>
            <button class="join-item btn" on:click=move |_| previous.call(previous_page)>
                "«"
            </button>
            <button class="join-item btn">
                {format!("Page {} / {}", current_page, total_pages)}
            </button>
            <button class="join-item btn" on:click=move |_| next.call(next_page)>
                "»"
            </button>
        </div>
    }
}
