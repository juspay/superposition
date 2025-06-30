use std::cmp::max;

use leptos::*;

#[component]
pub fn pagination(
    #[prop(default = String::new())] class: String,
    current_page: i64,
    total_pages: i64,
    #[prop(into)] on_change: Callback<i64>,
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

    let input_page = RwSignal::new(current_page.to_string());
    let input_invalid = RwSignal::new(false);

    let handle_jump = move || {
        if let Ok(page) = input_page.get().parse::<i64>() {
            if page < 1 || page > total_pages {
                input_invalid.set(true);
            } else {
                input_invalid.set(false);
                if page != current_page {
                    on_change.call(page);
                }
            }
        } else {
            input_invalid.set(true);
        }
    };

    view! {
        <div class=format!("join {class}")>
            <button class="join-item btn" on:click=move |_| on_change.call(previous_page)>
                "«"
            </button>
            <button class="join-item btn">
                {format!("Page {} / {}", current_page, total_pages)}
            </button>
            <div class="join-item btn !px-1">
                <div class=move || {
                    format!(
                        "input input-sm h-max w-20 px-1 flex justify-between items-center rounded-lg {}",
                        if input_invalid.get() { "input-error" } else { "" },
                    )
                }>
                    <input
                        type="number"
                        min="1"
                        max=total_pages
                        class="w-full rounded"
                        value=input_page
                        on:input=move |ev| {
                            input_page.set(event_target_value(&ev));
                            input_invalid.set(false);
                        }
                        on:keydown=move |ev| {
                            if ev.key() == "Enter" {
                                handle_jump();
                            }
                        }
                    />
                    <i class="ri-corner-down-left-line text-gray-400" />
                </div>
            </div>
            <button class="join-item btn" on:click=move |_| on_change.call(next_page)>
                "»"
            </button>
        </div>
    }
}
