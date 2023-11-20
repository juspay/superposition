use leptos::*;

#[component]
pub fn Pagination<NF, PF>(
    current_page: i64,
    total_pages: i64,
    next: NF,
    previous: PF,
) -> impl IntoView
where
    NF: Fn() + 'static,
    PF: Fn() + 'static,
{
    view! {
        <div class="join">
            <button class="join-item btn" on:click=move |_| previous()>
                "«"
            </button>
            <button class="join-item btn">
                {format!("Page {} / {}", current_page, total_pages)}
            </button>
            <button class="join-item btn" on:click=move |_| next()>
                "»"
            </button>
        </div>
    }
}
