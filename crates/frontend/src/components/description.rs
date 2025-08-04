use chrono::{DateTime, Utc};
use leptos::*;
use superposition_types::database::models::{ChangeReason, Description};

use crate::components::info_modal::InfoModal;

#[component]
pub fn info_description(
    #[prop(into)] description: String,
    #[prop(into)] change_reason: String,
) -> impl IntoView {
    let (show_modal, set_show_modal) = create_signal(false);
    let stored_description = store_value(description);
    let stored_change_reason = store_value(change_reason);

    view! {
        <svg
            viewBox="0 0 32 16"
            class="inline w-4 h-2 text-gray-500 hover:text-gray-700 cursor-pointer"
            on:click=move |_| set_show_modal.set(true)
        >
            <rect
                width="32"
                height="16"
                rx="4"
                ry="4"
                fill="currentColor"
                stroke-width="0"
                stroke-linecap="round"
            />
            <ellipse rx="2" ry="2" transform="translate(7 8)" fill="white" stroke-width="0" />
            <ellipse rx="2" ry="2" transform="translate(25 8)" fill="white" stroke-width="0" />
            <ellipse rx="2" ry="2" transform="translate(16 8)" fill="white" stroke-width="0" />
        </svg>
        <Show when=move || show_modal.get() fallback=|| ()>
            <Portal mount=document().body().unwrap()>
                <InfoModal
                    visible=true
                    description=stored_description.get_value()
                    change_reason=stored_change_reason.get_value()
                    on_close=Callback::new(move |_| set_show_modal.set(false))
                />
            </Portal>
        </Show>
    }
}

#[component]
pub fn content_description(
    #[prop(optional, into)] pre_data: Option<ViewFn>,
    description: Description,
    created_by: String,
    created_at: DateTime<Utc>,
    last_modified_by: String,
    last_modified_at: DateTime<Utc>,
    change_reason: ChangeReason,
    #[prop(optional, into)] post_data: Option<ViewFn>,
) -> impl IntoView {
    view! {
        <div class="card bg-base-100 max-w-screen shadow">
            <div class="card-body flex flex-row gap-2 flex-wrap">
                {pre_data.map(|f| f.run())} <div class="h-fit w-[250px]">
                    <div class="stat-title">"Description"</div>
                    <div
                        class="tooltip tooltip-bottom w-[inherit] text-left"
                        data-tip=String::from(&description)
                    >
                        <div class="stat-value text-sm text-ellipsis overflow-hidden">
                            {String::from(&description)}
                        </div>
                    </div>
                </div> <div class="h-fit w-[250px]">
                    <div class="stat-title">"Created by"</div>
                    <div class="stat-value text-sm">{created_by}</div>
                </div> <div class="h-fit w-[250px]">
                    <div class="stat-title">"Created at"</div>
                    <div class="stat-value text-sm">{created_at.format("%v %T").to_string()}</div>
                </div> <div class="h-fit w-[250px]">
                    <div class="stat-title">"Last Modified by"</div>
                    <div class="stat-value text-sm">{last_modified_by}</div>
                </div> <div class="h-fit w-[250px]">
                    <div class="stat-title">"Last Modified at"</div>
                    <div class="stat-value text-sm">
                        {last_modified_at.format("%v %T").to_string()}
                    </div>
                </div> <div class="h-fit w-[250px]">
                    <div class="stat-title">"Change Reason"</div>
                    <div
                        class="tooltip tooltip-bottom w-[inherit] text-left"
                        data-tip=String::from(&change_reason)
                    >
                        <div class="stat-value text-sm text-ellipsis overflow-hidden">
                            {String::from(&change_reason)}
                        </div>
                    </div>
                </div> {post_data.map(|f| f.run())}
            </div>
        </div>
    }
}
