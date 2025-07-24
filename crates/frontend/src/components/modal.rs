use leptos::*;

#[component]
pub fn modal<NF>(
    id: String,
    #[prop(default = String::new())] classnames: String,
    #[prop(default = String::new())] heading: String,
    handle_close: NF,
    children: Children,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let classnames = format!("modal-box {classnames}");
    view! {
        <dialog id=id class="modal modal-middle">
            <div class=classnames>
                <button
                    class="btn btn-sm btn-circle btn-ghost absolute right-2 top-2"
                    on:click=move |_| { handle_close() }
                >
                    <i class="ri-close-line"></i>
                </button>
                <h3 class="font-bold text-lg">{heading}</h3>
                {children()}
                <div class="modal-action"></div>
            </div>
        </dialog>
    }
}

#[component]
pub fn portal_modal(
    #[prop(default = "w-full max-w-md".to_string())] class: String,
    #[prop(into, default = String::new())] heading: String,
    #[prop(into)] handle_close: Callback<(), ()>,
    children: ChildrenFn,
) -> impl IntoView {
    view! {
        <Portal>
            <div class="fixed inset-0 bg-black bg-opacity-50 z-[1500] flex items-center justify-center">
                <dialog class=format!(
                    "modal-box transform-none relative box-content p-6 flex flex-col gap-4 bg-white rounded-lg shadow-lg {class}",
                )>
                    <button
                        class="btn btn-sm btn-circle btn-ghost absolute right-2 top-2"
                        on:click=move |_| { handle_close.call(()) }
                    >
                        <i class="ri-close-line"></i>
                    </button>
                    <h3 class="font-bold text-lg">{heading.clone()}</h3>
                    {children()}
                </dialog>
            </div>
        </Portal>
    }
}
