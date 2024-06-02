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
