use leptos::*;

#[component]
pub fn label(
    #[prop(into)] title: String,
    #[prop(into, optional)] info: Option<String>,
    #[prop(into, optional)] extra_info: Option<String>,
    #[prop(into, optional)] description: Option<String>,
    #[prop(into, default = String::new())] class: String,
) -> impl IntoView {
    view! {
        <label class=format!("label flex flex-col items-start justify-center {class}")>
            <div class="flex items-center gap-1 label-text font-semibold text-base">
                {title}
                {info
                    .map(|info| {
                        view! { <span class="text-sm font-normal text-slate-400">{info}</span> }
                    })}
                {extra_info
                    .map(|extra_info| {
                        view! {
                            <div class="group relative inline-block text-[10px] text-gray-700 cursor-pointer">
                                <p class="z-[1000] hidden absolute top-full left-1/2 w-[320px] p-2.5 group-hover:flex flex-col gap-4 bg-black text-white leading-relaxed rounded shadow-[0_4px_6px_rgba(0,0,0,0.1)] whitespace-normal translate-x-[20px] -translate-y-1/2">
                                    {extra_info}
                                </p>
                                <i class="ri-information-line ri-lg" />
                            </div>
                        }
                    })}
            </div>
            {description
                .map(|description| {
                    view! { <span class="label-text text-slate-400">{description}</span> }
                })}
        </label>
    }
}
