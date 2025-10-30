use leptos::*;

#[component]
pub fn tip(
    #[prop(into)] message: String,
    #[prop(optional, into)] code_snippet: Option<String>,
    #[prop(optional, into)] example: Option<String>,
    #[prop(optional, into)] class: Option<String>,
) -> impl IntoView {
    let class = class.unwrap_or_default();

    view! {
        <div class=format!(
            "bg-gradient-to-r from-purple-50 to-indigo-50 border-l-4 border-purple-500 p-3 rounded-r-lg {}",
            class,
        )>
            <div class="flex gap-2 items-center">
                <i class="ri-information-line text-purple-600 text-lg"></i>
                <div class="text-sm text-gray-700">
                    <span class="font-semibold">"Tip: "</span>
                    {message}
                    {code_snippet
                        .map(|code| {
                            view! {
                                <>
                                    " "
                                    <code class="bg-white text-purple-700 px-2 py-1 rounded border border-purple-200 font-mono text-xs font-semibold">
                                        {code}
                                    </code>
                                </>
                            }
                        })}
                    {example
                        .map(|ex| {
                            view! {
                                <>
                                    " (e.g., "
                                    <code class="bg-white text-purple-700 px-2 py-1 rounded border border-purple-200 font-mono text-xs">
                                        {ex}
                                    </code> ")"
                                </>
                            }
                        })}
                </div>
            </div>
        </div>
    }
}
