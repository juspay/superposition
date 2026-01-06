use leptos::*;

#[component]
pub fn Tip(
    #[prop(into)] message: String,
    #[prop(optional, into)] code_snippet: Option<String>,
    #[prop(optional, into)] example: Option<String>,
    #[prop(optional, into)] code_signature: Option<String>,
    #[prop(optional, into)] class: Option<String>,
) -> impl IntoView {
    let class = class.unwrap_or_default();

    view! {
        <div class=format!(
            "bg-gradient-to-r from-purple-50 to-indigo-50 border-l-4 border-purple-500 p-3 rounded-r-lg {}",
            class,
        )>
            <div class="flex gap-2">
                <i class="ri-information-line text-purple-600 text-lg flex-shrink-0 mt-0.5"></i>
                <div class="text-sm text-gray-700 flex-1 space-y-2">
                    <div>
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

                    {code_signature
                        .map(|sig| {
                            view! {
                                <pre class="bg-gray-800 text-green-400 p-3 rounded text-xs font-mono overflow-x-auto whitespace-pre-wrap">
                                    {sig}
                                </pre>
                            }
                        })}
                </div>
            </div>
        </div>
    }
}
