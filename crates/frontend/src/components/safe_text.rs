use leptos::*;

use crate::utils::escape_html;

/// Renders untrusted or dynamic text as inert literal text in SSR
/// (`SsrMode::Async` / `SsrMode::InOrder`) and CSR/hydration.
///
/// `leptos_dom` 0.6 (`0.6.11`–`0.6.15`, `ssr_in_order.rs`) emits non-reactive
/// `View::Text` nodes unescaped during SSR. Encapsulating the
/// `inner_html=escape_html(...)` workaround here keeps call sites uniform and
/// isolates the `leptos_macro` 0.6 attribute-order requirement in one place.
#[component]
pub fn SafeText(
    #[prop(into)] text: String,
    #[prop(into, optional)] class: Option<String>,
) -> impl IntoView {
    // Keep `inner_html` as the last keyed attribute: leptos_macro 0.6 SSR
    // resets `inner_html` back to `None` if any keyed attribute follows it.
    view! { <span class=class inner_html=escape_html(&text)></span> }
}

/// Wrapper around `<textarea>` that escapes its initial SSR content and keeps
/// `inner_html` as the last keyed attribute (required by `leptos_macro` 0.6 SSR).
#[component]
pub fn SafeTextarea(
    #[prop(into, optional)] id: Option<String>,
    #[prop(into, optional)] name: Option<String>,
    #[prop(into, optional)] placeholder: Option<String>,
    #[prop(into, optional)] class: Option<String>,
    #[prop(default = false)] disabled: bool,
    #[prop(into)] value: String,
    #[prop(into)] on_change: Callback<String, ()>,
) -> impl IntoView {
    view! {
        <textarea
            id=id
            name=name
            disabled=disabled
            placeholder=placeholder
            class=class
            on:change=move |ev| on_change.call(event_target_value(&ev))
            inner_html=escape_html(&value)
        ></textarea>
    }
}
