use leptos::*;
use leptos_meta::*;
use leptos_router::*;
use serde_json::json;

use crate::providers::alert_provider::AlertProvider;
use crate::routes::AppRoutes;
use crate::types::Envs;

#[component]
pub fn app(app_envs: Envs) -> impl IntoView {
    // Provides context that manages stylesheets, titles, meta tags, etc.
    provide_meta_context();
    let service_prefix = app_envs.service_prefix;
    provide_context(app_envs.clone());
    view! {
        <html data-theme="light">
            {move || {
                let base = match service_prefix {
                    "" | "/" => "".to_owned(),
                    prefix => "/".to_owned() + prefix,
                };
                let styles_href = base.to_owned() + "/pkg/style.css";
                let favicon_href = base.to_owned() + "/assets/favicon.ico";
                let wasm_href = base.to_owned() + "/pkg/frontend_bg.wasm";
                let js_href = base.to_owned() + "/pkg/frontend.js";
                let import_callback = "() => mod.hydrate()";
                view! {
                    <Stylesheet id="leptos" href=styles_href />
                    <Link rel="shortcut icon" type_="image/ico" href=favicon_href />
                    <Link
                        href="https://cdn.jsdelivr.net/npm/remixicon/fonts/remixicon.css"
                        rel="stylesheet"
                    />
                    <script
                        type_="text/javascript"
                        src="https://cdn.jsdelivr.net/npm/@andypf/json-viewer@2.1.5/dist/iife/index.min.js"
                    ></script>
                    {move || {
                        if base.is_empty() {
                            view! {}.into_view()
                        } else {
                            view! {
                                <link
                                    rel="preload"
                                    href=wasm_href.clone()
                                    as_="fetch"
                                    type_="application/wasm"
                                    crossorigin=""
                                />
                                <link as_="script" rel="modulepreload" href=js_href.clone() />
                                <script type_="module">
                                    {format!(
                                        r#"
                                    function idle(c) {{
                                        if ('requestIdleCallback' in window) {{
                                            window.requestIdleCallback(c);
                                        }} else {{
                                            c();
                                        }}
                                    }}
                                    idle(() => {{
                                        import('{js_href}')
                                            .then(mod => {{
                                                mod.default('{wasm_href}').then({import_callback});
                                            }})
                                    }});
                                    "#,
                                    )}

                                </script>
                            }
                                .into_view()
                        }
                    }}
                }
            }} // sets the document title
            <Title text="Welcome to Superposition" />
            <script type_="text/javascript">"__APP_ENVS=" {json!(app_envs).to_string()}</script>
            <Router base=service_prefix>
                <body class="m-0 min-h-screen bg-gray-50 font-mono">
                    <AlertProvider>
                        <Routes base=service_prefix.to_string()>
                            <AppRoutes />
                        </Routes>
                    </AlertProvider>
                </body>
            </Router>

        </html>
    }
}
