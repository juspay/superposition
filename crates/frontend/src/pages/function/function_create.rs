use crate::components::function_form::FunctionEditor;
use crate::types::FunctionResponse;
use leptos::*;
use leptos_router::use_navigate;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone, Debug)]
struct CombinedResource {
    function: Option<FunctionResponse>,
}

#[component]
pub fn create_function_view() -> impl IntoView {
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    view! {
        <div>

            <div class="mt-20 mb-20">Create Function</div>

            // <script type="module">
            //     {r#"

            //         import * as monaco from 'https://cdn.jsdelivr.net/npm/monaco-editor@0.39.0/+esm';

            //         window.editor = monaco.editor.create(document.querySelector('.monaco'), {

            //             value: [
            //                 'async function validate(value, key) {',
            //                 '   return true;',
            //                 '}'
            //             ].join('\n'),
            //             language: 'javascript',
            //             readOnly: false
            //         });

            //         const form = document.getElementById("MyForm");
            //         form.addEventListener("formdata", e =>
            //         {
            //             e.formData.set('function', window.editor.getValue());
            //         });

            //         "#
            //         .to_string()}

            // </script>
            <FunctionEditor
                edit=false
                handle_submit=move || {
                    let tenant = tenant_rs.get();
                    let redirect_url = format!("admin/{tenant}/function");
                    let navigate = use_navigate();
                    navigate(redirect_url.as_str(), Default::default())
                }
            />

        </div>
    }
    .into_view()
}
