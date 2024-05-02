use leptos::*;
use leptos_router::use_params_map;

use crate::types::FunctionResponse;

use super::utils::publish_function;

use crate::api::fetch_function;
use std::time::Duration;
use strum::EnumProperty;
use strum_macros::Display;
use web_sys::MouseEvent;

use crate::utils::get_element_by_id;
use web_sys::HtmlButtonElement;

use crate::components::function_form::function_form::{FunctionEditor, TestForm};

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Copy, Display, strum_macros::EnumProperty, PartialEq)]
enum CodeTab {
    #[strum(props(id = "draft_code_tab"))]
    DraftCode,
    #[strum(props(id = "published_code_tab"))]
    PublishedCode,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
struct CombinedResource {
    function: Option<FunctionResponse>,
}

#[component]
pub fn function_page() -> impl IntoView {
    let function_params = use_params_map();
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let source = move || {
        let t = tenant_rs.get();
        let function_name = function_params
            .with(|params| params.get("function_name").cloned().unwrap_or("1".into()));
        (function_name, t)
    };

    let (selected_tab_rs, selected_tab_ws) = create_signal(CodeTab::PublishedCode);
    let (editor_mode_rs, editor_mode_ws) = create_signal(true);
    let (test_mode_rs, test_mode_ws) = create_signal(false);
    let (show_publish_rs, show_publish_ws) = create_signal(false);
    let (publish_error_rs, publish_error_ws) = create_signal("".to_string());

    let combined_resource: Resource<(String, String), CombinedResource> =
        create_blocking_resource(source, |(function_name, tenant)| async move {
            let function_result =
                fetch_function(function_name.to_string(), tenant.to_string()).await;

            CombinedResource {
                function: function_result.ok(),
            }
        });

    view! {
        <Transition fallback=move || {
            view! { <h1>Loading....</h1> }
        }>
            {move || {
                let resource = match combined_resource.get() {
                    Some(res) => res,
                    None => return view! { <h1>Error fetching function</h1> }.into_view(),
                };
                let function = resource.function;
                match function {
                    Some(function) => {
                        let function_ef = function.clone();
                        let function_data = function.clone();
                        publish_error_ws.set("".to_string());
                        match function.published_at.clone() {
                            Some(val) => show_publish_ws.set(val < function.draft_edited_at),
                            None => show_publish_ws.set(true),
                        }
                        let publish_click = move |event: MouseEvent| {
                            event.prevent_default();
                            logging::log!("Submitting function form");
                            let tenant = tenant_rs.get();
                            let f_function_name = function_ef.function_name.clone();
                            spawn_local({
                                async move {
                                    let result = publish_function(f_function_name, tenant).await;
                                    match result {
                                        Ok(_) => {
                                            publish_error_ws.set("".to_string());
                                            combined_resource.refetch();
                                        }
                                        Err(e) => {
                                            publish_error_ws.set(e);
                                        }
                                    }
                                }
                            });
                        };
                        view! {
                            <div class="flex flex-col flex-row overflow-x-auto p-2 bg-transparent">

                                <div class="flex bg-base-100 flex-row gap-3 justify-between flex-wrap shadow m-5">
                                    <div class="stat w-2/12">
                                        <div class="stat-title">Function Name</div>
                                        <div>{function.function_name.clone()}</div>
                                    </div>
                                    <div class="stat w-2/12">
                                        <div class="stat-title">Published Runtime Version</div>
                                        <div>
                                            {function
                                                .published_runtime_version
                                                .clone()
                                                .unwrap_or("null".to_string())}
                                        </div>
                                    </div>
                                    <div class="stat w-2/12">
                                        <div class="stat-title">Function Description</div>
                                        <div>
                                            {format!("{}", function.function_description.clone())}
                                        </div>
                                    </div>
                                    <div class="stat w-2/12">
                                        <div class="stat-title">Draft Edited At</div>
                                        <div>

                                            {format!(
                                                "{}",
                                                function.draft_edited_at.clone().format("%v").to_string(),
                                            )}

                                        </div>
                                    </div>
                                    <div class="stat w-2/12">
                                        <div class="stat-title">Published At</div>
                                        <div>

                                            {match function.published_at.clone() {
                                                Some(val) => val.format("%v").to_string(),
                                                None => "null".to_string(),
                                            }}

                                        </div>
                                    </div>

                                </div>

                                <div class="flex flex-row justify-between">
                                    <div
                                        role="tablist"
                                        class="flex-row tabs m-6 w-30 self-start tabs-lifted tabs-md"
                                    >
                                        <a
                                            role="tab"
                                            id=CodeTab::PublishedCode
                                                .get_str("id")
                                                .expect("ID not defined for Resolve tab")
                                            class=move || match selected_tab_rs.get() {
                                                CodeTab::PublishedCode => {
                                                    "tab tab-active [--tab-border-color:#a651f5] text-center"
                                                }
                                                _ => "tab",
                                            }

                                            on:click=move |_| {
                                                selected_tab_ws.set(CodeTab::PublishedCode);
                                                publish_error_ws.set("".to_string());
                                                set_timeout(
                                                    || {
                                                        get_element_by_id::<HtmlButtonElement>("resolve_btn")
                                                            .map(|btn| btn.click());
                                                    },
                                                    Duration::new(1, 0),
                                                );
                                            }
                                        >

                                            Published Code
                                        </a>
                                        <a
                                            role="tab"
                                            id=CodeTab::DraftCode
                                                .get_str("id")
                                                .expect("ID not defined for Resolve tab")
                                            class=move || match selected_tab_rs.get() {
                                                CodeTab::DraftCode => {
                                                    "tab tab-active [--tab-border-color:orange] text-center"
                                                }
                                                _ => "tab",
                                            }

                                            on:click=move |_| {
                                                selected_tab_ws.set(CodeTab::DraftCode);
                                                publish_error_ws.set("".to_string());
                                                set_timeout(
                                                    || {
                                                        get_element_by_id::<HtmlButtonElement>("resolve_btn")
                                                            .map(|btn| btn.click());
                                                    },
                                                    Duration::new(1, 0),
                                                );
                                            }
                                        >

                                            Draft Code
                                        </a>
                                    </div>
                                    <div>
                                        {move || {
                                            selected_tab_rs
                                                .with(|tab| {
                                                    match tab {
                                                        CodeTab::PublishedCode => {
                                                            view! {
                                                                <div>
                                                                    <Show when=move || { test_mode_rs.get() == true }>
                                                                        <div class="flex flex-row justify-end join m-5">
                                                                            <button
                                                                                class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lgont-medium rounded-lg text-sm px-5 py-2.5 text-center"
                                                                                on:click=move |_| { test_mode_ws.set(false) }
                                                                            >
                                                                                <i class="gmdi-cancel-presentation-o"></i>
                                                                                Cancel

                                                                            </button>
                                                                        </div>

                                                                    </Show>

                                                                    <Show when=move || { test_mode_rs.get() == false }>
                                                                        <div class="flex flex-row justify-end join m-5">

                                                                            <button
                                                                                class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lgont-medium rounded-lg text-sm px-5 py-2.5 text-center"
                                                                                on:click=move |_| { test_mode_ws.set(true) }
                                                                            >
                                                                                <i class="fontisto-test-tube-alt"></i>
                                                                                Test
                                                                            </button>
                                                                        </div>

                                                                    </Show>
                                                                </div>
                                                            }
                                                        }
                                                        CodeTab::DraftCode => {
                                                            let publish_click_ef = publish_click.clone();
                                                            view! {
                                                                <div>
                                                                    <Show when=move || { test_mode_rs.get() == true }>
                                                                        <div class="flex flex-row justify-end join m-5">
                                                                            <button
                                                                                class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lgont-medium rounded-lg text-sm px-5 py-2.5 text-center"
                                                                                on:click=move |_| {
                                                                                    test_mode_ws.set(false);
                                                                                }
                                                                            >

                                                                                <i class="gmdi-cancel-presentation-o"></i>
                                                                                Cancel

                                                                            </button>
                                                                        </div>
                                                                    </Show>

                                                                    <Show when=move || { editor_mode_rs.get() == false }>
                                                                        <div class="flex flex-row justify-end join m-5">
                                                                            <button
                                                                                class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lgont-medium rounded-lg text-sm px-5 py-2.5 text-center"
                                                                                on:click=move |_| {
                                                                                    editor_mode_ws.set(true);
                                                                                }
                                                                            >

                                                                                <i class="gmdi-cancel-presentation-o"></i>
                                                                                Cancel

                                                                            </button>
                                                                        </div>

                                                                    </Show>
                                                                    <div class="flex flex-row justify-end join m-5">
                                                                        <Show when=move || {
                                                                            editor_mode_rs.get() == true && test_mode_rs.get() == false
                                                                                && show_publish_rs.get()
                                                                        }>
                                                                            <div class="flex">
                                                                                <p class="text-red-500">{move || publish_error_rs.get()}</p>
                                                                            </div>
                                                                            <button
                                                                                class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lgont-medium rounded-lg text-sm px-5 py-2.5 text-center"
                                                                                on:click=publish_click_ef.clone()
                                                                            >
                                                                                <i class="fontisto-test-tube-alt"></i>
                                                                                Publish
                                                                            </button>

                                                                        </Show>
                                                                        <Show when=move || {
                                                                            editor_mode_rs.get() == true && test_mode_rs.get() == false
                                                                        }>

                                                                            <button
                                                                                class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lgont-medium rounded-lg text-sm px-5 py-2.5 text-center"
                                                                                on:click=move |_| {
                                                                                    editor_mode_ws.set(false);
                                                                                    publish_error_ws.set("".to_string())
                                                                                }
                                                                            >

                                                                                <i class="ri-edit-line"></i>
                                                                                Edit

                                                                            </button>

                                                                            <button
                                                                                class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lgont-medium rounded-lg text-sm px-5 py-2.5 text-center"
                                                                                on:click=move |_| {
                                                                                    test_mode_ws.set(true);
                                                                                    publish_error_ws.set("".to_string())
                                                                                }
                                                                            >

                                                                                <i class="fontisto-test-tube-alt"></i>
                                                                                Test
                                                                            </button>

                                                                        </Show>
                                                                    </div>
                                                                </div>
                                                            }
                                                        }
                                                    }
                                                })
                                        }}

                                    </div>

                                </div>

                                {move || {
                                    let is_edit = editor_mode_rs.get();
                                    let is_test = test_mode_rs.get();
                                    let should_show = (editor_mode_rs.get() == true)
                                        && (test_mode_rs.get() == false);
                                    let fun_clone = function_data.clone();
                                    let fun_clone_ = function_data.clone();
                                    let pub_code = fun_clone
                                        .published_code
                                        .clone()
                                        .unwrap_or("//Code not published yet".to_string());
                                    selected_tab_rs
                                        .with(|tab| {
                                            match tab {
                                                CodeTab::PublishedCode => {
                                                    view! {
                                                        <Suspense fallback=move || {
                                                            view! { <p>"Loading (Suspense Fallback)..."</p> }
                                                        }>

                                                            {
                                                                let fun_pub = fun_clone.clone();
                                                                view! {
                                                                    <script type="module">
                                                                        {format!(
                                                                            r#"
                      
                                                              import * as monaco from 'https://cdn.jsdelivr.net/npm/monaco-editor@0.39.0/+esm';
                                                            
                                                              monaco.editor.create(document.querySelector('.monaco'), {{
                      
                                                                  value: `{pub_code}`,
                                                                  language: 'javascript',
                                                                  readOnly: true
                                                              }});
                                                              "#,
                                                                        )}

                                                                    </script>

                                                                    <Show when=move || { is_test == false }>

                                                                        <div class="monaco" style="min-height: 500px"></div>

                                                                    </Show>

                                                                    <Show when=move || { test_mode_rs.get() == true }>
                                                                        <div class="flex-row">

                                                                            <TestForm
                                                                                function_name=fun_pub.function_name.clone()
                                                                                stage="PUBLISHED".to_string()
                                                                                handle_submit=move || {}
                                                                            />

                                                                        </div>

                                                                    </Show>
                                                                }
                                                                    .into_view()
                                                            }

                                                        </Suspense>
                                                    }
                                                        .into_view()
                                                }
                                                CodeTab::DraftCode => {
                                                    view! {
                                                        <Suspense fallback=move || {
                                                            view! { <p>"Loading..."</p> }
                                                        }>

                                                            {
                                                                let function_edit = fun_clone_.clone();
                                                                let function_test = fun_clone_.clone();
                                                                let fun_code = fun_clone_.draft_code.clone();
                                                                view! {
                                                                    <Show when=move || {
                                                                        editor_mode_rs.get() == false && test_mode_rs.get() == false
                                                                    }>
                                                                        <div class="flex-row">
                                                                            <FunctionEditor
                                                                                edit=true
                                                                                function_name=function_edit.function_name.clone()
                                                                                function=function_edit.draft_code.clone()
                                                                                runtime_version=function_edit.draft_runtime_version.clone()
                                                                                description=function_edit.function_description.clone()
                                                                                handle_submit=move || {
                                                                                    combined_resource.refetch();
                                                                                    editor_mode_ws.set(true)
                                                                                }
                                                                            />

                                                                        </div>
                                                                    </Show>

                                                                    <script type="module">
                                                                        {format!(
                                                                            r#"
                    
                                                            import * as monaco from 'https://cdn.jsdelivr.net/npm/monaco-editor@0.39.0/+esm';
                                                          
                                                            window.editor = monaco.editor.create(document.querySelector('.monaco'), {{
                    
                                                                value: `{fun_code}`,
                                                                language: 'javascript',
                                                                readOnly: {is_edit}
                                                            }});
                    
                                                            const form = document.getElementById("MyForm");
                                                            form.addEventListener("formdata", e =>
                                                            {{
                                                                e.formData.set('function', window.editor.getValue());
                                                            }});
                                                            
                                                            "#,
                                                                        )}

                                                                    </script>

                                                                    <Show when=move || { should_show }>

                                                                        <div class="monaco" style="min-height: 500px"></div>

                                                                    </Show>

                                                                    <Show when=move || {
                                                                        test_mode_rs.get() == true && editor_mode_rs.get() == true
                                                                    }>
                                                                        <div class="flex-row">

                                                                            <TestForm
                                                                                function_name=function_test.function_name.clone()
                                                                                stage="DRAFT".to_string()
                                                                                handle_submit=move || {}
                                                                            />

                                                                        </div>

                                                                    </Show>
                                                                }
                                                                    .into_view()
                                                            }

                                                        </Suspense>
                                                    }
                                                }
                                            }
                                        })
                                }}

                            </div>
                        }
                            .into_view()
                    }
                    None => view! { <h1>Error fetching function</h1> }.into_view(),
                }
            }}

        </Transition>
    }
}
