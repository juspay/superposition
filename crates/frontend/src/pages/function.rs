pub mod function_create;
pub mod function_list;
pub mod utils;

use leptos::*;
use leptos_router::use_params_map;

use crate::{
    components::skeleton::{Skeleton, SkeletonVariant},
    types::FunctionResponse,
};

use self::utils::publish_function;

use crate::api::fetch_function;
use std::time::Duration;
use strum::EnumProperty;
use strum_macros::Display;
use web_sys::MouseEvent;

use crate::utils::get_element_by_id;
use web_sys::HtmlButtonElement;

use crate::components::function_form::{FunctionEditor, TestForm};

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
fn publish_view(function: StoredValue<FunctionResponse>) -> impl IntoView {
    let (test_mode, set_test_mode) = create_signal(false);

    let FunctionResponse {
        function_name,
        published_code,
        ..
    } = function.get_value();
    let published_code = published_code.unwrap_or_default();

    view! {
        <div>
            <Show when=move || { test_mode.get() }>
                <div class="flex flex-row justify-end join m-5">
                    <button
                        class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lgont-medium rounded-lg text-sm px-5 py-2.5 text-center"
                        on:click=move |_| { set_test_mode.set(false) }
                    >
                        <i class="gmdi-cancel-presentation-o"></i>
                        Cancel

                    </button>
                </div>
            </Show>

            <Show when=move || { !test_mode.get() }>
                <div class="flex flex-row justify-end join m-5">

                    <button
                        class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lgont-medium rounded-lg text-sm px-5 py-2.5 text-center"
                        on:click=move |_| { set_test_mode.set(true) }
                    >
                        <i class="fontisto-test-tube-alt"></i>
                        Test
                    </button>
                </div>

            </Show>
        </div>
        <div>
            <script type="module">
                      {format!(
                          r#"

            import * as monaco from 'https://cdn.jsdelivr.net/npm/monaco-editor@0.39.0/+esm';

            monaco.editor.create(document.querySelector('.monaco'), {{

                value: `{published_code}`,
                language: 'javascript',
                readOnly: true
            }});
            "#,
                      )}

            </script>

            <Show when=move || { !test_mode.get() }>
                <div class="monaco" style="min-height: 500px"></div>
            </Show>

            <Show when=move || { test_mode.get() }>
                <div class="flex-row">

                    <TestForm
                        function_name=function_name.clone()
                        stage="PUBLISHED".to_string()
                    />

                </div>

            </Show>
        </div>
    }
}

// Take handle publish and handle submit callbacks as props
#[component]
fn draft_view(
    show_publish: bool,
    publish_click: Callback<String, ()>,
    function: StoredValue<FunctionResponse>,
) -> impl IntoView {
    let (read_only, set_read_only) = create_signal(false);
    let (test_mode, set_test_mode) = create_signal(false);

    view! {
        <div>
            <Show when=move || { test_mode.get() }>
                <div class="flex flex-row justify-end join m-5">
                    <button
                        class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lgont-medium rounded-lg text-sm px-5 py-2.5 text-center"
                        on:click=move |_| {
                            set_test_mode.set(false);
                        }
                    >

                        <i class="gmdi-cancel-presentation-o"></i>
                        Cancel

                    </button>
                </div>
            </Show>

            <Show when=move || { !read_only.get() }>
                <div class="flex flex-row justify-end join m-5">
                    <button
                        class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lgont-medium rounded-lg text-sm px-5 py-2.5 text-center"
                        on:click=move |_| {
                            set_read_only.set(true);
                        }
                    >

                        <i class="gmdi-cancel-presentation-o"></i>
                        Cancel

                    </button>
                </div>

            </Show>
            <div class="flex flex-row justify-end join m-5">
                <Show when=move || {
                    read_only.get() && !test_mode.get()
                        && show_publish
                }>
                    <div class="flex">
                        // <p class="text-red-500">{move || publish_error_rs.get()}</p>
                    </div>
                    <button
                        class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lgont-medium rounded-lg text-sm px-5 py-2.5 text-center"
                        on:click=move |e| { e.prevent_default(); publish_click.call(function.get_value().function_name.clone()) }
                    >
                        <i class="fontisto-test-tube-alt"></i>
                        Publish
                    </button>

                </Show>
                <Show when=move || {
                    read_only.get() && !test_mode.get()
                }>

                    <button
                        class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lgont-medium rounded-lg text-sm px-5 py-2.5 text-center"
                        on:click=move |_| {
                            set_read_only.set(false);
                            // publish_error_ws.set("".to_string())
                        }
                    >

                        <i class="ri-edit-line"></i>
                        Edit

                    </button>

                    <button
                        class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lgont-medium rounded-lg text-sm px-5 py-2.5 text-center"
                        on:click=move |_| {
                            set_test_mode.set(true);
                            // publish_error_ws.set("".to_string())
                        }
                    >

                        <i class="fontisto-test-tube-alt"></i>
                        Test
                    </button>

                </Show>
            </div>
        </div>
        <div>
            <Show when=move || {
                !read_only.get() && !test_mode.get()
            }>
                <div class="flex-row">
                    <FunctionEditor
                        edit=true
                        function_name=function.get_value().function_name.clone()
                        function=function.get_value().draft_code.clone()
                        runtime_version=function.get_value().draft_runtime_version.clone()
                        description=function.get_value().function_description.clone()
                        handle_submit=move || {
                            // combined_resource.refetch();
                            set_read_only.set(true)
                        }
                    />

                </div>
            </Show>

            {move || {
                let code = function.get_value().draft_code;
                let read_only = read_only.get();
                view! {
                    <script type="module">
                        {format!(
                            r#"
                            import * as monaco from 'https://cdn.jsdelivr.net/npm/monaco-editor@0.39.0/+esm';

                            window.editor = monaco.editor.create(document.querySelector('.monaco'), {{

                                value: `{code}`,
                                language: 'javascript',
                                readOnly: {read_only}
                            }});

                            const form = document.getElementById("MyForm");
                            form.addEventListener("formdata", e =>
                            {{
                                e.formData.set('function', window.editor.getValue());
                            }});

                            "#,
                        )}
                    </script>
                }
            }}

            <Show when=move || { read_only.get() && !test_mode.get() }>

                <div class="monaco" style="min-height: 500px"></div>

            </Show>

            <Show when=move || {
                test_mode.get() && read_only.get()
            }>
                <div class="flex-row">

                    <TestForm
                        function_name=function.get_value().function_name
                        stage="DRAFT".to_string()
                    />

                </div>

            </Show>
        </div>
    }
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
    let (publish_error_rs, publish_error_ws) = create_signal("".to_string());

    let page_resource: Resource<(String, String), Option<FunctionResponse>> =
        create_blocking_resource(source, |(function_name, tenant)| async move {
            fetch_function(function_name.to_string(), tenant.to_string())
                .await
                .ok()
        });

    let publish_click = Callback::new(move |function_name| {
        logging::log!("Submitting function form");
        let tenant = tenant_rs.get();
        spawn_local({
            async move {
                let result = publish_function(function_name, tenant).await;
                match result {
                    Ok(_) => {
                        publish_error_ws.set("".to_string());
                        page_resource.refetch();
                    }
                    Err(e) => {
                        publish_error_ws.set(e);
                    }
                }
            }
        });
    });

    view! {
        <Transition fallback=move || {
            view! {
                <div class="m-5">
                    <Skeleton variant=SkeletonVariant::DetailPage/>
                </div>
            }
        }>
            {move || {
                let function = page_resource.get().flatten();

                if function.is_none() {
                    return view! { <h1>Error fetching function</h1> }.into_view();
                }

                let function = store_value(function.unwrap());
                let FunctionResponse { function_name, function_description, published_runtime_version, published_at, draft_edited_at, .. } = function.get_value();
                view! {
                    <div class="flex flex-col flex-row overflow-x-auto p-2 bg-transparent">

                        <div class="flex bg-base-100 flex-row gap-3 justify-between flex-wrap shadow m-5">
                            <div class="stat w-2/12">
                                <div class="stat-title">Function Name</div>
                                <div>{function_name}</div>
                            </div>
                            <div class="stat w-2/12">
                                <div class="stat-title">Published Runtime Version</div>
                                <div>
                                    {
                                        published_runtime_version
                                        .unwrap_or("-".to_string())}
                                </div>
                            </div>
                            <div class="stat w-2/12">
                                <div class="stat-title">Function Description</div>
                                <div>
                                    {function_description}
                                </div>
                            </div>
                            <div class="stat w-2/12">
                                <div class="stat-title">Draft Edited At</div>
                                <div>
                                    {format!(
                                        "{}",
                                        draft_edited_at.format("%v"),
                                    )}
                                </div>
                            </div>
                            <div class="stat w-2/12">
                                <div class="stat-title">Published At</div>
                                <div>
                                    {match published_at {
                                        Some(val) => val.format("%v").to_string(),
                                        None => "-".to_string(),
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
                                                if let Some(btn) = get_element_by_id::<
                                                    HtmlButtonElement,
                                                >("resolve_btn") {
                                                    btn.click()
                                                }
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
                                                if let Some(btn) = get_element_by_id::<
                                                    HtmlButtonElement,
                                                >("resolve_btn") {
                                                    btn.click()
                                                }
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
                                   let selected_tab = selected_tab_rs.get();
                                   let FunctionResponse { published_at, draft_edited_at, .. } = function.get_value();
                                   let show_publish = match published_at {
                                       Some(val) => val < draft_edited_at,
                                       None => true
                                   };
                                   match selected_tab {
                                       CodeTab::PublishedCode => {
                                           view! { <PublishView
                                               function=function
                                               /> }
                                       },
                                       CodeTab::DraftCode => {
                                           view! {
                                               <DraftView function=function publish_click=publish_click show_publish=show_publish  />
                                           }
                                       }
                                   }
                                }}
                            </div>
                        </div>
                    </div>
                }.into_view()
            }}
        </Transition>
    }
}
