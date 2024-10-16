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

use crate::components::{
    function_form::{FunctionEditor, TestForm},
    monaco_editor::MonacoEditor,
};

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

// TODO: Just rewrite this file and everything related to functions

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
            view! {
                <div class="m-5">
                    <Skeleton variant=SkeletonVariant::DetailPage/>
                </div>
            }
        }>
            {move || {
                let resource = match combined_resource.get() {
                    Some(res) => res,
                    None => return view! { <h1>Error fetching function</h1> }.into_view(),
                };
                let function = resource.function;
                match function {
                    Some(function) => {
                        let (function_rs, _) = create_signal(function);
                        publish_error_ws.set("".to_string());
                        match function_rs.get().published_at {
                            Some(val) => {
                                show_publish_ws.set(val < function_rs.get().draft_edited_at)
                            }
                            None => show_publish_ws.set(true),
                        }
                        let publish_click = move |event: MouseEvent| {
                            event.prevent_default();
                            logging::log!("Submitting function form");
                            let tenant = tenant_rs.get();
                            let f_function_name = function_rs.get().function_name;
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
                                        <div>{function_rs.get().function_name}</div>
                                    </div>
                                    <div class="stat w-2/12">
                                        <div class="stat-title">Published Runtime Version</div>
                                        <div>
                                            {function_rs
                                                .get()
                                                .published_runtime_version
                                                .unwrap_or("null".to_string())}
                                        </div>
                                    </div>
                                    <div class="stat w-2/12">
                                        <div class="stat-title">Function Description</div>
                                        <div>
                                            {function_rs.get().function_description.to_string()}
                                        </div>
                                    </div>
                                    <div class="stat w-2/12">
                                        <div class="stat-title">Draft Edited At</div>
                                        <div>

                                            {format!(
                                                "{}",
                                                function_rs.get().draft_edited_at.format("%v"),
                                            )}

                                        </div>
                                    </div>
                                    <div class="stat w-2/12">
                                        <div class="stat-title">Published At</div>
                                        <div>

                                            {match function_rs.get().published_at {
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
                                            selected_tab_rs
                                                .with(|tab| {
                                                    match tab {
                                                        CodeTab::PublishedCode => {
                                                            view! {
                                                                <div>
                                                                    <Show when=move || { test_mode_rs.get() }>
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

                                                                    <Show when=move || { !test_mode_rs.get() }>
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
                                                                    <Show when=move || { test_mode_rs.get() }>
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

                                                                    <Show when=move || { !editor_mode_rs.get() }>
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
                                                                            editor_mode_rs.get() && !test_mode_rs.get()
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
                                                                            editor_mode_rs.get() && !test_mode_rs.get()
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
                                    let should_show = editor_mode_rs.get() && !test_mode_rs.get();
                                    selected_tab_rs
                                        .with(|tab| {
                                            match tab {
                                                CodeTab::PublishedCode => {
                                                    view! {
                                                        <Suspense fallback=move || {
                                                            view! { <p>Loading ...</p> }
                                                        }>

                                                            {
                                                                let (fun_code_rs, fun_code_ws) = create_signal(
                                                                    function_rs
                                                                        .get()
                                                                        .published_code
                                                                        .unwrap_or(
                                                                            "// Code not published yet, publish function to see it here!"
                                                                                .to_string(),
                                                                        ),
                                                                );
                                                                let on_change = move |value| fun_code_ws.set(value);
                                                                view! {
                                                                    <Show when=move || { !is_test }>
                                                                        <MonacoEditor
                                                                            node_id="pub_editor_fn"
                                                                            data=fun_code_rs.get_untracked()
                                                                            on_change=on_change
                                                                            read_only=is_edit
                                                                            classes=vec!["min-h-[500px]"]
                                                                        />
                                                                    </Show>

                                                                    <Show when=move || { test_mode_rs.get() }>
                                                                        <div class="flex-row">

                                                                            <TestForm
                                                                                function_name=function_rs.get().function_name
                                                                                stage="PUBLISHED".to_string()
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
                                                            view! { <p>Loading ...</p> }
                                                        }>

                                                            {
                                                                let (fun_code_rs, fun_code_ws) = create_signal(
                                                                    function_rs.get().draft_code,
                                                                );
                                                                let on_change = move |value| fun_code_ws.set(value);
                                                                view! {
                                                                    <Show when=move || {
                                                                        !editor_mode_rs.get() && !test_mode_rs.get()
                                                                    }>
                                                                        <div class="flex-row">
                                                                            <FunctionEditor
                                                                                edit=true
                                                                                function_name=function_rs.get().function_name
                                                                                function=function_rs.get().draft_code
                                                                                runtime_version=function_rs.get().draft_runtime_version
                                                                                description=function_rs.get().function_description
                                                                                handle_submit=move || {
                                                                                    combined_resource.refetch();
                                                                                    editor_mode_ws.set(true)
                                                                                }
                                                                            />

                                                                        </div>
                                                                    </Show>

                                                                    <Show when=move || { should_show }>
                                                                        <MonacoEditor
                                                                            node_id="code_editor_fn"
                                                                            data=fun_code_rs.get_untracked()
                                                                            on_change=on_change
                                                                            read_only=is_edit
                                                                            classes=vec!["min-h-[500px]"]
                                                                        />

                                                                    </Show>

                                                                    <Show when=move || {
                                                                        test_mode_rs.get() && editor_mode_rs.get()
                                                                    }>
                                                                        <div class="flex">
                                                                            <div class="flex flex-row justify-between">
                                                                                <MonacoEditor
                                                                                    node_id="test_editor_fn"
                                                                                    data=fun_code_rs.get_untracked()
                                                                                    on_change=on_change
                                                                                    read_only=true
                                                                                    classes=vec!["min-w-[1000px]", "min-h-[500px]", "mr-5"]
                                                                                />
                                                                                <TestForm
                                                                                    function_name=function_rs.get().function_name
                                                                                    stage="DRAFT".to_string()
                                                                                />
                                                                            </div>
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
