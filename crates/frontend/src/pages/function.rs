pub mod function_create;
pub mod function_list;
pub mod publish_form;
pub mod utils;

use std::ops::Deref;

use chrono::{DateTime, Utc};
use leptos::*;
use leptos_router::use_params_map;
use publish_form::PublishForm;
use strum::IntoEnumIterator;
use superposition_types::{
    api::functions::Stage,
    database::models::{cac::FunctionType, ChangeReason, Description},
};

use crate::components::{
    function_form::FunctionEditor,
    skeleton::{Skeleton, SkeletonVariant},
};
use crate::types::{OrganisationId, Tenant};
use crate::utils::to_title_case;
use crate::{api::fetch_function, components::function_form::Mode};

fn stage_to_action(stage: Stage) -> String {
    match stage {
        Stage::Published => "Published".to_string(),
        Stage::Draft => "Draft Edited".to_string(),
    }
}

#[component]
fn function_info(
    function_type: FunctionType,
    description: Description,
    change_reason: ChangeReason,
    last_modified_by: String,
    last_modified_at: DateTime<Utc>,
) -> impl IntoView {
    view! {
        <div class="card bg-base-100 max-w-screen shadow">
            <div class="card-body flex flex-row gap-2 flex-wrap">
                <div class="h-fit w-[250px]">
                    <div class="stat-title">"Function Type"</div>
                    <div class="stat-value text-sm">{function_type.to_string()}</div>
                </div>
                <div class="h-fit w-[250px]">
                    <div class="stat-title">"Description"</div>
                    <div
                        class="tooltip tooltip-left w-[inherit] text-left"
                        data-tip=String::from(&description)
                    >
                        <div class="stat-value text-sm text-ellipsis overflow-hidden">
                            {String::from(&description)}
                        </div>
                    </div>
                </div>

                <div class="h-fit w-[250px]">
                    <div class="stat-title">"Last Modified by"</div>
                    <div class="stat-value text-sm">{last_modified_by}</div>
                </div>
                <div class="h-fit w-[250px]">
                    <div class="stat-title">"Last Modified at"</div>
                    <div class="stat-value text-sm">
                        {last_modified_at.format("%v %T").to_string()}
                    </div>
                </div>
                <div class="h-fit w-[250px]">
                    <div class="stat-title">"Change Reason"</div>
                    <div
                        class="tooltip tooltip-left w-[inherit] text-left"
                        data-tip=String::from(&change_reason)
                    >
                        <div class="stat-value text-sm text-ellipsis overflow-hidden">
                            {String::from(&change_reason)}
                        </div>
                    </div>
                </div>
            </div>
        </div>
    }
}

#[component]
fn function_code_info(
    #[prop(into)] version: Option<String>,
    action_time: Option<DateTime<Utc>>,
    action_by: Option<String>,
    tab_type: Stage,
) -> impl IntoView {
    if version.is_none() {
        return ().into_view();
    }
    let action_type = stage_to_action(tab_type);
    view! {
        <div class="card bg-base-100 max-w-screen shadow">
            <div class="card-body flex flex-row gap-2 flex-wrap">
                <div class="h-fit w-[250px]">
                    <div class="stat-title">"Version"</div>
                    <div class="stat-value text-sm">{version}</div>
                </div>
                {action_by
                    .map(|by| {
                        view! {
                            <div class="h-fit w-[250px]">
                                <div class="stat-title">{format!("{action_type} By")}</div>
                                <div class="stat-value text-sm">{by}</div>
                            </div>
                        }
                    })}
                {action_time
                    .map(|time| {
                        view! {
                            <div class="h-fit w-[250px]">
                                <div class="stat-title">{format!("{action_type} At")}</div>
                                <div class="stat-value text-sm">
                                    {time.format("%v %T").to_string()}
                                </div>
                            </div>
                        }
                    })}
            </div>
        </div>
    }.into_view()
}

#[component]
pub fn function_page() -> impl IntoView {
    let function_params = use_params_map();
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let function_name = StoredValue::new(function_params.with_untracked(|params| {
        params.get("function_name").cloned().unwrap_or("1".into())
    }));
    let show_publish_popup = RwSignal::new(false);

    let selected_tab_rws = RwSignal::new(Stage::Published);
    let mode_rws = RwSignal::new(Mode::Viewer);

    let function_resource = create_blocking_resource(
        move || (function_name.get_value(), workspace.get().0, org.get().0),
        |(function_name, workspace, org_id)| async move {
            let function_result =
                fetch_function(function_name.to_string(), workspace.to_string(), org_id)
                    .await;

            function_result.ok()
        },
    );

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::DetailPage /> }
        }>
            <div class="flex flex-col gap-4">
                {move || {
                    let function = match function_resource.get() {
                        Some(Some(func)) => func,
                        _ => return view! { <h1>Error fetching function</h1> }.into_view(),
                    };
                    let function_st = StoredValue::new(function.clone());
                    view! {
                        <h1 class="text-2xl font-extrabold">{function.function_name.clone()}</h1>
                        <FunctionInfo
                            function_type=function.function_type
                            description=function.description.clone()
                            change_reason=function.change_reason.clone()
                            last_modified_by=function.draft_edited_by.clone()
                            last_modified_at=function.draft_edited_at
                        />
                        <div class="flex justify-between">
                            <div role="tablist" class="tabs tabs-lifted">
                                {Stage::iter()
                                    .map(|tab| {
                                        view! {
                                            <a
                                                role="tab"
                                                class=move || {
                                                    if selected_tab_rws.get() == tab {
                                                        "tab tab-active [--tab-border-color:#a651f5] text-center font-bold"
                                                    } else {
                                                        "tab text-center font-bold"
                                                    }
                                                }
                                                on:click=move |_| {
                                                    selected_tab_rws.set(tab);
                                                    mode_rws.set(Mode::Viewer);
                                                }
                                            >
                                                {to_title_case(&tab.to_string())}
                                            </a>
                                        }
                                    })
                                    .collect_view()}
                            </div>
                            <div class="h-12 flex gap-2">
                                <div class="flex justify-end join">
                                    <Show when=move || {
                                        selected_tab_rws.get() == Stage::Draft
                                            && mode_rws.get() == Mode::Viewer
                                            && function
                                                .published_at
                                                .is_none_or(|val| val < function.draft_edited_at)
                                    }>
                                        <button
                                            class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg font-medium rounded-lg text-sm px-5 py-2.5 text-center"
                                            on:click=move |_| show_publish_popup.set(true)
                                        >
                                            <i class="ri-article-line" />
                                            Publish
                                        </button>
                                    </Show>
                                    <Show when=move || {
                                        selected_tab_rws.get() == Stage::Draft
                                            && mode_rws.get() == Mode::Viewer
                                    }>
                                        <button
                                            class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg font-medium rounded-lg text-sm px-5 py-2.5 text-center"
                                            on:click=move |_| mode_rws.set(Mode::Editor)
                                        >
                                            <i class="ri-edit-line" />
                                            Edit
                                        </button>
                                    </Show>
                                    <Show when=move || mode_rws.get() == Mode::Viewer>
                                        <button
                                            class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg font-medium rounded-lg text-sm px-5 py-2.5 text-center"
                                            on:click=move |_| mode_rws.set(Mode::Test)
                                        >
                                            <i class="ri-microscope-line" />
                                            Test
                                        </button>
                                    </Show>
                                </div>
                            </div>
                        </div>
                        {
                            let selected_tab = selected_tab_rws.get();
                            let (version, action_time, action_by) = match selected_tab {
                                Stage::Published => {
                                    (
                                        function.published_runtime_version.clone(),
                                        function.published_at,
                                        function.published_by.clone(),
                                    )
                                }
                                Stage::Draft => {
                                    (
                                        Some(function.draft_runtime_version.clone()),
                                        Some(function.draft_edited_at),
                                        Some(function.draft_edited_by.clone()),
                                    )
                                }
                            };

                            view! {
                                <FunctionCodeInfo
                                    version
                                    action_time
                                    action_by
                                    tab_type=selected_tab
                                />
                            }
                        }
                        <FunctionEditor
                            function_name=function_st.with_value(|f| f.function_name.clone())
                            function=function_st
                                .with_value(|f| match selected_tab_rws.get() {
                                    Stage::Published => {
                                        f.published_code
                                            .clone()
                                            .map(String::from)
                                            .unwrap_or_else(|| {
                                                "// Code not published yet, publish function to see it here!"
                                                    .to_string()
                                            })
                                    }
                                    Stage::Draft => String::from(f.draft_code.clone()),
                                })
                            function_type=function_st.with_value(|f| f.function_type)
                            runtime_version=function_st
                                .with_value(|f| f.draft_runtime_version.clone())
                            description=function_st
                                .with_value(|f| f.description.deref().to_string())
                            handle_submit=move |_| {
                                function_resource.refetch();
                                mode_rws.set(Mode::Viewer);
                            }
                            mode=mode_rws
                            selected_tab=selected_tab_rws
                            on_cancel=move |_| mode_rws.set(Mode::Viewer)
                        />
                        <Show when=move || show_publish_popup.get()>
                            <PublishForm
                                function_name=function_st.with_value(|f| f.function_name.clone())
                                handle_submit=move |_| {
                                    show_publish_popup.set(false);
                                    function_resource.refetch()
                                }
                                handle_close=move |_| show_publish_popup.set(false)
                            />
                        </Show>
                    }
                        .into_view()
                }}
            </div>
        </Suspense>
    }
}
