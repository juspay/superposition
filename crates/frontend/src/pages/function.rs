pub mod function_create;
pub mod function_list;
pub mod publish_form;
pub mod types;
pub mod utils;

use std::ops::Deref;

use chrono::{DateTime, Utc};
use leptos::*;
use leptos_router::{use_params_map, A};
use publish_form::PublishForm;
use strum::IntoEnumIterator;
use superposition_macros::box_params;
use superposition_types::{
    api::functions::Stage,
    custom_query::{CustomQuery, Query},
};
use types::PageParams;

use crate::api::fetch_function;
use crate::components::{
    button::Button,
    description::ContentDescription,
    function_form::{FunctionEditor, Mode},
    skeleton::{Skeleton, SkeletonVariant},
};
use crate::query_updater::{
    use_param_updater, use_signal_from_query, use_update_url_query,
};
use crate::types::{OrganisationId, Tenant};
use crate::utils::to_title_case;

fn stage_to_action(stage: Stage) -> String {
    match stage {
        Stage::Published => "Published".to_string(),
        Stage::Draft => "Draft Edited".to_string(),
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
    let path_params = use_params_map();
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let function_name = Memo::new(move |_| {
        path_params
            .with(|params| params.get("function_name").cloned().unwrap_or("1".into()))
    });
    let show_publish_popup = RwSignal::new(false);

    let page_params_rws = use_signal_from_query(move |query_string| {
        Query::<PageParams>::extract_non_empty(&query_string).into_inner()
    });
    let mode_rws = RwSignal::new(Mode::Viewer);
    use_param_updater(move || box_params!(page_params_rws.get()));

    let function_resource = create_blocking_resource(
        move || (function_name.get(), workspace.get().0, org.get().0),
        |(function_name, workspace, org_id)| async move {
            let function_result = fetch_function(function_name, workspace, org_id).await;

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
                        <ContentDescription
                            pre_data=move || {
                                view! {
                                    <div class="h-fit w-[250px]">
                                        <div class="stat-title">"Function Type"</div>
                                        <div class="stat-value text-sm">
                                            {function.function_type.to_string()}
                                        </div>
                                    </div>
                                }
                            }
                            description=function.description.clone()
                            change_reason=function.change_reason.clone()
                            created_by=function.created_by.clone()
                            created_at=function.created_at
                            last_modified_by=function.last_modified_by.clone()
                            last_modified_at=function.last_modified_at
                        />
                        <div class="flex justify-between">
                            <div role="tablist" class="tabs tabs-lifted">
                                {Stage::iter()
                                    .map(|tab| {
                                        let get_updated_query = use_update_url_query();
                                        view! {
                                            <A
                                                href=get_updated_query("tab", Some(tab.to_string()))
                                                attr:role="tab"
                                                class=move || {
                                                    if page_params_rws.with(|p| p.tab == tab) {
                                                        "tab tab-active [--tab-border-color:#a651f5] text-center font-bold"
                                                    } else {
                                                        "tab text-center font-bold"
                                                    }
                                                }
                                                on:click=move |_| {
                                                    page_params_rws.update(|p| p.tab = tab);
                                                    mode_rws.set(Mode::Viewer);
                                                }
                                            >
                                                {to_title_case(&tab.to_string())}
                                            </A>
                                        }
                                    })
                                    .collect_view()}
                            </div>
                            <div class="h-12 flex gap-2">
                                <div class="flex justify-end join">
                                    <Show when=move || {
                                        page_params_rws.with(|p| p.tab == Stage::Draft)
                                            && mode_rws.get() == Mode::Viewer
                                            && function
                                                .published_at
                                                .is_none_or(|val| val < function.draft_edited_at)
                                    }>
                                        <Button
                                            force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg rounded-lg"
                                            on_click=move |_| show_publish_popup.set(true)
                                            icon_class="ri-article-line"
                                            text="Publish"
                                        />
                                    </Show>
                                    <Show when=move || {
                                        page_params_rws.with(|p| p.tab == Stage::Draft)
                                            && mode_rws.get() == Mode::Viewer
                                    }>
                                        <Button
                                            force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg rounded-lg"
                                            on_click=move |_| mode_rws.set(Mode::Editor)
                                            icon_class="ri-edit-line"
                                            text="Edit"
                                        />
                                    </Show>
                                    <Show when=move || mode_rws.get() == Mode::Viewer>
                                        <Button
                                            force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg rounded-lg"
                                            on_click=move |_| mode_rws.set(Mode::Test)
                                            icon_class="ri-microscope-line"
                                            text="Test"
                                        />
                                    </Show>
                                </div>
                            </div>
                        </div>
                        {
                            let selected_tab = page_params_rws.with(|p| p.tab);
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
                                .with_value(|f| match page_params_rws.with(|p| p.tab) {
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
                            selected_tab=Signal::derive(move || page_params_rws.with(|p| p.tab))
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
