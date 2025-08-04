use std::ops::Deref;

use leptos::*;
use leptos_router::{use_navigate, use_params_map, A};
use superposition_types::{
    api::dimension::DimensionResponse, database::models::cac::DependencyGraph,
};

use crate::api::{delete_dimension, get_dimension};
use crate::components::badge::Badge;
use crate::components::description::ContentDescription;
use crate::components::{
    alert::AlertType,
    button::Button,
    dimension_form::{ChangeLogSummary, ChangeType, DimensionForm},
    drawer::PortalDrawer,
    input::Toggle,
    input::{Input, InputType},
    skeleton::{Skeleton, SkeletonVariant},
};
use crate::providers::{alert_provider::enqueue_alert, editor_provider::EditorProvider};
use crate::schema::{JsonSchemaType, SchemaType};
use crate::types::{OrganisationId, Tenant};
use crate::utils::use_url_base;

#[component]
fn tree_node(
    name: String,
    data: DependencyGraph,
    #[prop(default = vec![])] parents_last: Vec<bool>,
    #[prop(default = false)] root: bool,
) -> impl IntoView {
    let mut prefix = String::new();
    for (i, is_last) in parents_last.iter().enumerate() {
        if i == parents_last.len() - 1 {
            prefix.push_str(if *is_last { "└── " } else { "├── " });
        } else {
            prefix.push_str(if *is_last { "    " } else { "│   " });
        }
    }

    let children = data.get(&name).cloned().unwrap_or_default();

    view! {
        <div class="flex whitespace-pre">
            <div class="text-gray-500 font-mono">{prefix}</div>
            <div class="w-full flex flex-row gap-2 items-center">
                {if root {
                    view! {
                        <div class="flex gap-2 font-medium">
                            {name} <span class="stat-title">"(current)"</span>
                        </div>
                    }
                        .into_view()
                } else {
                    view! {
                        <A
                            href=format!("../{name}")
                            class="underline text-blue-600 hover:text-blue-800 text-ellipsis overflow-hidden"
                        >
                            {name}
                        </A>
                    }
                        .into_view()
                }}
                {if children.is_empty() {
                    ().into_view()
                } else {
                    view! { <div class="stat-title w-full max-w-fit">"depends on:"</div> }
                        .into_view()
                }}
            </div>
        </div>
        {children
            .iter()
            .enumerate()
            .map(|(i, child)| {
                let mut new_parents_last = parents_last.clone();
                new_parents_last.push(i == children.len() - 1);
                view! {
                    <TreeNode name=child.clone() data=data.clone() parents_last=new_parents_last />
                }
            })
            .collect_view()}
    }
}

#[component]
fn dimension_info(dimension: DimensionResponse) -> impl IntoView {
    view! {
        <div class="card bg-base-100 max-w-screen shadow">
            <div class="card-body">
                <h2 class="card-title">"Info"</h2>
                <div class="flex flex-col gap-4">
                    <div class="flex flex-row gap-6 flex-wrap">
                        <div class="h-fit w-[250px] flex gap-4">
                            <div class="stat-title">"Position"</div>
                            <div class="stat-value text-base">{*dimension.position}</div>
                        </div>
                        <div class="h-fit w-[250px] flex gap-4">
                            <div class="stat-title">"Mandatory"</div>
                            <div class="stat-value text-base">
                                <Toggle value=dimension.mandatory disabled=true on_change=|_| {} />
                            </div>
                        </div>
                    </div>
                    {if dimension.function_name.is_some()
                        || dimension.autocomplete_function_name.is_some()
                    {
                        view! {
                            <div class="flex flex-row gap-6 flex-wrap">
                                {dimension
                                    .function_name
                                    .map(|name| {
                                        view! {
                                            <div class="h-fit w-[250px] flex gap-4">
                                                <div class="stat-title">"Validation Function"</div>
                                                <A
                                                    href=format!("../../function/{name}")
                                                    class="text-base text-blue-500 underline underline-offset-2"
                                                >
                                                    {name}
                                                </A>
                                            </div>
                                        }
                                    })}
                                {dimension
                                    .autocomplete_function_name
                                    .map(|name| {
                                        view! {
                                            <div class="h-fit w-[250px] flex gap-4">
                                                <div class="stat-title">"Autocomplete Function"</div>
                                                <A
                                                    href=format!("../../function/{name}")
                                                    class="text-base text-blue-500 underline underline-offset-2"
                                                >
                                                    {name}
                                                </A>
                                            </div>
                                        }
                                    })}
                            </div>
                        }
                            .into_view()
                    } else {
                        ().into_view()
                    }}
                    <div class="flex gap-4">
                        <div class="stat-title">"Schema"</div>
                        <EditorProvider>
                            <Input
                                disabled=true
                                id="type-schema"
                                class="rounded-md resize-y w-[28rem]"
                                schema_type=SchemaType::Single(JsonSchemaType::Object)
                                value=dimension.schema
                                on_change=move |_| {}
                                r#type=InputType::Monaco(vec![])
                            />
                        </EditorProvider>
                    </div>
                </div>
            </div>
        </div>
    }
}

#[derive(Clone)]
enum Action {
    None,
    Edit,
    Delete,
}

#[component]
pub fn dimension_page() -> impl IntoView {
    let path_params = use_params_map();
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let dimension_name = Memo::new(move |_| {
        path_params
            .with(|params| params.get("dimension_name").cloned().unwrap_or("1".into()))
    });
    let action_rws = RwSignal::new(Action::None);
    let delete_inprogress_rws = RwSignal::new(false);

    let dimension_resource = create_blocking_resource(
        move || (dimension_name.get(), workspace.get().0, org.get().0),
        |(dimension_name, workspace, org_id)| async move {
            get_dimension(&dimension_name, &workspace, &org_id)
                .await
                .ok()
        },
    );

    let confirm_delete = Callback::new(move |_| {
        delete_inprogress_rws.set(true);
        spawn_local(async move {
            let result = delete_dimension(
                dimension_name.get_untracked(),
                workspace.get_untracked().0,
                org.get_untracked().0,
            )
            .await;
            delete_inprogress_rws.set(false);
            match result {
                Ok(_) => {
                    logging::log!("Dimension deleted successfully");
                    let navigate = use_navigate();
                    let base = use_url_base();
                    let redirect_url = format!(
                        "{base}/admin/{}/{}/dimensions",
                        org.get().0,
                        workspace.get().0,
                    );
                    navigate(&redirect_url, Default::default());
                    enqueue_alert(
                        String::from("Dimension deleted successfully"),
                        AlertType::Success,
                        5000,
                    );
                }
                Err(e) => {
                    logging::error!("Error deleting dimension: {:?}", e);
                    enqueue_alert(e, AlertType::Error, 5000);
                }
            }
        });
    });

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::DetailPage /> }
        }>
            {move || {
                let dimension = match dimension_resource.get() {
                    Some(Some(dim)) => dim,
                    _ => return view! { <h1>"Error fetching dimension"</h1> }.into_view(),
                };
                let dimension_st = StoredValue::new(dimension.clone());
                view! {
                    <div class="flex flex-col gap-4">
                        <div class="flex justify-between items-center gap-2">
                            <h1 class="text-2xl font-extrabold text-ellipsis overflow-hidden">
                                {dimension.dimension.clone()}
                            </h1>
                            {if dimension.dimension != "variantIds" {
                                view! {
                                    <div class="w-full max-w-fit flex flex-row join">
                                        <Button
                                            force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg rounded-lg"
                                            on_click=move |_| action_rws.set(Action::Edit)
                                            icon_class="ri-edit-line"
                                            text="Edit"
                                        />
                                        <Button
                                            force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg rounded-lg"
                                            on_click=move |_| action_rws.set(Action::Delete)
                                            icon_class="ri-delete-bin-line"
                                            text="Delete"
                                        />
                                    </div>
                                }
                                    .into_view()
                            } else {
                                ().into_view()
                            }}
                        </div>
                        <ContentDescription
                            description=dimension.description.clone()
                            change_reason=dimension.change_reason.clone()
                            created_by=dimension.created_by.clone()
                            created_at=dimension.created_at
                            last_modified_by=dimension.last_modified_by.clone()
                            last_modified_at=dimension.last_modified_at
                        />
                        <DimensionInfo dimension=dimension.clone() />
                        {if dimension.dependency_graph.is_empty() && dimension.dependents.is_empty()
                        {
                            ().into_view()
                        } else {
                            view! {
                                <div class="card bg-base-100 max-w-screen shadow">
                                    <div class="card-body">
                                        <h2 class="card-title">"Dependency Data"</h2>
                                        <div class="flex flex-col gap-4">
                                            {if dimension.dependencies.is_empty() {
                                                ().into_view()
                                            } else {
                                                view! {
                                                    <div class="flex flex-row gap-6 flex-wrap">
                                                        <div class="h-fit flex flex-col gap-1 overflow-x-scroll">
                                                            <div class="stat-title">
                                                                "Dimensions on which this dimension depends on"
                                                            </div>
                                                            <div class="w-[inherit] pl-5 whitespace-pre overflow-x-auto">
                                                                <TreeNode
                                                                    name=dimension.dimension.clone()
                                                                    data=dimension.dependency_graph.clone()
                                                                    root=true
                                                                />
                                                            </div>
                                                        </div>
                                                    </div>
                                                }
                                                    .into_view()
                                            }}
                                            {if dimension.dependents.is_empty() {
                                                ().into_view()
                                            } else {
                                                view! {
                                                    <div class="flex flex-row gap-6 flex-wrap">
                                                        <div class="h-fit flex flex-col gap-1">
                                                            <div class="stat-title">
                                                                "Dimensions dependent on this dimension"
                                                            </div>
                                                            <Badge
                                                                href_fn=|d| format!("../{d}")
                                                                options=Signal::derive(move || {
                                                                    dimension.dependents.clone()
                                                                })
                                                            />
                                                        </div>
                                                    </div>
                                                }
                                                    .into_view()
                                            }}
                                        </div>
                                    </div>
                                </div>
                            }
                                .into_view()
                        }}
                    </div>
                    {match action_rws.get() {
                        Action::None => ().into_view(),
                        Action::Edit => {
                            view! {
                                <PortalDrawer
                                    title="Edit Dimension"
                                    handle_close=move |_| action_rws.set(Action::None)
                                >
                                    <DimensionForm
                                        edit=true
                                        position=dimension_st.with_value(|d| *d.position as u32)
                                        dimension_name=dimension_st
                                            .with_value(|d| d.dimension.clone())
                                        dimension_schema=dimension_st
                                            .with_value(|d| d.schema.clone())
                                        dependencies=dimension_st
                                            .with_value(|d| d.dependencies.clone())
                                        validation_function_name=dimension_st
                                            .with_value(|d| d.function_name.clone())
                                        autocomplete_function_name=dimension_st
                                            .with_value(|d| d.autocomplete_function_name.clone())
                                        description=dimension_st
                                            .with_value(|d| d.description.deref().to_string())
                                        handle_submit=move |_| {
                                            dimension_resource.refetch();
                                            action_rws.set(Action::None);
                                        }
                                    />
                                </PortalDrawer>
                            }
                                .into_view()
                        }
                        Action::Delete => {
                            view! {
                                <ChangeLogSummary
                                    dimension_name=dimension_name.get()
                                    change_type=ChangeType::Delete
                                    on_close=move |_| action_rws.set(Action::None)
                                    on_confirm=confirm_delete
                                    inprogress=delete_inprogress_rws
                                />
                            }
                                .into_view()
                        }
                    }}
                }
                    .into_view()
            }}
        </Suspense>
    }
}
