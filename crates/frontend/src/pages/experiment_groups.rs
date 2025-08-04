use futures::join;
use leptos::*;
use leptos_router::{use_params_map, A};
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use superposition_types::{
    api::{
        experiment_groups::ExpGroupMemberRequest,
        experiments::{ExperimentListFilters, ExperimentResponse},
        workspace::WorkspaceResponse,
    },
    custom_query::{CommaSeparatedQParams, DimensionQuery, PaginationParams},
    database::models::{experimentation::ExperimentGroup, ChangeReason},
};

use crate::{
    api::{
        experiment_groups::{fetch, remove_members},
        fetch_experiments,
    },
    components::{
        alert::AlertType,
        button::Button,
        condition_pills::Condition as ConditionComponent,
        delete_modal::DeleteModal,
        description::ContentDescription,
        drawer::PortalDrawer,
        experiment_group_form::AddExperimentToGroupForm,
        skeleton::{Skeleton, SkeletonVariant},
        table::{
            types::{default_column_formatter, Column, ColumnSortable, Expandable},
            Table,
        },
    },
    logic::Conditions,
    providers::{
        alert_provider::enqueue_alert,
        condition_collapse_provider::ConditionCollapseProvider,
    },
    types::{OrganisationId, Tenant},
};

#[derive(Serialize, Deserialize, Clone, Debug)]
struct ExperimentGroupResource {
    pub group: ExperimentGroup,
    pub experiments: Vec<ExperimentResponse>,
}

#[derive(Serialize, Deserialize, Clone, Debug, Default)]
struct RemoveRequest {
    pub group_id: String,
    pub experiment_id: String,
}

fn table_columns(
    delete_group_rws: RwSignal<RemoveRequest>,
    delete_modal_ws: WriteSignal<bool>,
    group_id: String,
    strict_mode: bool,
) -> Vec<Column> {
    let group_id = StoredValue::new(group_id);

    vec![
        Column::new(
            "name".to_string(),
            false,
            move |value: &str, row: &Map<String, Value>| {
                let experiment_name = value.to_string();
                let experiment_id = row.get("id").map_or(String::from(""), |value| {
                    value.as_str().unwrap_or("").to_string()
                });

                view! {
                    <A href=format!("../../experiments/{experiment_id}") class="text-blue-500 underline underline-offset-2">
                        {experiment_name}
                    </A>
                }
                .into_view()
            },
            ColumnSortable::No,
            Expandable::Disabled,
            |_| default_column_formatter("Experiment Name"),
        ),
        Column::default("traffic_percentage".to_string()),
        Column::new(
            "context".to_string(),
            false,
            move |_, row: &Map<String, Value>| {
                let context = row
                    .get("context")
                    .and_then(|v| v.as_object().cloned())
                    .unwrap_or_default();
                let conditions =
                    Conditions::from_context_json(&context).unwrap_or_default();
                let id = row.get("id").map_or(String::from(""), |value| {
                    value.as_str().unwrap_or("").to_string()
                });
                view! { <ConditionComponent conditions grouped_view=false id strict_mode /> }
                    .into_view()
            },
            ColumnSortable::No,
            Expandable::Disabled,
            default_column_formatter,
        ),
        Column::default_with_cell_formatter(
            "actions".to_string(),
            move |_, row: &Map<String, Value>| {
                let id = row.get("id").map_or(String::from(""), |value| {
                    value.as_str().unwrap_or("").to_string()
                });
                let handle_delete = move |_| {
                    delete_group_rws.set(RemoveRequest {
                        group_id: group_id.get_value(),
                        experiment_id: id.clone(),
                    });
                    delete_modal_ws.set(true);
                };
                view! {
                    <span class="cursor-pointer text-red-500" on:click=handle_delete>
                        <i class="ri-delete-bin-5-line ri-xl text-red-500"></i>
                    </span>
                }
                .into_view()
            },
        ),
    ]
}

#[component]
fn experiment_group_info(group: ExperimentGroup) -> impl IntoView {
    let workspace_settings = use_context::<StoredValue<WorkspaceResponse>>().unwrap();
    let conditions = Conditions::from_context_json(&group.context).unwrap_or_default();

    view! {
        <div class="-mt-5 card bg-base-100 max-w-screen shadow">
            <div class="card-body flex flex-row gap-2 flex-wrap">
                <ConditionCollapseProvider>
                    <ConditionComponent
                        conditions
                        id="experiment-group-context"
                        class="h-fit w-[300px]"
                        strict_mode=workspace_settings.with_value(|w| w.strict_mode)
                    />
                </ConditionCollapseProvider>
            </div>
        </div>
    }
}

#[derive(Clone)]
enum Action {
    Add,
    None,
}

#[component]
pub fn experiment_groups() -> impl IntoView {
    let group_params = use_params_map();
    let workspace_settings = use_context::<StoredValue<WorkspaceResponse>>().unwrap();
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let (delete_modal_rs, delete_modal_ws) = create_signal(false);
    let delete_group_rws = RwSignal::new(RemoveRequest::default());
    let action_rws = RwSignal::new(Action::None);

    let source = move || {
        let tenant_id = workspace.get().0;
        let org_id = org.get().0;
        let group_id =
            group_params.with(|params| params.get("id").cloned().unwrap_or("1".into()));
        (group_id, tenant_id, org_id)
    };

    let experiment_group_resource: Resource<
        (String, String, String),
        Option<ExperimentGroupResource>,
    > = create_blocking_resource(source, |(group_id, tenant, org_id)| async move {
        let group_future = fetch(&group_id, &tenant, &org_id);
        let filters = ExperimentListFilters {
            experiment_group_ids: Some(CommaSeparatedQParams(vec![group_id.clone()])),
            ..ExperimentListFilters::default()
        };
        let pagination = PaginationParams::all_entries();
        let dimension_params = DimensionQuery::default();
        let experiments_future =
            fetch_experiments(&filters, &pagination, &dimension_params, &tenant, &org_id);

        let (group_result, experiments_result) = join!(group_future, experiments_future);

        Some(ExperimentGroupResource {
            group: group_result.ok()?,
            experiments: experiments_result.ok()?.data,
        })
    });

    let confirm_delete = Callback::new(move |change_reason: String| {
        let delete_request = delete_group_rws.get();
        spawn_local(async move {
            let tenant = workspace.get().0;
            let org_id = org.get().0;
            let member_experiment_ids =
                Vec::from([delete_request.experiment_id.parse().unwrap_or_default()]);
            let remove_request = ExpGroupMemberRequest {
                change_reason: ChangeReason::try_from(change_reason).unwrap_or_default(),
                member_experiment_ids,
            };
            if let Err(e) = remove_members(
                &delete_request.group_id,
                &remove_request,
                &tenant,
                &org_id,
            )
            .await
            {
                logging::error!("Failed to delete experiment group member: {}", e);
                enqueue_alert(
                    format!(
                        "Failed to delete experiment group member {}: {} due to error: {}",
                        &delete_request.group_id, delete_request.experiment_id, e
                    ),
                    AlertType::Error,
                    5000,
                );
                return;
            }
            enqueue_alert(
                "Experiment group member deleted successfully".to_string(),
                AlertType::Success,
                5000,
            );
            delete_modal_ws.set(false);
            experiment_group_resource.refetch();
        });
    });
    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::DetailPage /> }
        }>
            {move || {
                let Some(Some(resource)) = experiment_group_resource.get() else {
                    return view! {
                        <div>
                            An error occurred while fetching the experiment group, please try again later.
                        </div>
                    }
                        .into_view();
                };
                let table_columns = table_columns(
                    delete_group_rws,
                    delete_modal_ws,
                    resource.group.id.to_string(),
                    workspace_settings.with_value(|w| w.strict_mode),
                );
                let data = resource
                    .experiments
                    .iter()
                    .map(|ele| json!(ele).as_object().cloned().unwrap_or_default())
                    .collect::<Vec<Map<String, Value>>>()
                    .to_owned();

                view! {
                    <div class="h-full flex flex-col gap-10 overflow-x-auto bg-transparent">
                        <h1 class="text-2xl font-extrabold">{resource.group.name.clone()}</h1>
                        <ExperimentGroupInfo group=resource.group.clone() />
                        <ContentDescription
                            pre_data=move || {
                                view! {
                                    <div class="h-fit w-[250px]">
                                        <div class="stat-title">"Group ID"</div>
                                        <div class="stat-value text-sm">
                                            {resource.group.id.to_string()}
                                        </div>
                                    </div>
                                    <div class="h-fit w-[250px]">
                                        <div class="stat-title">"Traffic"</div>
                                        <div class="stat-value text-sm">
                                            {resource.group.traffic_percentage.to_string() + "%"}
                                        </div>
                                    </div>
                                }
                            }
                            description=resource.group.description.clone()
                            change_reason=resource.group.change_reason.clone()
                            created_at=resource.group.created_at
                            created_by=resource.group.created_by.clone()
                            last_modified_at=resource.group.last_modified_at
                            last_modified_by=resource.group.last_modified_by.clone()
                        />
                        <div class="-mt-5 flex flex-col gap-5">
                            <div class="flex justify-between items-center">
                                <h2 class="text-xl font-semibold">"Member Experiments"</h2>
                                <Button
                                    on_click=move |_| action_rws.set(Action::Add)
                                    text="Add Members"
                                    icon_class="ri-add-line"
                                />
                            </div>
                            <div class="card w-full bg-base-100 rounded-xl overflow-hidden shadow">
                                <div class="card-body overflow-y-auto overflow-x-visible">
                                    <Table
                                        class="!overflow-y-auto"
                                        rows=data
                                        key_column="id"
                                        columns=table_columns
                                    />
                                </div>
                            </div>
                        </div>
                    </div>
                    {move || match action_rws.get() {
                        Action::Add => {
                            view! {
                                <PortalDrawer
                                    title="Add members to the group"
                                    handle_close=move |_| action_rws.set(Action::None)
                                >
                                    <AddExperimentToGroupForm
                                        experiment_group_id=resource.group.id
                                        handle_submit=move |_| experiment_group_resource.refetch()
                                    />
                                </PortalDrawer>
                            }
                                .into_view()
                        }
                        Action::None => view! {}.into_view(),
                    }}
                }
                    .into_view()
            }}
            <DeleteModal
                modal_visible=delete_modal_rs
                with_change_reason=true
                confirm_delete
                set_modal_visible=delete_modal_ws
                header_text="Are you sure you want to delete this member from the group?"
                    .to_string()
            />

        </Suspense>
    }
}
