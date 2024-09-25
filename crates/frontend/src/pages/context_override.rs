use futures::join;
use leptos::*;
use leptos_router::A;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use superposition_types::{
    custom_query::PaginationParams,
    database::{models::cac::DefaultConfig, types::DimensionWithMandatory},
    Config, Context,
};

use crate::api::{delete_context, fetch_default_config, fetch_dimensions};
use crate::components::button::Button;
use crate::components::context_card::ContextCard;
use crate::components::context_form::utils::{create_context, update_context};
use crate::components::context_form::ContextForm;
use crate::components::delete_modal::DeleteModal;
use crate::components::override_form::OverrideForm;
use crate::components::skeleton::{Skeleton, SkeletonVariant};
use crate::logic::Conditions;
use crate::providers::condition_collapse_provider::ConditionCollapseProvider;
use crate::types::{OrganisationId, Tenant};
use crate::{
    api::fetch_config, components::alert::AlertType,
    providers::alert_provider::enqueue_alert,
};

#[derive(Clone, Debug, Default)]
pub struct Data {
    pub context: Conditions,
    pub overrides: Vec<(String, Value)>,
}

#[derive(Serialize, Deserialize, Clone, Debug, Default)]
struct PageResource {
    config: Config,
    dimensions: Vec<DimensionWithMandatory>,
    default_config: Vec<DefaultConfig>,
}

#[component]
fn form(
    context: Conditions,
    overrides: Vec<(String, Value)>,
    dimensions: Vec<DimensionWithMandatory>,
    edit: bool,
    default_config: Vec<DefaultConfig>,
    handle_submit: Callback<(), ()>,
    #[prop(default = String::new())] description: String,
    #[prop(default = String::new())] change_reason: String,
) -> impl IntoView {
    let tenant_s = use_context::<Signal<Tenant>>().unwrap();
    let org_s = use_context::<Signal<OrganisationId>>().unwrap();
    let (context, set_context) = create_signal(context);
    let (overrides, set_overrides) = create_signal(overrides);
    let dimensions = StoredValue::new(dimensions);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);

    let (description_rs, description_ws) = create_signal(description);
    let (change_reason_rs, change_reason_ws) = create_signal(change_reason);

    let on_submit = move |_| {
        req_inprogress_ws.set(true);
        spawn_local(async move {
            let f_context = context.get();
            let f_overrides = overrides.get();
            let result = if edit {
                update_context(
                    tenant_s.get().0,
                    Map::from_iter(f_overrides),
                    f_context,
                    description_rs.get(),
                    change_reason_rs.get(),
                    org_s.get().0,
                )
                .await
            } else {
                create_context(
                    tenant_s.get().0,
                    Map::from_iter(f_overrides),
                    f_context,
                    description_rs.get(),
                    change_reason_rs.get(),
                    org_s.get().0,
                )
                .await
            };

            match result {
                Ok(_) => {
                    logging::log!("Context and overrides submitted successfully");
                    handle_submit.call(());
                    let success_message = if edit {
                        "Context and overrides updated successfully!"
                    } else {
                        "Context and overrides created successfully!"
                    };
                    enqueue_alert(
                        String::from(success_message),
                        AlertType::Success,
                        5000,
                    );
                }
                Err(e) => {
                    logging::log!("Error submitting context and overrides: {:?}", e);
                    enqueue_alert(e, AlertType::Error, 5000);
                }
            }
            req_inprogress_ws.set(false);
        });
    };
    view! {
        <ContextForm
            dimensions=dimensions.get_value()
            context=context.get_untracked()
            handle_change=move |new_context| {
                set_context
                    .update(|value| {
                        *value = new_context;
                    });
            }

            disabled=edit
        />

        <div class="form-control">
            <label class="label">
                <span class="label-text">Description</span>
            </label>
            <textarea
                placeholder="Enter description"
                class="textarea textarea-bordered w-full max-w-md"
                value=description_rs.get_untracked()
                on:change=move |ev| {
                    let value = event_target_value(&ev);
                    description_ws.set(value);
                }
            />
        </div>

        <div class="form-control">
            <label class="label">
                <span class="label-text">Reason for Change</span>
            </label>
            <textarea
                placeholder="Enter a reason for this change"
                class="textarea textarea-bordered w-full max-w-md"
                value=change_reason_rs.get_untracked()
                on:change=move |ev| {
                    let value = event_target_value(&ev);
                    change_reason_ws.set(value);
                }
            />
        </div>

        <OverrideForm
            overrides=overrides.get_untracked()
            default_config=default_config
            handle_change=move |new_overrides| {
                set_overrides
                    .update(|value| {
                        *value = new_overrides;
                    });
            }
        />


        <div class="flex justify-start w-full mt-10">
            {move || {
                let loading = req_inprogess_rs.get();
                view! {
                    <Button
                        class="pl-[70px] pr-[70px] w-48 h-12".to_string()
                        text="Submit".to_string()
                        on_click=on_submit.clone()
                        loading
                    />
                }
            }}

        </div>
    }
}

#[component]
pub fn context_override() -> impl IntoView {
    let tenant_rws = use_context::<Signal<Tenant>>().unwrap();
    let org_rws = use_context::<Signal<OrganisationId>>().unwrap();

    let (modal_visible, set_modal_visible) = create_signal(false);
    let (delete_id, set_delete_id) = create_signal::<Option<String>>(None);

    let page_resource: Resource<(String, String), PageResource> =
        create_blocking_resource(
            move || (tenant_rws.get().0, org_rws.get().0),
            |(tenant, org_id)| async move {
                let empty_list_filters = PaginationParams::all_entries();
                let (config_result, dimensions_result, default_config_result) = join!(
                    fetch_config(&tenant, None, org_id.clone()),
                    fetch_dimensions(&empty_list_filters, &tenant, &org_id),
                    fetch_default_config(&empty_list_filters, &tenant, &org_id,)
                );
                PageResource {
                    config: config_result.unwrap_or_default(),
                    dimensions: dimensions_result
                        .unwrap_or_default()
                        .data
                        .into_iter()
                        .filter(|d| d.dimension != "variantIds")
                        .collect(),
                    default_config: default_config_result.unwrap_or_default().data,
                }
            },
        );

    let on_context_delete = Callback::new(move |id: String| {
        set_delete_id.set(Some(id.clone()));
        set_modal_visible.set(true);
    });

    let on_delete_confirm = Callback::new(move |_| {
        if let Some(id) = delete_id.get().clone() {
            spawn_local(async move {
                let result =
                    delete_context(tenant_rws.get().0, id, org_rws.get().0).await;

                match result {
                    Ok(_) => {
                        logging::log!("Context and overrides deleted successfully");
                        page_resource.refetch();
                    }
                    Err(e) => {
                        logging::log!("Error deleting context and overrides: {:?}", e);
                    }
                }
            });
        }
        set_delete_id.set(None);
        set_modal_visible.set(false);
    });

    view! {
        <div class="flex justify-between">
            <h2 class="card-title">Overrides</h2>

            <A href="new">
                <Button text="Create Override" on_click=move |_| {} />
            </A>
        </div>

        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::Block /> }
        }>
            <div class="space-y-6">
                {move || {
                    let config = page_resource.get().map(|v| v.config).unwrap_or_default();
                    let ctx_n_overrides = config
                        .contexts
                        .into_iter()
                        .map(|context| {
                            let overrides = context
                                .override_with_keys
                                .iter()
                                .flat_map(|id| {
                                    config
                                        .overrides
                                        .get(id)
                                        .cloned()
                                        .map_or(Map::new(), |overrides| overrides.into())
                                        .into_iter()
                                        .collect::<Vec<(String, Value)>>()
                                })
                                .collect::<Map<String, Value>>();
                            (context.clone(), overrides)
                        })
                        .collect::<Vec<(Context, Map<String, Value>)>>();
                    let is_empty = ctx_n_overrides.is_empty();
                    view! {
                        <Show when=move || is_empty>
                            <div class="flex-row" style="margin-top:20rem;">
                                <div class="flex justify-center text-gray-400">
                                    <i class="ri-file-add-line ri-xl"></i>
                                </div>
                                <div class="flex mt-4 font-semibold items-center text-gray-400 text-xl justify-center">
                                    "Start with creating an override"
                                </div>
                            </div>
                        </Show>
                        <ConditionCollapseProvider>

                            {ctx_n_overrides
                                .into_iter()
                                .map(|(context, overrides)| {
                                    view! {
                                        <ContextCard
                                            context=context
                                            overrides=overrides
                                            handle_delete=on_context_delete
                                        />
                                    }
                                })
                                .collect_view()}

                        </ConditionCollapseProvider>
                    }
                }}

            </div>

            <DeleteModal
                modal_visible=modal_visible
                confirm_delete=on_delete_confirm
                set_modal_visible=set_modal_visible
                header_text="Are you sure you want to delete this context? Action is irreversible."
                    .to_string()
            />

        </Suspense>
    }
}
