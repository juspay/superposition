use leptos::*;
use leptos_router::use_params_map;
use serde::{Deserialize, Serialize};
use superposition_types::database::models::experimentation::ExperimentGroup;

use crate::{
    api::experiment_group::fetch_experiment_group,
    components::{
        button::Button,
        condition_pills::Condition as ConditionComponent,
        experiment_group_form::ExperimentGroupForm,
        modal::PortalModal,
        skeleton::{Skeleton, SkeletonVariant},
    },
    logic::Conditions,
    providers::editor_provider::EditorProvider,
    types::{OrganisationId, Tenant},
};

#[component]
pub fn experiment_groups() -> impl IntoView {
    let group_params = use_params_map();
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();

    let source = move || {
        let t = tenant_rws.get().0;
        let org = org_rws.get().0;
        let group_id =
            group_params.with(|params| params.get("id").cloned().unwrap_or("1".into()));
        (group_id, t, org)
    };

    let experiment_group_resource: Resource<(String, String, String), Option<ExperimentGroup>> =
        create_blocking_resource(source, |(group_id, tenant, org_id)| async move {
            fetch_experiment_group(&group_id, &tenant, &org_id)
                .await
                .ok()
        });

    let handle_edit = move || {};

    let handle_delete = move || {
        // TODO: Implement delete confirmation modal
        logging::log!("Delete group requested");
    };

    view! {
        
    }
}
