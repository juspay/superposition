use leptos::*;

use crate::components::condition_pills::{use_condition_collapser, ConditionId};

#[component]
pub fn condition_collapse_provider(children: Children) -> impl IntoView {
    let (condition_id_rs, condition_id_ws) =
        create_signal::<ConditionId>(ConditionId(None));

    provide_context(condition_id_rs);
    provide_context(condition_id_ws);

    let collapse_event_handle = use_condition_collapser();
    on_cleanup(move || {
        collapse_event_handle.remove();
        logging::debug_warn!("removing event handle");
    });

    children()
}
