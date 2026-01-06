use leptos::*;

use crate::components::condition_pills::{ConditionId, use_condition_collapser};

#[component]
pub fn ConditionCollapseProvider(children: Children) -> impl IntoView {
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
