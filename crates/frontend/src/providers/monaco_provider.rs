use derive_more::{Deref, DerefMut};
use leptos::*;

#[derive(Debug, Clone, Deref, DerefMut, Default)]
pub struct ShowMonaco(pub bool);
#[derive(Debug, Clone, Deref, DerefMut, Default)]
pub struct MonacoData(pub String);

pub struct MonacoState {
    pub id: &'static str,
    pub show: bool,
    pub data: String,
}

pub fn use_monaco() -> (ReadSignal<MonacoState>, WriteSignal<MonacoState>) {
    let monaco_state_rs = use_context::<ReadSignal<MonacoState>>()
        .expect("use_monaco should be wrapped in MonacoProvider");
    let monaco_state_ws = use_context::<WriteSignal<MonacoState>>()
        .expect("use_monaco should be wrapped in MonacoProvider");

    (monaco_state_rs, monaco_state_ws)
}

#[component]
pub fn monaco_provider(id: &'static str, children: Children) -> impl IntoView {
    let (monaco_state_rs, monaco_state_ws) = create_signal(MonacoState {
        id,
        show: false,
        data: String::new(),
    });
    provide_context(monaco_state_rs);
    provide_context(monaco_state_ws);

    children()
}
