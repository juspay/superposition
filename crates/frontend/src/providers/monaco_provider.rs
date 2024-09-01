use derive_more::{Deref, DerefMut};
use leptos::*;

#[derive(Debug, Clone, Deref, DerefMut, Default)]
pub struct ShowMonaco(pub bool);
#[derive(Debug, Clone, Deref, DerefMut, Default)]
pub struct MonacoData(pub String);

#[derive(Debug, Clone)]
pub struct MonacoState {
    pub id: &'static str,
    pub show: bool,
    pub data: String,
    pub header: String,
    pub on_change: Callback<String, ()>
}

impl MonacoState {
    pub fn reset(&mut self) {
        self.show = false;
        self.data = String::new();
        self.header = String::new();
        self.on_change = Callback::new(move |_| {});
    }
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
        header: String::new(),
        on_change: Callback::new(move |_| {})
    });
    provide_context(monaco_state_rs);
    provide_context(monaco_state_ws);

    children()
}
