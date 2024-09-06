use derive_more::{Deref, DerefMut};
use leptos::*;

#[derive(Debug, Clone, Deref, DerefMut, Default)]
pub struct ShowMonaco(pub bool);
#[derive(Debug, Clone, Deref, DerefMut, Default)]
pub struct MonacoData(pub String);

#[derive(Debug, Clone)]
pub struct Editor {
    pub id: String,
    pub data: String,
    pub header: String,
}

impl Editor {
    pub fn reset(&mut self) {
        self.id = String::new();
        self.data = String::new();
        self.header = String::new();
    }
}

pub fn use_editor() -> (ReadSignal<Editor>, WriteSignal<Editor>) {
    let editor_rs = use_context::<ReadSignal<Editor>>()
        .expect("use_monaco should be wrapped in MonacoProvider");
    let editor_ws = use_context::<WriteSignal<Editor>>()
        .expect("use_monaco should be wrapped in MonacoProvider");

    (editor_rs, editor_ws)
}

#[component]
pub fn editor_provider(children: Children) -> impl IntoView {
    let (editor_rs, editor_ws) = create_signal(Editor {
        id: String::new(),
        data: String::new(),
        header: String::new(),
    });
    provide_context(editor_rs);
    provide_context(editor_ws);

    children()
}
