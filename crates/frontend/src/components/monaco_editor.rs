use std::{cell::RefCell, rc::Rc};

use leptos::*;
use monaco::api::CodeEditor;
use monaco::sys::editor::{IEditorMinimapOptions, IStandaloneEditorConstructionOptions};
use web_sys::KeyboardEvent;
#[derive(Debug, Clone, strum_macros::Display)]
#[strum(serialize_all = "lowercase")]
pub enum Languages {
    Javascript,
    Json,
}

pub type EditorModelCell = Rc<RefCell<Option<CodeEditor>>>;

#[component]
pub fn monaco_editor(
    node_id: &'static str,
    #[prop(into, default = String::new())] data: String,
    #[prop(into, default = Callback::new(move |_| {}))] on_change: Callback<String, ()>,
    data_rs: ReadSignal<String>,
    data_ws: WriteSignal<String>,
    #[prop(default = Languages::Javascript)] language: Languages,
    #[prop(default = vec!["min-h-50"])] classes: Vec<&'static str>,
    #[prop(default = false)] _validation: bool,
    #[prop(default = false)] read_only: bool,
) -> impl IntoView {
    let editor_ref = create_node_ref::<html::Div>();
    let (_, editor_ws) = create_signal(EditorModelCell::default());
    let styling = classes.join(" ");
    create_effect(move |_| {
        if let Some(node) = editor_ref.get() {
            monaco::workers::ensure_environment_set();
            logging::log!(
                "Is monaco environment set? {}",
                monaco::workers::is_environment_set()
            );
            let editor_settings = IStandaloneEditorConstructionOptions::default();
            let minimap_settings = IEditorMinimapOptions::default();
            minimap_settings.set_enabled(Some(false));
            editor_settings.set_language(Some(language.to_string().as_str()));
            editor_settings.set_automatic_layout(Some(true));
            logging::log!("Monaco Init value {}", data);
            editor_settings.set_value(Some(data.as_str()));
            // editor_settings.set_value(Some(data_rs.get().as_str()));
            editor_settings.set_render_final_newline(Some(true));
            editor_settings.set_read_only(Some(read_only));
            editor_settings.set_minimap(Some(&minimap_settings));
            let editor = CodeEditor::create(&node, Some(editor_settings));
            editor_ws.update(|prev| {
                prev.replace(Some(editor));
            });
            if let Ok(e) = KeyboardEvent::new("keyup") {
                node.dispatch_event(&e);
            }
        }
    });
    view! {
        <div id={node_id} class={styling} node_ref=editor_ref on:keyup=move |event| {
            let new_data = event_target_value(&event);
            logging::log!("Updating code");
            on_change.call(new_data.clone());
            // data_ws.set_untracked(new_data);
        }>
        </div>
    }
}
