use std::borrow::Borrow;
use std::rc::Rc;

use leptos::*;
use monaco::api::CodeEditor;
use monaco::sys::editor::{IEditorMinimapOptions, IStandaloneEditorConstructionOptions};
#[derive(Debug, Clone, strum_macros::Display)]
#[strum(serialize_all = "lowercase")]
pub enum Languages {
    Javascript,
    Json,
}

pub type EditorModelCell = Rc<Option<CodeEditor>>;

#[component]
pub fn monaco_editor(
    #[prop(into)] node_id: String,
    #[prop(into, default = String::new())] data: String,
    #[prop(into, default = Callback::new(move |_| {}))] on_change: Callback<String, ()>,
    #[prop(default = Languages::Javascript)] language: Languages,
    #[prop(default = vec!["min-h-50"])] classes: Vec<&'static str>,
    #[prop(default = false)] _validation: bool,
    #[prop(default = false)] read_only: bool,
) -> impl IntoView {
    let editor_ref = create_node_ref::<html::Div>();
    let (editor_rs, editor_ws) = create_signal(Rc::new(None));
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
            editor_settings.set_font_family(Some("ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, \"Liberation Mono\", \"Courier New\", monospace"));
            logging::log!("Monaco Init value {}", data);
            editor_settings.set_value(Some(data.as_str()));
            editor_settings.set_render_final_newline(Some(true));
            editor_settings.set_read_only(Some(read_only));
            editor_settings.set_minimap(Some(&minimap_settings));
            let editor = CodeEditor::create(&node, Some(editor_settings));

            editor_ws.set(Rc::new(Some(editor)));
        }
    });
    view! {
        <div
            id=node_id
            class=styling
            node_ref=editor_ref
            on:keyup=move |_| {
                match editor_rs.get().borrow() {
                    Some(editor) => {
                        logging::log!("here editor {:?}", editor.get_model().unwrap().get_value());
                        on_change.call(editor.get_model().unwrap().get_value());
                    }
                    None => {}
                }
            }
        >
        </div>
    }
}
