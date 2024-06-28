use leptos::*;
use monaco::sys::editor::{IEditorMinimapOptions, IStandaloneEditorConstructionOptions};

#[derive(Debug, Clone, strum_macros::Display)]
#[strum(serialize_all = "lowercase")]
pub enum Languages {
    Javascript,
    Json,
}

#[component]
pub fn monaco_editor(
    node_id: &'static str,
    #[prop(default = Languages::Javascript)] language: Languages,
    #[prop(default = String::new())] initial_data: String,
    #[prop(default = vec!["min-h-50"])] _classes: Vec<&'static str>,
    #[prop(default = false)] _auto_complete: bool,
    #[prop(default = false)] _schema_validation: bool,
    #[prop(default = false)] read_only: bool,
) -> impl IntoView {
    let editor_ref = create_node_ref::<html::Div>();
    create_effect(move |_| {
        if let Some(node) = editor_ref.get() {
            let editor_settings = IStandaloneEditorConstructionOptions::default();
            let minimap_settings = IEditorMinimapOptions::default();
            logging::log!("Inside monaco callback");
            minimap_settings.set_enabled(Some(false));
            editor_settings.set_language(Some(language.to_string().as_str()));
            editor_settings.set_automatic_layout(Some(true));
            editor_settings.set_value(Some(initial_data.to_string().as_str()));
            editor_settings.set_render_final_newline(Some(true));
            editor_settings.set_read_only(Some(read_only));
            editor_settings.set_minimap(Some(&minimap_settings));
            let editor = monaco::api::CodeEditor::create::<
                IStandaloneEditorConstructionOptions,
            >(&node, Some(editor_settings));
            on_cleanup(move || drop(editor));
            logging::log!("div: {:?}", node.outer_html());
        }
    });
    view! {
        <div id={node_id} style="min-height: 500px; min-width: 1000px" node_ref=editor_ref>
        </div>
    }
}
