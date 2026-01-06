use std::borrow::Borrow;
use std::collections::HashSet;
use std::rc::Rc;

use leptos::*;
use monaco::api::CodeEditor;
use monaco::sys::IDisposable;
use monaco::sys::editor::{IEditorMinimapOptions, IStandaloneEditorConstructionOptions};
use monaco::sys::languages::{CompletionItemProvider, register_completion_item_provider};
use serde_json::Value;
use wasm_bindgen::JsValue;
use wasm_bindgen::prelude::*;

#[derive(Debug, Clone, strum_macros::Display)]
#[strum(serialize_all = "lowercase")]
pub enum Languages {
    Javascript,
    Json,
}

pub type EditorModelCell = Rc<Option<CodeEditor>>;

#[wasm_bindgen(module = "/src-js/utils.js")]
extern "C" {
    #[wasm_bindgen(js_name = newSuggestionsProvider)]
    fn new_suggestions_provider(
        trigers: JsValue,
        suggestions: JsValue,
    ) -> CompletionItemProvider;
}

/// Returns an object which allows removing the suggestions.
fn set_monaco_suggestions(lang_id: &str, suggestions: &[Value]) -> Option<IDisposable> {
    let mut triggers = HashSet::new();
    for s in suggestions {
        match s {
            Value::Array(_) => triggers.insert('['),
            Value::Object(_) => triggers.insert('{'),
            Value::Bool(true) => triggers.insert('t'),
            Value::Bool(false) => triggers.insert('f'),
            // This will take care of negative numbers as well.
            Value::Number(n) => triggers.insert(n.to_string().chars().next().unwrap()),
            Value::String(_) => triggers.insert('"'),
            Value::Null => false,
        };
    }
    logging::debug_warn!(
        "Trying to set monaco suggestions: {:?}, for lang-id: {lang_id}, w/ triggers: {:?}",
        suggestions,
        &triggers
    );
    let triggers = JsValue::from(
        triggers
            .into_iter()
            .map(|c| c.to_string())
            .collect::<Vec<String>>(),
    );
    match serde_wasm_bindgen::to_value(suggestions) {
        Ok(jsv) => {
            let provider = new_suggestions_provider(triggers, jsv);
            Some(register_completion_item_provider(lang_id, &provider))
        }
        Err(e) => {
            logging::error!(
                r#"
                Failed to convert monaco suggestions to native JS values.
                Error: {e}
                "#
            );
            None
        }
    }
}

#[component]
pub fn MonacoEditor(
    #[prop(into)] node_id: String,
    #[prop(into, default = String::new())] data: String,
    #[prop(into, default = Callback::new(move |_| {}))] on_change: Callback<String, ()>,
    #[prop(default = Languages::Javascript)] language: Languages,
    #[prop(default = vec!["min-h-50"])] classes: Vec<&'static str>,
    #[prop(default = false)] _validation: bool,
    #[prop(default = false)] read_only: bool,
    #[prop(default = vec![])] suggestions: Vec<Value>,
) -> impl IntoView {
    let editor_ref = create_node_ref::<html::Div>();
    let (editor_rs, editor_ws) = create_signal(Rc::new(None));
    let styling = classes.join(" ");
    let idp = set_monaco_suggestions(language.to_string().as_str(), &suggestions);
    on_cleanup(move || {
        // Running to un-register completions, otherwise these suggestions will come up in other
        // monaco instances when using the same language.
        if let Some(i) = idp {
            i.dispose()
        }
    });
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
                let editor = editor_rs.get();
                let value = (editor.borrow() as &Option<CodeEditor>)
                    .as_ref()
                    .and_then(|editor| editor.get_model())
                    .map(|model| model.get_value());
                match value {
                    Some(value) => {
                        logging::log!("Change editor value {}", value);
                        on_change.call(value);
                    }
                    None => {
                        logging::log!("Failed to get editor value");
                    }
                }
            }
        />
    }
}
