use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

use ev::KeyboardEvent;
use leptos::*;
use monaco::api::{CodeEditor, TextModel};
use monaco::sys::editor::{
    ICodeEditor, IEditorMinimapOptions, IStandaloneEditorConstructionOptions,
};
use monaco::sys::languages::json::{DiagnosticsOptions, SeverityLevel};
use monaco::sys::Uri;
use rand::Rng;
use serde::Serialize;
use serde_json::Value;
use serde_wasm_bindgen::Serializer;
use wasm_bindgen::JsValue;
use wasm_bindgen_futures::spawn_local;

#[derive(Debug, Clone, PartialEq, strum_macros::Display)]
#[strum(serialize_all = "lowercase")]
pub enum TextContentType {
    Javascript,
    Json,
}

pub const METASCHEMA_JSON_SCHEMA_URI: &str = "http://json-schema.org/draft-07/schema";

const CHARSET: &[u8] = b"abcdefghijklmnopqrstuvwxyz";

pub type EditorModelCell = Rc<RefCell<Option<CodeEditor>>>;

fn get_diagnostic_settings_json(validate: bool, schema: &Value) -> DiagnosticsOptions {
    let serializer = Serializer::json_compatible();
    let schema = match schema.serialize(&serializer) {
        Ok(schema) => schema,
        Err(err) => {
            logging::log!(
                "Error occurred while processing diagnostic schema settings, it is {err}"
            );
            JsValue::default()
        }
    };
    DiagnosticsOptions::new(
        Some(false),
        None,
        Some(true),
        Some(SeverityLevel::Warning),
        Some(SeverityLevel::Warning),
        Some(schema),
        None,
        Some(validate),
    )
}

fn format_code(editor_rs: ReadSignal<EditorModelCell>) {
    let editor = editor_rs.get_untracked();
    spawn_local(async move {
        let editor = editor.borrow_mut();
        for element in editor.iter() {
            let i_code_editor: &ICodeEditor = element.as_ref().as_ref();
            let _ = wasm_bindgen_futures::JsFuture::from(
                i_code_editor
                    .get_action("editor.action.formatDocument")
                    .run(),
            )
            .await;
        }
    });
}

pub fn generate_uri_name() -> String {
    let mut rng = rand::thread_rng();
    let file_name: String = (0..10)
        .map(|_| {
            let idx = rng.gen_range(0..CHARSET.len());
            CHARSET[idx] as char
        })
        .collect();
    format!("a://b/{}.json", file_name)
}

#[component]
pub fn monaco_editor<NF>(
    node_id: &'static str,
    data_rs: ReadSignal<String>,
    update_fn: NF,
    #[prop(default = TextContentType::Javascript)] language: TextContentType,
    #[prop(default = vec!["min-h-50"])] classes: Vec<&'static str>,
    #[prop(default = false)] validation: bool,
    #[prop(default = Value::Null)] schemas: Value,
    #[prop(default = false)] read_only: bool,
    #[prop(default = generate_uri_name())] uri_name: String,
) -> impl IntoView
where
    NF: Fn(KeyboardEvent) + 'static,
{
    let editor_ref = create_node_ref::<html::Div>();
    let (editor_rs, editor_ws) = create_signal(EditorModelCell::default());
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
            editor_settings.set_value(Some(data_rs.get().as_str()));
            editor_settings.set_render_final_newline(Some(true));
            editor_settings.set_read_only(Some(read_only));
            editor_settings.set_automatic_layout(Some(true));
            editor_settings.set_minimap(Some(&minimap_settings));
            editor_settings.set_format_on_type(Some(true));
            editor_settings.set_format_on_paste(Some(true));
            if language == TextContentType::Json && validation {
                let diagnostics_options =
                    get_diagnostic_settings_json(validation, &schemas);
                monaco::sys::languages::json::JSON_DEFAULTS
                    .set_diagnostic_options(&diagnostics_options);
                let text_model = TextModel::create(
                    data_rs.get().as_str(),
                    Some(TextContentType::Json.to_string().as_str()),
                    Some(&Uri::parse(&uri_name, false)),
                );
                logging::log!("Text model: {:?}", text_model);
                if let Ok(text_model) = text_model {
                    editor_settings.set_model(Some(text_model.as_ref()));
                }
            }
            let editor = CodeEditor::create(&node, Some(editor_settings));
            logging::log!("Diagnostic Options for {node_id}: {:?}", monaco::sys::languages::json::JSON_DEFAULTS.diagnostic_options());
            logging::log!("URI for {node_id}: {:?}", editor.get_model().unwrap().as_ref().uri());
            editor_ws.update(|prev| {
                prev.replace(Some(editor));
            });
            if !data_rs.get().is_empty() {
                format_code(editor_rs);
            }
        }
    });
    view! {
        <div id={node_id} class={styling} node_ref=editor_ref on:keyup=update_fn>
        </div>
    }
}
