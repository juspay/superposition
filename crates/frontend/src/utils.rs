use crate::types::Envs;
use leptos::*;
use serde_json::Value;
use wasm_bindgen::JsCast;

pub fn modal_action(name: &str, action: &str) {
    if let Some(window) = web_sys::window() {
        if let Some(document) = window.document() {
            if let Some(modal) = document.get_element_by_id(name) {
                logging::log!("Modal found");
                if let Some(el) = modal.dyn_ref::<web_sys::HtmlDialogElement>() {
                    if action == "close" {
                        el.close();
                    } else {
                        let _ = el.show_modal();
                    }
                }
            } else {
                logging::log!("Modal element not found");
            }
        }
    }
}

pub fn get_host() -> String {
    let context = use_context::<Resource<(), Envs>>();
    context
        .map_or(None, |resource| resource.get())
        .map(|ctx| ctx.host)
        .unwrap_or(String::from("http://localhost:8080"))
}

pub fn get_tenants() -> Vec<String> {
    let context = use_context::<Resource<(), Envs>>();
    context
        .map_or(None, |resource| resource.get())
        .map(|ctx| ctx.tenants)
        .unwrap_or(vec![])
}

pub fn get_element_by_id<T>(id: &'static str) -> Option<T>
where
    T: wasm_bindgen::JsCast + Clone,
{
    let option_dom_ele = document().get_element_by_id(id);
    logging::log!("DOM element found {:?}", option_dom_ele);
    match option_dom_ele {
        Some(dom_ele) => dom_ele.dyn_ref::<T>().cloned(),
        None => None,
    }
}

pub fn show_modal(id: &'static str) {
    let option_dialog_ele = get_element_by_id::<web_sys::HtmlDialogElement>(id);
    if let Some(dialog_ele) = option_dialog_ele {
        let _ = dialog_ele.show_modal();
        logging::log!("{:?}", dialog_ele);
    }
}

pub fn close_modal(id: &'static str) {
    let option_dialog_ele = get_element_by_id::<web_sys::HtmlDialogElement>(id);
    if let Some(dialog_ele) = option_dialog_ele {
        let _ = dialog_ele.close();
        logging::log!("{:?}", dialog_ele);
    }
}

pub fn parse_string_to_json_value_vec(input: &str) -> Vec<Value> {
    // Parse the input string into a serde_json::Value
    let parsed = serde_json::from_str::<Value>(input);

    // Ensure the Value is an Array and convert it to Vec<Value>
    match parsed {
        Ok(Value::Array(arr)) => arr,
        _ => {
            logging::log!("Not a valid json in the input");
            vec![]
        }
    }
}
