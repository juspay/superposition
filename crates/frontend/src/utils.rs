use crate::types::Envs;
use leptos::*;
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
