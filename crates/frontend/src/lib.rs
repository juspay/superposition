mod api;
pub mod app;
pub mod components;
pub mod hoc;
pub mod pages;
pub mod providers;
pub mod types;
mod utils;
use cfg_if::cfg_if;

cfg_if! {
    if #[cfg(feature = "hydrate")] {
        use wasm_bindgen::prelude::wasm_bindgen;

        #[wasm_bindgen]
        pub fn hydrate() {
            use app::*;
            use leptos::*;


            console_error_panic_hook::set_once();
            let envs = utils::use_env();

            leptos::mount_to_body(move || {
                view! { <App app_envs={envs.clone()} /> }
            });
        }
    }
}
