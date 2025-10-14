use std::hash::Hash;

use leptos::logging;
use leptos::*;
use wasm_bindgen::JsCast;
use web_sys::{Element, HtmlDivElement};

use crate::bindings::sortable::{SortableConfig, SortableInstance};

#[component]
pub fn Sortable<T: Eq + Hash + Clone + 'static>(
    #[prop(into)] items: Signal<Vec<T>>,
    #[prop(into)] on_change: Callback<Vec<T>>,
    #[prop(into)] render_item: Callback<(usize, T), View>,
    #[prop(into, default = String::new())] class: String,
    /// Animation duration in milliseconds
    #[prop(default = 150)]
    animation: u32,
    #[prop(default = ".drag-handle".to_string())] handle_selector: String,
    #[prop(default = false)] disabled: bool,
) -> impl IntoView {
    let container_ref = NodeRef::<leptos::html::Div>::new();
    let sortable_instance = RwSignal::new(None as Option<SortableInstance>);

    // Initialize SortableJS when the element is mounted
    Effect::new(move |_| {
        if let Some(element) = container_ref.get() {
            let element: Element =
                <HtmlDivElement as Clone>::clone(&element).unchecked_into::<Element>();

            // Create configuration using builder pattern
            let config = SortableConfig::new()
                .animation(animation)
                .handle(handle_selector.clone())
                .disabled(disabled)
                .ghost_class("sortable-ghost".to_string())
                .chosen_class("sortable-chosen".to_string());

            // Create the sortable instance with wasm-bindgen bindings
            match SortableInstance::create_with_callback(
                &element,
                config,
                move |old_index: Option<u32>, new_index: Option<u32>| {
                    // Update the items order when drag ends
                    if let (Some(old), Some(new)) = (old_index, new_index) {
                        let mut items_vec = items.get();
                        if old != new
                            && (old as usize) < items_vec.len()
                            && (new as usize) < items_vec.len()
                        {
                            let item = items_vec.remove(old as usize);
                            items_vec.insert(new as usize, item);
                        }
                        on_change.call(items_vec);
                    }
                },
            ) {
                Ok(instance) => {
                    sortable_instance.set(Some(instance));
                }
                Err(e) => {
                    logging::error!("Failed to create SortableJS instance: {:?}", e);
                }
            }
        }
    });

    // Cleanup when component unmounts
    on_cleanup(move || {
        sortable_instance.with_untracked(|instance| {
            if let Some(instance) = instance {
                instance.destroy();
                logging::log!("SortableJS instance destroyed");
            }
        });
    });

    view! {
        <div node_ref=container_ref class=format!("sortable-container {class}")>
            <For
                each=move || { items.get().into_iter().enumerate().collect::<Vec<_>>() }
                key=|(_, item)| item.clone()
                children=move |item| { render_item.call(item) }
            />
        </div>
    }
}
