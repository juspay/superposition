use js_sys::Object;
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;
use web_sys::Element;

#[wasm_bindgen]
extern "C" {
    // Bind to the global Sortable constructor
    #[wasm_bindgen(js_name = "Sortable")]
    type Sortable;

    // Constructor
    #[wasm_bindgen(constructor)]
    fn new(element: &Element, options: &JsValue) -> Sortable;

    // Methods
    #[wasm_bindgen(method)]
    fn destroy(this: &Sortable);

    #[wasm_bindgen(method, js_name = toArray)]
    fn to_array(this: &Sortable) -> js_sys::Array;
}

#[wasm_bindgen]
extern "C" {
    // Event object bindings
    type SortableEvent;

    #[wasm_bindgen(method, getter, js_name = oldIndex)]
    fn old_index(this: &SortableEvent) -> Option<u32>;

    #[wasm_bindgen(method, getter, js_name = newIndex)]
    fn new_index(this: &SortableEvent) -> Option<u32>;
}

#[derive(Default, Clone, Debug)]
pub struct SortableConfig {
    pub animation: Option<u32>,
    pub handle: Option<String>,
    pub disabled: Option<bool>,
    pub group: Option<String>,
    pub ghost_class: Option<String>,
    pub chosen_class: Option<String>,
}

impl SortableConfig {
    pub fn new() -> Self {
        Self::default()
    }

    /// Builder pattern methods
    pub fn animation(mut self, ms: u32) -> Self {
        self.animation = Some(ms);
        self
    }

    pub fn handle(mut self, selector: impl Into<String>) -> Self {
        self.handle = Some(selector.into());
        self
    }

    pub fn disabled(mut self, disabled: bool) -> Self {
        self.disabled = Some(disabled);
        self
    }

    pub fn group(mut self, name: impl Into<String>) -> Self {
        self.group = Some(name.into());
        self
    }

    pub fn ghost_class(mut self, class: impl Into<String>) -> Self {
        self.ghost_class = Some(class.into());
        self
    }

    pub fn chosen_class(mut self, class: impl Into<String>) -> Self {
        self.chosen_class = Some(class.into());
        self
    }

    /// Convert to JavaScript object for SortableJS
    pub fn to_js_value(&self) -> Result<JsValue, JsValue> {
        let obj = Object::new();

        if let Some(animation) = self.animation {
            js_sys::Reflect::set(&obj, &"animation".into(), &animation.into())?;
        }

        if let Some(ref handle) = self.handle {
            js_sys::Reflect::set(&obj, &"handle".into(), &handle.into())?;
        }

        if let Some(disabled) = self.disabled {
            js_sys::Reflect::set(&obj, &"disabled".into(), &disabled.into())?;
        }

        if let Some(ref group) = self.group {
            js_sys::Reflect::set(&obj, &"group".into(), &group.into())?;
        }

        if let Some(ref ghost_class) = self.ghost_class {
            js_sys::Reflect::set(&obj, &"ghostClass".into(), &ghost_class.into())?;
        }

        if let Some(ref chosen_class) = self.chosen_class {
            js_sys::Reflect::set(&obj, &"chosenClass".into(), &chosen_class.into())?;
        }

        Ok(obj.into())
    }
}

///  ```
///     use frontend::bindings::sortable::{SortableConfig, SortableInstance};
///     use leptos::*;
///
///     #[component]
///     fn DemoComponent() -> impl IntoView {
///         Effect::new(move |_| {
///             let window = web_sys::window().unwrap();
///             let document = window.document().unwrap();
///             let element = document.get_element_by_id("my-list").unwrap();
///
///             let config = SortableConfig::new()
///                 .animation(150)
///                 .handle(".drag-handle")
///                 .ghost_class("sortable-ghost")
///                 .chosen_class("sortable-chosen")
///                 .disabled(false);
///
///            let sortable = SortableInstance::create_with_callback(
///                &element,
///                config,
///                |old_index: Option<u32>, new_index: Option<u32>| {
///                    if let (Some(old), Some(new)) = (old_index, new_index) {
///                        web_sys::console::log_1(&format!("Moved from {} to {}", old, new).into());
///                    }
///                },
///            );
///        });
///     }
///  ```
///
pub struct SortableInstance {
    sortable: Sortable,
}

impl SortableInstance {
    /// Create a new instance using bindings
    pub fn create_with_callback<F>(
        element: &Element,
        config: SortableConfig,
        on_end: F,
    ) -> Result<Self, JsValue>
    where
        F: Fn(Option<u32>, Option<u32>) + 'static,
    {
        // Convert config to JS object
        let options = config.to_js_value()?;

        // Add the onEnd callback to options
        let callback = Closure::wrap(Box::new(move |event: JsValue| {
            // Cast to SortableEvent and extract indices
            let event_obj: &SortableEvent = event.unchecked_ref();
            let old_index = event_obj.old_index();
            let new_index = event_obj.new_index();
            on_end(old_index, new_index);
        }) as Box<dyn Fn(JsValue)>);

        // Add callback to options object
        js_sys::Reflect::set(
            &options,
            &"onEnd".into(),
            callback.as_ref().unchecked_ref(),
        )?;

        // Create the Sortable instance using bindings
        let sortable = Sortable::new(element, &options);

        // Keep callback alive
        callback.forget();

        web_sys::console::log_1(&"SortableInstance created with real bindings!".into());

        Ok(Self { sortable })
    }

    /// Destroy the instance
    pub fn destroy(&self) {
        self.sortable.destroy();
        web_sys::console::log_1(&"SortableInstance destroyed".into());
    }

    /// Get current order using bindings
    pub fn get_order(&self) -> Vec<String> {
        let array = self.sortable.to_array();
        let mut result = Vec::new();

        for i in 0..array.length() {
            if let Ok(item) = array.get(i).dyn_into::<js_sys::JsString>() {
                result.push(String::from(item));
            }
        }

        result
    }
}
