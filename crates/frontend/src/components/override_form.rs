use std::collections::{HashMap, HashSet};

use crate::{
    components::{
        dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
        input_components::{BooleanToggle, EnumDropdown},
    },
    types::{DefaultConfig, Override},
    utils::{get_config_value, get_key_type, ConfigType},
};
use leptos::*;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use web_sys::MouseEvent;

// #[derive(Clone, Serialize, Deserialize, Debug)]
// struct Override {
//     key: RwSignal<Option<String>>,
//     value: RwSignal<Option<String>>,
// }

// impl Override {
//     fn new(key: Option<String>, value: Option<String>) -> Self {
//         Self {
//             key: create_rw_signal(key),
//             value: create_rw_signal(value),
//         }
//     }
// }

// #[component]
// pub fn TestForm(
//     overrides: ReadSignal<Vec<Override>>,
//     set_overrides: WriteSignal<Vec<Override>>,
// ) -> impl IntoView {
//     // let (overrides, set_overrides) = create_signal(vec![
//     //     Override::new(Some("key1".to_string()), Some("value1".to_string())),
//     //     Override::new(Some("key2".to_string()), Some("value2".to_string())),
//     // ]);

//     let add_override = move || {
//         set_overrides.update(|curr_overrides| {
//             let new_key = format!("key{}", curr_overrides.len() + 1);
//             curr_overrides.push(Override::new(Some(new_key), Some("".to_string())));
//         });
//     };

//     let remove_override = move |index: usize| {
//         set_overrides.update(|curr_overrides| {
//             curr_overrides.remove(index);
//         });
//     };

//     // create_effect(move |_| {
//     //     let f_override = overrides.get();
//     //     logging::log!("effect {:?}", f_override);
//     // });

//     view! {
//         <div>
//             <For each=move || overrides.get()
//                 key=|row| row.key.get().clone()
//                 children= move |row| {
//             { move || {
//                 logging::log!("re-rendered outer");
//                 view!{
//                 <div class="w-2/5 mb-2">
//                     <textarea
//                         placeholder="Enter override here"
//                         name="override"
//                         class="input input-bordered w-[450px] flex items-center bg-white text-gray-700 shadow-md pt-3"
//                         on:change={
//                             let key_signal = row.key;
//                             let value_signal = row.value;
//                             move |event| {
//                                 event.prevent_default();
//                                 let input_value = event_target_value(&event);
//                                 value_signal.set(Some(input_value.clone()));
//                             }
//                         }
//                     >
//                         {move || row.value.get().clone().unwrap_or_default()}
//                     </textarea>
//                     { move || {
//                         logging::log!("re-rendered button");
//                         view!{
//                     <button
//                         class="btn btn-ghost btn-circle btn-sm"
//                         on:click=move |ev| {
//                             ev.prevent_default();
//                             set_overrides.update(|overrides| {
//                                 overrides.retain(|this| this.key.get() != row.key.get());
//                             });
//                         }
//                     >
//                         "Delete"
//                     </button>
//                         }
//                     } }
//                 </div>
//             }}}
//         }
//             />
//             <div>
//                 <button
//                     class="btn btn-primary"
//                     on:click=move |ev| {
//                         leptos::logging::log!("click event");
//                         ev.prevent_default();
//                         add_override();
//                     }
//                 >
//                     "Add Override"
//                 </button>
//             </div>
//         </div>
//     }
// }



#[component]
pub fn override_form(
    overrides: ReadSignal<Vec<Override>>,
    set_overrides: WriteSignal<Vec<Override>>,
    default_config: Vec<DefaultConfig>,
    #[prop(default = false)] is_standalone: bool,
    #[prop(default = false)] disable_remove: bool,
    #[prop(default = true)] show_add_override: bool,
    #[prop(into, default = None)] handle_key_remove: Option<Callback<String, ()>>,
) -> impl IntoView
// where
//     NF: Fn(Vec<(String, Value)>) + 'static,
{
    let default_config = StoredValue::new(default_config);
    let (override_keys, set_override_keys) = create_signal(
        HashSet::<String>::from_iter(overrides.get().clone().iter().map(|ele| ele.key.get())
    ));
    
    let default_config_map: HashMap<String, DefaultConfig> = default_config
        .get_value()
        .into_iter()
        .map(|ele| (ele.clone().key, ele))
        .collect();

    // let on_submit2 = move |event: MouseEvent| {
    //     event.prevent_default();
    //     logging::log!("<<>> start");
    //     let a: Vec<(Option<String>, Option<String>)> = overrides.get().into_iter().map(|ele| {
    //         (ele.key.get(), ele.value.get())
    //     }).collect();
    //     logging::log!("<<>> end {:?}", a);
    // };

    let on_submit = move |event: MouseEvent| {
        event.prevent_default();
        logging::log!("{:?}", overrides.get());
    };

    let handle_config_key_select = Callback::new(move |default_config: DefaultConfig| {
        let config_key = default_config.key;
        set_overrides.update(|value| {
            value.push(Override::new(config_key.clone(), json!("")));
        });
        set_override_keys.update(|keys| {
            keys.insert(config_key);
        })
    });

    let get_default_config_val = move |config_key_value: String, value: String| {
        get_config_value(
            &config_key_value,
            &value,
            &default_config
                .get_value()
                .into_iter()
                .map(ConfigType::DefaultConfig)
                .collect::<Vec<_>>(),
        )
        .expect("can't parse default config key")
    };

    let update_overrides = move |config_key_value: &str, value: String| {
        logging::log!("<<>> {:?} {:?}", config_key_value, value);
        let default_config_val =
            get_default_config_val(config_key_value.to_owned(), value);
        set_overrides.update(|curr_overrides| {
            let position = curr_overrides
                .iter()
                .position(|ele| ele.key.get().to_owned() == config_key_value);
            if let Some(idx) = position {
                curr_overrides[idx].value.set(json!(default_config_val));
            }
        });
    };

    view! {
        <div>
            <div class="form-control space-y-4">
                <div class="flex items-center justify-between gap-4">
                    <label class="label">
                        <span class="label-text font-semibold text-base">Overrides</span>
                    </label>
                </div>
                <div class="card w-full bg-slate-50">
                    <div class="card-body">
                        <Show when=move || {overrides.get().is_empty() && show_add_override}>
                            <div class="flex justify-center">
                                <Dropdown
                                    dropdown_btn_type=DropdownBtnType::Link
                                    dropdown_direction=DropdownDirection::Down
                                    dropdown_text=String::from("Add Override")
                                    dropdown_icon=String::from("ri-add-line")
                                    dropdown_options=default_config
                                    .get_value()
                                    on_select=handle_config_key_select
                                />
                            </div>
                        </Show>
                            
                        <Show when=move || overrides.get().is_empty()>
                            <div class="p-4 text-gray-400 flex flex-col justify-center items-center">
                                <div>
                                    <i class="ri-add-circle-line text-xl"></i>
                                </div>
                                <div>
                                    <span class="text-semibold text-sm">Add Override</span>
                                </div>
                            </div>
                        </Show>
                        <For each=move || overrides.get()
                            key=|row| row.key.get().clone()
                            children=move |row| {
                                    let config_key_label = row.key.get().to_string();
                                    let config_key_value = row.key.get().to_string();
                                    let config_value = row.value.get().to_string().replace('"', "");
                                    let schema: Map<String, Value> = serde_json::from_value(
                                            default_config_map
                                                .get(&config_key_label)
                                                .unwrap()
                                                .schema
                                                .clone(),
                                        )
                                        .unwrap();
                                    let key_type = get_key_type(&schema);
                                    view! {
                                        <div>
                                            <div class="flex items-center gap-4">
                                                <div class="form-control">
                                                    <label class="label font-medium font-mono text-sm">
                                                        <span class="label-text">{config_key_label.clone()} ":"</span>
                                                    </label>
                                                </div>
                                                <div class="form-control">
                                                    {match key_type.as_str() {
                                                        "ENUM" => {
                                                            view! {
                                                                <EnumDropdown
                                                                    schema
                                                                    config_value=config_value
                                                                    handle_change=Callback::new(move |selected_enum: String| {
                                                                        // update_overrides(&config_key_value, selected_enum)
                                                                        let default_config_val = get_default_config_val(config_key_value.to_owned(), selected_enum);
                                                                        row.value.set(default_config_val);
                                                                        logging::log!("<<>> {:?}", row.value.get().to_string().replace('"', ""));
                                                                    })

                                                                    class=String::from("mt-2")
                                                                />
                                                            }
                                                                .into_view()
                                                        }
                                                        "BOOLEAN" => {
                                                            if config_value.is_empty() {
                                                                // update_overrides(
                                                                //     &config_key_value,
                                                                //     config_value.parse::<bool>().unwrap_or(false).to_string(),
                                                                // );
                                                                let default_config_val = get_default_config_val(config_key_value.to_owned(), config_value.parse::<bool>().unwrap_or(false).to_string());
                                                                row.value.set(default_config_val);
                                                            }
                                                            view! {
                                                                <BooleanToggle
                                                                    config_value
                                                                    update_value=Callback::new(move |flag: String| {
                                                                        // update_overrides(&config_key_value, flag);
                                                                        let default_config_val = get_default_config_val(config_key_value.to_owned(), flag);
                                                                        row.value.set(default_config_val);
                                                                    })
                                                                />
                                                            }
                                                                .into_view()
                                                        }
                                                        _ => {
                                                            view! {
                                                                <div class="w-2/5">
                                                                    <textarea
                                                                        type="text"
                                                                        placeholder="Enter override here"
                                                                        name="override"
                                                                        class="input input-bordered w-[450px] flex items-center bg-white text-gray-700 shadow-md pt-3"
                                                                        on:change=move |event| {
                                                                            let input_value = event_target_value(&event);
                                                                            // update_overrides(&config_key_value, input_value);
                                                                            let default_config_val = get_default_config_val(config_key_value.to_owned(), input_value);
                                                                            row.value.set(default_config_val);
                                                                        }
                                                                    
                                                                    >{config_value}</textarea>
                                                                </div>
                                                            }
                                                                .into_view()
                                                        }
                                                    }}

                                                </div>
                                                <div class="w-1/5">

                                                    {if !disable_remove {
                                                        view! {
                                                            <button
                                                                class="btn btn-ghost btn-circle btn-sm"
                                                                on:click=move |ev| {
                                                                    ev.prevent_default();
                                                                    match handle_key_remove {
                                                                        Some(f) => f.call(config_key_label.clone()),
                                                                        None => {
                                                                            set_overrides
                                                                                .update(|value| {
                                                                                    let position = value
                                                                                        .iter()
                                                                                        .position(|ele| ele.key.get().to_owned() == config_key_label);
                                                                                    if let Some(idx) = position {
                                                                                        value.remove(idx);
                                                                                    }
                                                                                })
                                                                        }
                                                                    };
                                                                }
                                                            >

                                                                <i class="ri-delete-bin-2-line text-xl text-2xl font-bold"></i>
                                                            </button>
                                                        }
                                                            .into_view()
                                                    } else {
                                                        view! {}.into_view()
                                                    }}

                                                </div>
                                            </div>
                                        </div>
                                    }
                                }
                                />

                        <Show when=move || { !overrides.get().is_empty() && show_add_override }>
                            <div class="mt-4">

                                {move || {
                                    let unused_config_keys = default_config
                                        .get_value()
                                        .into_iter()
                                        .filter(|config| !override_keys.get().contains(&config.key))
                                        .collect::<Vec<DefaultConfig>>();
                                    view! {
                                        <Dropdown
                                            dropdown_direction=DropdownDirection::Down
                                            dropdown_text=String::from("Add Override")
                                            dropdown_icon=String::from("ri-add-line")
                                            dropdown_options=unused_config_keys.clone()
                                            on_select=handle_config_key_select
                                        />
                                    }
                                }}

                            </div>
                        </Show>
                    </div>
                </div>
            </div>
            // <TestForm overrides=overridess set_overrides=set_overridess/>
            // <button class="btn" on:click:undelegated=on_submit2>
            //     Save
            // </button>
            <Show when=move || is_standalone>
                <div class="flex justify-end">
                    <button class="btn" on:click:undelegated=on_submit>
                        Save
                    </button>
                </div>
            </Show>
        </div>
    }
}
