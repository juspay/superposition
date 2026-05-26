use leptos::*;
use strum_macros::Display;
use web_sys::MouseEvent;

use crate::components::button::{Button, ButtonStyle};

#[derive(Clone, Copy, PartialEq, PartialOrd, Display)]
#[strum(serialize_all = "snake_case")]
pub enum StepType {
    ContextStep,
    OverrideStep,
}

#[derive(Clone, Copy, PartialEq)]
pub enum StepStatus {
    Active,
    Completed,
    Pending,
}

#[derive(Clone)]
pub struct Step {
    pub title: String,
    pub description: Option<String>,
    pub step_type: StepType,
}

// Removing 'a causes a compile error, seems to be a clippy false positive
#[allow(clippy::needless_lifetimes)]
#[component]
pub fn StepIndicator<'a>(
    steps: &'a [Step],
    #[prop(into)] current_step: MaybeSignal<StepType>,
    #[prop(into, optional)] on_step_click: Option<Callback<usize, ()>>,
) -> impl IntoView {
    let total_steps = steps.len();

    view! {
        <div class="w-full">
            <div class="flex items-center justify-center">
                {steps
                    .iter()
                    .enumerate()
                    .map(move |(idx, step)| {
                        let title = step.title.clone();
                        let description = step.description.clone();
                        let step_type = step.step_type;

                        view! {
                            <div class="flex items-center">
                                <div class="flex flex-col items-center">
                                    <div
                                        class=move || {
                                            let cs = current_step.get();
                                            let common = "w-10 h-10 rounded-full flex items-center justify-center font-semibold text-lg border-2";
                                            if step_type == cs {
                                                format!(
                                                    "{} bg-purple-600 text-white border-purple-600",
                                                    common,
                                                )
                                            } else if step_type < cs {
                                                format!(
                                                    "{} bg-purple-100 text-purple-600 border-purple-600",
                                                    common,
                                                )
                                            } else {
                                                format!(
                                                    "{} bg-gray-100 text-gray-400 border-gray-300",
                                                    common,
                                                )
                                            }
                                        }
                                        class:cursor-pointer=move || {
                                            let cs = current_step.get();
                                            on_step_click.is_some() && step_type <= cs
                                        }
                                        class:hover:ring-2=move || {
                                            let cs = current_step.get();
                                            on_step_click.is_some() && step_type <= cs
                                        }
                                        class:hover:ring-purple-300=move || {
                                            let cs = current_step.get();
                                            on_step_click.is_some() && step_type <= cs
                                        }
                                        class:transition-all=true
                                        on:click=move |_| {
                                            let cs = current_step.get_untracked();
                                            if let Some(callback) = on_step_click {
                                                if step_type <= cs {
                                                    callback.call(idx);
                                                }
                                            }
                                        }
                                    >
                                        {move || {
                                            let cs = current_step.get();
                                            let is_completed = step_type < cs;
                                            if is_completed {
                                                view! { <i class="ri-check-line text-xl"></i> }.into_view()
                                            } else {
                                                view! { <span>{idx + 1}</span> }.into_view()
                                            }
                                        }}
                                    </div>
                                    <div class="mt-2 text-center">
                                        <p class=move || {
                                            let cs = current_step.get();
                                            let is_active = step_type == cs;
                                            let is_completed = step_type < cs;
                                            if is_active {
                                                "text-sm font-semibold text-purple-600"
                                            } else if is_completed {
                                                "text-sm font-semibold text-purple-500"
                                            } else {
                                                "text-sm font-medium text-gray-400"
                                            }
                                        }>{title}</p>
                                        {description
                                            .map(|desc| {
                                                view! {
                                                    <p class=move || {
                                                        let cs = current_step.get();
                                                        if step_type == cs {
                                                            "text-xs text-gray-600"
                                                        } else {
                                                            "text-xs text-gray-400"
                                                        }
                                                    }>{desc}</p>
                                                }
                                            })}
                                    </div>
                                </div>
                                {if idx != total_steps - 1 {
                                    view! {
                                        <div class=move || {
                                            let cs = current_step.get();
                                            if step_type < cs {
                                                "flex-1 h-1 bg-purple-600 mx-4 mt-[-24px]"
                                            } else {
                                                "flex-1 h-1 bg-gray-300 mx-4 mt-[-24px]"
                                            }
                                        }></div>
                                    }
                                        .into_view()
                                } else {
                                    ().into_view()
                                }}
                            </div>
                        }
                    })
                    .collect_view()}
            </div>
        </div>
    }
}

#[component]
pub fn StepNavigation(
    #[prop(into)] current_step: MaybeSignal<StepType>,
    #[prop(into)] on_previous: Callback<(), ()>,
    #[prop(into)] on_next: Callback<(), ()>,
    #[prop(optional)] on_submit: Option<Callback<(), ()>>,
    #[prop(default = false)] is_last_step: bool,
    #[prop(default = false)] next_disabled: bool,
    #[prop(into, optional)] submit_loading: MaybeSignal<bool>,
    #[prop(default = "Next".to_string())] next_text: String,
    #[prop(default = "Submit".to_string())] submit_text: String,
    #[prop(default = "Previous".to_string())] previous_text: String,
) -> impl IntoView {
    let _is_last_step = is_last_step; // Used for future extensibility
    let on_submit = StoredValue::new(on_submit);
    let next_text = StoredValue::new(next_text);
    let submit_text = StoredValue::new(submit_text);
    let previous_text = StoredValue::new(previous_text);

    let show_previous = move || -> bool {
        let step = current_step.get();
        step != StepType::ContextStep
    };

    view! {
        <div class="flex justify-between items-center pt-6 pb-2">
            <div>
                {move || {
                    if show_previous() {
                        let on_prev_click = Callback::new(move |_: MouseEvent| {
                            on_previous.call(())
                        });
                        view! {
                            <Button
                                text=previous_text.get_value()
                                on_click=on_prev_click
                                icon_class="ri-arrow-left-line"
                                style=ButtonStyle::Outline
                            />
                        }
                            .into_view()
                    } else {
                        ().into_view()
                    }
                }}
            </div>
            <div class="flex gap-3">
                {move || {
                    if current_step.get() == StepType::OverrideStep {
                        let cb = on_submit.get_value();
                        let on_submit_click = Callback::new(move |_: MouseEvent| {
                            if let Some(callback) = cb {
                                callback.call(());
                            }
                        });
                        view! {
                            <Button
                                text=submit_text.get_value()
                                on_click=on_submit_click
                                icon_class="ri-check-line"
                                loading=submit_loading.get()
                            />
                        }
                            .into_view()
                    } else {
                        let on_next_click = Callback::new(move |_: MouseEvent| on_next.call(()));
                        view! {
                            <Button
                                text=next_text.get_value()
                                on_click=on_next_click
                                icon_class="ri-arrow-right-line"
                                disabled=next_disabled
                            />
                        }
                            .into_view()
                    }
                }}
            </div>
        </div>
    }
}

#[component]
pub fn MultiStepForm(
    steps: Vec<Step>,
    current_step: RwSignal<StepType>,
    children: Children,
) -> impl IntoView {
    view! {
        <div class="flex flex-col gap-6">
            <StepIndicator steps=&steps current_step=current_step />
            <div class="flex-1">{children()}</div>
        </div>
    }
}
