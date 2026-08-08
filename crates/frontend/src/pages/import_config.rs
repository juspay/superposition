use leptos::*;
use leptos_router::A;
use superposition_types::MarkupFormat;
use wasm_bindgen::{JsCast, closure::Closure};
use web_sys::{Event, FileReader, HtmlInputElement};

use crate::{
    api::config_import::{
        self, ImportEntityReport, ImportOptions, ImportStrategy,
        ImportSummary,
    },
    components::{
        alert::AlertType,
        button::{Button, ButtonStyle},
        modal::PortalModal,
    },
    providers::alert_provider::enqueue_alert,
    types::{OrganisationId, Workspace},
    utils::use_url_base,
};

fn format_file_size(size: u64) -> String {
    const KB: f64 = 1024.0;
    const MB: f64 = KB * 1024.0;
    let size = size as f64;

    if size >= MB {
        format!("{:.1} MB", size / MB)
    } else if size >= KB {
        format!("{:.1} KB", size / KB)
    } else {
        format!("{} B", size as u64)
    }
}

fn strategy_choice_class(active: bool) -> String {
    let state = if active {
        "border-purple-400 bg-white text-purple-900 shadow-sm ring-1 ring-purple-100"
    } else {
        "border-transparent text-gray-700 hover:border-gray-200 hover:bg-white"
    };

    format!(
        "min-h-[92px] rounded-md border px-4 py-3 text-left transition-colors {state}"
    )
}

fn strategy_icon_class(active: bool) -> String {
    let state = if active {
        "bg-purple-50 text-purple-700"
    } else {
        "bg-gray-100 text-gray-600"
    };

    format!("mt-0.5 flex h-7 w-7 shrink-0 items-center justify-center rounded-md {state}")
}

#[component]
fn StrategyChoice(
    strategy: ImportStrategy,
    selected: RwSignal<ImportStrategy>,
    title: &'static str,
    description: &'static str,
    icon: &'static str,
    on_select: Callback<ImportStrategy>,
) -> impl IntoView {
    view! {
        <button
            type="button"
            aria-pressed=move || selected.get() == strategy
            class=move || strategy_choice_class(selected.get() == strategy)
            on:click=move |_| on_select.call(strategy)
        >
            <div class="flex items-start gap-3">
                <span class=move || strategy_icon_class(selected.get() == strategy)>
                    <i class=icon />
                </span>
                <span class="min-w-0">
                    <span class="block text-sm font-semibold">{title}</span>
                    <span class="mt-1 block text-xs leading-5 text-gray-500">
                        {description}
                    </span>
                </span>
            </div>
        </button>
    }
}

fn metric_text(value: usize, label: &str) -> String {
    format!("{value} {label}")
}

#[component]
fn SummaryStripItem(
    #[prop(into)] title: String,
    #[prop(into)] icon: String,
    report: ImportEntityReport,
) -> impl IntoView {
    view! {
        <div class="flex min-w-0 items-start gap-3 px-4 py-3">
            <span class="flex h-9 w-9 shrink-0 items-center justify-center rounded-lg bg-purple-50 text-purple-700">
                <i class=format!("{icon} text-lg") />
            </span>
            <div class="min-w-0">
                <div class="text-sm font-semibold text-gray-900">{title}</div>
                <div class="mt-2 flex flex-wrap gap-x-4 gap-y-1 text-xs text-gray-500">
                    <span>
                        <span class="font-semibold text-emerald-600">{report.created}</span>
                        " created"
                    </span>
                    <span>
                        <span class="font-semibold text-blue-600">{report.updated}</span>
                        " updated"
                    </span>
                    <span>
                        <span class="font-semibold text-gray-600">{report.skipped}</span>
                        " skipped"
                    </span>
                    <span>
                        <span class="font-semibold text-rose-600">{report.deleted}</span>
                        " deleted"
                    </span>
                </div>
            </div>
        </div>
    }
}

#[component]
fn SummaryStrip(summary: ImportSummary) -> impl IntoView {
    view! {
        <div class="grid rounded-lg border border-gray-200 bg-white divide-y divide-gray-200 lg:grid-cols-3 lg:divide-x lg:divide-y-0">
            <SummaryStripItem
                title="Dimensions"
                icon="ri-ruler-2-fill"
                report=summary.dimensions.clone()
            />
            <SummaryStripItem
                title="Default Config"
                icon="ri-tools-line"
                report=summary.default_configs.clone()
            />
            <SummaryStripItem
                title="Overrides"
                icon="ri-guide-fill"
                report=summary.contexts.clone()
            />
        </div>
    }
}

#[component]
fn ImportSummaryPanel(
    summary: ImportSummary,
    #[prop(into)] heading: String,
    #[prop(into, default = String::new())] version_href: String,
) -> impl IntoView {
    let total_changes = summary.total_changes();
    let total_deleted = summary.total_deleted();
    let has_deleted = total_deleted > 0;
    let strategy = summary.strategy.as_str();
    let config_version = summary.config_version.clone();

    view! {
        <section class="flex flex-col gap-4">
            <div class="flex flex-col gap-3 md:flex-row md:items-center md:justify-between">
                <div>
                    <h2 class="text-lg font-semibold text-gray-900">{heading}</h2>
                    <div class="mt-1 flex flex-wrap gap-2 text-xs">
                        <span class="badge badge-neutral">{strategy}</span>
                        <span class="badge badge-outline">
                            {if summary.dry_run { "Preview" } else { "Applied" }}
                        </span>
                        <span class="badge badge-outline">
                            {metric_text(total_changes, "total")}
                        </span>
                        <Show when=move || has_deleted>
                            <span class="badge badge-error text-white">
                                {metric_text(total_deleted, "deleted")}
                            </span>
                        </Show>
                    </div>
                </div>
                <Show when=move || config_version.is_some()>
                    <A class="btn btn-sm btn-purple-outline w-fit" href=version_href.clone()>
                        "Open Config Version"
                        <i class="ri-arrow-right-up-line" />
                    </A>
                </Show>
            </div>
            <SummaryStrip summary=summary.clone() />
        </section>
    }
}

#[component]
pub fn ImportConfig() -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let base = StoredValue::new(use_url_base());

    let file_name_rws = RwSignal::new(String::new());
    let file_size_rws = RwSignal::new(None::<u64>);
    let file_text_rws = RwSignal::new(String::new());
    let format_rws = RwSignal::new(MarkupFormat::Toml);
    let strategy_rws = RwSignal::new(ImportStrategy::Upsert);
    let tags_rws = RwSignal::new(String::new());
    let preview_rws = RwSignal::new(None::<ImportSummary>);
    let applied_rws = RwSignal::new(None::<ImportSummary>);
    let preview_loading_rws = RwSignal::new(false);
    let apply_loading_rws = RwSignal::new(false);
    let show_confirm_rws = RwSignal::new(false);
    let file_read_generation_rws = RwSignal::new(0_u64);
    let file_input_ref = create_node_ref::<html::Input>();

    let clear_results = move || {
        preview_rws.set(None);
        applied_rws.set(None);
        show_confirm_rws.set(false);
    };
    let select_strategy = Callback::new(move |strategy| {
        strategy_rws.set(strategy);
        clear_results();
    });

    let clear_file = move |_| {
        file_read_generation_rws
            .update(|generation| *generation = generation.wrapping_add(1));
        file_name_rws.set(String::new());
        file_size_rws.set(None);
        file_text_rws.set(String::new());
        clear_results();
        if let Some(input) = file_input_ref.get() {
            input.set_value("");
        }
    };

    let open_file_picker = move |_| {
        if let Some(input) = file_input_ref.get() {
            input.click();
        }
    };

    let submit_import = Callback::new(move |dry_run: bool| {
        let file_text = file_text_rws.get_untracked();
        if file_text.trim().is_empty() {
            enqueue_alert(
                "Choose a config file before importing.".to_string(),
                AlertType::Error,
                3000,
            );
            return;
        }

        let workspace = workspace.get_untracked().0;
        let org_id = org.get_untracked().0;
        let format = format_rws.get_untracked();
        let strategy = strategy_rws.get_untracked();
        let options = ImportOptions {
            strategy,
            dry_run,
            config_tags: tags_rws.get_untracked(),
        };

        if dry_run {
            preview_loading_rws.set(true);
        } else {
            apply_loading_rws.set(true);
            show_confirm_rws.set(false);
        }

        spawn_local(async move {
            let result = config_import::import_config(
                file_text, format, options, &workspace, &org_id,
            )
            .await;

            match result {
                Ok(summary) if dry_run => {
                    preview_rws.set(Some(summary));
                    enqueue_alert(
                        "Import preview is ready.".to_string(),
                        AlertType::Success,
                        3000,
                    );
                }
                Ok(summary) => {
                    preview_rws.set(None);
                    applied_rws.set(Some(summary));
                    enqueue_alert(
                        "Config import applied.".to_string(),
                        AlertType::Success,
                        3000,
                    );
                }
                Err(error) => {
                    enqueue_alert(error, AlertType::Error, 5000);
                }
            }

            if dry_run {
                preview_loading_rws.set(false);
            } else {
                apply_loading_rws.set(false);
            }
        });
    });

    let on_file_change =
        move |ev: Event| {
            let input = event_target::<HtmlInputElement>(&ev);
            let Some(file) = input.files().and_then(|files| files.get(0)) else {
                return;
            };

            let file_name = file.name();
            file_name_rws.set(file_name.clone());
            file_size_rws.set(Some(file.size() as u64));
            format_rws.set(
                if file_name.to_ascii_lowercase().ends_with(".json") {
                    MarkupFormat::Json
                } else {
                    MarkupFormat::Toml
                },
            );
            file_text_rws.set(String::new());
            clear_results();
            let read_generation =
                file_read_generation_rws.get_untracked().wrapping_add(1);
            file_read_generation_rws.set(read_generation);

            let Ok(reader) = FileReader::new() else {
                enqueue_alert(
                    "Unable to read the selected file.".to_string(),
                    AlertType::Error,
                    3000,
                );
                return;
            };
            let reader_for_load = reader.clone();
            let onload = Closure::<dyn FnMut(Event)>::new(move |_| {
                if file_read_generation_rws.get_untracked() != read_generation {
                    return;
                }

                match reader_for_load
                    .result()
                    .ok()
                    .and_then(|value| value.as_string())
                {
                    Some(text) => file_text_rws.set(text),
                    None => enqueue_alert(
                        "Unable to read the selected file as text.".to_string(),
                        AlertType::Error,
                        3000,
                    ),
                }
            });

            reader.set_onload(Some(onload.as_ref().unchecked_ref()));
            let file_blob: &web_sys::Blob = file.as_ref();
            if reader.read_as_text(file_blob).is_err() {
                enqueue_alert(
                    "Unable to start reading the selected file.".to_string(),
                    AlertType::Error,
                    3000,
                );
            }
            onload.forget();
        };

    let apply_preview = move |_| {
        let requires_confirmation = preview_rws.with(|summary| {
            summary
                .as_ref()
                .map(|summary| {
                    strategy_rws.get_untracked() == ImportStrategy::Replace
                        || summary.total_deleted() > 0
                })
                .unwrap_or(false)
        });

        if requires_confirmation {
            show_confirm_rws.set(true);
        } else {
            submit_import.call(false);
        }
    };

    view! {
        <div class="h-full overflow-y-auto">
            <div class="flex min-h-full flex-col gap-4">
                <div class="flex flex-col gap-2">
                    <div>
                        <h1 class="text-2xl font-semibold text-gray-900">"Import Config"</h1>
                        <p class="mt-1 max-w-3xl text-sm text-gray-600">
                            "Import dimensions, default config, and overrides from a Superposition config file."
                        </p>
                    </div>
                </div>

                <section class="card bg-base-100 shadow">
                    <div class="card-body gap-6">
                        <div class="form-control gap-2">
                            <label class="label p-0">
                                <span class="label-text font-semibold">"File"</span>
                            </label>
                            <input
                                ref_=file_input_ref
                                type="file"
                                accept=".toml,.json,application/toml,application/json"
                                class="hidden"
                                on:change=on_file_change
                            />
                            <div class="flex flex-col gap-3 rounded-lg border border-gray-200 bg-white px-4 py-3 md:flex-row md:items-center md:justify-between">
                                <div class="flex min-w-0 items-center gap-3">
                                    <span class="flex h-10 w-10 shrink-0 items-center justify-center rounded-lg bg-purple-50 text-purple-700">
                                        <i class="ri-file-text-line text-xl" />
                                    </span>
                                    <div class="min-w-0">
                                        <div class="truncate text-sm font-semibold text-gray-800">
                                            {move || {
                                                if file_name_rws.with(String::is_empty) {
                                                    "No file selected".to_string()
                                                } else {
                                                    file_name_rws.get()
                                                }
                                            }}
                                        </div>
                                        <div class="mt-1 flex h-5 flex-wrap items-center gap-x-3 gap-y-1 text-xs text-gray-500">
                                            <Show
                                                when=move || !file_name_rws.with(String::is_empty)
                                                fallback=move || {
                                                    view! { <span>"Choose a Superposition config file."</span> }
                                                }
                                            >
                                                <span>
                                                    {move || {
                                                        file_size_rws
                                                            .get()
                                                            .map(format_file_size)
                                                            .unwrap_or_default()
                                                    }}
                                                </span>
                                            </Show>
                                        </div>
                                    </div>
                                </div>
                                <div class="flex shrink-0 flex-wrap gap-2">
                                    <button
                                        type="button"
                                        class="btn btn-sm btn-purple-outline"
                                        on:click=open_file_picker
                                    >
                                        <i class="ri-upload-cloud-2-line" />
                                        "Choose File"
                                    </button>
                                    <button
                                        type="button"
                                        class="btn btn-sm btn-outline border-gray-200"
                                        disabled=move || file_name_rws.with(String::is_empty)
                                        on:click=clear_file
                                    >
                                        "Clear"
                                    </button>
                                </div>
                            </div>
                        </div>

                        <div class="form-control gap-2">
                            <label class="label p-0">
                                <span class="label-text font-semibold">"Import Strategy"</span>
                            </label>
                            <div class="grid gap-1 rounded-lg border border-gray-200 bg-gray-50 p-1 md:grid-cols-3">
                                <StrategyChoice
                                    strategy=ImportStrategy::CreateOnly
                                    selected=strategy_rws
                                    title="Create only"
                                    description="Create missing items, skip existing ones."
                                    icon="ri-add-line"
                                    on_select=select_strategy
                                />
                                <StrategyChoice
                                    strategy=ImportStrategy::Upsert
                                    selected=strategy_rws
                                    title="Create or update"
                                    description="Create missing items, update existing ones."
                                    icon="ri-loop-left-line"
                                    on_select=select_strategy
                                />
                                <StrategyChoice
                                    strategy=ImportStrategy::Replace
                                    selected=strategy_rws
                                    title="Replace workspace"
                                    description="Mirror the file, remove missing items."
                                    icon="ri-restart-line"
                                    on_select=select_strategy
                                />
                            </div>
                        </div>

                        <div class="collapse collapse-arrow rounded-lg border border-gray-200 bg-gray-50">
                            <input type="checkbox" />
                            <div class="collapse-title text-sm font-semibold text-gray-800">
                                "Advanced Options"
                            </div>
                            <div class="collapse-content flex flex-col gap-4">
                                <div class="form-control gap-2">
                                    <label class="label p-0">
                                        <span class="label-text font-medium">"Config Tags"</span>
                                    </label>
                                    <input
                                        type="text"
                                        class="input input-bordered w-full bg-white"
                                        placeholder="release, backup, migration"
                                        prop:value=move || tags_rws.get()
                                        on:input=move |ev| {
                                            tags_rws.set(event_target_value(&ev));
                                            clear_results();
                                        }
                                    />
                                </div>
                            </div>
                        </div>

                        <div class="flex flex-col gap-3 border-t border-gray-100 pt-2 md:flex-row md:justify-end">
                            <button
                                type="button"
                                class="btn-purple flex h-11 min-w-[11rem] items-center justify-center gap-2 rounded-lg px-5 py-2.5 text-sm font-medium"
                                disabled=move || {
                                    file_text_rws.with(|text| text.trim().is_empty())
                                        || preview_loading_rws.get() || apply_loading_rws.get()
                                }
                                on:click=move |_| submit_import.call(true)
                            >
                                <Show
                                    when=move || preview_loading_rws.get()
                                    fallback=move || {
                                        view! {
                                            <>"Preview Changes" <i class="ri-search-eye-line" /></>
                                        }
                                    }
                                >
                                    <span class="loading loading-dots loading-sm" />
                                </Show>
                            </button>
                            <button
                                type="button"
                                class="btn btn-purple-outline flex h-11 min-w-[11rem] items-center justify-center gap-2 text-sm font-medium"
                                disabled=move || {
                                    !preview_rws.with(|summary| summary.is_some())
                                        || preview_loading_rws.get() || apply_loading_rws.get()
                                }
                                on:click=apply_preview
                            >
                                <Show
                                    when=move || apply_loading_rws.get()
                                    fallback=move || {
                                        view! {
                                            <>"Apply Import" <i class="ri-upload-cloud-2-line" /></>
                                        }
                                    }
                                >
                                    <span class="loading loading-dots loading-sm" />
                                </Show>
                            </button>
                        </div>
                    </div>
                </section>

                {move || {
                    preview_rws
                        .get()
                        .map(|summary| {
                            view! {
                                <section class="card bg-base-100 shadow">
                                    <div class="card-body">
                                        <ImportSummaryPanel
                                            summary=summary
                                            heading="Preview Summary"
                                        />
                                    </div>
                                </section>
                            }
                        })
                }}

                <Show when=move || {
                    applied_rws.with(|summary| summary.is_some())
                }>
                    {move || {
                        applied_rws
                            .get()
                            .map(|summary| {
                                let href = summary
                                    .config_version
                                    .as_ref()
                                    .map(|version| {
                                        format!(
                                            "{}/admin/{}/{}/config/versions/{}",
                                            base.get_value(),
                                            org.get_untracked().0,
                                            workspace.get_untracked().0,
                                            version,
                                        )
                                    });
                                view! {
                                    <section class="card bg-base-100 shadow">
                                        <div class="card-body">
                                            <ImportSummaryPanel
                                                summary=summary
                                                heading="Applied Summary"
                                                version_href=href.unwrap_or_default()
                                            />
                                        </div>
                                    </section>
                                }
                            })
                    }}
                </Show>

                <Show when=move || show_confirm_rws.get()>
                    <PortalModal
                        class="w-full max-w-lg"
                        heading="Apply Import"
                        handle_close=Callback::new(move |_| show_confirm_rws.set(false))
                    >
                        <div class="flex flex-col gap-4">
                            <div class="rounded-lg bg-amber-50 p-4 text-sm text-amber-800">
                                <div class="flex gap-2">
                                    <i class="ri-error-warning-line text-lg" />
                                    <div>
                                        <div class="font-semibold">"Confirm workspace changes"</div>
                                        <div class="mt-1">
                                            {move || {
                                                let deleted = preview_rws
                                                    .with(|summary| {
                                                        summary
                                                            .as_ref()
                                                            .map(ImportSummary::total_deleted)
                                                            .unwrap_or_default()
                                                    });
                                                if deleted > 0 {
                                                    format!(
                                                        "This import will delete {deleted} item(s) missing from the file.",
                                                    )
                                                } else {
                                                    "This import will apply the previewed replace operation."
                                                        .to_string()
                                                }
                                            }}
                                        </div>
                                    </div>
                                </div>
                            </div>
                            <div class="flex justify-end gap-3">
                                <Button
                                    text="Cancel"
                                    icon_class="ri-close-line"
                                    style=ButtonStyle::Outline
                                    on_click=move |_| show_confirm_rws.set(false)
                                />
                                {move || {
                                    view! {
                                        <Button
                                            text="Apply Import"
                                            icon_class="ri-upload-cloud-2-line"
                                            loading=apply_loading_rws.get()
                                            on_click=move |_| submit_import.call(false)
                                        />
                                    }
                                }}
                            </div>
                        </div>
                    </PortalModal>
                </Show>
            </div>
        </div>
    }
}
