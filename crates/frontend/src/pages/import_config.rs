use std::collections::{BTreeMap, BTreeSet};

use leptos::*;
use leptos_router::A;
use serde_json::{Map, Value};
use wasm_bindgen::{JsCast, closure::Closure};
use web_sys::{Event, FileReader, HtmlInputElement};

use crate::{
    api::config_import::{
        self, ImportEntityReport, ImportFormat, ImportOnError, ImportOptions,
        ImportStrategy, ImportSummary,
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

fn parse_config_text(text: &str, format: ImportFormat) -> Result<Value, String> {
    match format {
        ImportFormat::Json => serde_json::from_str(text)
            .map_err(|err| format!("Unable to parse JSON preview: {err}")),
        ImportFormat::Toml => {
            let value: toml::Value = toml::from_str(text)
                .map_err(|err| format!("Unable to parse TOML preview: {err}"))?;
            serde_json::to_value(value)
                .map_err(|err| format!("Unable to normalize TOML preview: {err}"))
        }
    }
}

fn object_section(config: &Value, section: &str) -> BTreeMap<String, Value> {
    config
        .get(section)
        .and_then(Value::as_object)
        .map(|items| {
            items
                .iter()
                .map(|(key, value)| (key.clone(), value.clone()))
                .collect()
        })
        .unwrap_or_default()
}

fn array_section(config: &Value, section: &str) -> Vec<Value> {
    config
        .get(section)
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_default()
}

fn value_preview(value: &Value) -> String {
    match value {
        Value::Null => "null".to_string(),
        Value::Bool(value) => value.to_string(),
        Value::Number(value) => value.to_string(),
        Value::String(value) => value.clone(),
        _ => serde_json::to_string(value).unwrap_or_else(|_| "-".to_string()),
    }
}

fn canonical_json(value: &Value) -> String {
    serde_json::to_string(value).unwrap_or_default()
}

fn description_preview(entry: &Value) -> String {
    entry
        .get("description")
        .and_then(Value::as_str)
        .filter(|description| !description.trim().is_empty())
        .map(ToString::to_string)
        .unwrap_or_else(|| "-".to_string())
}

fn default_config_value_preview(entry: &Value) -> String {
    entry
        .get("value")
        .map(value_preview)
        .unwrap_or_else(|| value_preview(entry))
}

fn context_preview(context: &Value) -> String {
    let Some(context) = context.as_object() else {
        return "-".to_string();
    };

    if context.is_empty() {
        return "default".to_string();
    }

    context
        .iter()
        .map(|(key, value)| format!("{key} = {}", value_preview(value)))
        .collect::<Vec<_>>()
        .join(", ")
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ReviewAction {
    Create,
    Update,
    Delete,
    Skip,
}

impl ReviewAction {
    fn label(self) -> &'static str {
        match self {
            Self::Create => "Create",
            Self::Update => "Update",
            Self::Delete => "Delete",
            Self::Skip => "Skip",
        }
    }

    fn class(self) -> &'static str {
        match self {
            Self::Create => "border-emerald-100 bg-emerald-50 text-emerald-700",
            Self::Update => "border-blue-100 bg-blue-50 text-blue-700",
            Self::Delete => "border-rose-100 bg-rose-50 text-rose-700",
            Self::Skip => "border-gray-200 bg-gray-50 text-gray-600",
        }
    }
}

#[derive(Clone, Debug)]
struct ReviewRow {
    name: String,
    action: ReviewAction,
    description: String,
    scope: String,
    value: String,
}

#[derive(Clone, Debug, Default)]
struct ReviewTables {
    dimensions: Vec<ReviewRow>,
    default_configs: Vec<ReviewRow>,
    overrides: Vec<ReviewRow>,
}

#[derive(Clone, Debug)]
struct OverrideEntry {
    name: String,
    scope: String,
    value: String,
    raw: Value,
}

fn review_action(
    imported: Option<&Value>,
    current: Option<&Value>,
    strategy: ImportStrategy,
) -> Option<ReviewAction> {
    match (imported, current) {
        (Some(_), None) => Some(ReviewAction::Create),
        (Some(_), Some(_)) if strategy == ImportStrategy::CreateOnly => {
            Some(ReviewAction::Skip)
        }
        (Some(_), Some(_)) => Some(ReviewAction::Update),
        (None, Some(_)) if strategy == ImportStrategy::Replace => {
            Some(ReviewAction::Delete)
        }
        (None, Some(_)) => None,
        (None, None) => None,
    }
}

fn build_map_review_rows(
    imported_config: &Value,
    current_config: &Value,
    section: &str,
    strategy: ImportStrategy,
    row_builder: fn(&str, ReviewAction, &Value) -> ReviewRow,
) -> Vec<ReviewRow> {
    let imported = object_section(imported_config, section);
    let current = object_section(current_config, section);
    let mut keys = BTreeSet::new();
    keys.extend(imported.keys().cloned());
    if strategy == ImportStrategy::Replace {
        keys.extend(current.keys().cloned());
    }

    keys.into_iter()
        .filter_map(|name| {
            let imported_entry = imported.get(&name);
            let current_entry = current.get(&name);
            let action = review_action(imported_entry, current_entry, strategy)?;
            let entry = imported_entry.or(current_entry)?;

            Some(row_builder(&name, action, entry))
        })
        .collect()
}

fn dimension_review_row(name: &str, action: ReviewAction, entry: &Value) -> ReviewRow {
    ReviewRow {
        name: name.to_string(),
        action,
        description: description_preview(entry),
        scope: String::new(),
        value: String::new(),
    }
}

fn default_config_review_row(
    name: &str,
    action: ReviewAction,
    entry: &Value,
) -> ReviewRow {
    ReviewRow {
        name: name.to_string(),
        action,
        description: String::new(),
        scope: String::new(),
        value: default_config_value_preview(entry),
    }
}

fn flatten_overrides(config: &Value) -> BTreeMap<String, OverrideEntry> {
    let mut entries = BTreeMap::new();

    for override_block in array_section(config, "overrides") {
        let Some(object) = override_block.as_object() else {
            continue;
        };
        let context = object
            .get("_context_")
            .cloned()
            .unwrap_or_else(|| Value::Object(Map::new()));
        let context_key = canonical_json(&context);
        let scope = context_preview(&context);

        for (name, value) in object {
            if name == "_context_" {
                continue;
            }

            let mut raw = Map::new();
            raw.insert("_context_".to_string(), context.clone());
            raw.insert(name.clone(), value.clone());
            entries.insert(
                format!("{context_key}::{name}"),
                OverrideEntry {
                    name: name.clone(),
                    scope: scope.clone(),
                    value: value_preview(value),
                    raw: Value::Object(raw),
                },
            );
        }
    }

    entries
}

fn build_override_review_rows(
    imported_config: &Value,
    current_config: &Value,
    strategy: ImportStrategy,
) -> Vec<ReviewRow> {
    let imported = flatten_overrides(imported_config);
    let current = flatten_overrides(current_config);
    let mut keys = BTreeSet::new();
    keys.extend(imported.keys().cloned());
    if strategy == ImportStrategy::Replace {
        keys.extend(current.keys().cloned());
    }

    keys.into_iter()
        .filter_map(|key| {
            let imported_entry = imported.get(&key);
            let current_entry = current.get(&key);
            let imported_raw = imported_entry.map(|entry| &entry.raw);
            let current_raw = current_entry.map(|entry| &entry.raw);
            let action = review_action(imported_raw, current_raw, strategy)?;
            let entry = imported_entry.or(current_entry)?;

            Some(ReviewRow {
                name: entry.name.clone(),
                action,
                description: String::new(),
                scope: entry.scope.clone(),
                value: entry.value.clone(),
            })
        })
        .collect()
}

fn build_review_tables(
    import_text: &str,
    current_text: &str,
    format: ImportFormat,
    strategy: ImportStrategy,
) -> Result<ReviewTables, String> {
    let imported_config = parse_config_text(import_text, format)?;
    let current_config = parse_config_text(current_text, format)?;

    Ok(ReviewTables {
        dimensions: build_map_review_rows(
            &imported_config,
            &current_config,
            "dimensions",
            strategy,
            dimension_review_row,
        ),
        default_configs: build_map_review_rows(
            &imported_config,
            &current_config,
            "default-configs",
            strategy,
            default_config_review_row,
        ),
        overrides: build_override_review_rows(
            &imported_config,
            &current_config,
            strategy,
        ),
    })
}

#[derive(Clone, Debug)]
struct SummaryError {
    section: &'static str,
    id: String,
    message: String,
}

fn collect_errors(summary: &ImportSummary) -> Vec<SummaryError> {
    let mut errors = Vec::new();
    for (section, report) in [
        ("Dimensions", &summary.dimensions),
        ("Default Config", &summary.default_configs),
        ("Overrides", &summary.contexts),
    ] {
        for error in &report.errors {
            errors.push(SummaryError {
                section,
                id: error.id.clone(),
                message: error.message.clone(),
            });
        }
    }
    errors
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
    let total_errors = summary.total_errors();
    let has_deleted = total_deleted > 0;
    let has_errors = total_errors > 0;
    let strategy = summary.strategy.clone();
    let config_version = summary.config_version.clone();
    let errors = StoredValue::new(collect_errors(&summary));

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
                        <Show when=move || has_errors>
                            <span class="badge badge-warning text-white">
                                {metric_text(total_errors, "errors")}
                            </span>
                        </Show>
                    </div>
                </div>
                <Show when=move || config_version.is_some()>
                    <A
                        class="btn btn-sm btn-purple-outline w-fit"
                        href=version_href.clone()
                    >
                        "Open Config Version"
                        <i class="ri-arrow-right-up-line" />
                    </A>
                </Show>
            </div>
            <SummaryStrip summary=summary.clone() />
            <Show when=move || !errors.get_value().is_empty()>
                <div class="rounded-lg border border-red-100 bg-red-50 p-3">
                    <div class="mb-2 flex items-center gap-2 text-sm font-semibold text-red-700">
                        <i class="ri-error-warning-line" />
                        <span>"Errors"</span>
                    </div>
                    <div class="max-h-44 overflow-y-auto">
                        <For
                            each=move || errors.get_value()
                            key=|error| format!("{}:{}:{}", error.section, error.id, error.message)
                            children=move |error| {
                                view! {
                                    <div class="border-t border-red-100 py-2 first:border-t-0">
                                        <div class="text-sm font-medium text-red-800">
                                            {format!("{}: {}", error.section, error.id)}
                                        </div>
                                        <div class="text-xs text-red-700">{error.message}</div>
                                    </div>
                                }
                            }
                        />
                    </div>
                </div>
            </Show>
        </section>
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ReviewTableKind {
    Dimensions,
    DefaultConfig,
    Overrides,
}

#[component]
fn ActionBadge(action: ReviewAction) -> impl IntoView {
    view! {
        <span class=format!(
            "inline-flex rounded-full border px-2 py-0.5 text-xs font-semibold {}",
            action.class(),
        )>
            {action.label()}
        </span>
    }
}

#[component]
fn ReviewTableSection(
    #[prop(into)] title: String,
    #[prop(into)] icon: String,
    rows: Vec<ReviewRow>,
    kind: ReviewTableKind,
) -> impl IntoView {
    let row_count = rows.len();
    let rows = StoredValue::new(rows);

    view! {
        <section class="rounded-lg border border-gray-200 bg-white">
            <div class="flex items-center justify-between gap-3 px-4 py-3">
                <div class="flex items-center gap-2">
                    <i class=format!("{icon} text-lg text-purple-700") />
                    <h3 class="text-sm font-semibold text-gray-900">
                        {format!("{title} ({row_count})")}
                    </h3>
                </div>
            </div>
            <div class="overflow-x-auto">
                <table class="w-full min-w-[680px] border-t border-gray-200 text-sm">
                    <thead>
                        <tr class="bg-gray-50 text-left text-xs font-semibold text-gray-500">
                            <th class="w-[32%] px-4 py-2">"Name"</th>
                            <th class="w-[18%] px-4 py-2">"Action"</th>
                            {match kind {
                                ReviewTableKind::Dimensions => view! {
                                    <th class="px-4 py-2">"Description"</th>
                                }.into_view(),
                                ReviewTableKind::DefaultConfig => view! {
                                    <th class="px-4 py-2">"Value"</th>
                                }.into_view(),
                                ReviewTableKind::Overrides => view! {
                                    <>
                                        <th class="w-[24%] px-4 py-2">"Scope"</th>
                                        <th class="px-4 py-2">"Value"</th>
                                    </>
                                }.into_view(),
                            }}
                        </tr>
                    </thead>
                    <tbody>
                        <Show
                            when=move || row_count != 0
                            fallback=move || {
                                let colspan = if kind == ReviewTableKind::Overrides {
                                    "4"
                                } else {
                                    "3"
                                };
                                view! {
                                    <tr>
                                        <td
                                            colspan=colspan
                                            class="px-4 py-5 text-center text-sm text-gray-500"
                                        >
                                            "No changes in this section."
                                        </td>
                                    </tr>
                                }
                            }
                        >
                            <For
                                each=move || rows.get_value()
                                key=|row| {
                                    format!(
                                        "{}:{}:{}:{}",
                                        row.name,
                                        row.action.label(),
                                        row.scope,
                                        row.value,
                                    )
                                }
                                children=move |row| {
                                    match kind {
                                        ReviewTableKind::Dimensions => view! {
                                            <tr class="border-t border-gray-100">
                                                <td class="px-4 py-2 font-medium text-gray-800">{row.name}</td>
                                                <td class="px-4 py-2"><ActionBadge action=row.action /></td>
                                                <td class="px-4 py-2 text-gray-600">{row.description}</td>
                                            </tr>
                                        }.into_view(),
                                        ReviewTableKind::DefaultConfig => view! {
                                            <tr class="border-t border-gray-100">
                                                <td class="px-4 py-2 font-medium text-gray-800">{row.name}</td>
                                                <td class="px-4 py-2"><ActionBadge action=row.action /></td>
                                                <td class="px-4 py-2 font-mono text-xs text-gray-700">{row.value}</td>
                                            </tr>
                                        }.into_view(),
                                        ReviewTableKind::Overrides => view! {
                                            <tr class="border-t border-gray-100">
                                                <td class="px-4 py-2 font-medium text-gray-800">{row.name}</td>
                                                <td class="px-4 py-2"><ActionBadge action=row.action /></td>
                                                <td class="px-4 py-2 text-gray-600">{row.scope}</td>
                                                <td class="px-4 py-2 font-mono text-xs text-gray-700">{row.value}</td>
                                            </tr>
                                        }.into_view(),
                                    }
                                }
                            />
                        </Show>
                    </tbody>
                </table>
            </div>
            <div class="border-t border-gray-100 px-4 py-2 text-xs text-gray-500">
                {format!("Showing {row_count} of {row_count}")}
            </div>
        </section>
    }
}

#[component]
fn ReviewTablesPanel(tables: ReviewTables) -> impl IntoView {
    view! {
        <div class="flex flex-col gap-3">
            <ReviewTableSection
                title="Dimensions"
                icon="ri-ruler-2-fill"
                rows=tables.dimensions
                kind=ReviewTableKind::Dimensions
            />
            <ReviewTableSection
                title="Default Config"
                icon="ri-tools-line"
                rows=tables.default_configs
                kind=ReviewTableKind::DefaultConfig
            />
            <ReviewTableSection
                title="Overrides"
                icon="ri-guide-fill"
                rows=tables.overrides
                kind=ReviewTableKind::Overrides
            />
        </div>
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
    let format_rws = RwSignal::new(ImportFormat::Toml);
    let strategy_rws = RwSignal::new(ImportStrategy::Upsert);
    let continue_on_error_rws = RwSignal::new(false);
    let tags_rws = RwSignal::new(String::new());
    let preview_rws = RwSignal::new(None::<ImportSummary>);
    let applied_rws = RwSignal::new(None::<ImportSummary>);
    let review_tables_rws = RwSignal::new(None::<ReviewTables>);
    let preview_loading_rws = RwSignal::new(false);
    let apply_loading_rws = RwSignal::new(false);
    let show_confirm_rws = RwSignal::new(false);
    let file_input_ref = create_node_ref::<html::Input>();

    let clear_results = move || {
        preview_rws.set(None);
        applied_rws.set(None);
        review_tables_rws.set(None);
        show_confirm_rws.set(false);
    };

    let clear_file = move |_| {
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
        let on_error = if continue_on_error_rws.get_untracked() {
            ImportOnError::Continue
        } else {
            ImportOnError::Abort
        };
        let options = ImportOptions {
            strategy,
            on_error,
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
            let import_text = file_text.clone();
            let result = config_import::import_config(
                file_text, format, options, &workspace, &org_id,
            )
            .await;

            match result {
                Ok(summary) if dry_run => {
                    match config_import::export_config(format, &workspace, &org_id).await
                    {
                        Ok(current_config) => {
                            match build_review_tables(
                                &import_text,
                                &current_config,
                                format,
                                strategy,
                            ) {
                                Ok(tables) => review_tables_rws.set(Some(tables)),
                                Err(error) => {
                                    review_tables_rws.set(None);
                                    enqueue_alert(error, AlertType::Error, 5000);
                                }
                            }
                        }
                        Err(error) => {
                            review_tables_rws.set(None);
                            enqueue_alert(error, AlertType::Error, 5000);
                        }
                    }
                    preview_rws.set(Some(summary));
                    enqueue_alert(
                        "Import preview is ready.".to_string(),
                        AlertType::Success,
                        3000,
                    );
                }
                Ok(summary) => {
                    preview_rws.set(None);
                    review_tables_rws.set(None);
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
            format_rws.set(ImportFormat::from_file_name(&file_name));
            file_text_rws.set(String::new());
            clear_results();

            let Ok(reader) = FileReader::new() else {
                enqueue_alert(
                    "Unable to read the selected file.".to_string(),
                    AlertType::Error,
                    3000,
                );
                return;
            };
            let reader_for_load = reader.clone();
            let onload = Closure::<dyn FnMut(Event)>::new(move |_| match reader_for_load
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
                                                fallback=move || view! {
                                                    <span>"Choose a Superposition config file."</span>
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
                                <button
                                    type="button"
                                    class=move || strategy_choice_class(strategy_rws.get() == ImportStrategy::CreateOnly)
                                    on:click=move |_| {
                                        strategy_rws.set(ImportStrategy::CreateOnly);
                                        clear_results();
                                    }
                                >
                                    <div class="flex items-start gap-3">
                                        <span class=move || strategy_icon_class(strategy_rws.get() == ImportStrategy::CreateOnly)>
                                            <i class="ri-add-line" />
                                        </span>
                                        <span class="min-w-0">
                                            <span class="block text-sm font-semibold">
                                                "Create only"
                                            </span>
                                            <span class="mt-1 block text-xs leading-5 text-gray-500">
                                                "Create missing items, skip existing ones."
                                            </span>
                                        </span>
                                    </div>
                                </button>
                                <button
                                    type="button"
                                    class=move || strategy_choice_class(strategy_rws.get() == ImportStrategy::Upsert)
                                    on:click=move |_| {
                                        strategy_rws.set(ImportStrategy::Upsert);
                                        clear_results();
                                    }
                                >
                                    <div class="flex items-start gap-3">
                                        <span class=move || strategy_icon_class(strategy_rws.get() == ImportStrategy::Upsert)>
                                            <i class="ri-loop-left-line" />
                                        </span>
                                        <span class="min-w-0">
                                            <span class="block text-sm font-semibold">
                                                "Create or update"
                                            </span>
                                            <span class="mt-1 block text-xs leading-5 text-gray-500">
                                                "Create missing items, update existing ones."
                                            </span>
                                        </span>
                                    </div>
                                </button>
                                <button
                                    type="button"
                                    class=move || strategy_choice_class(strategy_rws.get() == ImportStrategy::Replace)
                                    on:click=move |_| {
                                        strategy_rws.set(ImportStrategy::Replace);
                                        clear_results();
                                    }
                                >
                                    <div class="flex items-start gap-3">
                                        <span class=move || strategy_icon_class(strategy_rws.get() == ImportStrategy::Replace)>
                                            <i class="ri-restart-line" />
                                        </span>
                                        <span class="min-w-0">
                                            <span class="block text-sm font-semibold">
                                                "Replace workspace"
                                            </span>
                                            <span class="mt-1 block text-xs leading-5 text-gray-500">
                                                "Mirror the file, remove missing items."
                                            </span>
                                        </span>
                                    </div>
                                </button>
                            </div>
                        </div>

                        <div class="collapse collapse-arrow rounded-lg border border-gray-200 bg-gray-50">
                            <input type="checkbox" />
                            <div class="collapse-title text-sm font-semibold text-gray-800">
                                "Advanced Options"
                            </div>
                            <div class="collapse-content flex flex-col gap-4">
                                <label class="flex items-start justify-between gap-4 rounded-lg bg-white p-3">
                                    <span>
                                        <span class="block text-sm font-medium text-gray-800">
                                            "Continue on item errors"
                                        </span>
                                        <span class="block text-xs text-gray-500">
                                            "Valid items can still be imported."
                                        </span>
                                    </span>
                                    <input
                                        type="checkbox"
                                        class="toggle toggle-primary"
                                        checked=move || continue_on_error_rws.get()
                                        on:change=move |ev| {
                                            continue_on_error_rws.set(event_target_checked(&ev));
                                            clear_results();
                                        }
                                    />
                                </label>
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
                                        || preview_loading_rws.get()
                                        || apply_loading_rws.get()
                                }
                                on:click=move |_| submit_import.call(true)
                            >
                                <Show
                                    when=move || preview_loading_rws.get()
                                    fallback=move || view! {
                                        <>
                                            "Preview Changes"
                                            <i class="ri-search-eye-line" />
                                        </>
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
                                        || preview_loading_rws.get()
                                        || apply_loading_rws.get()
                                }
                                on:click=apply_preview
                            >
                                <Show
                                    when=move || apply_loading_rws.get()
                                    fallback=move || view! {
                                        <>
                                            "Apply Import"
                                            <i class="ri-upload-cloud-2-line" />
                                        </>
                                    }
                                >
                                    <span class="loading loading-dots loading-sm" />
                                </Show>
                            </button>
                        </div>
                    </div>
                </section>

                <Show when=move || preview_rws.with(|summary| summary.is_some())>
                    {move || {
                        preview_rws
                            .get()
                            .map(|_| {
                                view! {
                                    <section class="card bg-base-100 shadow">
                                        <div class="card-body gap-5">
                                            <div class="flex flex-col gap-2 md:flex-row md:items-center md:justify-between">
                                                <div class="flex flex-wrap items-center gap-2">
                                                    <h2 class="text-lg font-semibold text-gray-900">
                                                        "Review Changes"
                                                    </h2>
                                                    <Show when=move || !file_name_rws.with(String::is_empty)>
                                                        <span class="badge badge-outline">
                                                            {move || file_name_rws.get()}
                                                        </span>
                                                    </Show>
                                                </div>
                                            </div>
                                            <Show when=move || review_tables_rws.with(|tables| tables.is_some())>
                                                {move || {
                                                    review_tables_rws
                                                        .get()
                                                        .map(|tables| {
                                                            view! {
                                                                <ReviewTablesPanel tables=tables />
                                                            }
                                                        })
                                                }}
                                            </Show>
                                        </div>
                                    </section>
                                }
                            })
                    }}
                </Show>

                <Show when=move || applied_rws.with(|summary| summary.is_some())>
                    {move || {
                        applied_rws
                            .get()
                            .map(|summary| {
                                let href = summary.config_version.as_ref().map(|version| {
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
