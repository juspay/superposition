use leptos::*;
use serde_json::Value;
use superposition_types::database::models::{
    MetricDefinition, MetricDirection, MetricSelection, MetricSource, Metrics,
    experimentation::ExperimentMetrics,
};

use crate::{
    components::{
        dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
        form::label::Label,
        input::{Input, InputType, StringArrayInput, Toggle},
    },
    schema::{JsonSchemaType, SchemaType},
};

#[component]
pub fn MetricsForm(
    #[prop(default = Metrics::default())] metrics: Metrics,
    on_change: Callback<Metrics>,
) -> impl IntoView {
    let metrics_rws = RwSignal::new(metrics);

    Effect::new(move |_| on_change.call(metrics_rws.get()));

    let toggle_enabled = Callback::new(move |v| {
        metrics_rws.update(|m| {
            m.enabled = v;
            if m.enabled
                && (m.source.is_none()
                    || !matches!(m.source, Some(MetricSource::Grafana { .. })))
            {
                m.source = Some(MetricSource::default());
            }
        })
    });

    let grafana_form_view = move || {
        view! {
            <div class="max-w-md w-full pl-2.5 border-t border-dashed">
                <div class="form-control">
                    <label class="label">
                        <span class="label-text-alt">Grafana Base URL</span>
                    </label>
                    <Input
                        r#type=InputType::Text
                        placeholder="Base URL".to_string()
                        class="input-md w-full max-w-md"
                        schema_type=SchemaType::Single(JsonSchemaType::String)
                        value=Value::String(
                            metrics_rws
                                .with(|m| {
                                    if let Some(MetricSource::Grafana { ref base_url, .. }) = m
                                        .source
                                    {
                                        base_url.clone()
                                    } else {
                                        String::new()
                                    }
                                }),
                        )
                        on_change=move |val: Value| {
                            let new_value = val.as_str().unwrap_or_default().to_string();
                            metrics_rws
                                .update(|m| {
                                    if let Some(MetricSource::Grafana { ref mut base_url, .. }) = m
                                        .source
                                    {
                                        *base_url = new_value;
                                    }
                                });
                        }
                    />
                </div>
                <div class="form-control">
                    <label class="label">
                        <span class="label-text-alt">Grafana Dashboard UID</span>
                    </label>
                    <Input
                        r#type=InputType::Text
                        placeholder="Dashboard UID".to_string()
                        class="input-md w-full max-w-md"
                        schema_type=SchemaType::Single(JsonSchemaType::String)
                        value=Value::String(
                            metrics_rws
                                .with(|m| {
                                    if let Some(MetricSource::Grafana { ref dashboard_uid, .. }) = m
                                        .source
                                    {
                                        dashboard_uid.clone()
                                    } else {
                                        String::new()
                                    }
                                }),
                        )
                        on_change=move |val: Value| {
                            let new_value = val.as_str().unwrap_or_default().to_string();
                            metrics_rws
                                .update(|m| {
                                    if let Some(
                                        MetricSource::Grafana { ref mut dashboard_uid, .. },
                                    ) = m.source
                                    {
                                        *dashboard_uid = new_value;
                                    }
                                });
                        }
                    />
                </div>

                <div class="form-control">
                    <label class="label">
                        <span class="label-text-alt">Grafana Dashboard Slug</span>
                    </label>
                    <Input
                        r#type=InputType::Text
                        placeholder="Dashboard Slug".to_string()
                        class="input-md w-full max-w-md"
                        schema_type=SchemaType::Single(JsonSchemaType::String)
                        value=Value::String(
                            metrics_rws
                                .with(|m| {
                                    if let Some(MetricSource::Grafana { ref dashboard_slug, .. }) = m
                                        .source
                                    {
                                        dashboard_slug.clone()
                                    } else {
                                        String::new()
                                    }
                                }),
                        )
                        on_change=move |val: Value| {
                            let new_value = val.as_str().unwrap_or_default().to_string();
                            metrics_rws
                                .update(|m| {
                                    if let Some(
                                        MetricSource::Grafana { ref mut dashboard_slug, .. },
                                    ) = m.source
                                    {
                                        *dashboard_slug = new_value;
                                    }
                                });
                        }
                    />
                </div>

                <div class="form-control">
                    <label class="label">
                        <span class="label-text-alt">Variant ID Alias (Optional)</span>
                    </label>
                    <Input
                        r#type=InputType::Text
                        placeholder="Variant ID Alias".to_string()
                        class="input-md w-full max-w-md"
                        schema_type=SchemaType::Single(JsonSchemaType::String)
                        value=Value::String(
                            metrics_rws
                                .with(|m| {
                                    m.source
                                        .as_ref()
                                        .and_then(|s| match s {
                                            MetricSource::Grafana { variant_id_alias, .. } => {
                                                variant_id_alias.clone()
                                            }
                                        })
                                        .unwrap_or_default()
                                }),
                        )
                        on_change=move |val: Value| {
                            let new_value = val.as_str().unwrap_or_default().to_string();
                            metrics_rws
                                .update(|m| {
                                    if let Some(
                                        MetricSource::Grafana { ref mut variant_id_alias, .. },
                                    ) = m.source
                                    {
                                        *variant_id_alias = if new_value.is_empty() {
                                            None
                                        } else {
                                            Some(new_value)
                                        };
                                    }
                                });
                        }
                    />
                </div>
                <div class="form-control">
                    <Label
                        title="Metric Definitions"
                        description="Press enter to add a metric name"
                    />
                    <StringArrayInput
                        options=metrics_rws
                            .with(|metrics| {
                                metrics
                                    .list
                                    .as_deref()
                                    .unwrap_or_default()
                                    .iter()
                                    .map(|metric| metric.name.clone())
                                    .collect()
                            })
                        unique=true
                        show_label=false
                        on_change=Callback::new(move |list: Vec<String>| {
                            metrics_rws
                                .update(|metrics| {
                                    let existing = metrics.list.take().unwrap_or_default();
                                    metrics.list = Some(
                                        list
                                            .into_iter()
                                            .map(|name| {
                                                existing
                                                    .iter()
                                                    .find(|metric| metric.name == name)
                                                    .cloned()
                                                    .unwrap_or(MetricDefinition {
                                                        name,
                                                        direction: MetricDirection::Maximize,
                                                    })
                                            })
                                            .collect(),
                                    );
                                });
                        })
                    />
                    <div class="flex flex-col gap-2 mt-3">
                        <For
                            each=move || {
                                metrics_rws.with(|metrics| metrics.list.clone().unwrap_or_default())
                            }
                            key=|metric| metric.name.clone()
                            children=move |metric| {
                                let metric_name = StoredValue::new(metric.name);
                                let direction_label = match metric.direction {
                                    MetricDirection::Maximize => "Maximize",
                                    MetricDirection::Minimize => "Minimize",
                                };
                                let direction_options: Vec<MetricDirection> = vec![
                                    MetricDirection::Maximize,
                                    MetricDirection::Minimize,
                                ];
                                view! {
                                    <div class="flex items-center justify-between gap-4">
                                        <span class="text-sm truncate">
                                            {metric_name.get_value()}
                                        </span>
                                        <Dropdown
                                            dropdown_width="w-44"
                                            dropdown_direction=DropdownDirection::Down
                                            dropdown_btn_type=DropdownBtnType::Select
                                            searchable=false
                                            dropdown_text=direction_label.to_string()
                                            dropdown_options=direction_options
                                            on_select=Callback::new(move |direction: MetricDirection| {
                                                metrics_rws
                                                    .update(|metrics| {
                                                        if let Some(metric) = metrics
                                                            .list
                                                            .as_mut()
                                                            .and_then(|list| {
                                                                list.iter_mut()
                                                                    .find(|metric| { metric.name == metric_name.get_value() })
                                                            })
                                                        {
                                                            metric.direction = direction;
                                                        }
                                                    });
                                            })
                                        />
                                    </div>
                                }
                            }
                        />
                    </div>
                </div>
            </div>
        }
    };

    view! {
        <div class="flex flex-col">
            <div class="w-fit flex items-center gap-2">
                <Toggle value=metrics_rws.with_untracked(|m| m.enabled) on_change=toggle_enabled />
                <Label
                    title="Metrics"
                    extra_info="To view metrics from Grafana, make sure that your setup allows iframe embedding. Also, experiment viewers must have access to the Grafana instance, to view the metrics."
                />
            </div>

            <Show when=move || {
                metrics_rws
                    .with(|m| m.enabled && matches!(m.source, Some(MetricSource::Grafana { .. })))
            }>{grafana_form_view}</Show>
        </div>
    }
}

#[component]
pub fn ExperimentMetricsForm(
    workspace_metrics: Metrics,
    #[prop(default = ExperimentMetrics::default())] metrics: ExperimentMetrics,
    on_change: Callback<ExperimentMetrics>,
) -> impl IntoView {
    let available_metrics =
        StoredValue::new(workspace_metrics.list.clone().unwrap_or_default());
    let workspace_source = StoredValue::new(workspace_metrics.source.clone());
    let workspace_enabled = workspace_metrics.enabled;
    // Selection is only offered when the workspace defines a metric list;
    // matches the server rule at helpers.rs::validate_metric_selection.
    let has_workspace_list = workspace_metrics
        .list
        .as_ref()
        .is_some_and(|list| !list.is_empty());

    let selection = metrics.selection().cloned();
    let can_toggle = workspace_enabled || selection.is_some();
    let enabled = RwSignal::new(selection.is_some());
    let primary = RwSignal::new(
        selection
            .as_ref()
            .map(|selection| selection.primary.clone())
            .unwrap_or_default(),
    );
    let secondary = RwSignal::new(
        selection
            .as_ref()
            .and_then(|selection| selection.secondary.clone()),
    );
    let guardrail = RwSignal::new(
        selection
            .as_ref()
            .map(|selection| selection.guardrail.clone())
            .unwrap_or_default(),
    );
    // Per-experiment source override is offered whenever the workspace has a
    // source. `Metrics` deserialization guarantees a `Some` source is usable
    // (blank Grafana fields are normalized to `None`), so a bare `is_some()`
    // matches the server-side rule at handlers.rs.
    let has_workspace_source = workspace_source.with_value(|s| s.as_ref().is_some());
    let initial_source = if has_workspace_source {
        metrics
            .source()
            .cloned()
            .or_else(|| workspace_source.get_value())
    } else {
        None
    };
    let custom_source = RwSignal::new(initial_source);

    let emit = move || {
        let selection = (enabled.get_untracked() && has_workspace_list).then(|| {
            MetricSelection {
                primary: primary.get_untracked(),
                secondary: secondary.get_untracked(),
                guardrail: guardrail.get_untracked(),
            }
        });
        // Only send source when it differs from the workspace source
        let source = custom_source.get_untracked().filter(|source| {
            workspace_source.with_value(|ws| ws.as_ref() != Some(source))
        });
        on_change.call(ExperimentMetrics::from_parts(selection, source));
    };

    let grafana_source_fields = move |source_rws: RwSignal<Option<MetricSource>>| {
        let get_field = move |f: fn(&MetricSource) -> String| {
            source_rws.with(|s| s.as_ref().map(f).unwrap_or_default())
        };

        view! {
            <div class="grid grid-cols-1 md:grid-cols-2 gap-4 pl-2.5 border-t border-dashed">
                <div class="form-control">
                    <label class="label">
                        <span class="label-text-alt">Grafana Base URL</span>
                    </label>
                    <Input
                        r#type=InputType::Text
                        placeholder="Base URL".to_string()
                        class="input-md w-full"
                        schema_type=SchemaType::Single(JsonSchemaType::String)
                        value=Value::String(
                            get_field(|s| match s {
                                MetricSource::Grafana { base_url, .. } => base_url.clone(),
                            }),
                        )
                        on_change=move |val: Value| {
                            let v = val.as_str().unwrap_or_default().to_string();
                            source_rws
                                .update(|s| {
                                    if let Some(MetricSource::Grafana { base_url, .. }) = s.as_mut()
                                    {
                                        *base_url = v;
                                    }
                                });
                            emit();
                        }
                    />
                </div>
                <div class="form-control">
                    <label class="label">
                        <span class="label-text-alt">Dashboard UID</span>
                    </label>
                    <Input
                        r#type=InputType::Text
                        placeholder="Dashboard UID".to_string()
                        class="input-md w-full"
                        schema_type=SchemaType::Single(JsonSchemaType::String)
                        value=Value::String(
                            get_field(|s| match s {
                                MetricSource::Grafana { dashboard_uid, .. } => dashboard_uid.clone(),
                            }),
                        )
                        on_change=move |val: Value| {
                            let v = val.as_str().unwrap_or_default().to_string();
                            source_rws
                                .update(|s| {
                                    if let Some(MetricSource::Grafana { dashboard_uid, .. }) = s
                                        .as_mut()
                                    {
                                        *dashboard_uid = v;
                                    }
                                });
                            emit();
                        }
                    />
                </div>
                <div class="form-control">
                    <label class="label">
                        <span class="label-text-alt">Dashboard Slug</span>
                    </label>
                    <Input
                        r#type=InputType::Text
                        placeholder="Dashboard Slug".to_string()
                        class="input-md w-full"
                        schema_type=SchemaType::Single(JsonSchemaType::String)
                        value=Value::String(
                            get_field(|s| match s {
                                MetricSource::Grafana { dashboard_slug, .. } => {
                                    dashboard_slug.clone()
                                }
                            }),
                        )
                        on_change=move |val: Value| {
                            let v = val.as_str().unwrap_or_default().to_string();
                            source_rws
                                .update(|s| {
                                    if let Some(MetricSource::Grafana { dashboard_slug, .. }) = s
                                        .as_mut()
                                    {
                                        *dashboard_slug = v;
                                    }
                                });
                            emit();
                        }
                    />
                </div>
                <div class="form-control">
                    <label class="label">
                        <span class="label-text-alt">Variant ID Alias (Optional)</span>
                    </label>
                    <Input
                        r#type=InputType::Text
                        placeholder="Variant ID Alias".to_string()
                        class="input-md w-full"
                        schema_type=SchemaType::Single(JsonSchemaType::String)
                        value=Value::String(
                            get_field(|s| match s {
                                MetricSource::Grafana { variant_id_alias, .. } => {
                                    variant_id_alias.clone().unwrap_or_default()
                                }
                            }),
                        )
                        on_change=move |val: Value| {
                            let v = val.as_str().unwrap_or_default().to_string();
                            source_rws
                                .update(|s| {
                                    if let Some(MetricSource::Grafana { variant_id_alias, .. }) = s
                                        .as_mut()
                                    {
                                        *variant_id_alias = if v.is_empty() {
                                            None
                                        } else {
                                            Some(v)
                                        };
                                    }
                                });
                            emit();
                        }
                    />
                </div>
            </div>
        }
    };

    view! {
        <div class="flex flex-col gap-3">
            <div class="w-fit flex items-center gap-2">
                <Toggle
                    value=enabled.get_untracked()
                    disabled=!can_toggle
                    on_change=move |value| {
                        enabled.set(value);
                        emit();
                    }
                />
                <Label
                    title="Experiment Metrics"
                    info=if workspace_enabled {
                        String::new()
                    } else {
                        "Disabled at workspace level".to_string()
                    }
                />
            </div>
            <Show when=move || enabled.get()>
                <Show when=move || has_workspace_list>
                    <div class="flex flex-col gap-4 max-w-md">
                        <div class="form-control">
                            <Label title="Primary Metric" />
                            <Dropdown
                                dropdown_width="w-full"
                                dropdown_direction=DropdownDirection::Down
                                dropdown_btn_type=DropdownBtnType::Select
                                dropdown_text={
                                    let name = primary.get_untracked().name;
                                    if name.is_empty() {
                                        "Select primary metric".to_string()
                                    } else {
                                        name
                                    }
                                }
                                dropdown_options=available_metrics.get_value()
                                on_select=Callback::new(move |metric: MetricDefinition| {
                                    primary.set(metric);
                                    emit();
                                })
                            />
                        </div>
                        <div class="form-control">
                            <Label title="Secondary Metric" info="(Optional)" />
                            <Dropdown
                                dropdown_width="w-full"
                                dropdown_direction=DropdownDirection::Down
                                dropdown_btn_type=DropdownBtnType::Select
                                dropdown_text=secondary
                                    .get_untracked()
                                    .map(|metric| metric.name)
                                    .unwrap_or_else(|| "No secondary metric".to_string())
                                dropdown_options=available_metrics.get_value()
                                on_select=Callback::new(move |metric: MetricDefinition| {
                                    secondary.set(Some(metric));
                                    emit();
                                })
                            />
                        </div>
                        <div class="form-control">
                            <Label title="Guardrail Metric" />
                            <Dropdown
                                dropdown_width="w-full"
                                dropdown_direction=DropdownDirection::Down
                                dropdown_btn_type=DropdownBtnType::Select
                                dropdown_text={
                                    let name = guardrail.get_untracked().name;
                                    if name.is_empty() {
                                        "Select guardrail metric".to_string()
                                    } else {
                                        name
                                    }
                                }
                                dropdown_options=available_metrics.get_value()
                                on_select=Callback::new(move |metric: MetricDefinition| {
                                    guardrail.set(metric);
                                    emit();
                                })
                            />
                        </div>
                    </div>
                </Show>
                {has_workspace_source
                    .then(|| {
                        view! {
                            <div class="form-control max-w-4xl mt-2">
                                <Label
                                    title="Metrics Source"
                                    info="Override the workspace metrics source for this experiment"
                                />
                                {grafana_source_fields(custom_source)}
                            </div>
                        }
                    })}
            </Show>
        </div>
    }
}
