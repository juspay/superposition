use leptos::*;
use serde_json::Value;
use strum::IntoEnumIterator;
use superposition_types::database::models::{
    MetricDefinition, MetricDirection, MetricSelection, MetricSource, Metrics,
    NonEmptyString, experimentation::ExperimentMetrics,
};

use crate::{
    components::{
        dropdown::{Dropdown, DropdownBtnType, DropdownDirection, utils::DropdownOption},
        form::label::Label,
        input::{Input, InputType, StringArrayInput, Toggle},
    },
    schema::{JsonSchemaType, SchemaType},
};

#[component]
fn SourceForm(
    source: Option<MetricSource>,
    #[prop(into)] on_change: Callback<Option<MetricSource>>,
) -> impl IntoView {
    let source_rws = RwSignal::new(source);

    Effect::new(move |_| {
        let source = source_rws.get();
        let is_empty = matches!(&source,
            Some(MetricSource::Grafana {
                base_url,
                dashboard_uid,
                dashboard_slug,
                variant_id_alias
            }) if base_url.is_empty()
                && dashboard_uid.is_empty()
                && dashboard_slug.is_empty()
                && variant_id_alias
                .as_ref()
                .is_none_or(|alias| alias.is_empty()));

        on_change.call(source.filter(|_| !is_empty));
    });

    view! {
        <div class="form-control">
            <Label title="Metrics Source" />
            <div class="max-w-md pl-2.5 border-t border-dashed">
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
                            source_rws
                                .with_untracked(|s| {
                                    if let Some(MetricSource::Grafana { base_url, .. }) = s {
                                        base_url.clone()
                                    } else {
                                        String::new()
                                    }
                                }),
                        )
                        on_change=move |val: Value| {
                            let new_value = val.as_str().unwrap_or_default().to_string();
                            source_rws
                                .update(|s| {
                                    if s.is_none() {
                                        *s = Some(MetricSource::default());
                                    }
                                    if let Some(MetricSource::Grafana { base_url, .. }) = s {
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
                            source_rws
                                .with_untracked(|s| {
                                    if let Some(MetricSource::Grafana { dashboard_uid, .. }) = s {
                                        dashboard_uid.clone()
                                    } else {
                                        String::new()
                                    }
                                }),
                        )
                        on_change=move |val: Value| {
                            let new_value = val.as_str().unwrap_or_default().to_string();
                            source_rws
                                .update(|s| {
                                    if s.is_none() {
                                        *s = Some(MetricSource::default());
                                    }
                                    if let Some(MetricSource::Grafana { dashboard_uid, .. }) = s {
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
                            source_rws
                                .with_untracked(|s| {
                                    if let Some(MetricSource::Grafana { dashboard_slug, .. }) = s {
                                        dashboard_slug.clone()
                                    } else {
                                        String::new()
                                    }
                                }),
                        )
                        on_change=move |val: Value| {
                            let new_value = val.as_str().unwrap_or_default().to_string();
                            source_rws
                                .update(|s| {
                                    if s.is_none() {
                                        *s = Some(MetricSource::default());
                                    }
                                    if let Some(MetricSource::Grafana { dashboard_slug, .. }) = s {
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
                            source_rws
                                .with_untracked(|s| {
                                    s.as_ref()
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
                            source_rws
                                .update(|s| {
                                    if s.is_none() {
                                        *s = Some(MetricSource::default());
                                    }
                                    if let Some(MetricSource::Grafana { variant_id_alias, .. }) = s {
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
            </div>
        </div>
    }
}

#[component]
pub fn MetricsForm(
    #[prop(default = Metrics::default())] metrics: Metrics,
    on_change: Callback<Metrics>,
) -> impl IntoView {
    let metrics_rws = RwSignal::new(metrics);

    Effect::new(move |_| on_change.call(metrics_rws.get()));

    let metrics_definition = move || {
        view! {
            <div class="form-control gap-2">
                <div class="form-control">
                    <Label title="Metric Definitions" info="Press enter to add a metric name" />
                    <StringArrayInput
                        options=metrics_rws
                            .with(|metrics| {
                                metrics
                                    .definitions
                                    .as_deref()
                                    .unwrap_or_default()
                                    .iter()
                                    .map(|metric| metric.name.to_string())
                                    .collect()
                            })
                        unique=true
                        show_label=false
                        on_change=Callback::new(move |list: Vec<String>| {
                            metrics_rws
                                .update(|metrics| {
                                    let existing = metrics.definitions.take().unwrap_or_default();
                                    let new_list = list
                                        .into_iter()
                                        .map(|name| {
                                            let direction = existing
                                                .iter()
                                                .find(|metric| *metric.name == name)
                                                .map(|metric| metric.direction)
                                                .unwrap_or_default();
                                            MetricDefinition {
                                                name: name.try_into().unwrap_or_default(),
                                                direction,
                                            }
                                        })
                                        .collect::<Vec<_>>();
                                    metrics.definitions = (!new_list.is_empty())
                                        .then_some(new_list);
                                });
                        })
                    />
                </div>
                <div class="pl-2.5 flex flex-col">
                    <For
                        each=move || {
                            metrics_rws
                                .with(|metrics| metrics.definitions.clone().unwrap_or_default())
                        }
                        key=|metric| metric.name.clone()
                        children=move |metric| {
                            let metric_name = StoredValue::new(metric.name);
                            view! {
                                <div class="flex flex-col first:border-t border-dashed">
                                    <label class="label">
                                        <span class="label-text-alt">
                                            {metric_name.get_value().to_string()}
                                        </span>
                                    </label>
                                    <Dropdown
                                        dropdown_width="w-44"
                                        dropdown_direction=DropdownDirection::Down
                                        dropdown_btn_type=DropdownBtnType::Select
                                        searchable=false
                                        dropdown_text=metric.direction.label()
                                        dropdown_options=MetricDirection::iter().collect()
                                        on_select=move |direction: MetricDirection| {
                                            metrics_rws
                                                .update(|metrics| {
                                                    if let Some(metric) = metrics
                                                        .definitions
                                                        .as_mut()
                                                        .and_then(|list| {
                                                            list.iter_mut()
                                                                .find(|metric| { metric.name == metric_name.get_value() })
                                                        })
                                                    {
                                                        metric.direction = direction;
                                                    }
                                                });
                                        }
                                    />
                                </div>
                            }
                        }
                    />
                </div>
            </div>
        }
    };

    view! {
        <div class="flex flex-col">
            <div class="w-fit flex items-center gap-2">
                <Toggle
                    value=metrics_rws.with_untracked(|m| m.enabled)
                    on_change=move |v| metrics_rws.update(|m| m.enabled = v)
                />
                <Label
                    title="Metrics"
                    extra_info="To view metrics from Grafana, make sure that your setup allows iframe embedding. Also, experiment viewers must have access to the Grafana instance, to view the metrics."
                />
            </div>
            <div class="max-w-md w-full pl-2.5 flex flex-col gap-2">
                <Show when=move || metrics_rws.with(|m| m.enabled)>
                    <SourceForm
                        source=metrics_rws.with_untracked(|m| m.source.clone())
                        on_change=move |source| {
                            metrics_rws.update_untracked(|m| m.source = source);
                            on_change.call(metrics_rws.get_untracked());
                        }
                    />
                </Show>
                {metrics_definition}
            </div>
        </div>
    }
}

#[component]
pub fn ExperimentMetricsForm(
    definitions: Vec<MetricDefinition>,
    experiment_metrics: ExperimentMetrics,
    on_change: Callback<ExperimentMetrics>,
) -> impl IntoView {
    let has_workspace_list = !definitions.is_empty();

    let definitions_st = StoredValue::new(definitions);
    let experiment_metrics_rws = RwSignal::new(experiment_metrics);

    Effect::new(move |_| on_change.call(experiment_metrics_rws.get()));

    view! {
        <div class="flex flex-col">
            <div class="w-fit flex items-center gap-2">
                <Toggle
                    value=experiment_metrics_rws.with_untracked(|m| m.enabled)
                    on_change=move |v| experiment_metrics_rws.update(|em| em.enabled = v)
                />
                <Label
                    title="Experiment Metrics"
                    extra_info="To view metrics from Grafana, make sure that your setup allows iframe embedding. Also, experiment viewers must have access to the Grafana instance, to view the metrics."
                />
            </div>
            <div>
                <Show when=move || experiment_metrics_rws.with(|m| m.enabled)>
                    <div class="max-w-md w-full pl-2.5 flex flex-col gap-2">
                        <SourceForm
                            source=experiment_metrics_rws.with_untracked(|m| m.source.clone())
                            on_change=move |source| {
                                experiment_metrics_rws.update_untracked(|m| m.source = source);
                                on_change.call(experiment_metrics_rws.get_untracked());
                            }
                        />
                        <Show when=move || has_workspace_list>
                            <div class="form-control">
                                <Label title="Metric Selections" />
                                <div class="max-w-md pl-2.5 border-t border-dashed">
                                    <div class="form-control">
                                        <label class="label">
                                            <span class="label-text-alt">"Primary Metric"</span>
                                        </label>
                                        <Dropdown
                                            dropdown_width="w-full"
                                            dropdown_direction=DropdownDirection::Down
                                            dropdown_btn_type=DropdownBtnType::Select
                                            dropdown_text=experiment_metrics_rws
                                                .with_untracked(|em| {
                                                    em.selection
                                                        .as_ref()
                                                        .and_then(|s| {
                                                            (s.primary.name != NonEmptyString::default())
                                                                .then_some(s.primary.name.to_string())
                                                        })
                                                })
                                                .unwrap_or_else(|| "Select primary metric".to_string())
                                            dropdown_options=definitions_st.get_value()
                                            on_select=move |metric: MetricDefinition| {
                                                experiment_metrics_rws
                                                    .update(|em| {
                                                        if em.selection.is_none() {
                                                            em.selection = Some(MetricSelection::default());
                                                        }
                                                        if let Some(ref mut selection) = em.selection {
                                                            selection.primary = metric;
                                                        }
                                                    })
                                            }
                                        />
                                    </div>
                                    <div class="form-control">
                                        <label class="label">
                                            <span class="label-text-alt">
                                                "Secondary Metric (Optional)"
                                            </span>
                                        </label>
                                        <div class="flex gap-2">
                                            <Dropdown
                                                dropdown_width="w-full"
                                                dropdown_direction=DropdownDirection::Down
                                                dropdown_btn_type=DropdownBtnType::Select
                                                dropdown_text=experiment_metrics_rws
                                                    .with(|em| {
                                                        em.selection
                                                            .as_ref()
                                                            .and_then(|s| s.secondary.as_ref())
                                                            .map(|m| m.name.to_string())
                                                    })
                                                    .unwrap_or_else(|| "Select secondary metric".to_string())
                                                dropdown_options=definitions_st.get_value()
                                                on_select=move |metric: MetricDefinition| {
                                                    experiment_metrics_rws
                                                        .update(|em| {
                                                            if em.selection.is_none() {
                                                                em.selection = Some(MetricSelection::default());
                                                            }
                                                            if let Some(ref mut selection) = em.selection {
                                                                selection.secondary = Some(metric);
                                                            }
                                                        })
                                                }
                                            />
                                            {move || {
                                                if experiment_metrics_rws
                                                    .with(|em| {
                                                        em.selection
                                                            .as_ref()
                                                            .map(|s| s.secondary.is_some())
                                                            .unwrap_or_default()
                                                    })
                                                {
                                                    view! {
                                                        <i
                                                            class="ri-close-circle-fill self-center"
                                                            on:click=move |ev| {
                                                                ev.prevent_default();
                                                                experiment_metrics_rws
                                                                    .update(|em| {
                                                                        if let Some(ref mut selection) = em.selection {
                                                                            selection.secondary = None;
                                                                        }
                                                                    })
                                                            }
                                                        />
                                                    }
                                                        .into_view()
                                                } else {
                                                    ().into_view()
                                                }
                                            }}
                                        </div>
                                    </div>
                                    <div class="form-control">
                                        <label class="label">
                                            <span class="label-text-alt">"Guardrail Metric"</span>
                                        </label>
                                        <Dropdown
                                            dropdown_width="w-full"
                                            dropdown_direction=DropdownDirection::Down
                                            dropdown_btn_type=DropdownBtnType::Select
                                            dropdown_text=experiment_metrics_rws
                                                .with_untracked(|em| {
                                                    em.selection
                                                        .as_ref()
                                                        .and_then(|s| {
                                                            (s.guardrail != NonEmptyString::default())
                                                                .then_some(s.guardrail.to_string())
                                                        })
                                                })
                                                .unwrap_or_else(|| "Select guardrail metric".to_string())
                                            dropdown_options=definitions_st
                                                .with_value(|defs| {
                                                    defs.iter().map(|def| def.name.clone()).collect()
                                                })
                                            on_select=move |metric: NonEmptyString| {
                                                experiment_metrics_rws
                                                    .update(|em| {
                                                        if em.selection.is_none() {
                                                            em.selection = Some(MetricSelection::default());
                                                        }
                                                        if let Some(ref mut selection) = em.selection {
                                                            selection.guardrail = metric;
                                                        }
                                                    })
                                            }
                                        />
                                    </div>
                                </div>
                            </div>
                        </Show>
                    </div>
                </Show>
            </div>
        </div>
    }
}
