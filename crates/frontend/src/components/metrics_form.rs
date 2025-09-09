use leptos::*;
use serde_json::Value;
use superposition_types::database::models::{MetricSource, Metrics};

use crate::{
    components::{
        form::label::Label,
        input::{Input, InputType, Toggle},
    },
    schema::{JsonSchemaType, SchemaType},
};

#[component]
pub fn metrics_form(
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
