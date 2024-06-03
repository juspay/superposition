use std::time::Duration;

use leptos::*;

use crate::components::alert::{Alert, AlertType};

#[derive(Clone, Debug)]
pub struct AlertQueue {
    pub counter: u64,
    pub alerts: Vec<Alert>,
}

fn enqueue(alert: Alert, set_queue: WriteSignal<AlertQueue>) {
    set_queue.update(|v| {
        v.counter = v.counter + 1;
        v.alerts.push(alert.clone());
    });

    set_timeout(
        move || {
            set_queue.update(|v| {
                let pos = v.alerts.iter().position(|item| item.id == alert.id);
                if let Some(i) = pos {
                    v.alerts.remove(i);
                }
            })
        },
        Duration::from_millis(alert.timeout),
    )
}

pub fn enqueue_alert_default(text: String) {
    let rs = use_context::<ReadSignal<AlertQueue>>();
    let ws = use_context::<WriteSignal<AlertQueue>>();

    match (rs, ws) {
        (Some(queue), Some(set_queue)) => {
            let id = queue.get().counter;
            let alert = Alert::default(id, text);

            enqueue(alert, set_queue)
        }
        _ => {
            logging::error!("alert_ws context not available");
        }
    }
}

pub fn enqueue_alert(text: String, alert_type: AlertType, timeout: u64) {
    let rs = use_context::<ReadSignal<AlertQueue>>();
    let ws = use_context::<WriteSignal<AlertQueue>>();

    match (rs, ws) {
        (Some(queue), Some(set_queue)) => {
            let id = queue.get().counter;
            let alert = Alert::new(id, text, alert_type, timeout);

            enqueue(alert, set_queue)
        }
        _ => {
            logging::error!("alert queue context not available");
        }
    }
}

#[component]
pub fn alert_provider(children: Children) -> impl IntoView {
    let (alert_queue_rs, alert_queue_ws) = create_signal::<AlertQueue>(AlertQueue {
        counter: 0,
        alerts: Vec::new(),
    });

    provide_context(alert_queue_rs);
    provide_context(alert_queue_ws);

    children()
}
