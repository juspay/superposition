use leptos::*;

#[derive(PartialEq, Clone, Debug, strum_macros::Display)]
#[strum(serialize_all = "lowercase")]
pub enum AlertType {
    Info,
    Success,
    Warning,
    Error,
}

#[derive(Clone, Debug)]
pub struct Alert {
    pub id: u64,
    pub text: String,
    pub alert_type: AlertType,
    pub timeout: u64,
}

impl Alert {
    pub fn default(id: u64, text: String) -> Self {
        Alert {
            id,
            text,
            alert_type: AlertType::Info,
            timeout: 500,
        }
    }
    pub fn new(id: u64, text: String, alert_type: AlertType, timeout: u64) -> Self {
        Alert {
            id,
            text,
            alert_type,
            timeout,
        }
    }
}

impl AlertType {
    pub fn to_css_class(&self) -> &'static str {
        match self {
            AlertType::Info => "alert-info",
            AlertType::Success => "alert-success",
            AlertType::Warning => "alert-warning",
            AlertType::Error => "alert-error text-white",
        }
    }

    pub fn to_icon_class(&self) -> &'static str {
        match self {
            AlertType::Info => "ri-information-line",
            AlertType::Success => "ri-checkbox-circle-line",
            AlertType::Warning => "ri-error-warning-line",
            AlertType::Error => "ri-close-circle-line",
        }
    }
}

#[component]
pub fn alert(alert: Alert) -> impl IntoView {
    let outer_div_class = format!("alert {}", alert.alert_type.to_css_class());
    let content_icon = alert.alert_type.to_icon_class();
    view! {
        <div role="alert" class=outer_div_class>
            <i class=content_icon></i>
            <span>{alert.text}</span>
        </div>
    }
}