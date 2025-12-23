use leptos::*;

#[derive(Clone, Copy, PartialEq, Default)]
pub enum TooltipPosition {
    Top,
    Right,
    #[default]
    Bottom,
    Left,
    TopRight,
    TopLeft,
    BottomRight,
    BottomLeft,
}

impl TooltipPosition {
    fn to_class(self) -> &'static str {
        match self {
            TooltipPosition::Top => "bottom-full left-1/2 -translate-x-1/2",
            TooltipPosition::Right => "left-full top-1/2 -translate-y-1/2",
            TooltipPosition::Bottom => "top-full left-1/2 -translate-x-1/2",
            TooltipPosition::Left => "right-full top-1/2 -translate-y-1/2",
            TooltipPosition::TopRight => "bottom-full left-0",
            TooltipPosition::TopLeft => "bottom-full right-0",
            TooltipPosition::BottomRight => "top-full left-0",
            TooltipPosition::BottomLeft => "top-full right-0",
        }
    }
}

#[component]
pub fn label(
    #[prop(into)] title: String,
    #[prop(into, optional)] info: Option<String>,
    #[prop(into, optional)] extra_info: Option<String>,
    #[prop(into, optional)] description: Option<String>,
    #[prop(into, default = String::new())] class: String,
    #[prop(default = TooltipPosition::default())] tooltip_position: TooltipPosition,
) -> impl IntoView {
    let position_class = tooltip_position.to_class();

    view! {
        <label class=format!("label flex flex-col items-start justify-center {class}")>
            <div class="flex items-center gap-1 label-text font-semibold text-base">
                {title}
                {info
                    .map(|info| {
                        view! { <span class="text-sm font-normal text-slate-400">{info}</span> }
                    })}
                {extra_info
                    .map(|extra_info| {
                        view! {
                            <div class="group relative inline-block text-[10px] text-gray-700 cursor-pointer">
                                <p class=format!(
                                    "z-[1000] hidden absolute w-[320px] max-w-[90vw] p-2.5 group-hover:flex flex-col gap-4 bg-black text-white leading-relaxed rounded shadow-[0_4px_6px_rgba(0,0,0,0.1)] whitespace-normal {}",
                                    position_class,
                                )>{extra_info}</p>
                                <i class="ri-information-line ri-lg" />
                            </div>
                        }
                    })}
            </div>
            {description
                .map(|description| {
                    view! { <span class="label-text text-slate-400">{description}</span> }
                })}
        </label>
    }
}
