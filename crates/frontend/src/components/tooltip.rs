use leptos::*;

pub enum TooltipPosition {
    Top,
    Bottom,
    Left,
    Right,
}

#[component]
pub fn Tooltip(
    #[prop(into)] icon_class: String,
    children: Children,
    #[prop(default = TooltipPosition::Right)] position: TooltipPosition,
) -> impl IntoView {
    let position_class = match position {
        TooltipPosition::Top => "top-full left-1/2 -translate-x-1/2 translate-y-[-10px]",
        TooltipPosition::Bottom => {
            "bottom-full left-1/2 -translate-x-1/2 translate-y-[10px]"
        }
        TooltipPosition::Left => {
            "top-1/2 right-full -translate-y-1/2 translate-x-[-10px]"
        }
        TooltipPosition::Right => "top-1/2 left-full -translate-y-1/2 translate-x-[10px]",
    };

    view! {
        <div class="group relative inline-block text-xs text-gray-700 cursor-pointer">
            <div class=format!(
                "z-[1000] hidden absolute p-2.5 group-hover:block bg-white rounded shadow-[0_4px_6px_rgba(0,0,0,0.1)] whitespace-normal {}",
                position_class,
            )>{children()}</div>
            <i class=icon_class />
        </div>
    }
}
