use leptos::*;

#[derive(Debug, Clone)]
pub enum SkeletonVariant {
    Block,
    Page,
    Content,
    DetailPage,
}

#[component]
pub fn skeleton(
    #[prop(into, default = String::from("w-full"))] style_class: String,
    #[prop(default = SkeletonVariant::Page)] variant: SkeletonVariant,
) -> impl IntoView {
    let container_div_styles = format!("flex flex-col gap-4 {style_class}");

    match variant {
        SkeletonVariant::Page => view! {
            <div class=container_div_styles>
                <div class="skeleton w-52 h-24"></div>
                <div class="skeleton w-full h-[40vh]"></div>
            </div>
        }
        .into_view(),
        SkeletonVariant::Content => view! {
            <div class=container_div_styles>
                <div class="skeleton h-4 w-1/2"></div>
                <div class="skeleton h-4 w-full"></div>
                <div class="skeleton h-4 w-full"></div>
                <div class="skeleton h-4 w-full"></div>
            </div>
        }
        .into_view(),
        SkeletonVariant::Block => view! {
            <div class=container_div_styles>
                <div class="skeleton w-full h-[40vh]"></div>
            </div>
        }
        .into_view(),
        SkeletonVariant::DetailPage => view! {
            <div class=container_div_styles>
                <div class="skeleton w-full h-24"></div>
                <div class="skeleton w-full h-[40vh]"></div>
            </div>
        }
        .into_view(),
    }
}
