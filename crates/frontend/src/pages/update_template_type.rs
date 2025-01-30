use leptos::*;
use leptos_router::{use_params, Params};

use crate::{
    api::fetch_type,
    components::{skeleton::Skeleton, type_template_form::TypeTemplateForm},
};

#[derive(PartialEq, Clone, Debug)]
struct PageParams {
    type_name: String,
}

impl Params for PageParams {
    fn from_map(
        map: &leptos_router::ParamsMap,
    ) -> Result<Self, leptos_router::ParamsError> {
        map.get("type_name")
            .map(|v| PageParams {
                type_name: v.clone(),
            })
            .ok_or(leptos_router::ParamsError::MissingParam(
                "type_name is missing".to_owned(),
            ))
    }
}

#[component]
pub fn update_template_type() -> impl IntoView {
    let tenant_rs = use_context::<Signal<String>>().unwrap();
    let path_params = use_params::<PageParams>();
    let page_resource = create_blocking_resource(
        move || (tenant_rs.get(), path_params.get()),
        |(t, path_params)| async move {
            if let Err(err) = path_params {
                logging::log!("failed to get type due to: {:?}", err);
                return Err(err.to_string());
            }
            fetch_type(&t, &(path_params.unwrap().type_name))
                .await
                .map_err(|e| e.to_string())
        },
    );

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton /> }
        }>
            {move || {
                let type_template = page_resource.get();
                if type_template.is_none() {
                    return view! { <p>"Failed to load type template to update"</p> }.into_view();
                }
                match type_template.unwrap() {
                    Ok(t) => {
                        view! {
                            <TypeTemplateForm
                                edit=true
                                type_name=t.type_name
                                type_schema=t.type_schema
                                class="w-1/2 h-main-content p-8 rounded-2xl border bg-white overflow-y-auto"
                                handle_submit=move || {}
                            />
                        }
                            .into_view()
                    }
                    Err(e) => view! { <p>{e}</p> }.into_view(),
                }
            }}
        </Suspense>
    }
}
