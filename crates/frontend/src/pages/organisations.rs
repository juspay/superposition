use leptos::*;
use serde_json::{Map, Value};

use crate::api::fetch_organisations;
use crate::components::table::types::{default_column_formatter, Expandable};
use crate::components::{
    skeleton::Skeleton,
    stat::Stat,
    table::{
        types::{Column, ColumnSortable},
        Table,
    },
};
use crate::utils::use_url_base;

#[component]
pub fn organisations() -> impl IntoView {
    let base = use_url_base();
    let organisation_resource = create_blocking_resource(
        || (),
        |_| async { fetch_organisations().await.unwrap_or_default() },
    );

    let table_columns = StoredValue::new({
        let base = base.clone();
        let navigate = move |value: &str, _row: &Map<String, Value>| {
            let organisation_id = value.to_string();
            view! {
                <button
                    formaction=format!("{base}/organisations/switch/{organisation_id}")
                    class="cursor-pointer text-blue-500"
                >
                    {organisation_id}
                </button>
            }
            .into_view()
        };

        vec![Column::new(
            "organisation_id".to_string(),
            false,
            navigate,
            ColumnSortable::No,
            Expandable::Disabled,
            default_column_formatter,
        )]
    });

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton /> }
        }>
            {move || {
                let organisations = organisation_resource.get().unwrap_or_default();
                let table_rows = organisations
                    .clone()
                    .into_iter()
                    .map(|organisation| {
                        let mut map = Map::new();
                        map.insert(String::from("organisation_id"), Value::String(organisation));
                        map
                    })
                    .collect::<Vec<Map<String, Value>>>();
                view! {
                    <form class="h-full flex flex-col gap-4">
                        <Stat
                            heading="Oraganisations"
                            icon="ri-building-fill"
                            number=organisations.len().to_string()
                        />
                        <div class="card w-full bg-base-100 rounded-xl overflow-hidden shadow">
                            <div class="card-body overflow-y-auto overflow-x-visible">
                                <Table
                                    class="!overflow-y-auto"
                                    rows=table_rows
                                    key_column="organisation_id"
                                    columns=table_columns.get_value()
                                />
                            </div>
                        </div>
                    </form>
                }
            }}
        </Suspense>
    }
}
