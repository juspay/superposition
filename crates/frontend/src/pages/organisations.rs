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
use crate::utils::use_host_server;

#[component]
pub fn organisations() -> impl IntoView {
    let host = use_host_server();
    let organisation_resource = create_blocking_resource(
        || (),
        |_| async { fetch_organisations().await.unwrap_or_default() },
    );

    let table_columns = create_memo(move |_| {
        let host = host.clone();
        let navigate = move |_: &str, row: &Map<String, Value>| {
            let organisation_id = row["organisation_id"]
                .as_str()
                .unwrap_or_default()
                .to_string();
            view! {
                <button
                    formaction=format!("{host}/organisations/switch/{organisation_id}")
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
        <form class="p-8">
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
                            map.insert(
                                String::from("organisation_id"),
                                Value::String(organisation),
                            );
                            map
                        })
                        .collect::<Vec<Map<String, Value>>>();
                    view! {
                        <div class="pb-4">
                            <Stat
                                heading="Oraganisations"
                                icon="ri-building-fill"
                                number=organisations.len().to_string()
                            />
                        </div>
                        <Table
                            class="card-body card rounded-lg w-full bg-base-100 shadow"
                            rows=table_rows
                            key_column="id".to_string()
                            columns=table_columns.get()
                        />
                    }
                }}
            </Suspense>
        </form>
    }
}
