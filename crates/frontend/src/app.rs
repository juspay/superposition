use leptos::*;
use leptos_meta::*;
use leptos_router::*;

use crate::hoc::layout::layout::Layout;
use crate::pages::ExperimentList::ExperimentList::ExperimentList;
use crate::pages::{Home::Home::Home, NotFound::NotFound::NotFound};
use crate::types::AppRoute;

#[component]
pub fn App(cx: Scope) -> impl IntoView {
    // Provides context that manages stylesheets, titles, meta tags, etc.
    provide_meta_context(cx);
    view! { cx,
        // injects a stylesheet into the document <head>
        // id=leptos means cargo-leptos will hot-reload this stylesheet
        <Stylesheet id="leptos" href="/pkg/style.css"/>
        // <Link href="https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC" crossorigin="anonymous"/>
        <Link rel="shortcut icon" type_="image/ico" href="/assets/favicon.ico"/>
        <Link href="https://cdn.jsdelivr.net/npm/remixicon@3.5.0/fonts/remixicon.css" rel="stylesheet" />
        // sets the document title
        <Title text="Welcome to Context Aware Config"/>
        // content for this welcome page
        <Router>
            <body class="m-0 min-h-screen bg-gray-50">
                <Layout>
                    <Routes>
                        <Route ssr=SsrMode::PartiallyBlocked path="/admin/experiments" view=ExperimentList />
                        <Route ssr=SsrMode::PartiallyBlocked path="" view=Home/>
                        <Route path="/*any" view=NotFound/>
                    </Routes>
                </Layout>
            </body>
        </Router>
    }
}
