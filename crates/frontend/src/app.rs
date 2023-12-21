use leptos::*;
use leptos_meta::*;
use leptos_router::*;

use crate::hoc::layout::layout::Layout;
use crate::pages::Dimensions::Dimensions::Dimensions;
use crate::pages::ExperimentList::ExperimentList::ExperimentList;
use crate::pages::{
    ContextOverride::ContextOverride::ContextOverride,
    DefaultConfig::DefaultConfig::DefaultConfig, Experiment::ExperimentPage,
    Home::Home::Home, NotFound::NotFound::NotFound,
};
#[component]
pub fn App() -> impl IntoView {
    // Provides context that manages stylesheets, titles, meta tags, etc.
    provide_meta_context();
    view! {
        <html data-theme="light">
            <Stylesheet id="leptos" href="/pkg/style.css"/>
            <Link rel="shortcut icon" type_="image/ico" href="/assets/favicon.ico"/>
            <Link
                href="https://cdn.jsdelivr.net/npm/remixicon@3.5.0/fonts/remixicon.css"
                rel="stylesheet"
            />
            // sets the document title
            <Title text="Welcome to Context Aware Config"/>
            // content for this welcome page
            <Router>
                <body class="m-0 min-h-screen bg-gray-50 font-mono">
                    <Layout>
                        <Routes>
                            <Route
                                ssr=SsrMode::PartiallyBlocked
                                path="/admin/:tenant/dimensions"
                                view=Dimensions
                            />
                            <Route
                                ssr=SsrMode::PartiallyBlocked
                                path="admin/:tenant/experiments"
                                view=ExperimentList
                            />
                            <Route
                                ssr=SsrMode::PartiallyBlocked
                                path="admin/:tenant/experiments/:id"
                                view=ExperimentPage
                            />
                            <Route ssr=SsrMode::PartiallyBlocked path="/admin" view=Home/>
                            <Route
                                ssr=SsrMode::PartiallyBlocked
                                path="/admin/:tenant/default-config"
                                view=DefaultConfig
                            />
                            <Route
                                ssr=SsrMode::PartiallyBlocked
                                path="/admin/:tenant/overrides"
                                view=ContextOverride
                            />
                            <Route
                                ssr=SsrMode::PartiallyBlocked
                                path="admin/:tenant/resolve"
                                view=Home
                            />
                            <Route path="/*any" view=NotFound/>

                        </Routes>
                    </Layout>
                </body>
            </Router>
        </html>
    }
}
