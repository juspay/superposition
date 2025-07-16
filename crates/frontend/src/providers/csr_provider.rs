use derive_more::Deref;
use leptos::*;

#[derive(Deref, Clone, Copy)]
pub struct Csr(bool);

#[component]
pub fn client_side_ready_provider(children: Children) -> impl IntoView {
    let csr_rws = RwSignal::new(Csr(false));
    Effect::new(move |_| csr_rws.set(Csr(true)));
    provide_context(Signal::<Csr>::from(csr_rws));

    children()
}

pub fn use_client_side_ready() -> Signal<Csr> {
    use_context::<Signal<Csr>>().unwrap()
}
