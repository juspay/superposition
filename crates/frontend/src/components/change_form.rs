use leptos::*; 

#[component]
pub fn ChangeForm ( 
    title: String , 
    placeholder: String, 
    class: String ,
    read_signal: ReadSignal<String> , 
    write_signal: WriteSignal<String> ,
) -> impl IntoView {
    view! {
        <div class="form-control">
            <label class="label">
                <span class="label-text">{title}</span>
            </label>
            <textarea 
                type="text"
                placeholder={placeholder}
                class={class}
                value={read_signal.get_untracked()}
                on:change={move |ev| {
                    let value = event_target_value(&ev);
                    write_signal.set(value); 
                }}
            />
        </div>
    }
}