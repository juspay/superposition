fn main() {
    // Skip cbindgen generation when building on docs.rs
    // Docs.rs has a sandboxed environment that doesn't allow writing to arbitrary paths
    if std::env::var("DOCS_RS").is_ok() {
        return;
    }

    let crate_dir = std::env!("CARGO_MANIFEST_DIR");
    let mut config: cbindgen::Config = Default::default();
    config.language = cbindgen::Language::C;
    if let Err(message) =
        cbindgen::generate_with_config(crate_dir, config).map(|bindings| {
            bindings.write_to_file("../../target/include/superposition_core.h")
        })
    {
        println!("cargo::error={}", message);
    }
}
