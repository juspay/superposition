fn main() {
    // Skip cbindgen generation when building on docs.rs
    // Docs.rs has a sandboxed environment that doesn't allow writing to arbitrary paths
    if std::env::var("DOCS_RS").is_ok() {
        return;
    }

    let crate_dir = std::env!("CARGO_MANIFEST_DIR");
    let mut config: cbindgen::Config = Default::default();
    config.language = cbindgen::Language::C;
    // Parse the source tree directly instead of using `with_crate`, which runs
    // `cargo metadata` over the whole workspace. When this crate is used as a
    // git dependency, that downloads every workspace member's dependencies
    // (server, frontend, deno, ...), which downstream registry proxies may block.
    if let Err(message) = cbindgen::Builder::new()
        .with_config(config)
        .with_src(format!("{crate_dir}/src/lib.rs"))
        .generate()
        .map(|bindings| {
            bindings.write_to_file("../../target/include/superposition_core.h")
        })
    {
        println!("cargo::error={}", message);
    }
}
