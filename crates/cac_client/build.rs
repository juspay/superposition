fn main() {
    let crate_dir = std::env!("CARGO_MANIFEST_DIR");
    let mut config: cbindgen::Config = Default::default();
    config.language = cbindgen::Language::C;
    cbindgen::generate_with_config(crate_dir, config)
        .expect("Failed to generate bindings")
        .write_to_file("../../headers/libcac_client.h");
}
