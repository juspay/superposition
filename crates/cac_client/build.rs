use std::env;

fn main() {
    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let mut config: cbindgen::Config = Default::default();
    config.language = cbindgen::Language::C;
    println!("Calling build.rs in cac_client");
    cbindgen::generate_with_config(&crate_dir, config)
      .unwrap()
      .write_to_file("../../headers/libcac_client.h");
}
