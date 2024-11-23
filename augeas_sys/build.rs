extern crate bindgen;
extern crate pkg_config;

use std::env;
use std::path::PathBuf;

fn main() {
    let augeas = pkg_config::Config::new()
        .atleast_version("1.13.0")
        .probe("augeas")
        .unwrap();

    let include_paths = augeas
        .include_paths
        .iter()
        .map(|path| format!("-I{}", path.display()));

    let bindings = bindgen::Builder::default()
        .header("wrapper.h")
        .clang_args(include_paths)
        .allowlist_function("aug_.*")
        .allowlist_type("aug_.*")
        .allowlist_var("aug_.*")
        .generate()
        .expect("Unable to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
