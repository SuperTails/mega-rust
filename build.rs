use std::env;
use std::path::PathBuf;

fn main() {
    println!("cargo:rustc-link-lib=musashi");
    println!("cargo:rustc-link-lib=static=unicorn");
    println!("cargo:rustc-link-search=/usr/lib64");
    println!("cargo:rerun-if-changed=../Musashi/libmusashi.a");

    let bindings = bindgen::Builder::default()
        .header("../Musashi/m68k.h")
        .blacklist_function("m68k_read_memory_(8|16|32)")
        .blacklist_function("m68k_read_disassembler_(8|16|32)")
        .blacklist_function("m68k_write_memory_(8|16|32)")
        .blacklist_function("m68k_read_immediate_(16|32)")
        .blacklist_function("m68k_read_pcrelative_(8|16|32)")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        .generate()
        .expect("Unable to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Failed to write bindings");
}
