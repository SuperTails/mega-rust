use std::env;
use std::path::PathBuf;

fn main() {
    println!("cargo:rustc-link-lib=musashi");
    println!("cargo:rustc-link-lib=static=unicorn");
    println!("cargo:rustc-link-search=/usr/lib64");
    println!("cargo:rerun-if-changed=../Musashi/libmusashi.a");

    let bindings = bindgen::Builder::default()
        .header("../Musashi/m68k.h")
        .header("../Nuked-OPN2/ym3438.h")
        .header("../emu76489/emu76489.h")
        .blacklist_function("m68k_read_memory_(8|16|32)")
        .blacklist_function("m68k_read_disassembler_(8|16|32)")
        .blacklist_function("m68k_write_memory_(8|16|32)")
        // but why tho
        .blacklist_function("SNG_readIO")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        .generate()
        .expect("Unable to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Failed to write bindings");

    cc::Build::new()
        .opt_level(2)
        .file("../Nuked-OPN2/ym3438.c")
        .file("../emu76489/emu76489.c")
        .compile("libs");
}
