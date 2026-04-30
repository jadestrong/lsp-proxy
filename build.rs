fn main() {
    println!("cargo:rerun-if-changed=proto/rpc.proto");

    // Prefer a user-supplied protoc if $PROTOC is set; otherwise fall back to
    // the bundled binary so the crate builds without a system install.
    if std::env::var_os("PROTOC").is_none() {
        let protoc_path = protoc_bin_vendored::protoc_bin_path()
            .expect("protoc-bin-vendored binary not found");
        std::env::set_var("PROTOC", protoc_path);
    }

    let mut config = prost_build::Config::new();
    config
        .compile_protos(&["proto/rpc.proto"], &["proto"])
        .expect("failed to compile rpc.proto");
}
