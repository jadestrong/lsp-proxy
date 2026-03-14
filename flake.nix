{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs { inherit system overlays; };
        rustToolchain = pkgs.rust-bin.nightly.latest.default;
      in
      {
        packages.default = pkgs.rustPlatform.buildRustPackage {
          pname = "emacs-lsp-proxy";
          version = "0.7.2";
          src = ./.;
          cargoLock.lockFile = ./Cargo.lock;
          RUSTFLAGS = "-C target-cpu=native";
          buildType = "release";
	  
          meta = with pkgs.lib; {
            description = "An LSP client for Emacs implemented in Rust";
            homepage = "https://github.com/jadestrong/lsp-proxy";
            license = licenses.gpl3Only;
            mainProgram = "emacs-lsp-proxy";
          };
        };
      });
}
