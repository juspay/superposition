{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nix-community/naersk";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay = { url = "github:oxalica/rust-overlay"; };
  };

  outputs = { self,rust-overlay, flake-utils, naersk, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = (import nixpkgs) {
          inherit system;
          overlays = [ rust-overlay.overlay ];
        };

        naersk' = pkgs.callPackage naersk {};

      in rec {
        # For `nix build` & `nix run`:
        defaultPackage = naersk'.buildPackage {
          nativeBuildInputs = with pkgs; [ postgresql_12 ];
          src = ./.;
        };

        # For `nix develop`:
        devShell = pkgs.mkShell {
          RUST_SRC_PATH = "${pkgs.rust.packages.stable.rustPlatform.rustLibSrc}";
          # bring your local shell properties into nix env
          # shellHook = ''
          #   echo "you are now in the nix shell"
          #   eval $($SHELL)
          #   '';
          nativeBuildInputs =
            let
              univPkgs = with pkgs; [
                  # Build requirements
                  cargo
                  libiconv
                  openssl
                  postgresql_12
                  # Extras
                  rust-analyzer
                  rustfmt
                  bacon
                  cargo-watch
                  clippy
                  diesel-cli
                  docker-compose
                  stdenv.cc
                  pkg-config
                  awscli
                  jq
                  nodejs_18
                  leptosfmt
                  wasm-pack
                  leptosfmt
                  curl
                 ( rust-bin.stable.latest.default.override {
                   extensions = [ "rust-src" ];
                   targets = [ "wasm32-unknown-unknown" ];
                   })
                ];
              darwinPkgs = with pkgs; [
                  darwin.apple_sdk.frameworks.Security
                  darwin.apple_sdk.frameworks.SystemConfiguration
                ];
            in
              univPkgs ++  (if pkgs.stdenv.isDarwin then darwinPkgs else []);
        };
      }
    );
}