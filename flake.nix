{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nix-community/naersk";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
  };

  outputs = { self, flake-utils, naersk, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = (import nixpkgs) {
          inherit system;
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
          nativeBuildInputs =
            let
              univPkgs = with pkgs; [
                  # Build requirements
                  rustc
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
                  pkgconfig
                ];
              darwinPkgs = with pkgs; [
                  darwin.apple_sdk.frameworks.Security
                ];
            in
              univPkgs ++  (if pkgs.stdenv.isDarwin then darwinPkgs else []);
        };
      }
    );
}
