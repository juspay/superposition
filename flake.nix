{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    crane.url = "github:ipetkov/crane";
    crane.inputs.nixpkgs.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";
    rust-overlay = { url = "github:oxalica/rust-overlay"; };
  };

  outputs = inputs@{ self, rust-overlay, flake-parts, crane, nixpkgs, haskell-flake, ... }:

    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ];

      imports = [
        inputs.haskell-flake.flakeModule
        ./clients/hs-cac-client
        ./clients/hs-exp-client
      ];
      perSystem = { config, self', inputs', system, pkgs, lib, haskell-flake, ... }:
        let
          craneLib = crane.lib.${system};
          cac_args = {
            pname = "cac_client";
            src = ./crates/cac_client;
            buildInputs = lib.optionals pkgs.stdenv.isDarwin
              (with pkgs.darwin.apple_sdk.frameworks; [
                Security
                SystemConfiguration
              ]) ++ [
              pkgs.libiconv
              pkgs.openssl
            ];
            nativeBuildInputs = [
              pkgs.pkg-config
            ];
          };
          exp_args = {
            pname = "superposition_client";
            src = ./crates/superposition_client;
            buildInputs = lib.optionals pkgs.stdenv.isDarwin
              (with pkgs.darwin.apple_sdk.frameworks; [
                Security
                SystemConfiguration
              ]) ++ [
              pkgs.libiconv
              pkgs.openssl
            ];
            nativeBuildInputs = [
              pkgs.pkg-config
            ];
          };
          cacCargoArtifacts = craneLib.buildDepsOnly cac_args;
          expCargoArtifacts = craneLib.buildDepsOnly exp_args;
          cac_client = craneLib.buildPackage (cac_args // {
            inherit cacCargoArtifacts;
          });
          superposition_client = craneLib.buildPackage (exp_args // {
            inherit expCargoArtifacts;
          });
        in
        rec {
          _module.args.pkgs = import nixpkgs {
            inherit system;
            overlays = [ rust-overlay.overlay ];

          };
          formatter = pkgs.nixpkgs-fmt;
          # For `nix build` & `nix run`:

          packages.clients = craneLib.buildPackage {
            nativeBuildInputs = with pkgs; [ postgresql_12 iconv darwin.apple_sdk.frameworks.Security ];
            src = craneLib.path ./.; # FIXME: move rust stuff to subdir
          };

          # For `nix develop`:
          devShells.default = pkgs.mkShell {
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
                  pkgs.nodejs_18
                  leptosfmt
                  wasm-pack
                  curl
                  (rust-bin.stable.latest.default.override {
                    extensions = [ "rust-src" ];
                    targets = [ "wasm32-unknown-unknown" ];
                  })
                ];
                darwinPkgs = with pkgs; [
                  darwin.apple_sdk.frameworks.Security
                  darwin.apple_sdk.frameworks.SystemConfiguration
                ];
              in
              univPkgs ++ (if pkgs.stdenv.isDarwin then darwinPkgs else [ ]);
          };
        };
    };
}
