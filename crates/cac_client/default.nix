# Nix for Rust project management
{ inputs, ... }: {
  perSystem = { config, self', pkgs, lib, system, ... }:
    let
      rustToolchain = (pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml).override {
        extensions = [
          "rust-src"
          "rust-analyzer"
          "clippy"
        ];
      };
      craneLib = (inputs.common.inputs.crane.mkLib pkgs).overrideToolchain rustToolchain;
      args = {
        pname = "cac_client";
        src = ./.;
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
      cargoVendorDir = craneLib.vendorCargoDeps {
        src = ./../../.;
      };
      cargoArtifacts = craneLib.buildDepsOnly args;
      package = craneLib.buildPackage (args // {
        inherit cargoArtifacts;
        doCheck = false; # FIXME: tests require services to be running
      });

      check = craneLib.cargoClippy (args // {
        inherit cargoArtifacts;
        cargoClippyExtraArgs = "--all-targets --all-features -- --deny warnings";
      });
    in
    {
      packages.cac_client = package;

      checks.clippy = check;

      #   # Flake outputs
      #   devShells.rust-exp = pkgs.mkShell {
      #     inputsFrom = [
      #       package # Makes the buildInputs of the package available in devShell (so cargo can link against Nix libraries)
      #     ];
      #     shellHook = ''
      #       # For rust-analyzer 'hover' tooltips to work.
      #       export RUST_SRC_PATH="${rustToolchain}/lib/rustlib/src/rust/library";
      #     '';
      #     nativeBuildInputs = with pkgs; [
      #       # Add your dev tools here.
      #       rustToolchain
      #       cargo-watch
      #     ];
      #   };
    };
}
