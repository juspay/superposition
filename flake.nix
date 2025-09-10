{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    systems.url = "github:nix-systems/default";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    haskell-flake.url = "github:srid/haskell-flake";
    rust-flake.url = "github:juspay/rust-flake";
  };

  outputs =
    inputs:

    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;

      imports = [
        inputs.haskell-flake.flakeModule
        inputs.rust-flake.flakeModules.default
        inputs.rust-flake.flakeModules.nixpkgs
        inputs.pre-commit-hooks.flakeModule
        ./nix/pre-commit.nix
        ./clients/haskell
        ./nix/rust.nix
        ./nix/om.nix
      ];

      perSystem =
        {
          pkgs,
          self',
          config,
          ...
        }:
        let
          ## In nixpkgs, `gradle` is a wrapped bin & resets the JAVA_HOME env.
          ## So to workaround it using a gradle property to point to the
          ## nix store jdk.
          ## FIXME Figure out a way to avoid this.
          gradle_jdk17 = (
            pkgs.writeShellScriptBin "gradle" "${pkgs.gradle_8}/bin/gradle -Porg.gradle.java.installations.paths=${pkgs.jdk17_headless}/lib/openjdk \${@}"
          );
        in
        {
          formatter = pkgs.nixpkgs-fmt;
          packages.smithy-cli = pkgs.callPackage ./nix/smithy-cli.nix { };
          devShells.default = pkgs.mkShell {
            inputsFrom = [
              # self'.devShells.haskell
              self'.devShells.rust
              config.pre-commit.devShell
            ];
            # Add your devShell tools here
            packages = with pkgs; [
              bun
              self'.packages.smithy-cli
              docker-compose
              gnumake
              # Why do we need this?
              stdenv.cc
              awscli2
              jq
              nodejs_24
              nixpkgs-fmt
              bacon
              cargo-watch
              diesel-cli
              leptosfmt
              wasm-pack
              yq
              jdk21_headless
              gradle_jdk17
              uv
              # go client
              # go
            ];
            shellHook = ''
              export JAVA_HOME=${pkgs.jdk17_headless}/lib/openjdk
            '';
          };
          checks = {
            compile-java = pkgs.writeShellScript "compile-java" ''
              cd clients/java
              ${gradle_jdk17} ffi:build sdk:build open-feature-provider:bulid
            '';
          };
        };
    };
}
