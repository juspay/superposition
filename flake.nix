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
              (callPackage gradle-packages.gradle_8 {
                java = pkgs.jdk17;
              })
              jdk17
              uv
              # go client
              # go
            ];
          };
        };
    };
}
