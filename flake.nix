{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    haskell-flake.url = "github:srid/haskell-flake";
    rust-flake = {
      url = "github:juspay/rust-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    open-feature-hs = {
      url = "github:juspay/open-feature-haskell-sdk";
      inputs.haskell-flake.follows = "haskell-flake";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
    };
    # Compiler cache for rustc (https://github.com/kunobi-ninja/kache).
    # `stable` tracks released versions.
    kache = {
      url = "github:kunobi-ninja/kache/stable";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs:

    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];

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
          system,
          ...
        }:
        let
          # Skip kache's own test suite: it takes minutes and has sandbox-flaky
          # tests (rustdoc version probe timing out) not yet on its skip list.
          kache = inputs.kache.packages.${system}.default.overrideAttrs (_: {
            doCheck = false;
          });
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
              kache
              self'.packages.smithy-cli
              docker-compose
              gnumake
              # Why do we need this?
              stdenv.cc
              awscli2
              jq
              nodejs_26
              nixpkgs-fmt
              bacon
              cargo-watch
              cargo-edit
              cargo-msrv
              diesel-cli
              leptosfmt
              wasm-pack
              yq
              (gradle-packages.mkGradle {
                version = "8.14.3";
                hash = "sha256-vXEQIhNJMGCVbsIp2Ua+7lcVjb2J0OYrkbyg+ixfNTE=";
                defaultJava = jdk17;
              })
              jdk17
              #uv
              # go client
              # go
            ];

            # Route rustc through kache so repeated builds (worktrees, branch
            # switches) reuse cached compiler output.
            RUSTC_WRAPPER = "${kache}/bin/kache";

            shellHook = ''
                # If it exists from the host system, kill it
                unset DEVELOPER_DIR
            '';
          };

        };
    };
}
