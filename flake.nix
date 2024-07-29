{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    systems.url = "github:nix-systems/default";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    zig = {
        url = "github:ziglang/zig";
        inputs.nixpkgs.follows = "nixpkgs";
        };
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:

    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;

      imports = [
        inputs.haskell-flake.flakeModule
        inputs.pre-commit-hooks.flakeModule
        ./nix/pre-commit.nix
        ./clients/haskell
        ./rust.nix
      ];

      perSystem = { pkgs, self', config, ... }: {
        devShells.default = pkgs.mkShell {
          inputsFrom = [
            self'.devShells.rust
            self'.devShells.haskell
            config.pre-commit.devShell
          ];
          packages = with pkgs; [
            docker-compose
            gnumake
            # Why do we need this?
            stdenv.cc
            awscli2
            jq
            nodejs_18
            nixpkgs-fmt
            zig_0_12
            zls
          ];
        };
      };
    };
}
