{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    systems.url = "github:nix-systems/default";
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
        ./clients/haskell
        ./rust.nix
      ];

      perSystem = { pkgs, self', ... }: {
        devShells.default = pkgs.mkShell {
          inputsFrom = [
            self'.devShells.rust
          ];
          packages = with pkgs; [
            docker-compose
            # Why do we need this?
            stdenv.cc
            awscli
            jq
            nodejs_18
            nixpkgs-fmt
          ];
        };
      };
    };
}