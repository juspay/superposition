{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-wbcli.url = "github:NixOS/nixpkgs/10b813040df67c4039086db0f6eaf65c536886c6";
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
          system,
          ...
        }:
        {
          formatter = pkgs.nixpkgs-fmt;
          packages.default = self'.packages.superposition;
          packages.static-assets = pkgs.buildNpmPackage {
            name = "static-assets";
            version = "1.0.0";
            src = ./.;
            nativeBuildInputs = with pkgs; [
              tailwindcss
            ];
            makeCacheWritable = true;
            ## We will have to update this whenever we add a new node dependency.
            npmDepsHash = "sha256-nav8cgHvfDNQyUBoFnqiT/YmWBpjdl7/+EZm3CZODW4=";
            buildPhase = ''
              cd crates/frontend
              tailwindcss -i ./styles/tailwind.css -o ../../style.css
              cd -
              mkdir -p $out/bin/target/site
              cp -r node_modules $out/bin/target
              cp style.css $out/bin/target/site
              cp Cargo.toml $out/bin

              mkdir -p $out/bin/crates/superposition
              cp crates/superposition/Superposition.cac.toml $out/bin/crates/superposition/
            '';
            installPhase = "true";
          };
          packages.container-image = pkgs.dockerTools.buildImage {
            name = "superposition";
            tag = "latest";

            # What to include in the image
            copyToRoot = pkgs.buildEnv {
              name = "image-root";
              paths = [
                self'.packages.superposition
                self'.packages.frontend
                self'.packages.static-assets
                pkgs.nodejs_20
              ];
              pathsToLink = [ "/bin" ];
            };

            # Configuration
            config = {
              WorkingDir = "/bin";
              Cmd = [ "superposition" ];
              ExposedPorts = {
                "8080/tcp" = { };
              };
            };
          };
          devShells.default = pkgs.mkShell {
            inputsFrom = [
              self'.devShells.rust
              # self'.devShells.haskell
              config.pre-commit.devShell
            ];
            # Add your devShell tools here
            packages = with pkgs; [
              gnumake
              # Why do we need this?
              stdenv.cc
              awscli2
              jq
              nodejs_18
              nixpkgs-fmt
              bacon
              cargo-watch
              diesel-cli
              leptosfmt
              wasm-pack
              tailwindcss
              podman
              podman-compose
              ## For inspecting OCI(docker) images.
              dive
              # go client
              # go
            ];
          };
        };
    };
}
