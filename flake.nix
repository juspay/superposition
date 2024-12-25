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
          packages.frontend-assets = pkgs.buildNpmPackage {
            name = "frontend-assets";
            version = "1.0.0";
            src = ./.;
            nativeBuildInputs = with pkgs; [
              tailwindcss
            ];
            makeCacheWritable = true;
            # Will have to update this whenever we add a new dep.
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
          packages.frontend-wasm =
            let
              rustToolchain = pkgs.rust-bin.stable.latest.default.override {
                targets = [ "wasm32-unknown-unknown" ];
              };
              wasm-bg-cli = inputs.nixpkgs-wbcli.legacyPackages.${system}.wasm-bindgen-cli;
            in
            pkgs.rustPlatform.buildRustPackage {
              pname = "frontend-wasm";
              version = "0.1.0";

              src = ./.;
              cargoHash = "sha256-jtBw4ahSl88L0iuCXxQgZVm1EcboWRJMNtjxLVTtzts=";

              cargoLock = {
                lockFile = ./Cargo.lock;
                outputHashes = {
                  "jsonlogic-0.5.3" = "sha256-fU1CuPWQd4wb1O0jLyPHkg/kBRwdsaKT7QTNJ7SIT74=";
                  "monaco-0.5.0" = "sha256-hflU+w4ZyPG12xM1t6fhL7FWP1Ne4U4gb1o9kl1vPYE=";
                };
              };
              nativeBuildInputs = with pkgs; [
                wasm-pack
                rustToolchain
                pkg-config
                wasm-bg-cli
              ];
              buildPhase = ''
                export RUSTUP_TOOLCHAIN="${rustToolchain}"
                ls -la
                cd crates/frontend
                # Create the output directory first
                mkdir -p $out/pkg

                # Use a temporary directory for the build
                export HOME=$TMPDIR
                export RUST_LOG=info

                # Run wasm-pack build with the temporary directory
                wasm-pack build \
                  --locked \
                  --target=web \
                  --mode no-install \
                  --no-default-features --features=hydrate

                # Copy the results to the output directory
                mkdir -p $out/bin/target/site
                cp -r ./pkg $out/bin/target/site
              '';
              installPhase = "true";
              doCheck = false;
              doCrossCheck = false;
            };
          packages.image = pkgs.dockerTools.buildImage {
            name = "superposition";
            tag = "latest";

            # What to include in the image
            copyToRoot = pkgs.buildEnv {
              name = "image-root";
              paths = [
                self'.packages.superposition
                self'.packages.frontend-wasm
                self'.packages.frontend-assets
                pkgs.nodejs_20
                pkgs.coreutils
              ];
              pathsToLink = [ "/bin" ];
            };

            # Configuration
            config = {
              WorkingDir = "/bin";
              Cmd = [ "/bin/superposition" ];
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
              docker-compose
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
              # go client
              # go
            ];
          };
        };
    };
}
