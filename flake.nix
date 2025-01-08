{
  inputs = {
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    services-flake.url = "github:juspay/services-flake";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    old-nixpkgs.url = "github:NixOS/nixpkgs/10b813040df67c4039086db0f6eaf65c536886c6";
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
        inputs.process-compose-flake.flakeModule
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
          process-compose."integration-services" = {
            imports = [
              inputs.services-flake.processComposeModules.default
              ./nix/localstack.nix
            ];
            services.postgres."database" = {
              enable = true;
              initialScript.before = ''
                CREATE USER postgres WITH password 'docker';
              '';
              initialDatabases = [
                {
                  name = "config";
                  schemas = [ ./docker-compose/postgres/db_init.sql ];
                }
              ];
            };
            services.localstack.enable = true;
            settings.processes.test = {

            };
          };
          formatter = pkgs.nixpkgs-fmt;
          packages.static-assets = pkgs.buildNpmPackage {
            name = "static-assets";
            version = "0.0.0";
            src = ./.;
            nativeBuildInputs = with pkgs; [
              tailwindcss
            ];
            makeCacheWritable = true;
            ## We will have to update this whenever we add a new node dependency.
            npmDepsHash = "sha256-nav8cgHvfDNQyUBoFnqiT/YmWBpjdl7/+EZm3CZODW4=";
            nodejs = pkgs.nodejs-18_x;
            buildPhase = ''
              cd crates/frontend
              tailwindcss -i ./styles/tailwind.css -o ../../style.css
              cd -
              mkdir -p $out/static/target/site
              cp -r node_modules $out/static/target
              cp style.css $out/static/target/site
              ## Cargo.toml is needed by leptos.
              cp Superposition.cac.toml Cargo.toml $out/static
            '';
            installPhase = "true";
          };
          packages.image = pkgs.dockerTools.buildImage {
            name = "superposition";
            tag = "latest";

            # What to include in the image
            copyToRoot = pkgs.buildEnv {
              name = "image-root";
              paths = [
                self'.packages.superposition
                self'.packages.frontend
                self'.packages.static-assets
                pkgs.nodejs-18_x
                pkgs.cacert
              ];
              pathsToLink = [
                "/bin"
                "/static"
              ];
            };

            # Configuration
            config = {
              WorkingDir = "/static";
              Cmd = [ "/bin/superposition" ];
              ExposedPorts = {
                "8080/tcp" = { };
              };
              Env = [
                "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
              ];
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
              process-compose
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
              ## For inspecting OCI(docker) images.
              dive
              # go client
              # go
            ];
          };
        };
    };
}
