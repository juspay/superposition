{ inputs, ... }:
{
  perSystem = { config, self', pkgs, lib, system, ... }:
    let
      rustToolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
      craneLib = (inputs.crane.mkLib pkgs).overrideToolchain rustToolchain;
      craneArgs = {
        pname = "superposition";
        version = "0.0.1";
        src = ./.;
        buildInputs = lib.optionals pkgs.stdenv.isDarwin
          (with pkgs.darwin.apple_sdk.frameworks; [
            Security
            SystemConfiguration
            pkgs.fixDarwinDylibNames
          ]) ++ [
          pkgs.libiconv
          pkgs.openssl
          pkgs.postgresql_12
        ];
        nativeBuildInputs = [
          pkgs.pkg-config
        ];
      };

      cargoArtifacts = craneLib.buildDepsOnly craneArgs;
      package = craneLib.buildPackage (craneArgs // {
        inherit cargoArtifacts;
        # https://discourse.nixos.org/t/how-to-use-install-name-tool-on-darwin/9931/2
        postInstall = ''
          ${if pkgs.stdenv.isDarwin then "fixDarwinDylibNames" else ""}
        '';
      });

      check = craneLib.cargoClippy (craneArgs // {
        inherit cargoArtifacts;
        cargoClippyExtraArgs = "--all-targets --all-features -- --deny warnings";
      });
    in
    {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [ inputs.rust-overlay.overlays.default ];

      };

      packages.superposition = package;

      # TODO: Uncomment after fixing: https://github.com/juspay/superposition/issues/19
      # checks.clippy = check;

      # Flake outputs
      devShells.rust = pkgs.mkShell {
        inputsFrom = [
          package # Makes the buildInputs of the package available in devShell (so cargo can link against Nix libraries)
        ];
        shellHook = ''
          # For rust-analyzer 'hover' tooltips to work.
          export RUST_SRC_PATH="${rustToolchain}/lib/rustlib/src/rust/library";
        '';
        nativeBuildInputs = with pkgs; [
          # Add your dev tools here.
          bacon
          cargo-watch
          diesel-cli
          leptosfmt
          rustToolchain
          wasm-pack
        ] ++ craneArgs.nativeBuildInputs;
      };
    };
}