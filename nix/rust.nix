{ inputs, ... }:
{
  debug = true;
  perSystem =
    {
      config,
      self',
      pkgs,
      lib,
      system,
      ...
    }:
    let
      inherit (pkgs.stdenv) isDarwin;
      inherit (pkgs.darwin) apple_sdk;
      globalCrateConfig = {
        crane.clippy.enable = false; # https://github.com/juspay/superposition/issues/19
      };
    in
    {
      rust-project = {
        # NOTE: It may be possible to obviate much of these buildInputs using
        # https://github.com/juspay/rust-flake/issues/14
        # To avoid unnecessary rebuilds, start from cleaned source, and then add the Nix files necessary to `nix run` it. Finally, add any files required by the Rust build.
        src =
          let
            # Like crane's filterCargoSources, but doesn't blindly include all TOML files!
            filterCargoSources =
              path: type:
              config.rust-project.crane-lib.filterCargoSources path type
              && !(lib.hasSuffix ".toml" path && !lib.hasSuffix "Cargo.toml" path);
          in
          lib.cleanSourceWith {
            src = inputs.self;
            filter = path: type:
              filterCargoSources path type
              ## Include js files from frontend.
              || (lib.hasInfix "frontend/src-js" path && lib.hasSuffix ".js" path);
          };
        crates = {
          "cac_client" = {
            imports = [ globalCrateConfig ];
            autoWire = true; # Used by Haskell client
            crane = {
              args = {
                buildInputs =
                  lib.optionals isDarwin ([
                    apple_sdk.frameworks.Security
                    pkgs.fixDarwinDylibNames
                  ])
                  ++ [
                    pkgs.postgresql_12
                    pkgs.openssl
                  ];
              };
              extraBuildArgs = {
                # https://discourse.nixos.org/t/how-to-use-install-name-tool-on-darwin/9931/2
                postInstall = ''
                  ${if isDarwin then "fixDarwinDylibNames" else ""}
                '';
              };
            };
          };
          "experimentation_client" = {
            autoWire = true; # Used by Haskell client
            crane = {
              args = {
                buildInputs =
                  lib.optionals isDarwin ([
                    apple_sdk.frameworks.Security
                    pkgs.fixDarwinDylibNames
                  ])
                  ++ [
                    pkgs.postgresql_12
                    pkgs.openssl
                  ];
              };
              extraBuildArgs = {
                # https://discourse.nixos.org/t/how-to-use-install-name-tool-on-darwin/9931/2
                postInstall = ''
                  ${if isDarwin then "fixDarwinDylibNames" else ""}
                '';
              };
            };
          };
          "experimentation_example" = {
            crane = {
              args = {
                buildInputs =
                  lib.optionals isDarwin ([
                    apple_sdk.frameworks.Security
                  ])
                  ++ [
                    pkgs.openssl
                  ];
              };
            };
          };
          "cac_client_integration_example" = {
            imports = [ globalCrateConfig ];
            crane = {
              args = {
                buildInputs =
                  lib.optionals isDarwin ([
                    apple_sdk.frameworks.Security
                  ])
                  ++ [
                    pkgs.openssl
                    pkgs.postgresql_12
                  ];
              };
            };
          };
          "superposition" = {
            imports = [ globalCrateConfig ];
            crane = {
              args = {
                buildInputs =
                  lib.optionals isDarwin ([
                    apple_sdk.frameworks.Security
                    apple_sdk.frameworks.SystemConfiguration
                    pkgs.fixDarwinDylibNames
                  ])
                  ++ [
                    pkgs.libiconv
                    pkgs.openssl
                    pkgs.postgresql_12
                  ];
                nativeBuildInputs = with pkgs; [
                  pkg-config
                ];
              };
              extraBuildArgs = {
                # https://discourse.nixos.org/t/how-to-use-install-name-tool-on-darwin/9931/2
                postInstall = ''
                  ${if isDarwin then "fixDarwinDylibNames" else ""}
                '';
              };
            };
          };
        };
      };
    };
}
