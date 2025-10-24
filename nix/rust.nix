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
              && !(
                lib.hasSuffix ".toml" path
                && !lib.hasSuffix "Cargo.toml" path
                && !lib.hasSuffix "cbindgen.toml" path
              );
          in
          lib.cleanSourceWith {
            src = inputs.self;
            filter =
              path: type:
              filterCargoSources path type
              ## Include js files from frontend.
              || (lib.hasInfix "frontend/src-js" path && lib.hasSuffix ".js" path);
          };
        crates = {
          "superposition_types" = {
            imports = [ globalCrateConfig ];
          };
          "cac_client" = {
            imports = [ globalCrateConfig ];
            autoWire = [
              "crate"
              "clippy"
              ## "doc"
            ]; # Used by Haskell client
            crane = {
              args = {
                buildInputs =
                  lib.optionals isDarwin ([
                    apple_sdk.frameworks.Security
                    apple_sdk.frameworks.SystemConfiguration
                    pkgs.fixDarwinDylibNames
                  ])
                  ++ [
                    pkgs.postgresql_15
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
          "context_aware_config" = {
            imports = [ globalCrateConfig ];
            autoWire = [
              "crate"
              "clippy"
              ## "doc"
            ]; # Used by Haskell client
            crane = {
              args = {
                buildInputs =
                  lib.optionals isDarwin ([
                    apple_sdk.frameworks.Security
                    apple_sdk.frameworks.SystemConfiguration
                    pkgs.fixDarwinDylibNames
                  ])
                  ++ [
                    pkgs.postgresql_15
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
          "service_utils" = {
            imports = [ globalCrateConfig ];
            autoWire = [
              "crate"
              "clippy"
              # ## "doc"
            ]; # Used by Haskell client
            crane = {
              args = {
                buildInputs =
                  lib.optionals isDarwin ([
                    apple_sdk.frameworks.Security
                    apple_sdk.frameworks.SystemConfiguration
                    pkgs.fixDarwinDylibNames
                  ])
                  ++ [
                    pkgs.postgresql_15
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
          "superposition_provider" = {
            imports = [ globalCrateConfig ];
            autoWire = [
              "crate"
              "clippy"
              # ## "doc"
            ];
            crane = {
              args = {
                buildInputs =
                  lib.optionals isDarwin ([
                    apple_sdk.frameworks.Security
                    apple_sdk.frameworks.SystemConfiguration
                    pkgs.fixDarwinDylibNames
                  ])
                  ++ [
                    pkgs.postgresql_15
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
          "experimentation_platform" = {
            imports = [ globalCrateConfig ];
            autoWire = [
              "crate"
              "clippy"
              ## "doc"
            ]; # Used by Haskell client
            crane = {
              args = {
                buildInputs =
                  lib.optionals isDarwin ([
                    apple_sdk.frameworks.Security
                    apple_sdk.frameworks.SystemConfiguration
                    pkgs.fixDarwinDylibNames
                  ])
                  ++ [
                    pkgs.postgresql_15
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
          "frontend" = {
            imports = [ globalCrateConfig ];
            autoWire = [
              "crate"
              "clippy"
              ## "doc"
            ]; # Used by Haskell client
            crane = {
              args = {
                buildInputs =
                  lib.optionals isDarwin ([
                    apple_sdk.frameworks.Security
                    apple_sdk.frameworks.SystemConfiguration
                    pkgs.fixDarwinDylibNames
                  ])
                  ++ [
                    pkgs.postgresql_15
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
          "superposition_core" = {
            imports = [ globalCrateConfig ];
            autoWire = [
              "crate"
              "clippy"
              ## "doc"
            ]; # Used by Haskell client
            crane = {
              args = {
                nativeBuildInputs = [
                  pkgs.rust-cbindgen
                  pkgs.pkg-config
                ];
                buildInputs =
                  lib.optionals isDarwin ([
                    apple_sdk.frameworks.Security
                    apple_sdk.frameworks.SystemConfiguration
                    pkgs.fixDarwinDylibNames
                  ])
                  ++ [
                    pkgs.postgresql_15
                    pkgs.openssl
                  ];
              };
              extraBuildArgs = {
                # https://discourse.nixos.org/t/how-to-use-install-name-tool-on-darwin/9931/2
                postInstall = ''
                  ${if isDarwin then "fixDarwinDylibNames" else ""}
                  ls -la
                  cbindgen --config crates/superposition_core/cbindgen.toml --crate superposition_core --output $out/include/superposition_core.h
                '';
              };
            };
          };
          "superposition_sdk" = {
            imports = [ globalCrateConfig ];
            autoWire = [
              "crate"
              "clippy"
              ## "doc"
            ];
            crane = {
              args = {
                buildInputs =
                  lib.optionals isDarwin ([
                    apple_sdk.frameworks.Security
                    apple_sdk.frameworks.SystemConfiguration
                    pkgs.fixDarwinDylibNames
                  ])
                  ++ [
                    pkgs.postgresql_15
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
            autoWire = [
              "crate"
              "clippy"
              ## "doc"
            ]; # Used by Haskell client
            crane = {
              args = {
                buildInputs =
                  lib.optionals isDarwin ([
                    apple_sdk.frameworks.Security
                    apple_sdk.frameworks.SystemConfiguration
                    pkgs.fixDarwinDylibNames
                  ])
                  ++ [
                    pkgs.postgresql_15
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
                    apple_sdk.frameworks.SystemConfiguration
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
                    apple_sdk.frameworks.SystemConfiguration
                  ])
                  ++ [
                    pkgs.openssl
                    pkgs.postgresql_15
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
                    pkgs.postgresql_15
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
