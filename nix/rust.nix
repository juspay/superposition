{ inputs, ... }:
{
  debug = true;
  perSystem = { config, self', pkgs, lib, system, ... }:
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
        crates = {
          "cac_client" = {
            imports = [ globalCrateConfig ];
            autoWire = true; # Used by Haskell client
            crane = {
              args = {
                buildInputs = lib.optionals isDarwin
                  ([
                    apple_sdk.frameworks.Security
                    pkgs.fixDarwinDylibNames
                  ]) ++ [
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
                buildInputs = lib.optionals isDarwin
                  ([
                    apple_sdk.frameworks.Security
                    pkgs.fixDarwinDylibNames
                  ]) ++ [
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
                buildInputs = lib.optionals isDarwin
                  ([
                    apple_sdk.frameworks.Security
                  ]) ++ [
                  pkgs.openssl
                ];
              };
            };
          };
          "cac_client_integration_example" = {
            imports = [ globalCrateConfig ];
            crane = {
              args = {
                buildInputs = lib.optionals isDarwin
                  ([
                    apple_sdk.frameworks.Security
                  ]) ++ [
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
                buildInputs = lib.optionals isDarwin
                  ([
                    apple_sdk.frameworks.Security
                    apple_sdk.frameworks.SystemConfiguration
                    pkgs.fixDarwinDylibNames
                  ]) ++ [
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
