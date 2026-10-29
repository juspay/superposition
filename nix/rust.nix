{ inputs, ... }:
{
  debug = true;
  perSystem =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    let
      inherit (pkgs.stdenv) isDarwin;
      globalCrateConfig = {
        crane.clippy.enable = false; # https://github.com/juspay/superposition/issues/19
      };
      v8Target = if isDarwin then "aarch64-apple-darwin" else "x86_64-unknown-linux-gnu";
      v8_lib = pkgs.fetchurl {
        url = "https://github.com/denoland/rusty_v8/releases/download/v142.2.0/librusty_v8_release_${v8Target}.a.gz";
        sha256 =
          if isDarwin then
            "11d3nhqvq9mjx5yby7hzac7h7wcspyf3x9zzpsg3jfzijlrppwjy"
          else
            "1vd3p5g4srr0pl9cm7a7id2w7qfqaxm0pvplycydhk1hixzahyf4";
      };
      v8_binding = pkgs.fetchurl {
        url = "https://github.com/denoland/rusty_v8/releases/download/v142.2.0/src_binding_release_${v8Target}.rs";
        sha256 =
          if isDarwin then
            "1k2bscx0rhqaahvyvm2l58d2iy2phd9zfpic2p6fxnpx8i7qri94"
          else
            "0s883snlshbbkjcgjchckfny709czbjacyfqpcv2zbnnfi25haz4";
      };
      v8_mirror = pkgs.linkFarm "v8-mirror" [
        {
          name = "v142.2.0/librusty_v8_release_${v8Target}.a.gz";
          path = v8_lib;
        }
        {
          name = "v142.2.0/src_binding_release_${v8Target}.rs";
          path = v8_binding;
        }
      ];
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
              )
              || lib.hasSuffix ".conf" path;
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
                  lib.optionals isDarwin [
                    pkgs.fixDarwinDylibNames
                  ]
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
                RUSTY_V8_MIRROR = v8_mirror;
                RUSTY_V8_SRC_BINDING_PATH = "${v8_mirror}/v142.2.0/src_binding_release_${v8Target}.rs";
                buildInputs =
                  lib.optionals isDarwin [
                    pkgs.fixDarwinDylibNames
                  ]
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
                  lib.optionals isDarwin [
                    pkgs.fixDarwinDylibNames
                  ]
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
                  lib.optionals isDarwin [
                    pkgs.fixDarwinDylibNames
                  ]
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
                  lib.optionals isDarwin [
                    pkgs.fixDarwinDylibNames
                  ]
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
                  lib.optionals isDarwin [
                    pkgs.fixDarwinDylibNames
                  ]
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
                  pkgs.pkg-config
                ];
                buildInputs =
                  lib.optionals isDarwin [
                    pkgs.fixDarwinDylibNames
                  ]
                  ++ [
                    pkgs.postgresql_15
                    pkgs.openssl
                  ];
              };
              extraBuildArgs = {
                # https://discourse.nixos.org/t/how-to-use-install-name-tool-on-darwin/9931/2
                postInstall = ''
                  ${if isDarwin then "fixDarwinDylibNames" else ""}
                  mkdir -p $out/include
                  cp target/include/superposition_core.h $out/include/
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
                  lib.optionals isDarwin [
                    pkgs.fixDarwinDylibNames
                  ]
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
                  lib.optionals isDarwin [
                    pkgs.fixDarwinDylibNames
                  ]
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
                buildInputs = [
                  pkgs.openssl
                ];
              };
            };
          };
          "cac_client_integration_example" = {
            imports = [ globalCrateConfig ];
            crane = {
              args = {
                buildInputs = [
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
                RUSTY_V8_MIRROR = v8_mirror;
                RUSTY_V8_SRC_BINDING_PATH = "${v8_mirror}/v142.2.0/src_binding_release_${v8Target}.rs";
                buildInputs =
                  lib.optionals isDarwin [
                    pkgs.fixDarwinDylibNames
                  ]
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
          "superposition_config_file_examples" = {
            crane = {
              args = {
                buildInputs = [
                  pkgs.openssl
                  pkgs.postgresql_15
                ];
              };
            };
          };
        };
      };
    };
}
