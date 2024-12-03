{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
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
          ...
        }:
        {
          formatter = pkgs.nixpkgs-fmt;
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
              llvmPackages.libclang
              xmlsec
              libtool
              # go client
              # go
            ];
            # Set C flags for Rust's bindgen program. Unlike ordinary C
            # compilation, bindgen does not invoke $CC directly. Instead it
            # uses LLVM's libclang. To make sure all necessary flags are
            # included we need to look in a few places.
            shellHook = with pkgs;  ''
              export LIBCLANG_PATH="${llvmPackages.libclang.lib}/lib";
              export BINDGEN_EXTRA_CLANG_ARGS="$(< ${stdenv.cc}/nix-support/libc-crt1-cflags) \
                    $(< ${stdenv.cc}/nix-support/libc-cflags) \
                    $(< ${stdenv.cc}/nix-support/cc-cflags) \
                    $(< ${stdenv.cc}/nix-support/libcxx-cxxflags) \
                    ${lib.optionalString stdenv.cc.isClang "-idirafter ${stdenv.cc.cc}/lib/clang/${lib.getVersion stdenv.cc.cc}/include"} \
                    ${lib.optionalString stdenv.cc.isGNU "-isystem ${stdenv.cc.cc}/include/c++/${lib.getVersion stdenv.cc.cc} -isystem ${stdenv.cc.cc}/include/c++/${lib.getVersion stdenv.cc.cc}/${stdenv.hostPlatform.config} -idirafter ${stdenv.cc.cc}/lib/gcc/${stdenv.hostPlatform.config}/${lib.getVersion stdenv.cc.cc}/include"} \
                  "
            '';
          };
        };
    };
}
