{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nix-community/naersk";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
  };

  outputs = { self, flake-utils, naersk, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = (import nixpkgs) {
          inherit system;
        };

        naersk' = pkgs.callPackage naersk {};

      in rec {
        # For `nix build` & `nix run`:
        defaultPackage = naersk'.buildPackage {
          nativeBuildInputs = with pkgs; [ postgresql_12 ];
          src = ./.;
        };

        # For `nix develop`:
        devShell = pkgs.mkShell {
          nativeBuildInputs = with pkgs;
          [
            # Build requirements
            rustc
            cargo
            libiconv
            openssl
            postgresql_12
            # Extras
            rust-analyzer
            rustfmt
            bacon
            cargo-watch
            diesel-cli
            docker-compose
            stdenv.cc
            # move this to system specific devshell
            darwin.apple_sdk.frameworks.Security
          ];
          # buildInputs = with pkgs; [ ];
        };
      }
    );
}
