{
  perSystem = { config, pkgs, self', lib, ... }: {
    devShells.swift = let
        compileCac = pkgs.writeShellScriptBin "compileCac" ''
            swiftc cac/main.swift -lcac_client -import-objc-header cac/Bridging-Header.h -o cac-swift
        '';
    in
        pkgs.mkShell {
        name = "superposition-swift-clients";
        shellHook = ''
            export LIBRARY_PATH=${self'.packages.superposition}/lib
        '';
        buildInputs = with pkgs; [
            swift
            swiftPackages.Foundation
            compileCac
        ];
        };
  };
}
