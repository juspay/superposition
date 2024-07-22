{
  perSystem = { config, pkgs, self', lib, ... }: {
    devShells.swift = let
        compileCac = pkgs.writeShellScriptBin "compileCac" ''
            swiftc cac/main.swift -L../../target/debug -lcac_client -import-objc-header cac/Bridging-Header.h -o cac-swift
        '';
        compileExp = pkgs.writeShellScriptBin "compileExp" ''
            swiftc exp/main.swift example/exp_example.swift -L../../target/debug -lexperimentation_client -import-objc-header exp/Bridging-Header.h -o exp-swift
        '';
    in
    pkgs.mkShell {
        name = "superposition-swift-clients";
        buildInputs = with pkgs; [
            swift
            swiftPackages.Foundation
            compileCac
            compileExp
        ];
    };
  };
}
