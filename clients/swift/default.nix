{
  perSystem = { config, pkgs, self', lib, ... }: {
    devShells.swift = let
        compileTest = pkgs.writeShellScriptBin "compileTest" ''
            swiftc \
                swift/main.swift swift/cac.swift swift/exp.swift swift/types.swift swift/utils.swift swift/test.swift \
                -L$SUPERPOSITION_LIB_PATH \
                -I$SUPERPOSITION_INCLUDE_PATH \
                -lcac_client \
                -lexperimentation_client \
                -import-objc-header swift/Bridging-Header.h \
                -o test_client.out
        '';
    in
    pkgs.mkShell {
        name = "superposition-swift-clients";
        buildInputs = with pkgs; [
            swift
            swiftPackages.Foundation
            compileTest
        ];
    };
  };
}
