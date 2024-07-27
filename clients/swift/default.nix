{
  perSystem = { config, pkgs, self', lib, ... }: {
    devShells.swift = let
        compileTest = pkgs.writeShellScriptBin "compileTest" ''
            swiftc \
                swift/main.swift swift/cac.swift swift/exp.swift swift/types.swift swift/utils.swift swift/test.swift \
                -L../../target/debug \
                -lcac_client \
                -lexperimentation_client \
                -import-objc-header swift/Bridging-Header.h \
                -o test_client.out
        '';

        # TODO: use nix to build module
        # compileExample = pkgs.writeShellScriptBin "compileExample" ''
        #     mkdir example/modules
        #     swiftc cac/cac.swift -L../../target/debug -lcac_client -import-objc-header cac/cac-bridging-header.h -emit-module -emit-module-path example/modules/cac.swiftmodule
        #     swiftc exp/exp.swift -L../../target/debug -lexperimentation_client -import-objc-header exp/exp-bridging-header.h -emit-module -emit-module-path example/modules/exp.swiftmodule
        #     swiftc -I./example/modules example/cac_example.swift example/modules/cac.swiftmodule
        #     swiftc -I./example/modules example/exp_example.swift example/modules/exp.swiftmodule
        # '';
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
