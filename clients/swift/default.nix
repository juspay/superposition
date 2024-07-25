{
  perSystem = { config, pkgs, self', lib, ... }: {
    devShells.swift = let
        compileCac = pkgs.writeShellScriptBin "compileCac" ''
            swiftc cac/cac.swift -L../../target/debug -lcac_client -import-objc-header cac/cac-bridging-header.h -o cac-swift.out
        '';
        compileExp = pkgs.writeShellScriptBin "compileExp" ''
            swiftc exp/exp.swift -L../../target/debug -lexperimentation_client -import-objc-header exp/exp-bridging-header.h -o exp-swift.out
        '';
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
            compileCac
            compileExp
            # compileExample
        ];
    };
  };
}
